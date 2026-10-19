"""The status of runs: one ``RunStatus`` per run folder, the data behind the ``rs`` table.

R source: R/getRunStatus.R.

Deviations and fixes (details in PORTING-BUGS.md):

- rows are a list of ``RunStatus`` dataclasses instead of a data frame; ``to_dataframe`` builds the
  frame with the R column names. Two folders of the same name no longer overwrite each other
  (BUG-009);
- only existing directories are processed, in mtime order when ``newest_first`` (BUG-010);
- a GDX file that cannot be read never aborts the status: the latest-gdx choice falls back to
  the modification time (BUG-011) and the modelstat/convergence/abort readers treat it as absent;
- the last ``*** Status:`` line of ``full.log`` counts; R silently kept ``NA`` when there were
  several (BUG-012), and its BRE pattern matched any `` Status: `` line (BUG-013);
- the REMIND warning count reads the last ``There were N warnings`` line and accepts ``N or more``
  (BUG-014); numbered warnings are counted across all ``Warning messages:`` blocks, as R did
  (BUG-015);
- missing pieces that made R stop with an error are tolerated: a missing ``timeGAMSStart`` leaves
  the runtime at ``NA`` (BUG-016), an unknown or unusable ``cm_iteration_max`` or a missing
  ``o_iterationNumber`` while ``s80_bool`` is 0 shows the regional modelstats (BUG-017, BUG-023),
  ``NULL`` entries of ``projectSummations.rds`` are ``NA`` (BUG-019), an empty
  ``_range_errors.txt`` counts 0 (BUG-020);
- the csv files of the plausibility checks are read with the ``csv`` module, not with
  ``read.csv2``'s quirks (BUG-045); ``to_dataframe`` always has every column (BUG-046);
- a ``runstatistics.rda`` whose ``config`` has no ``model_name`` follows the REMIND rules where R
  stopped (BUG-056); the recursive status of a coupled run's MAgPIE folder does not follow a
  ``Starting MAgPIE...`` line of its own (BUG-025); a REMIND ``stats$modelstat`` that is ``NA``
  shows ``NA`` where R left the cell blank (a vector kept ``NA`` in R too, BUG-059); the MAgPIE
  log rules apply when the log file is named ``log-mag.txt``, R tested the whole path (BUG-103);
- ``paste("Run MAgPIE", NULL)`` / ``paste0("mag-", i, " ", NULL)`` leave no trailing space (BUG-007).
"""

from __future__ import annotations

import math
import os
import re
from collections.abc import Sequence
from dataclasses import dataclass, fields
from datetime import UTC, datetime
from pathlib import Path
from typing import Any

import pandas as pd

from modelstats import _shell, _timefmt
from modelstats._config import cfg_get, find_config, load_config, r_as_number, r_equals, r_greater_than
from modelstats._gdx import GdxReadError, read_first_found, read_records, read_scalar
from modelstats._rdata import read_rda, read_rds
from modelstats._sorting import existing_dirs
from modelstats._textscan import (
    all_matches,
    count_files,
    csv_line_count,
    csv_unique_values,
    find_all_text,
    last_line,
    last_match,
    last_nonempty_line,
)
from modelstats._timefmt import niceround, r_num_str, r_paste0, r_round, r_str
from modelstats.found_in_slurm import found_in_slurm
from modelstats.run_type import run_type

__all__ = ["EXPLAIN_MODELSTAT", "R_COLUMNS", "RunStatus", "get_run_status", "to_dataframe"]

_MAG_COUPLING = re.compile(r".*-mag-([0-9]{1,2})\Z", re.DOTALL)  # R's sub() on cfg_mag$results_folder
EXPLAIN_MODELSTAT = {
    "1": "Optimal",
    "2": "Locally Optimal",
    "3": "Unbounded",
    "4": "Infeasible",
    "5": "Locally Infes",
    "6": "Intermed Infes",
    "7": "Intermed Nonoptimal",
    "13": "Error No Solution",
}

R_COLUMNS = {
    "job_in_slurm": "jobInSLURM",
    "run_type": "RunType",
    "modelstat": "modelstat",
    "run_in_app_results": "runInAppResults",
    "mif": "Mif",
    "iter": "Iter",
    "run_status": "RunStatus",
    "warnings": "Warnings",
    "conv": "Conv",
    "runtime": "Runtime",
    "summation_errors": "summationErrors",
    "range_errors": "rangeErrors",
    "fix_errors": "fixErrors",
    "missing_proj_vars": "missingProjVars",
    "proj_summation_errors": "projSummationErrors",
    "proj_summation_errors_regional": "projSummationErrorsRegional",
}

_SLURM_ERRORS = (
    ("DUE TO TIME LIMIT", "Timeout interrupt"),
    ("memory|oom-kill", "Memory interrupt"),
    ("DUE TO PREEMPTION", "Preempt interrupt"),
    ("DUE TO JOB REQUEUE", "Run requeued"),
    ("CANCELLED", "Run cancelled"),
)


@dataclass
class RunStatus:
    """The status cells of one run; ``None`` is R's ``NA`` (blank), the string ``"NA"`` prints as ``NA``."""

    name: str
    path: Path
    job_in_slurm: str = "NA"
    run_type: str = "NA"
    modelstat: str = "NA"
    run_in_app_results: str | None = None
    mif: str = "NA"
    iter: str = "NA"
    run_status: str = "NA"
    warnings: str | None = None
    conv: str | None = None
    runtime: float | None = None
    summation_errors: int | None = None
    range_errors: int | None = None
    fix_errors: int | None = None
    missing_proj_vars: int | float | str | None = None
    proj_summation_errors: int | float | str | None = None
    proj_summation_errors_regional: int | float | str | None = None


@dataclass(frozen=True)
class _RunFiles:
    """The files of a run folder the status is read from (``# Define files``)."""

    run_dir: Path
    config: Path | None
    runstatistics: Path
    gdx: Path
    gdx_non_optimal: Path
    full_gms: Path
    full_log: Path
    slurm_log: Path
    log_txt: Path
    log_mag_txt: Path
    abort_gdx: Path
    latest_gdx: Path | None

    @classmethod
    def of(cls, run_dir: Path) -> _RunFiles:
        log_txt = run_dir / "log.txt"
        log_mag_txt = run_dir / "log-mag.txt"
        gdx, gdx_non_optimal = run_dir / "fulldata.gdx", run_dir / "non_optimal.gdx"
        return cls(
            run_dir=run_dir,
            config=find_config(run_dir),
            runstatistics=run_dir / "runstatistics.rda",
            gdx=gdx,
            gdx_non_optimal=gdx_non_optimal,
            full_gms=run_dir / "full.gms",
            full_log=run_dir / "full.log",
            slurm_log=run_dir / "slurm.log",
            log_txt=log_txt,
            log_mag_txt=log_mag_txt if log_mag_txt.exists() else log_txt,
            abort_gdx=run_dir / "abort.gdx",
            latest_gdx=_latest_gdx([gdx, gdx_non_optimal]),
        )


def _latest_gdx(candidates: Sequence[Path]) -> Path | None:
    """The gdx with the highest iteration number (the first of equals), else the newest.

    ``None`` without any gdx, or when every iteration number is ``NA`` (``which.max`` of NAs).
    """
    existing = [path for path in candidates if path.exists()]
    if len(existing) < 2:
        return existing[0] if existing else None
    iterations: list[float] = []
    for path in existing:
        try:
            number = read_scalar(path, "o_iterationNumber")
        except GdxReadError:
            number = None
        if number is None:  # BUG-011: an unreadable gdx or a missing number: the modification time decides
            return max(existing, key=lambda path: path.stat().st_mtime)
        iterations.append(number)
    known = [
        index for index, number in enumerate(iterations) if not math.isnan(number)
    ]  # which.max() skips NA
    if not known:
        return None  # which.max(c(NA, NA)) is integer(0): no latest gdx
    return existing[max(known, key=iterations.__getitem__)]


def _gdx_values(values: Sequence[float]) -> list[float | None]:
    """GDX values for ``r_paste0``: the NA special value (a NaN) prints as ``NA``."""
    return [None if math.isnan(value) else value for value in values]


def _hours_since(path: Path, now: datetime) -> float:
    return (now - datetime.fromtimestamp(path.stat().st_mtime, UTC)).total_seconds() / 3600


def _is_magpie(stats: dict[str, Any] | None) -> bool:
    """``isTRUE(stats$config$model_name == "MAgPIE")``."""
    return r_equals(cfg_get(stats, "config", "model_name"), "MAgPIE")


def _read_stats(files: _RunFiles) -> dict[str, Any] | None:
    """The ``stats`` list of ``runstatistics.rda``, or ``None``."""
    if not files.runstatistics.exists():
        return None
    stats = read_rda(files.runstatistics).get("stats")
    return stats if isinstance(stats, dict) else None


def _modelstat(files: _RunFiles, stats: dict[str, Any] | None) -> str:
    """``# modelstat``: from the latest gdx, else from ``runstatistics.rda``, explained when known."""
    out = "NA"
    if files.latest_gdx is not None:
        try:
            values = read_first_found(files.latest_gdx, ["o_modelstat", "p80_modelstat"])
        except GdxReadError:
            values = None
        if values is not None:
            out = r_paste0(_gdx_values(values)).replace("0", ".")
    if out == "NA" and stats is not None and "config" in stats and "modelstat" in stats:
        modelstat = stats["modelstat"]
        if _is_magpie(stats):
            out = "NA" if modelstat is None else r_paste0(modelstat)  # an entry that exists is NA, not NULL
        elif modelstat is not None and not isinstance(modelstat, list | dict):
            out = r_str(modelstat)
    if out in EXPLAIN_MODELSTAT:
        out = f"{out}: {EXPLAIN_MODELSTAT[out]}"
    return out


def _run_in_app_results(stats: dict[str, Any] | None) -> str:
    """``# runInAppResults``: has the run been added to the appResults overview on the cluster?"""
    if stats is None or stats.get("id") is None:
        return "no"
    if _is_magpie(stats):
        # R: paste0(Sys.getenv(...), "/", id, ".rds"): an unset variable means the root folder
        archive = Path(os.environ.get(_shell.MAGPIE_RESULTS_ARCHIVE_ENV, "") + "/")
    else:
        archive = _shell.REMIND_RESULTS_ARCHIVE
    report = archive / f"{r_str(stats['id'])}.rds"
    overview = archive / "overview.rds"
    if report.exists() and (not overview.exists() or overview.stat().st_mtime + 600 > report.stat().st_mtime):
        return "yes"
    return "no"


def _remind_stem(cfg: dict[str, Any]) -> str:
    """``paste0("REMIND_generic_", cfg$title)``: the stem of the reporting files (``NULL``: nothing)."""
    title = cfg.get("title")
    return f"REMIND_generic_{'' if title is None else r_str(title)}"


def _mif_files(files: _RunFiles, cfg: dict[str, Any]) -> tuple[Path, Path]:
    stem = _remind_stem(cfg)
    return files.run_dir / f"{stem}.mif", files.run_dir / f"{stem}_summation_errors.csv"


def _mif(files: _RunFiles, cfg: dict[str, Any] | None, stats: dict[str, Any] | None) -> str:
    """``# MIF``: is the reporting file there (and free of summation errors)?"""
    if cfg is None:
        return "NA"
    if _is_magpie(stats):
        mif = files.run_dir / "validation.mif"
        return "yes" if mif.exists() and mif.stat().st_size > 99999 else "no"
    mif, summation_errors = _mif_files(files, cfg)
    if not mif.exists():
        return "no"
    return "sumErr" if summation_errors.exists() else "yes"


def _iteration_max(files: _RunFiles, cfg: dict[str, Any] | None, run_type_: str) -> str | None:
    """``# Iter``: ``cm_iteration_max`` from the config, or from ``full.gms`` for autoconverging nash runs."""
    value = cfg_get(cfg, "gms", "cm_iteration_max")
    maximum = None if value is None else r_str(value)
    if (
        r_greater_than(cfg_get(cfg, "gms", "cm_nash_autoconverge"), 0)
        and "nash" in run_type_
        and files.full_gms.exists()
    ):
        line = last_match(files.full_gms, r"cm_iteration_max = [1-9].+;[ ]*$")
        maximum = None if line is None else re.sub(r";[ ]*", "", re.sub(r"^.*= ", "", line, count=1), count=1)
    return maximum


def _slurm_error_status(log_txt: Path) -> str | None:
    """What ``slurmstepd`` wrote into ``log.txt`` when it ended the job."""
    errors = all_matches(log_txt, r"slurmstepd.*error")
    for pattern, status in _SLURM_ERRORS:
        if any(re.search(pattern, error) for error in errors):
            return status
    return None


def _conopt_status(files: _RunFiles, now: datetime) -> str | None:
    """``conoptspy >Nh`` when the CONOPT grid logs stalled while the gdx is older than 15 minutes."""
    grid_logs = list(files.run_dir.glob("225*/grid*/gmsgrid.log"))
    if not grid_logs:
        return None
    newest = max(grid_logs, key=lambda path: path.stat().st_mtime)
    conopt_delay = r_round(_hours_since(newest, now) - 0.049, 1)
    gdx_delay = 1.0
    if files.latest_gdx is not None and files.latest_gdx.exists():
        gdx_delay = _hours_since(files.latest_gdx, now)
    if conopt_delay > 0.1 and gdx_delay > 0.25:
        return f"conoptspy >{niceround(conopt_delay, 1)}h"
    return None


def _magpie_running_status(
    files: _RunFiles, cfg: dict[str, Any] | None, *, user: str, on_cluster: bool, now: datetime
) -> str | None:
    """``mag-<iteration> <MAgPIE Iter>`` while a coupled run's MAgPIE is running."""
    if not files.log_txt.exists() or last_nonempty_line(files.log_txt) != "Starting MAgPIE...":
        return None
    path_magpie = cfg_get(cfg, "path_magpie")
    results_folder = cfg_get(cfg, "cfg_mag", "results_folder")
    coupling = "" if results_folder is None else _MAG_COUPLING.sub(r"\1", str(results_folder))
    status = f"mag-{coupling}"
    # BUG-018: R's getRunStatus() of the MAgPIE folder gave no row (no folder, or a file), an NA row
    # (a missing folder) or the real row; only its Iter is used
    if path_magpie is None or results_folder is None:
        return status
    magpie_dir = Path(
        f"{path_magpie}/{results_folder}"
    )  # file.path(): an absolute results folder is appended
    if not magpie_dir.exists():
        return f"{status} NA"
    if not magpie_dir.is_dir():
        return status
    magpie = _status_of(
        magpie_dir.resolve(), user=user, detailed=True, on_cluster=on_cluster, now=now, follow_magpie=False
    )
    return f"{status} {magpie.iter}"


def _abort_status(files: _RunFiles) -> str | None:
    """``Abort <region> N*Infes`` when ``abort.gdx`` records consecutive infeasibilities."""
    try:
        max_infes = read_scalar(files.abort_gdx, "cm_abortOnConsecFail")
        tracked = read_records(files.abort_gdx, "p80_trackConsecFail")
    except GdxReadError:
        return None
    if max_infes is None or not max_infes > 0 or tracked is None:
        return None
    region_column = "all_regi" if "all_regi" in tracked.columns else tracked.columns[0]
    regions = list(
        dict.fromkeys(
            str(r) for r, v in zip(tracked[region_column], tracked["value"], strict=True) if v == max_infes
        )
    )
    if not regions:
        return None
    where = f"{regions[0]} " if len(regions) == 1 else f"{len(regions)}R*"
    return f"Abort {where}{r_num_str(max_infes)}*Infes"


def _magpie_log_status(files: _RunFiles) -> str | None:
    """``Run MAgPIE <loop>`` / ``Wait MAgPIE lock`` from ``log-mag.txt`` (or ``log.txt``)."""
    status = None
    started = last_match(files.log_mag_txt, "Preparing MAgPIE") is not None
    stored = last_match(files.log_mag_txt, "MAgPIE output was stored") is not None
    if started and not stored:
        full_log = str(files.full_log)
        magpie_log = Path(full_log.replace("output", "magpie/output").replace("-rem-", "-mag-"))
        if not magpie_log.exists():
            magpie_log = Path(full_log.replace("output", "../magpie/output").replace("-rem-", "-mag-"))
        loop = None
        if magpie_log.exists():
            line = last_match(magpie_log, "LOOPS")
            loop = None if line is None else re.sub(r"^.*= ", "", line, count=1)
        if last_match(files.log_mag_txt, "Start getReport") is not None:
            loop = "report"
        status = "Run MAgPIE" if loop is None else f"Run MAgPIE {loop}"
    tail = last_line(files.log_mag_txt)
    if tail is not None and "try to acquire model lock" in tail:
        status = "Wait MAgPIE lock"
    return status


def _run_status(
    row: RunStatus,
    files: _RunFiles,
    *,
    cfg: dict[str, Any] | None,
    iteration_max: str | None,
    on_cluster: bool,
    now: datetime,
    user: str,
    follow_magpie: bool,
) -> None:
    """``# RunStatus`` (and ``Iter``): the state of the GAMS run and its surroundings."""
    if not files.full_log.exists():
        tail = last_line(files.log_txt) if files.log_txt.exists() else None
        row.run_status = (
            "Wait REMIND lock"
            if tail is not None and "try to acquire model lock" in tail
            else "full.log missing"
        )
        return
    line = last_match(files.full_log, "LOOPS")
    if line is not None:
        row.iter = re.sub(r"^.*= ", "", line, count=1)
    if iteration_max is not None:
        row.iter = f"{row.iter}/{iteration_max}"
    statuses = all_matches(files.full_log, r"\*\*\* Status: ")
    if statuses:  # BUG-012: the last line; R's substr(..., 1, 17)
        row.run_status = statuses[-1].replace("*** Status: ", "", 1).replace("(s)", "", 1)[:17]
    in_slurm = row.job_in_slurm
    if on_cluster and row.run_status == "NA":
        if in_slurm == "no" or in_slurm.endswith("pending"):
            if files.log_txt.exists():
                row.run_status = _slurm_error_status(files.log_txt) or row.run_status
            else:
                row.run_status = "Run interrupted" if in_slurm == "no" else "Run restarted"
        else:
            row.run_status = "Run in progress"
            row.run_status = _conopt_status(files, now) or row.run_status
            if follow_magpie:
                magpie = _magpie_running_status(files, cfg, user=user, on_cluster=on_cluster, now=now)
                row.run_status = magpie or row.run_status
    elif row.run_status == "NA":
        row.run_status = "Run interrupted"
    if row.run_status == "Normal completion" and files.log_txt.exists() and in_slurm != "no":
        started = last_match(files.log_txt, "Starting output generation for") is not None
        finished = last_match(files.log_txt, "Finished output generation for") is not None
        if started and not finished:
            row.run_status = "Running reporting"
    if row.run_status == "Execution error" and files.abort_gdx.exists():
        row.run_status = _abort_status(files) or row.run_status
    is_mag_log = files.log_mag_txt.name == "log-mag.txt"
    if (
        files.log_mag_txt.exists()
        and in_slurm != "no"
        and (row.run_status == "Normal completion" or is_mag_log)
    ):
        row.run_status = _magpie_log_status(files) or row.run_status


def _warnings(files: _RunFiles, stats: dict[str, Any] | None) -> str:
    """``# Warnings``: MAgPIE counts the warnings of ``slurm.log``, REMIND those of ``log.txt``."""
    magpie = _is_magpie(stats)
    if files.slurm_log.exists() and magpie:
        blocks = find_all_text(files.slurm_log, r"Warning messages:\n([0-9]+:(.*\n)?.*\n)*([0-9]+)")
        if blocks:
            return blocks[-1].group(3)
        return "1" if last_match(files.slurm_log, "Warning message:") is not None else "0"
    if files.log_txt.exists() and not magpie:
        totals = find_all_text(files.log_txt, r"There were ([0-9]+)( or more)? warnings")
        if totals:
            return totals[-1].group(1)
        blocks = find_all_text(files.log_txt, r"Warning messages:\n([0-9]+:(.*\n)?.*\n)*")
        return str(sum(len(re.findall(r"^[0-9]+:", block.group(0), flags=re.MULTILINE)) for block in blocks))
    return "NA"


def _conv(files: _RunFiles, latest_gdx: Path, iteration_max: str | None) -> str:
    """``# Conv``: converged / not_converged / the modelstats of all regions, from the latest gdx."""
    try:
        s80_bool = read_scalar(latest_gdx, "s80_bool")
        iteration = read_scalar(latest_gdx, "o_iterationNumber")
    except GdxReadError:
        return "NA"
    if s80_bool is None or math.isnan(s80_bool):
        return "NA"  # BUG-023: R stopped on if (NA)
    if s80_bool == 1:
        return "converged (had INFES)" if files.gdx_non_optimal.exists() else "converged"
    maximum = None if iteration_max is None else r_as_number(iteration_max)
    if s80_bool == 0 and maximum is not None and iteration is not None and maximum == iteration:
        return "not_converged"
    try:
        repy = read_records(latest_gdx, "p80_repy")
    except GdxReadError:
        return "NA"
    if repy is None:
        return ""  # BUG-101: readGDX() gave NULL with a warning, paste() of it is ""
    info_column = "solveinfo80" if "solveinfo80" in repy.columns else repy.columns[-2]  # R: [, , "modelstat"]
    modelstats = repy[repy[info_column] == "modelstat"]["value"]
    return r_paste0(_gdx_values([float(value) for value in modelstats]))


def _calibration(row: RunStatus, files: _RunFiles) -> None:
    """``# Calib Iter``: the CES calibration iteration and ``Clb_converged``."""
    chunks = [
        c
        for line in all_matches(files.log_txt, "CES calibration iteration")
        for c in re.findall(r"[0-9]{1,2}", line)
    ]
    if chunks and float(chunks[-1]) > 0:
        row.iter = f"{row.iter} Clb: {chunks[-1]}"
    if row.conv in ("converged", "converged (had INFES)") and (
        count_files(files.run_dir, "fulldata_*.gdx") > 10 or count_files(files.run_dir, "input_*.gdx") > 10
    ):
        row.conv = "Clb_converged"


def _runtime(job_in_slurm: str, stats: dict[str, Any] | None, now: datetime) -> float | None:
    """``# Runtime`` in seconds: GAMS start to end, or prepare start until now while the job runs."""
    if stats is None:
        return None
    if "timeGAMSEnd" in stats:
        end, start = stats.get("timeGAMSEnd"), stats.get("timeGAMSStart")
        if isinstance(end, datetime) and isinstance(start, datetime):
            return r_round((end - start).total_seconds())
        return None
    if "timePrepareStart" in stats and job_in_slurm != "no":
        start = stats.get("timePrepareStart")
        if isinstance(start, datetime):
            return r_round((now - start).total_seconds())
    return None


def _as_scalar(value: Any) -> int | float | str | None:
    """A scalar of ``projectSummations.rds`` as stored (R printed it as is, ``TRUE`` included).

    A one-item list, named or not, holds its item (R stored it as the value); ``NULL`` (R: an
    error), a vector of several values (R: an error) and a list of several items (R: the first,
    with a warning) are ``NA``; a factor of length one is its label where R printed its code
    (BUG-019).
    """
    if isinstance(value, list | dict) and len(value) == 1:
        value = next(iter(value.values())) if isinstance(value, dict) else value[0]
    return value if isinstance(value, int | float | str) else None


def _plausibility(row: RunStatus, files: _RunFiles, cfg: dict[str, Any]) -> None:
    """``# additional info on plausibility checks in REMIND runs``: the sanity columns."""
    mif, summation_errors = _mif_files(files, cfg)
    if not mif.exists():
        return
    row.summation_errors = csv_unique_values(summation_errors, "variable") if summation_errors.exists() else 0
    range_errors = files.run_dir / f"{_remind_stem(cfg)}_range_errors.txt"
    row.range_errors = csv_line_count(range_errors, ";") if range_errors.exists() else 0
    fix_errors = files.run_dir / "log_fixOnRef.csv"
    row.fix_errors = csv_unique_values(fix_errors, "variable") if fix_errors.exists() else 0
    summations = files.run_dir / "projectSummations.rds"
    if summations.exists():
        scenario_mip = cfg_get(read_rds(summations), "ScenarioMIP")
        row.missing_proj_vars = _as_scalar(cfg_get(scenario_mip, "missingVars"))
        row.proj_summation_errors = _as_scalar(cfg_get(scenario_mip, "checkSummations"))
        row.proj_summation_errors_regional = _as_scalar(cfg_get(scenario_mip, "checkSummationsRegional"))


def _status_of(
    run_dir: Path, *, user: str, detailed: bool, on_cluster: bool, now: datetime, follow_magpie: bool = True
) -> RunStatus:
    """One iteration of the R ``for (i in mydir)`` loop.

    ``follow_magpie=False`` is the recursive call for a coupled run's MAgPIE folder: it does not
    follow a ``Starting MAgPIE...`` line again, so a folder pointing at itself cannot loop.
    """
    row = RunStatus(name=run_dir.name, path=run_dir)
    row.job_in_slurm = found_in_slurm(run_dir, user) if on_cluster else "NA"
    files = _RunFiles.of(run_dir)
    cfg = load_config(files.config) if files.config is not None else None
    row.run_type = run_type(run_dir, cfg) if cfg is not None else "NA"
    stats = _read_stats(files)
    row.modelstat = _modelstat(files, stats)
    if on_cluster:
        row.run_in_app_results = _run_in_app_results(stats)
    row.mif = _mif(files, cfg, stats)
    iteration_max = _iteration_max(files, cfg, row.run_type)
    _run_status(
        row,
        files,
        cfg=cfg,
        iteration_max=iteration_max,
        on_cluster=on_cluster,
        now=now,
        user=user,
        follow_magpie=follow_magpie,
    )
    if not detailed:
        return row
    row.warnings = _warnings(files, stats)
    row.conv = "NA"
    if "nash" in row.run_type and files.latest_gdx is not None:
        row.conv = _conv(files, files.latest_gdx, iteration_max)
    calibrating = "Calib" in row.run_type or r_equals(cfg_get(cfg, "gms", "CES_parameters"), "calibrate")
    if calibrating and files.log_txt.exists():
        _calibration(row, files)
    row.runtime = _runtime(row.job_in_slurm, stats, now)
    if cfg is not None and not _is_magpie(stats):
        _plausibility(row, files, cfg)
    return row


def get_run_status(
    dirs: Path | Sequence[Path],
    *,
    newest_first: bool = True,
    user: str | None = None,
    detailed: bool = True,
) -> list[RunStatus]:
    """Returns the current status of a run or a vector of runs.

    Args:
        dirs: Path(s) to the folder(s) where the run(s) is (are) performed. Paths that are not
            directories are skipped.
        newest_first: Order the runs by modification time, newest first (R: ``sort = "nf"``).
        user: The user whose runs will be shown (default: the current user).
        detailed: If False, avoids costly information gathering and returns less info (the
            warnings, convergence, runtime and sanity columns stay ``None``).

    Returns:
        One ``RunStatus`` per existing directory, in the requested order.

    Raises:
        ModelstatsError: A configuration, ``runstatistics.rda``, ``projectSummations.rds`` or csv
            file of a run cannot be read (``ConfigReadError``, ``RDataReadError``, ...). GDX files
            that cannot be read never raise: their cells stay ``NA``.
        OSError: A file of a run cannot be opened.
    """
    if user is None:
        user = _shell.current_user()
    paths = [dirs] if isinstance(dirs, Path) else list(dirs)
    run_dirs = existing_dirs([path.resolve() for path in paths], newest_first=newest_first)
    on_cluster = _shell.on_cluster()
    now = _timefmt.now()
    return [
        _status_of(run_dir, user=user, detailed=detailed, on_cluster=on_cluster, now=now)
        for run_dir in run_dirs
    ]


def to_dataframe(rows: Sequence[RunStatus]) -> pd.DataFrame:
    """The status table as R returned it: one row per run (index: folder name), R column names.

    Every column is present (``None`` where R had no column: ``runInAppResults`` off the cluster,
    the detailed columns with ``detailed=False``, the plausibility columns without a mif).
    """
    columns = [field.name for field in fields(RunStatus) if field.name in R_COLUMNS]
    frame = pd.DataFrame(
        [[getattr(row, column) for column in columns] for row in rows],
        columns=[R_COLUMNS[c] for c in columns],
    )
    frame.index = pd.Index([row.name for row in rows])
    return frame
