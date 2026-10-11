"""``modelstats.run_status``: every rule of ``getRunStatus()`` (``03`` section 3.1) on synthetic runs.

The folders are built with the helpers of ``test_facts.py``; every test names the R lines it reproduces
and the register entry where the rule is a recorded quirk. The goldens (``tests/golden/test_status.py``)
pin the same rules on the fixture and synthetic cases; these tests cover the branches no golden reaches
(the ``mag-<i>`` recursion, ``Run restarted``, the pending ``Abort`` shapes, the error order) and the
record contract (absent columns, the D-13 merge, the data frame).
"""

from __future__ import annotations

from collections.abc import Mapping
from pathlib import Path
from typing import Any

import numpy as np
import pytest

from modelstats.env import ClusterPaths, FakeEffects
from modelstats.facts import ARGUMENT_OF_LENGTH_ZERO, MISSING_VALUE_IN_IF, RunReadError
from modelstats.run_status import (
    INCORRECT_DIMENSIONS,
    REPLACEMENT_LENGTH_ZERO,
    RunStatus,
    SanityCounts,
    get_run_status,
    merge_records,
    ordered_directories,
    to_dataframe,
    whole_seconds,
)
from tests.unit.test_facts import (
    ME,
    NOT_ROOT,
    NOW,
    NOW_EPOCH,
    effects,
    make_run,
    remind_cfg,
    remind_stats,
    set_mtime,
    unreadable,
    write_config,
    write_gdx,
    write_stats,
)

HOUR = 3600.0
STATUS_OK = "*** Status: Normal completion\n"
STATUS_ERROR = "*** Status: Execution error(s)\n"
LOOPS = "--- LOOPS iteration = 26\n"
WARNINGS_27 = "There were 27 warnings (use warnings() to see them)\n"
MAGPIE_YML = "title: t\nmodel_name: MAgPIE\ngms:\n  optimization: nlp_apr17\n"


# ---------------------------------------------------------------------------------------------
# Builders
# ---------------------------------------------------------------------------------------------


def remind_run(
    root: Path,
    name: str = "run",
    *,
    full_log: str | None = LOOPS + STATUS_OK,
    log_txt: str | None = WARNINGS_27,
    full_gms: str | None = "cm_iteration_max = 50;\n",
    cfg: Mapping[str, Any] | None = None,
    stats: Mapping[str, Any] | None = None,
    gdx: Mapping[str, float] | None = None,
    repy: Mapping[str, float] | None = None,
    mif: bool = True,
    extra: Mapping[str, str | bytes] | None = None,
) -> Path:
    """A REMIND run folder; ``None`` leaves a file out, ``gdx`` are the scalars of ``fulldata.gdx``."""
    files: dict[str, str | bytes] = dict(extra or {})
    if full_log is not None:
        files["full.log"] = full_log
    if log_txt is not None:
        files["log.txt"] = log_txt
    if full_gms is not None:
        files["full.gms"] = full_gms
    if mif:
        files["REMIND_generic_t.mif"] = "mif"
    run = make_run(root, name, files)
    write_config(run, cfg if cfg is not None else remind_cfg())
    write_stats(run, stats if stats is not None else remind_stats())
    if gdx is not None:
        write_gdx(run / "fulldata.gdx", scalars=gdx, repy=repy)
    return run


def magpie_run(root: Path, name: str = "mag", *, files: Mapping[str, str | bytes] | None = None) -> Path:
    """A MAgPIE run folder: ``config.yml``, a MAgPIE ``runstatistics.rda`` and the given files."""
    run = make_run(root, name, {"config.yml": MAGPIE_YML, **(files or {})})
    write_stats(run, remind_stats(model_name="MAgPIE", modelstat=(2.0, 2.0, 2.0)))
    return run


def running_job(
    run: Path, state: str = "RUNNING", elapsed: str = "1:23:45", user: str = ME
) -> dict[str, str]:
    """A canned ``squeue`` with one job of ``user`` in ``run``."""
    return {"squeue_all.txt": f"{user} {run} {run.name} {elapsed} {state} standby\n"}


def one(root: Path, run: Path, env: FakeEffects | None = None, **kwargs: Any) -> RunStatus:
    """The one record of ``get_run_status([run])``."""
    records = get_run_status([str(run)], effects=env if env is not None else effects(root), **kwargs)
    assert len(records) == 1
    return records[0]


# ---------------------------------------------------------------------------------------------
# Order and identity (R/getRunStatus.R:25-37)
# ---------------------------------------------------------------------------------------------


def test_newest_first_lists_directories_only(tmp_path: Path) -> None:
    older = make_run(tmp_path, "older")
    newer = make_run(tmp_path, "newer")
    set_mtime(older, NOW_EPOCH - 2 * HOUR)
    set_mtime(newer, NOW_EPOCH - HOUR)
    plain = tmp_path / "file.txt"
    plain.write_text("x")
    env = effects(tmp_path)
    paths = [str(older), str(plain), str(newer)]
    assert ordered_directories(paths, "nf", env) == [str(newer), str(older)]
    assert ordered_directories(paths, "none", env) == paths
    assert [r.run for r in get_run_status(paths, effects=env)] == ["newer", "older"]
    assert [r.run for r in get_run_status(paths, sort="none", effects=env)] == ["older", "file.txt", "newer"]


def test_missing_paths_become_na_rows_last(tmp_path: Path) -> None:
    """``file.info()`` of a missing path is an all-NA row; ``a[isdir == TRUE, ]`` keeps it as ``"NA"``."""
    run = make_run(tmp_path, "run")
    env = effects(tmp_path)
    paths = [f"{tmp_path}/missing1", str(run), f"{tmp_path}/missing2"]
    assert ordered_directories(paths, "nf", env) == [str(run), "NA", "NA.1"]
    records = get_run_status(paths, effects=env)
    assert [r.run for r in records] == ["run", "NA", "NA.1"]
    na = records[1]
    assert na.path == "NA" and na.RunType == "NA" and na.RunStatus == "full.log missing"
    assert na.Warnings == "NA" and na.Conv == "NA" and na.Runtime is None and na.sanity is None


def test_a_single_path_and_a_relative_path_are_accepted(tmp_path: Path) -> None:
    make_run(tmp_path, "run")
    env = effects(tmp_path)
    assert one(tmp_path, tmp_path / "run").run == "run"
    assert [r.run for r in get_run_status("run", effects=env)] == ["run"]


def test_duplicate_basenames_merge_into_one_row(tmp_path: Path) -> None:
    """BUG-022 / D-13 (pending): the later row overwrites what it assigns and keeps the rest."""
    with_mif = remind_run(
        tmp_path / "a", "same", gdx={"o_iterationNumber": 26, "o_modelstat": 2, "s80_bool": 1}
    )
    without = remind_run(tmp_path / "b", "same", mif=False, full_log=LOOPS + STATUS_ERROR)
    set_mtime(with_mif, NOW_EPOCH - HOUR / 2)  # newest first: with_mif is read first, without overwrites it
    set_mtime(without, NOW_EPOCH - HOUR)
    records = get_run_status([str(with_mif), str(without)], effects=effects(tmp_path))
    assert len(records) == 1
    record = records[0]
    assert record.run == "same" and record.path == str(without)
    assert record.RunStatus == "Execution error" and record.Mif == "no" and record.Conv == "NA"
    assert record.sanity is not None and record.sanity.summationErrors == 0  # kept from the first row


def test_merge_records_keeps_the_earlier_sanity_block() -> None:
    sanity = SanityCounts(1, 2, 3, None, None, None)
    earlier = RunStatus("r", "/a/r", "no", "nash", "NA", "no", "yes", "1", "s", sanity=sanity)
    later = RunStatus("r", "/b/r", "no", "nash", "NA", "no", "no", "2", "t")
    merged = merge_records(earlier, later)
    assert merged.path == "/b/r" and merged.Mif == "no" and merged.Iter == "2" and merged.sanity is sanity
    assert merge_records(later, earlier).sanity is sanity


# ---------------------------------------------------------------------------------------------
# The record contract (03 section 5)
# ---------------------------------------------------------------------------------------------


def test_brief_record_lacks_the_detailed_columns(tmp_path: Path) -> None:
    run = remind_run(tmp_path)
    brief = one(tmp_path, run, detailed=False)
    assert brief.detailed is False and brief.Warnings is None and brief.Conv is None and brief.sanity is None
    assert list(brief.to_row()) == ["jobInSLURM", "RunType", "modelstat", "Mif", "Iter", "RunStatus"]
    detailed = one(tmp_path, run)
    assert list(detailed.to_row()) == [
        "jobInSLURM",
        "RunType",
        "modelstat",
        "Mif",
        "Iter",
        "RunStatus",
        "Warnings",
        "Conv",
        "Runtime",
        "summationErrors",
        "rangeErrors",
        "fixErrors",
        "missingProjVars",
        "projSummationErrors",
        "projSummationErrorsRegional",
    ]
    assert detailed.cells == detailed.to_row()


def test_run_in_app_results_only_on_the_cluster(tmp_path: Path) -> None:
    run = remind_run(tmp_path)
    assert one(tmp_path, run).runInAppResults is None
    on_cluster = one(tmp_path, run, effects(tmp_path, on_cluster=True))
    assert on_cluster.runInAppResults == "no" and "runInAppResults" in on_cluster.to_row()


def test_to_dataframe_has_run_names_and_r_columns(tmp_path: Path) -> None:
    run = remind_run(tmp_path, gdx={"o_iterationNumber": 26, "o_modelstat": 2, "s80_bool": 1})
    frame = to_dataframe(get_run_status([str(run)], effects=effects(tmp_path)))
    assert list(frame.index) == ["run"] and frame.index.name == "run"
    assert list(frame.columns)[:3] == ["jobInSLURM", "RunType", "modelstat"]
    assert frame.loc["run", "Runtime"] == 8304 and frame.loc["run", "Conv"] == "converged"


# ---------------------------------------------------------------------------------------------
# jobInSLURM, RunType (R/getRunStatus.R:38, 69-74)
# ---------------------------------------------------------------------------------------------


def test_job_in_slurm(tmp_path: Path) -> None:
    run = remind_run(tmp_path)
    assert one(tmp_path, run).jobInSLURM == "NA"
    assert one(tmp_path, run, effects(tmp_path, on_cluster=True)).jobInSLURM == "no"
    assert (
        one(tmp_path, run, effects(tmp_path, on_cluster=True, slurm=running_job(run))).jobInSLURM == "standby"
    )
    other = effects(tmp_path, on_cluster=True, slurm=running_job(run, user="alice"))
    assert one(tmp_path, run, other).jobInSLURM == "alice"
    assert one(tmp_path, run, other, user="alice").jobInSLURM == "standby"


def test_run_type_without_a_config_is_na_and_col_run_type_is_not_called(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run", {"full.log": STATUS_OK, "full.lst": " setGlobal optimization  nash\n"})
    record = one(tmp_path, run)
    assert record.RunType == "NA" and record.Mif == "NA" and record.RunStatus == "Normal completion"


def test_magpie_config_without_optimization_fails_the_assignment(tmp_path: Path) -> None:
    """BUG-046 / D-28: ``colRunType()`` returns ``NULL`` and ``out[i, "RunType"] <- NULL`` errors."""
    run = make_run(tmp_path, "run", {"config.yml": "title: t\nmodel_name: MAgPIE\ngms:\n  foo: 1\n"})
    with pytest.raises(RunReadError, match=REPLACEMENT_LENGTH_ZERO):
        one(tmp_path, run)


# ---------------------------------------------------------------------------------------------
# modelstat (R/getRunStatus.R:82-102)
# ---------------------------------------------------------------------------------------------


def test_modelstat_from_the_gdx_is_explained(tmp_path: Path) -> None:
    run = remind_run(tmp_path, gdx={"o_iterationNumber": 26, "o_modelstat": 2, "s80_bool": 1})
    assert one(tmp_path, run).modelstat == "2: Locally Optimal"
    run = remind_run(tmp_path, "infes", gdx={"o_iterationNumber": 1, "o_modelstat": 13, "s80_bool": 1})
    assert one(tmp_path, run).modelstat == "13: Error No Solution"
    mag = magpie_run(tmp_path, files={"full.log": STATUS_OK})
    write_gdx(mag / "fulldata.gdx", magpie_modelstat=(2, 2, 10))
    assert one(tmp_path, mag).modelstat == "221."  # gsub("0", ".", "2210")


def test_modelstat_falls_back_to_runstatistics(tmp_path: Path) -> None:
    assert one(tmp_path, remind_run(tmp_path, "a")).modelstat == "2: Locally Optimal"
    vector = remind_run(tmp_path, "b", stats=remind_stats(modelstat=(2.0, 2.0)))
    assert one(tmp_path, vector).modelstat == "NA"  # the try() of :94 fails on a length-2 value
    mag = magpie_run(tmp_path, files={"full.log": STATUS_OK})
    assert one(tmp_path, mag).modelstat == "222"
    nothing = remind_run(tmp_path, "c", stats={"config": {"model_name": "REMIND"}})
    assert one(tmp_path, nothing).modelstat == "NA"


def test_modelstat_fallback_aborts_on_a_null_or_na_model_name(tmp_path: Path) -> None:
    """``if (stats$config$model_name == "MAgPIE")`` (``:91``) without ``isTRUE``: the BUG-005 input."""
    null = remind_run(tmp_path, "null", stats=remind_stats(model_name=None))
    with pytest.raises(RunReadError, match=ARGUMENT_OF_LENGTH_ZERO):
        one(tmp_path, null)
    na = remind_run(tmp_path, "na", stats={"config": {"model_name": np.array([None], dtype=object)}})
    with pytest.raises(RunReadError, match=MISSING_VALUE_IN_IF):
        one(tmp_path, na)


# ---------------------------------------------------------------------------------------------
# runInAppResults (R/getRunStatus.R:105-116)
# ---------------------------------------------------------------------------------------------


def test_run_in_app_results(tmp_path: Path) -> None:
    archive = tmp_path / "archive"
    archive.mkdir()
    paths = ClusterPaths(remind_results_archive=f"{archive}/")
    env = effects(tmp_path, on_cluster=True, cluster_paths=paths)
    run = remind_run(tmp_path, stats=remind_stats(run_id=42.0))
    assert one(tmp_path, run, env).runInAppResults == "no"
    (archive / "42.rds").write_text("")
    assert one(tmp_path, run, env).runInAppResults == "yes"  # no overview.rds: vacuous (BUG-010)
    (archive / "overview.rds").write_text("")
    set_mtime(archive / "overview.rds", NOW_EPOCH - 10 * HOUR)
    assert one(tmp_path, run, env).runInAppResults == "no"
    without_id = remind_run(tmp_path, "noid", stats=remind_stats(run_id=None))
    assert one(tmp_path, without_id, env).runInAppResults == "no"


def test_run_in_app_results_for_magpie_uses_the_environment_archive(tmp_path: Path) -> None:
    archive = tmp_path / "magpie-archive"
    archive.mkdir()
    (archive / "7.rds").write_text("")
    env = effects(tmp_path, on_cluster=True, environ={"MAGPIE_RESULTS_ARCHIVE_PATH": str(archive)})
    mag = make_run(tmp_path, "mag", {"config.yml": MAGPIE_YML, "full.log": STATUS_OK})
    write_stats(mag, remind_stats(model_name="MAgPIE", run_id=7.0))
    assert one(tmp_path, mag, env).runInAppResults == "yes"


# ---------------------------------------------------------------------------------------------
# Mif (R/getRunStatus.R:121-136)
# ---------------------------------------------------------------------------------------------


def test_mif_remind(tmp_path: Path) -> None:
    assert one(tmp_path, remind_run(tmp_path, "a")).Mif == "yes"
    assert one(tmp_path, remind_run(tmp_path, "b", mif=False)).Mif == "no"
    errors = remind_run(tmp_path, "c", extra={"REMIND_generic_t_summation_errors.csv": "variable\nX\n"})
    assert one(tmp_path, errors).Mif == "sumErr"


def test_mif_magpie_size_threshold(tmp_path: Path) -> None:
    small = magpie_run(tmp_path, "small", files={"full.log": STATUS_OK, "validation.mif": "x" * 99999})
    assert one(tmp_path, small).Mif == "no"
    big = magpie_run(tmp_path, "big", files={"full.log": STATUS_OK, "validation.mif": "x" * 100000})
    assert one(tmp_path, big).Mif == "yes"


# ---------------------------------------------------------------------------------------------
# Iter (R/getRunStatus.R:141-155, 300)
# ---------------------------------------------------------------------------------------------


def test_iter_takes_the_maximum_from_full_gms_for_autoconverging_nash_runs(tmp_path: Path) -> None:
    assert one(tmp_path, remind_run(tmp_path, "a")).Iter == "26/50"
    plain = remind_run(tmp_path, "b", cfg=remind_cfg(autoconverge=0))
    assert one(tmp_path, plain).Iter == "26/100"
    strings = remind_run(tmp_path, "c", cfg=remind_cfg(iteration_max="1", autoconverge="1"), full_gms=None)
    assert one(tmp_path, strings).Iter == "26/1"
    one_region = remind_run(tmp_path, "d", cfg=remind_cfg(optimization="testOneRegi", iteration_max="1"))
    assert one(tmp_path, one_region).Iter == "26/1"
    no_line = remind_run(tmp_path, "e", full_gms="$setglobal x\n")
    assert one(tmp_path, no_line).Iter == "26"  # character(0): no suffix
    no_max = remind_run(tmp_path, "f", cfg=remind_cfg(iteration_max=None, autoconverge=None))
    assert one(tmp_path, no_max).Iter == "26"


def test_iter_without_loops_or_full_log(tmp_path: Path) -> None:
    assert one(tmp_path, remind_run(tmp_path, "a", full_log=STATUS_OK)).Iter == "NA/50"
    assert one(tmp_path, remind_run(tmp_path, "b", full_log=None)).Iter == "NA"


# ---------------------------------------------------------------------------------------------
# RunStatus (R/getRunStatus.R:152-243)
# ---------------------------------------------------------------------------------------------


def test_status_line_rules(tmp_path: Path) -> None:
    """One line is the status; none or several leave ``"NA"`` (BUG-036) and fall into the interrupt rules."""
    assert one(tmp_path, remind_run(tmp_path, "a")).RunStatus == "Normal completion"
    assert one(tmp_path, remind_run(tmp_path, "b", full_log=STATUS_ERROR)).RunStatus == "Execution error"
    two = remind_run(tmp_path, "c", full_log=STATUS_OK + STATUS_OK)
    assert one(tmp_path, two).RunStatus == "Run interrupted"
    none = remind_run(tmp_path, "d", full_log=LOOPS)
    assert one(tmp_path, none).RunStatus == "Run interrupted"
    long = remind_run(tmp_path, "e", full_log="*** Status: Terminated by user interrupt\n")
    assert one(tmp_path, long).RunStatus == "Terminated by use"


@pytest.mark.parametrize(
    ("line", "label"),
    [
        ("slurmstepd: error: *** JOB 1 ON cs CANCELLED AT 2026 DUE TO TIME LIMIT ***", "Timeout interrupt"),
        ("slurmstepd: error: Detected 1 oom-kill event(s) in StepId=2.batch.", "Memory interrupt"),
        ("slurmstepd: error: *** JOB 3 ON cs CANCELLED AT 2026 DUE TO PREEMPTION ***", "Preempt interrupt"),
        ("slurmstepd: error: *** JOB 4 ON cs CANCELLED AT 2026 DUE TO JOB REQUEUE ***", "Run requeued"),
        ("slurmstepd: error: *** JOB 5 ON cs CANCELLED AT 2026 ***", "Run cancelled"),
        ("slurmstepd: error: execve(): something unexpected", "NA"),
    ],
)
def test_interrupts_on_the_cluster(tmp_path: Path, line: str, label: str) -> None:
    """``:160-175``; the last row is BUG-030 (no recognised error keeps ``"NA"``)."""
    run = remind_run(tmp_path, full_log=LOOPS, log_txt=f"log\n{line}\n")
    assert one(tmp_path, run, effects(tmp_path, on_cluster=True)).RunStatus == label


def test_interrupted_or_restarted_without_log_txt(tmp_path: Path) -> None:
    run = remind_run(tmp_path, full_log=LOOPS, log_txt=None)
    assert one(tmp_path, run, effects(tmp_path, on_cluster=True)).RunStatus == "Run interrupted"
    pending = effects(tmp_path, on_cluster=True, slurm=running_job(run, state="PENDING", elapsed="0:00"))
    assert one(tmp_path, run, pending).RunStatus == "Run restarted"
    with_log = remind_run(tmp_path, "b", full_log=LOOPS, log_txt="no error here\n")
    pending = effects(tmp_path, on_cluster=True, slurm=running_job(with_log, state="PENDING", elapsed="0:00"))
    assert one(tmp_path, with_log, pending).RunStatus == "NA"


def test_run_in_progress_and_conopt_spy(tmp_path: Path) -> None:
    """``:177-186``: the delays of the grid logs and the GDX, R's rounding and ``niceround``."""
    gdx = {"o_iterationNumber": 12, "o_modelstat": 2, "s80_bool": 0}
    run = remind_run(tmp_path, full_log=LOOPS, gdx=gdx, repy={"EUR": 2})
    env = effects(tmp_path, on_cluster=True, slurm=running_job(run))
    assert one(tmp_path, run, env).RunStatus == "Run in progress"  # no grid log
    grid = run / "225a" / "grid1" / "gmsgrid.log"
    grid.parent.mkdir(parents=True)
    grid.write_text("")
    set_mtime(grid, NOW_EPOCH - 0.2 * HOUR)
    set_mtime(run / "fulldata.gdx", NOW_EPOCH - 0.3 * HOUR)
    assert one(tmp_path, run, env).RunStatus == "conoptspy >0.2h"
    set_mtime(grid, NOW_EPOCH - 0.149 * HOUR)  # round(0.149 - 0.049, 1) = 0.1, not > 0.1
    assert one(tmp_path, run, env).RunStatus == "Run in progress"
    set_mtime(grid, NOW_EPOCH - 12.3 * HOUR)
    assert one(tmp_path, run, env).RunStatus == "conoptspy >12h"
    set_mtime(run / "fulldata.gdx", NOW_EPOCH - 0.25 * HOUR)  # gdxdelay > 0.25 is FALSE at equality
    assert one(tmp_path, run, env).RunStatus == "Run in progress"
    later = effects(tmp_path, on_cluster=True, slurm=running_job(run), now=NOW.replace(hour=13))
    assert one(tmp_path, run, later).RunStatus == "conoptspy >13h"


def test_magpie_iteration_status(tmp_path: Path) -> None:
    """``:188-196``: the recursive ``getRunStatus()`` on ``file.path(path_magpie, results_folder)``."""
    magpie = tmp_path / "magpie"
    make_run(magpie / "output", "C_x-mag-3", {"full.log": "--- LOOPS = y2050\n"})
    cfg = remind_cfg(path_magpie=str(magpie), results_folder="output/C_x-mag-3")
    run = remind_run(tmp_path, cfg=cfg, full_log=LOOPS, log_txt="x\nStarting MAgPIE...\n\n")
    env = effects(tmp_path, on_cluster=True, slurm=running_job(run))
    assert one(tmp_path, run, env).RunStatus == "mag-3 y2050"
    # BUG-021: the Nash coupling layout leaves the template in the configuration
    template = remind_cfg(path_magpie=str(magpie), results_folder="output/:title::date:")
    run = remind_run(tmp_path, "nash", cfg=template, full_log=LOOPS, log_txt="Starting MAgPIE...\n")
    env = effects(tmp_path, on_cluster=True, slurm=running_job(run))
    assert one(tmp_path, run, env).RunStatus == "mag-output/:title::date: NA"
    no_path = remind_cfg(results_folder="output/C_x-mag-3")
    run = remind_run(tmp_path, "nopath", cfg=no_path, full_log=LOOPS, log_txt="Starting MAgPIE...\n")
    env = effects(tmp_path, on_cluster=True, slurm=running_job(run))
    assert one(tmp_path, run, env).RunStatus == "mag-3 "  # file.path(NULL, x) is character(0)
    off_cluster = one(tmp_path, run)
    assert off_cluster.RunStatus == "Run interrupted"  # the branch is cluster-only


def test_running_reporting(tmp_path: Path) -> None:
    started = "Starting output generation for t\n"
    run = remind_run(tmp_path, log_txt=started)
    env = effects(tmp_path, on_cluster=True, slurm=running_job(run))
    assert one(tmp_path, run, env).RunStatus == "Running reporting"
    assert one(tmp_path, run, effects(tmp_path, on_cluster=True)).RunStatus == "Normal completion"
    finished = remind_run(tmp_path, "done", log_txt=started + "Finished output generation for t\n")
    env = effects(tmp_path, on_cluster=True, slurm=running_job(finished))
    assert one(tmp_path, finished, env).RunStatus == "Normal completion"


def test_abort_on_consecutive_infeasibilities(tmp_path: Path) -> None:
    """``:210-220``: equality with the threshold (BUG-002, D-10), one region named, several counted."""
    run = remind_run(tmp_path, full_log=STATUS_ERROR)
    write_gdx(run / "abort.gdx", scalars={"cm_abortOnConsecFail": 2}, track={"EUR": {3: 2}, "REF": {3: 1}})
    assert one(tmp_path, run).RunStatus == "Abort EUR 2*Infes"
    write_gdx(run / "abort.gdx", scalars={"cm_abortOnConsecFail": 2}, track={"EUR": {3: 2}, "CHA": {2: 2}})
    assert one(tmp_path, run).RunStatus == "Abort 2R*2*Infes"
    write_gdx(run / "abort.gdx", scalars={"cm_abortOnConsecFail": 2}, track={"EUR": {3: 3}})
    assert one(tmp_path, run).RunStatus == "Execution error"
    write_gdx(run / "abort.gdx", scalars={"cm_abortOnConsecFail": 0}, track={"EUR": {3: 0}})
    assert one(tmp_path, run).RunStatus == "Execution error"
    write_gdx(run / "abort.gdx", scalars={"cm_abortOnConsecFail": 2})
    assert one(tmp_path, run).RunStatus == "Execution error"
    write_gdx(run / "abort.gdx", vector_threshold=[2, 2], track={"EUR": {3: 2}})
    assert one(tmp_path, run).RunStatus == "Execution error"  # isTRUE(c(2, 2) > 0) is FALSE (:214)
    write_gdx(run / "abort.gdx", vector_threshold=[2], track={"EUR": {3: 2}})
    assert one(tmp_path, run).RunStatus == "Abort EUR 2*Infes"  # a one-element vector passes isTRUE
    completed = remind_run(tmp_path, "ok")
    write_gdx(completed / "abort.gdx", scalars={"cm_abortOnConsecFail": 2}, track={"EUR": {3: 2}})
    assert one(tmp_path, completed).RunStatus == "Normal completion"


@NOT_ROOT
def test_unreadable_full_log_is_run_interrupted_like_a_failing_grep(tmp_path: Path) -> None:
    """``:154, 157, 201``: the greps print nothing, the ``try()`` assignments fail, the fallback applies.

    R on a run whose ``full.log`` is ``chmod 000``: two ``grep: ... Permission denied`` lines on stderr,
    then ``Iter "NA/100"`` (``cm_iteration_max`` from the configuration) and ``RunStatus "Run
    interrupted"``; the detailed columns do not abort either.
    """
    run = remind_run(tmp_path, full_gms=None)
    assert one(tmp_path, run, detailed=False).RunStatus == "Normal completion"
    with unreadable(run / "full.log"):
        brief = one(tmp_path, run, detailed=False)
        assert (brief.Iter, brief.RunStatus) == ("NA/100", "Run interrupted")
        assert one(tmp_path, run, detailed=True).RunStatus == "Run interrupted"


def test_run_magpie_overrides_for_an_old_style_coupled_run(tmp_path: Path) -> None:
    """``:222-238`` with ``log-mag.txt``: the loop of the sibling ``full.log``, ``report``, the lock."""
    assert "output" not in str(tmp_path)
    root = tmp_path / "model"
    log_mag = "acquired model lock\n### COUPLING ### Preparing MAgPIE\nStarting MAgPIE run\n"
    run = remind_run(root / "remind" / "output", "C_x-rem-2", full_log=LOOPS, extra={"log-mag.txt": log_mag})
    env = effects(tmp_path, on_cluster=True, slurm=running_job(run))
    assert one(tmp_path, run, env).RunStatus == "Run MAgPIE "  # no sibling full.log: paste with NULL
    make_run(root / "remind" / "magpie" / "output", "C_x-mag-2", {"full.log": "--- LOOPS = y2050\n"})
    assert one(tmp_path, run, env).RunStatus == "Run MAgPIE y2050"
    (run / "log-mag.txt").write_text(log_mag + "Start getReport\n")
    assert one(tmp_path, run, env).RunStatus == "Run MAgPIE report"
    (run / "log-mag.txt").write_text(log_mag + "try to acquire model lock\n")
    assert one(tmp_path, run, env).RunStatus == "Wait MAgPIE lock"
    # tac | grep -m 1 finds one line of each kind wherever they are: once stored, the override is gone
    (run / "log-mag.txt").write_text(log_mag + "MAgPIE output was stored\nPreparing MAgPIE\n")
    assert one(tmp_path, run, env).RunStatus == "Run in progress"
    assert one(tmp_path, run, effects(tmp_path, on_cluster=True)).RunStatus == "NA"  # job "no": no override


def test_run_magpie_override_for_a_completed_run_reads_log_txt(tmp_path: Path) -> None:
    assert "output" not in str(tmp_path)
    run = remind_run(tmp_path / "output", log_txt="Preparing MAgPIE\n")
    env = effects(tmp_path, on_cluster=True, slurm=running_job(run))
    assert one(tmp_path, run, env).RunStatus == "Run MAgPIE "
    # a path without "output": gsub() leaves it alone and the run's own full.log supplies the loop
    own = remind_run(tmp_path, "own", log_txt="Preparing MAgPIE\n")
    env = effects(tmp_path, on_cluster=True, slurm=running_job(own))
    assert one(tmp_path, own, env).RunStatus == "Run MAgPIE 26"
    running = remind_run(tmp_path, "running", full_log=LOOPS, log_txt="Preparing MAgPIE\n")
    env = effects(tmp_path, on_cluster=True, slurm=running_job(running))
    assert one(tmp_path, running, env).RunStatus == "Run in progress"  # not completed, no log-mag.txt


def test_wait_remind_lock_and_full_log_missing(tmp_path: Path) -> None:
    lock = remind_run(tmp_path, "lock", full_log=None, log_txt="x\ntry to acquire model lock\n")
    assert one(tmp_path, lock).RunStatus == "Wait REMIND lock"
    assert one(tmp_path, remind_run(tmp_path, "nolog", full_log=None)).RunStatus == "full.log missing"
    assert (
        one(tmp_path, remind_run(tmp_path, "nothing", full_log=None, log_txt=None)).RunStatus
        == "full.log missing"
    )


# ---------------------------------------------------------------------------------------------
# Warnings (R/getRunStatus.R:256-273)
# ---------------------------------------------------------------------------------------------


def test_warnings_remind(tmp_path: Path) -> None:
    assert one(tmp_path, remind_run(tmp_path, "a")).Warnings == "27"
    block = remind_run(tmp_path, "b", log_txt="Warning messages:\n1: first\n2: second\n")
    assert one(tmp_path, block).Warnings == "2"
    assert one(tmp_path, remind_run(tmp_path, "c", log_txt="clean\n")).Warnings == "0"
    assert one(tmp_path, remind_run(tmp_path, "d", log_txt=None)).Warnings == "NA"
    # BUG-029: the singular form and the "50 or more" summary are not recognised
    singular = remind_run(tmp_path, "e", log_txt="Warning message:\nsomething\n")
    assert one(tmp_path, singular).Warnings == "0"
    fifty = remind_run(
        tmp_path, "f", log_txt="There were 50 or more warnings (use warnings() to see the first 50)\n"
    )
    assert one(tmp_path, fifty).Warnings == "0"


def test_warnings_magpie(tmp_path: Path) -> None:
    block = magpie_run(
        tmp_path, "a", files={"full.log": STATUS_OK, "slurm.log": "Warning messages:\n1: a\n  x\n2: b\n"}
    )
    assert one(tmp_path, block).Warnings == "2"
    single = magpie_run(tmp_path, "b", files={"full.log": STATUS_OK, "slurm.log": "Warning message:\nx\n"})
    assert one(tmp_path, single).Warnings == "1"
    clean = magpie_run(tmp_path, "c", files={"full.log": STATUS_OK, "slurm.log": "fine\n"})
    assert one(tmp_path, clean).Warnings == "0"
    assert one(tmp_path, magpie_run(tmp_path, "d", files={"full.log": STATUS_OK})).Warnings == "NA"
    log_txt = magpie_run(tmp_path, "e", files={"full.log": STATUS_OK, "log.txt": WARNINGS_27})
    assert one(tmp_path, log_txt).Warnings == "NA"  # log.txt counts for REMIND only


# ---------------------------------------------------------------------------------------------
# Conv (R/getRunStatus.R:278-291)
# ---------------------------------------------------------------------------------------------


def test_conv_rules(tmp_path: Path) -> None:
    converged = remind_run(tmp_path, "a", gdx={"o_iterationNumber": 26, "o_modelstat": 2, "s80_bool": 1})
    assert one(tmp_path, converged).Conv == "converged"
    write_gdx(converged / "non_optimal.gdx", scalars={"o_iterationNumber": 3, "o_modelstat": 5})
    assert one(tmp_path, converged).Conv == "converged (had INFES)"
    at_max = remind_run(tmp_path, "b", gdx={"o_iterationNumber": 50, "o_modelstat": 2, "s80_bool": 0})
    assert one(tmp_path, at_max).Conv == "not_converged"
    running = remind_run(
        tmp_path,
        "c",
        gdx={"o_iterationNumber": 12, "o_modelstat": 2, "s80_bool": 0},
        repy={"EUR": 2, "CHA": 4},
    )
    assert one(tmp_path, running).Conv == "042000000000"  # CAZ CHA EUR ...: set order, 0 without a record
    assert one(tmp_path, remind_run(tmp_path, "d")).Conv == "NA"  # no GDX
    one_region = remind_run(
        tmp_path, "e", cfg=remind_cfg(optimization="testOneRegi"), gdx={"o_iterationNumber": 1, "s80_bool": 1}
    )
    assert one(tmp_path, one_region).Conv == "NA"  # not a nash run type
    assert one(tmp_path, converged, detailed=False).Conv is None


def test_conv_aborts_as_r_does(tmp_path: Path) -> None:
    no_flag = remind_run(tmp_path, "a", gdx={"o_iterationNumber": 26, "o_modelstat": 2})
    with pytest.raises(RunReadError, match=ARGUMENT_OF_LENGTH_ZERO):
        one(tmp_path, no_flag)
    # BUG-038 / D-23: a selected GDX without o_iterationNumber and s80_bool == 0
    no_iteration = remind_run(tmp_path, "b", gdx={"o_modelstat": 2, "s80_bool": 0})
    with pytest.raises(RunReadError, match=MISSING_VALUE_IN_IF):
        one(tmp_path, no_iteration)
    no_maximum = remind_run(
        tmp_path,
        "c",
        cfg=remind_cfg(iteration_max=None, autoconverge=None),
        gdx={"o_iterationNumber": 2, "s80_bool": 0},
    )
    with pytest.raises(RunReadError, match=MISSING_VALUE_IN_IF):
        one(tmp_path, no_maximum)
    no_report = remind_run(tmp_path, "d", gdx={"o_iterationNumber": 2, "o_modelstat": 2, "s80_bool": 0})
    with pytest.raises(RunReadError, match=INCORRECT_DIMENSIONS):
        one(tmp_path, no_report)
    assert one(tmp_path, no_report, detailed=False).RunStatus == "Normal completion"  # brief: no Conv


# ---------------------------------------------------------------------------------------------
# Calibration (R/getRunStatus.R:296-304)
# ---------------------------------------------------------------------------------------------


def test_calibration_suffix_and_clb_converged_threshold(tmp_path: Path) -> None:
    """BUG-001 / D-09: ``Clb_converged`` needs more than ten calibration files."""
    log = WARNINGS_27 + "CES calibration iteration: 7\nCES calibration iteration: 10\n"
    gdx = {"o_iterationNumber": 26, "o_modelstat": 2, "s80_bool": 1}
    ten = remind_run(tmp_path, "ten", cfg=remind_cfg(ces="calibrate"), log_txt=log, gdx=gdx)
    for i in range(1, 11):
        (ten / f"fulldata_{i}.gdx").write_text("")
    record = one(tmp_path, ten)
    assert record.RunType == "Calib_nash" and record.Iter == "26/50 Clb: 10" and record.Conv == "converged"
    (ten / "fulldata_11.gdx").write_text("")
    assert one(tmp_path, ten).Conv == "Clb_converged"
    inputs = remind_run(tmp_path, "inputs", cfg=remind_cfg(ces="calibrate"), log_txt=WARNINGS_27, gdx=gdx)
    for i in range(1, 12):
        (inputs / f"input_{i}.gdx").write_text("")
    record = one(tmp_path, inputs)
    assert record.Iter == "26/50" and record.Conv == "Clb_converged"  # no iteration line: no suffix
    assert one(tmp_path, ten, detailed=False).Iter == "26/50"  # brief mode: no suffix


# ---------------------------------------------------------------------------------------------
# Runtime (R/getRunStatus.R:309-313)
# ---------------------------------------------------------------------------------------------


def test_runtime(tmp_path: Path) -> None:
    assert one(tmp_path, remind_run(tmp_path, "a")).Runtime == 8304  # round(8304.5) half to even
    live = remind_run(tmp_path, "b", stats=remind_stats(gams_end=None, prepare_start=NOW_EPOCH - 1000.6))
    assert one(tmp_path, live).Runtime == 1001  # off the cluster "NA" != "no": live runtime
    assert one(tmp_path, live, effects(tmp_path, on_cluster=True)).Runtime is None
    env = effects(tmp_path, on_cluster=True, slurm=running_job(live))
    assert one(tmp_path, live, env).Runtime == 1001
    nothing = remind_run(
        tmp_path, "c", stats=remind_stats(gams_start=None, gams_end=None, prepare_start=None)
    )
    assert one(tmp_path, nothing).Runtime is None
    no_stats = make_run(tmp_path, "d", {"full.log": STATUS_OK})
    assert one(tmp_path, no_stats).Runtime is None


# ---------------------------------------------------------------------------------------------
# Sanity block (R/getRunStatus.R:316-360)
# ---------------------------------------------------------------------------------------------


def test_whole_seconds_rounds_half_to_even() -> None:
    """R 4.6.1: ``round(c(0.5, 1.5, 2.5, 62730.5, -0.5), 0)`` is ``0 2 2 62730 0`` (``:311``)."""
    assert [whole_seconds(x) for x in (0.5, 1.5, 2.5, 62730.5, -0.5, 7.4, 7.6)] == [0, 2, 2, 62730, 0, 7, 8]


def test_sanity_counts(tmp_path: Path) -> None:
    run = remind_run(
        tmp_path,
        extra={
            "REMIND_generic_t_summation_errors.csv": "model,variable,value\na,X,1\na,Y,2\na,X,3\n",
            "REMIND_generic_t_range_errors.txt": "variable;min;max\nA;0;1\n",
            "log_fixOnRef.csv": "variable,region\nV1,EUR\nV2,EUR\n",
        },
    )
    record = one(tmp_path, run)
    assert record.Mif == "sumErr"
    assert record.sanity == SanityCounts(2, 1, 2, None, None, None)
    from modelstats.rdata_io import write_rds

    write_rds(
        run / "projectSummations.rds",
        {"ScenarioMIP": {"missingVars": 2, "checkSummations": 32, "checkSummationsRegional": 0}},
    )
    assert one(tmp_path, run).sanity == SanityCounts(2, 1, 2, 2, 32, 0)
    write_rds(run / "projectSummations.rds", {"Other": {"missingVars": 2}})
    with pytest.raises(RunReadError, match=REPLACEMENT_LENGTH_ZERO):
        one(tmp_path, run)


def test_sanity_block_is_absent_without_mif_or_for_magpie(tmp_path: Path) -> None:
    assert one(tmp_path, remind_run(tmp_path, "a", mif=False)).sanity is None
    assert one(tmp_path, remind_run(tmp_path, "b")).sanity == SanityCounts(0, 0, 0, None, None, None)
    assert one(tmp_path, magpie_run(tmp_path, files={"full.log": STATUS_OK})).sanity is None
    no_config = make_run(tmp_path, "c", {"full.log": STATUS_OK, "REMIND_generic_.mif": "m"})
    assert one(tmp_path, no_config).sanity is None


# ---------------------------------------------------------------------------------------------
# Error order (R reads the GDX selection before the configuration)
# ---------------------------------------------------------------------------------------------


def test_the_first_r_error_wins(tmp_path: Path) -> None:
    run = remind_run(tmp_path, extra={"fulldata.gdx": "not a gdx", "config.Rdata.bak": ""})
    write_gdx(run / "non_optimal.gdx", scalars={"o_iterationNumber": 2})
    with pytest.raises(RunReadError, match="fulldata.gdx"):
        one(tmp_path, run)
    (run / "fulldata.gdx").unlink()
    with pytest.raises(RunReadError, match="invalid 'description' argument"):
        one(tmp_path, run)
