"""The automated model tests (AMT): start the test runs, later evaluate and report them.

R source: R/modeltests.R.

Deviations and fixes (details in PORTING-BUGS.md):

- no ``chdir``: every path is derived from ``AmtPaths`` and every program gets its ``cwd``; the
  state that R kept in ``lastcommit.rds``, ``runsToStart.rds`` and ``gRS.rds`` lives in
  ``amt-state.json`` (read from the R files when the JSON does not exist yet); ``runcode.rds`` is
  still written for the R-side readers (BUG-065);
- ``.testsstatus`` is the parent folder's file in every step; R built its path once as
  ``paste0(mydir, "../.testsstatus")`` (BUG-066); its first line decides (BUG-091); two
  ``modeltests`` runs exclude each other through a lock file;
- the model's git update and the ``Rscript start.R`` calls abort the step when they fail; the
  other programs (MAgPIE's git, ``make``, ``rsync``, ``sbatch``, the report repository's git) are
  logged and the step goes on; R ignored every exit status (BUG-067); the output of the programs
  goes to the log (BUG-081);
- the Mattermost message is sent as a JSON document; R's ``curl -d '{"text": "..."}'`` broke on
  quotes in the message (BUG-068); the webhook URL comes from ``--mattermost-url`` or
  ``AMT_MATTERMOST_URL`` and is never echoed;
- a converged run without an earlier run of its scenario is not compared (R went on with
  ``max(character(0))`` and stopped at ``archive/NA``, BUG-069); an earlier run found in
  ``archive/`` keeps its name in the
  compareScenarios job (BUG-082); a run whose status cannot be read is skipped when the table of
  known runs is extended, and gets a README line and a summary sentence when it is one of the
  started runs; a table of the Mattermost message that fails on such a run becomes an ``Error:``
  block (BUG-055, BUG-070); the table of known runs never shrinks, so archived runs stay
  comparable (BUG-102); the runs are listed in the package's run order (BUG-054); the runcode and
  ``cfg$title`` are Python regular expressions (BUG-072);
- the ``[ FAIL`` lines of ``test-full.log`` are judged as R did: only a single line counts (BUG-071);
- a missing ``git log``, runcode or last tested commit stops the step with a message (BUG-083);
  the MAgPIE start really waits for the default run (BUG-084); the evaluation waits for the jobs
  inside the model folder (BUG-085) and takes the dates of the MAgPIE window and the archive
  cut-off after that wait (BUG-086); ``archive/`` is created, runs without a date are skipped
  (BUG-087); the data changelog is copied once, as R's ``file.copy()`` did (BUG-088); the sbatch
  command line is echoed on stdout as R's ``cat()`` wrote it (BUG-089); a missing user is the
  current user (BUG-090); the Mattermost tables show no ``[1] "No runs found"`` (BUG-041) and
  absolute paths in skipped lines (BUG-092);
  an earlier run that is gone from disk is not compared (BUG-095); the README always has the
  cluster's columns (BUG-096); a second ``modeltests`` call waits for no one, it does nothing
  (BUG-098); ``selectScenarios()`` and ``addToDataChangelog()`` run in ``Rscript`` bridges
  (BUG-099); publishing without a report repository stops before the wait (BUG-100);
- ``message()`` output goes through ``logging`` on stderr.
"""

from __future__ import annotations

import fcntl
import json
import logging
import os
import re
import shlex
import shutil
import subprocess
import tempfile
from collections.abc import Generator, Mapping, Sequence
from contextlib import contextmanager
from dataclasses import dataclass, field
from datetime import date, datetime, timedelta
from pathlib import Path
from typing import Annotated, Any, Literal, overload

import typer
from rich.text import Text

from modelstats import _console, _shell, _timefmt
from modelstats._config import load_config
from modelstats._rdata import RDataReadError, read_rda, read_rds_frame, read_rds_string, write_rds_string
from modelstats._sorting import run_sort_key, subdirs
from modelstats._textscan import compile_pattern
from modelstats._timefmt import format_runtime
from modelstats.errors import ModelstatsError
from modelstats.loop_runs import print_run_table
from modelstats.print_output import CLUSTER_COLUMNS, format_row
from modelstats.run_status import RunStatus, get_run_status
from modelstats.sanity_checks import print_sanity_checks

__all__ = [
    "AmtPaths",
    "AmtState",
    "Model",
    "app",
    "delete_empty_realization_folders",
    "evaluate_runs",
    "modeltests",
    "notify_mattermost",
    "start_runs",
]

log = logging.getLogger(__name__)

Model = Literal["REMIND", "MAgPIE"]

MATTERMOST_URL_ENV = "AMT_MATTERMOST_URL"
SQUEUE_FORMAT = "%i %q %T %C %M %j %V %L %e %Z"
RSE_GDX = "rse@rse.pik-potsdam.de:/webservice/data/example/remind2_test-convGDX2MIF_SSP2-NPi-AMT.gdx"
ARCHIVE_AFTER_DAYS = 90
_CONVERGED = ("converged", "converged (had INFES)")
_TIMESTAMP = re.compile(r"_[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}\.[0-9]{2}\.[0-9]{2}")
_DATE_IN_NAME = re.compile(r".*_([0-9]{4}-[0-9]{2}-[0-9]{2})_.*$")
_MODULE_PREFIX = re.compile(r"^.*.modules/[0-9a-zA-Z_]{1,}/")
_REALIZATION_SUFFIX = re.compile(r'/realization.gms"$')
_README_TITLES = (
    "Run                                           ",
    "Runtime    ",
    "",
    "RunType    ",
    "RunStatus         ",
    "Warnings ",
    "Iter            ",
    "Conv                 ",
    "modelstat          ",
    "Mif   ",
    "AppResults",
)
_README_WIDTHS = (*(len(title) for title in _README_TITLES[:-1]), 3)

# R expressions of the AMT, run with ``Rscript -e``; values reach them as command line arguments
_SCENARIO_PREFIX = "AMT-SCENARIO "
_SELECT_SCENARIOS_R = (
    'settings <- read.csv2("config/scenario_config.csv", stringsAsFactors = FALSE, row.names = 1, '
    'comment.char = "#", na.strings = ""); '
    'invisible(sapply(list.files("scripts/start", pattern = "\\\\.R$", full.names = TRUE), source)); '
    'runsToStart <- selectScenarios(settings = settings, interactive = FALSE, startgroup = "AMT"); '
    f'writeLines(c("", paste0("{_SCENARIO_PREFIX}", row.names(runsToStart), recycle0 = TRUE)))'
)
_ADD_TO_CHANGELOG_R = (  # arguments: the run folder, the changelog file, the version id
    "a <- commandArgs(trailingOnly = TRUE); "
    'magpie4::addToDataChangelog(report = readRDS(file.path(a[[1]], "report.rds")), '
    "changelog = a[[2]], versionId = a[[3]])"
)


@dataclass(frozen=True)
class AmtPaths:
    """Every folder and file the AMT touches, derived from the (absolute) model folder."""

    model_dir: Path
    report_repo: Path | None = None

    def __post_init__(self) -> None:
        """Absolute folders: the wait for the runs matches them against squeue's work dirs."""
        object.__setattr__(self, "model_dir", self.model_dir.resolve())
        if self.report_repo is not None:
            object.__setattr__(self, "report_repo", self.report_repo.resolve())

    @property
    def output_dir(self) -> Path:
        """The run folders of the model."""
        return self.model_dir / "output"

    @property
    def archive_dir(self) -> Path:
        """Where runs older than ``ARCHIVE_AFTER_DAYS`` go."""
        return self.output_dir / "archive"

    @property
    def status_file(self) -> Path:
        """``.testsstatus`` next to the model folder: what the next cron call has to do."""
        return self.model_dir.parent / ".testsstatus"

    @property
    def state_file(self) -> Path:
        """``amt-state.json``: runcode, last tested commit, planned runs, known run results."""
        return self.model_dir / "amt-state.json"

    @property
    def lock_file(self) -> Path:
        """The lock that keeps two ``modeltests`` calls apart."""
        return self.model_dir / ".modeltests.lock"

    @property
    def runcode_rds(self) -> Path:
        """``runcode.rds``, still read by the R tools."""
        return self.model_dir / "runcode.rds"

    @property
    def magpie_dir(self) -> Path:
        """The coupled MAgPIE checkout inside the REMIND folder."""
        return self.model_dir / "magpie"

    @property
    def test_full_log(self) -> Path:
        """Where ``make test-full-slurm`` writes its log."""
        return self.model_dir / "test-full.log"


@dataclass
class AmtState:
    """The AMT bookkeeping, stored as ``amt-state.json``.

    ``runs`` maps a run folder name to its ``Conv`` and ``Mif`` cells (R's ``gRS.rds``).
    """

    runcode: str | None = None
    last_commit: str | None = None
    runs_to_start: list[str] = field(default_factory=list)
    runs: dict[str, dict[str, str | None]] = field(default_factory=dict)

    @classmethod
    def load(cls, paths: AmtPaths) -> AmtState:
        """The state from ``amt-state.json``, or from the R files of an older checkout.

        Raises:
            ModelstatsError: The JSON file cannot be read or is not the expected document.
            RDataReadError: One of the R-era files is unusable.
        """
        if not paths.state_file.exists():
            return cls._from_r_files(paths)
        try:
            data = json.loads(paths.state_file.read_text(encoding="utf-8"))
        except (OSError, ValueError) as error:
            raise ModelstatsError(f"{paths.state_file}: not a valid JSON state file: {error}") from error
        return cls._from_json(paths.state_file, data)

    @classmethod
    def _from_json(cls, source: Path, data: object) -> AmtState:
        """The state of a JSON document, every field checked (the file may be edited by hand)."""

        def text(value: object, field_name: str) -> str | None:
            if value is None or isinstance(value, str):
                return value
            raise ModelstatsError(f"{source}: '{field_name}' must be a string or null, not {value!r}")

        if not isinstance(data, dict):
            raise ModelstatsError(f"{source}: expected a JSON object")
        runs_to_start = data.get("runs_to_start")
        runs_to_start = [] if runs_to_start is None else runs_to_start
        if not isinstance(runs_to_start, list) or not all(isinstance(name, str) for name in runs_to_start):
            raise ModelstatsError(f"{source}: 'runs_to_start' must be a list of strings")
        runs = data.get("runs")
        runs = {} if runs is None else runs
        if not isinstance(runs, dict) or not all(isinstance(cells, dict) for cells in runs.values()):
            raise ModelstatsError(f"{source}: 'runs' must be an object of objects")
        return cls(
            runcode=text(data.get("runcode"), "runcode"),
            last_commit=text(data.get("last_commit"), "last_commit"),
            runs_to_start=list(runs_to_start),
            runs={
                name: {
                    "Conv": text(cells.get("Conv"), f"runs.{name}.Conv"),
                    "Mif": text(cells.get("Mif"), f"runs.{name}.Mif"),
                }
                for name, cells in runs.items()
            },
        )

    @classmethod
    def _from_r_files(cls, paths: AmtPaths) -> AmtState:
        """The bookkeeping of an R-era checkout: the four ``.rds`` files."""
        state = cls()
        if paths.runcode_rds.exists():
            state.runcode = read_rds_string(paths.runcode_rds)
        last_commit = paths.model_dir / "lastcommit.rds"
        if last_commit.exists():
            state.last_commit = read_rds_string(last_commit)
        runs_to_start = paths.model_dir / "runsToStart.rds"
        if runs_to_start.exists():
            state.runs_to_start = [str(name) for name in read_rds_frame(runs_to_start).index]
        grs = paths.output_dir / "gRS.rds"
        if grs.exists():
            for name, row in read_rds_frame(grs).iterrows():
                state.runs[str(name)] = {"Conv": _cell(row.get("Conv")), "Mif": _cell(row.get("Mif"))}
        return state

    def save(self, paths: AmtPaths) -> None:
        """Write ``amt-state.json`` in one step (a new file replaces the old one).

        Raises:
            OSError: The file cannot be written.
        """
        data = {
            "runcode": self.runcode,
            "last_commit": self.last_commit,
            "runs_to_start": self.runs_to_start,
            "runs": self.runs,
        }
        staged = paths.state_file.with_name(paths.state_file.name + ".tmp")
        staged.write_text(json.dumps(data, indent=2) + "\n", encoding="utf-8")
        staged.replace(paths.state_file)


def _cell(value: object) -> str | None:
    return None if value is None else str(value)


def _timestamp() -> str:
    return _timefmt.now().strftime("%Y-%m-%d %H:%M:%S")


def _log_output(*streams: str | None) -> None:
    """What a program wrote, into the log (R's ``system()`` passed it through to the cron log)."""
    for text in streams:
        if text:
            log.info("%s", text.rstrip("\n"))


@overload
def _run(
    argv: Sequence[str], *, cwd: Path, env: Mapping[str, str] | None = None, required: Literal[True] = True
) -> subprocess.CompletedProcess[str]: ...


@overload
def _run(
    argv: Sequence[str], *, cwd: Path, env: Mapping[str, str] | None = None, required: Literal[False]
) -> subprocess.CompletedProcess[str] | None: ...


def _run(
    argv: Sequence[str], *, cwd: Path, env: Mapping[str, str] | None = None, required: bool = True
) -> subprocess.CompletedProcess[str] | None:
    """A program of the AMT, its output logged.

    A failure aborts the step when the program is ``required``: ``CalledProcessError`` for a
    non-zero exit status (a missing program is status 127, as in a shell), ``ModelstatsError`` for
    a missing working folder or a file that cannot be executed. Otherwise it is logged and the
    step goes on, as it did in R (BUG-067); ``None`` is returned.
    """
    try:
        completed = _shell.run(argv, cwd=cwd, check=True, env=env)
    except subprocess.CalledProcessError as error:
        _log_output(error.stdout, error.stderr)
        if required:
            raise
        log.warning("%s failed in %s with exit status %s; going on", shlex.join(argv), cwd, error.returncode)
        return None
    except OSError as error:
        if required:
            raise ModelstatsError(f"{shlex.join(argv)} could not run in {cwd}: {error}") from error
        log.warning("%s could not run in %s: %s; going on", shlex.join(argv), cwd, error)
        return None
    _log_output(completed.stdout, completed.stderr)
    return completed


def _replace_in_file(path: Path, old: str, new: str) -> None:
    """``sed -i 's/old/new/' path``: the first occurrence on each line, line endings kept."""
    try:
        with path.open(encoding="utf-8", errors="surrogateescape", newline="") as handle:
            lines = handle.readlines()  # bytes that are not UTF-8 round-trip, as with sed
        staged = path.with_name(path.name + ".tmp")  # sed -i: a new file replaces the old one
        try:
            with staged.open("w", encoding="utf-8", errors="surrogateescape", newline="") as handle:
                handle.writelines(line.replace(old, new, 1) for line in lines)
            shutil.copymode(path, staged)  # sed -i keeps the mode (a group-writable checkout)
            staged.replace(path)
        except OSError:
            staged.unlink(missing_ok=True)
            raise
    except OSError as error:
        raise ModelstatsError(f"{path}: {error}") from error


def _cfg_text(cfg: Mapping[str, Any], key: str, source: Path) -> str:
    """``cfg$key`` of a run's config, which has to be one string."""
    value = cfg.get(key)
    if not isinstance(value, str):
        raise ModelstatsError(f"{source}: cfg${key} is not a string: {value!r}")
    return value


# --- modeltests -----------------------------------------------------------------------------------


def modeltests(
    model_dir: Path = Path(),
    *,
    model: Model,
    user: str | None = None,
    report_repo: Path | None = None,
    publish: bool = True,
    compare_scenarios: bool = True,
    mattermost_url: str | None = None,
) -> None:
    """Run the next step of the automated model tests of a model.

    A ``config/scenario_config.csv`` file (relative to the model folder) has to exist in the
    model, describing the scenarios to be tested. Also, a git repository is needed to push the
    generated README.md for documentation and automated reporting of test results.

    Args:
        model_dir: Path to the folder where the model is found.
        model: Model name (``REMIND`` or ``MAgPIE``).
        user: The user that starts the jobs (default: the current user, BUG-090).
        report_repo: Path to the git clone that sends the report.
        publish: Whether the README is committed and pushed to ``report_repo``.
        compare_scenarios: Whether compareScenarios runs for converged REMIND runs.
        mattermost_url: Webhook for the Mattermost notification (``None``: no message).

    Raises:
        ModelstatsError: ``.testsstatus`` or the state cannot be read, or a step fails.
        subprocess.CalledProcessError: A required program of a step failed.
        OSError: ``.testsstatus`` or a file of a step cannot be written.
    """
    paths = AmtPaths(model_dir=model_dir, report_repo=report_repo)
    if not user:
        user = _shell.current_user()  # BUG-090
    log.info(
        "\n=========================================================\n"
        "Begin of AMT procedure %s in %s\n"
        "=========================================================\n",
        _timestamp(),
        model_dir,
    )
    try:
        lock = paths.lock_file.open("a", encoding="utf-8")
    except OSError as error:
        raise ModelstatsError(f"{paths.lock_file}: {error.strerror}") from error
    with lock:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            log.warning("Another modeltests is running in %s. Doing nothing", paths.model_dir)
            return
        status = _read_status(paths)
        if status == "next:start":
            log.info("Found 'next:start' in %s\nCalling 'startRuns'", paths.status_file)
            start_runs(paths, model=model, user=user)
            # make sure next call will evaluate runs
            log.info("Writing 'next:evaluate' to %s", paths.status_file)
            paths.status_file.write_text("next:evaluate\n", encoding="utf-8")
        elif status == "next:evaluate":
            log.info("Found 'next:evaluate' in %s\nCalling 'evaluateRuns'", paths.status_file)
            evaluate_runs(
                paths,
                model=model,
                user=user,
                compare_scenarios=compare_scenarios,
                publish=publish,
                mattermost_url=mattermost_url,
            )
            # make sure next call will start runs
            log.info("Writing 'next:start' to %s", paths.status_file)
            paths.status_file.write_text("next:start\n", encoding="utf-8")
        else:
            log.info("Found '%s' in %s. Doing nothing", status, paths.status_file)


def _read_status(paths: AmtPaths) -> str:
    """The first line of ``.testsstatus`` (BUG-091)."""
    try:
        text = paths.status_file.read_bytes().decode("utf-8", errors="replace")  # readLines() never failed
    except OSError as error:
        raise ModelstatsError(f"{paths.status_file}: {error.strerror}") from error
    lines = text.splitlines()
    return lines[0].strip() if lines else ""


def notify_mattermost(message: str, url: str) -> None:
    """Post one message to a Mattermost webhook (a failed post is logged, never raised).

    Args:
        message: The text, Markdown as Mattermost renders it.
        url: The webhook; it is never echoed.
    """
    _shell.post_json(url, {"text": message})


def _read_runtime(run_dir: Path) -> float | None:
    """The GAMS runtime of a run in hours, from ``runstatistics.rda``."""
    stats = read_rda(run_dir / "runstatistics.rda").get("stats")
    if not isinstance(stats, dict):
        return None
    end, start = stats.get("timeGAMSEnd"), stats.get("timeGAMSStart")
    if isinstance(end, datetime) and isinstance(start, datetime):
        return (end - start).total_seconds() / 3600
    return None


def delete_empty_realization_folders(model_dir: Path) -> None:
    """Remove realization folders that ``module.gms`` no longer lists.

    Empty realization folders break the models, but can be left over because git does not delete
    empty folders if all files in the folder are deleted.

    Args:
        model_dir: The model folder holding ``modules/``.

    Raises:
        OSError: A ``module.gms`` cannot be read (a folder that cannot be removed is a warning).
    """
    for module_file in sorted(model_dir.glob("modules/**/module.gms")):
        module_dir = module_file.parent
        lines = module_file.read_text(encoding="utf-8", errors="replace").splitlines()
        listed = {
            _REALIZATION_SUFFIX.sub("", _MODULE_PREFIX.sub("", line))
            for line in lines
            if "realization.gms" in line
        }
        keep = {"module.gms", "input", *listed}
        for entry in sorted(module_dir.iterdir()):
            if entry.name in keep or entry.name.startswith("."):
                continue
            try:
                if entry.is_dir(follow_symlinks=False):
                    shutil.rmtree(entry)
                else:
                    entry.unlink()
            except OSError as error:  # R's unlink() never stopped
                log.warning("Could not remove %s: %s", entry, error)


# --- startRuns ----------------------------------------------------------------------------------


def start_runs(paths: AmtPaths, *, model: Model, user: str) -> None:
    """Update the model, start the test runs and remember what was started.

    Args:
        paths: The model folder and its files.
        model: ``REMIND`` or ``MAgPIE``.
        user: The user whose SLURM jobs are watched (MAgPIE: the default run).

    Raises:
        ModelstatsError: The state or ``default.cfg`` cannot be read or written, a required program
            cannot start, or ``squeue`` keeps failing.
        subprocess.CalledProcessError: The model's git update, a ``Rscript start.R`` call or the
            ``selectScenarios()`` bridge failed.
        OSError: A ``module.gms``, the state file or ``runcode.rds`` cannot be read or written.
    """
    today = _timefmt.today()
    runcode = f".*-AMT_{today.isoformat()}|.*-AMT_{(today + timedelta(days=1)).isoformat()}"
    state = AmtState.load(paths)
    _run(["git", "reset", "--hard", "origin/develop"], cwd=paths.model_dir)
    _run(["git", "pull"], cwd=paths.model_dir)
    default_cfg = paths.model_dir / "config" / "default.cfg"
    # Force downloading of the input data in the first run
    _replace_in_file(default_cfg, "cfg$force_download <- FALSE", "cfg$force_download <- TRUE")
    delete_empty_realization_folders(paths.model_dir)
    if model == "REMIND":
        # Do not ask when installing dependencies in piamenv::fixDeps
        env = {**os.environ, "autoRenvFixDeps": "TRUE"}
        log.info("Configuring and starting single testOneRegi-AMT")
        slurm = "slurmConfig=--qos=priority --nodes=1 --tasks-per-node=1 --wait --time=2:00:00"
        _run(["Rscript", "start.R", "--testOneRegi", "titletag=AMT", slurm], cwd=paths.model_dir, env=env)
        log.info("Configuring and starting bundle of AMT runs")
        # do not download input data every run, reset force_download
        _replace_in_file(default_cfg, "cfg$force_download <- TRUE", "cfg$force_download <- FALSE")
        slurm = "slurmConfig=--qos=standby --nodes=1 --tasks-per-node=12 --time=36:00:00"
        argv = ["Rscript", "start.R", "startgroup=AMT", "titletag=AMT", slurm, "config/scenario_config.csv"]
        _run(argv, cwd=paths.model_dir, env=env)
        # Create and save a list of runs that should have been started in order
        # to determine later which runs were not started
        state.runs_to_start = [f"{name}-AMT" for name in _select_scenarios(paths.model_dir, env)]
        _remember_runcode(paths, state, runcode)
        # start make test-full: pull changes to magpie develop, then execute the test
        if _run(["git", "reset", "--hard", "origin/develop"], cwd=paths.magpie_dir, env=env, required=False):
            _run(["git", "pull"], cwd=paths.magpie_dir, env=env, required=False)  # R: reset && pull
        _run(["make", "test-full-slurm"], cwd=paths.model_dir, env=env, required=False)
    else:
        # default run to download input data
        _run(["Rscript", "start.R", "runscripts=default", "submit=SLURM priority"], cwd=paths.model_dir)
        # wait for default run to finish (BUG-084)
        pattern = re.compile(re.escape(str(paths.model_dir)) + "$")
        failures = _SqueueFailures()
        while True:
            _shell.sleep(300)
            jobs = _squeue_jobs(user, failures)
            if jobs is not None and not any(pattern.search(job) for job in jobs):
                break  # a failing squeue keeps the wait going
        # now start actual test runs
        _run(["Rscript", "start.R", "runscripts=test_runs", "submit=SLURM standby"], cwd=paths.model_dir)
        _remember_runcode(paths, state, runcode)
    log.info("Function 'startRuns' finished.")


def _remember_runcode(paths: AmtPaths, state: AmtState, runcode: str) -> None:
    """``saveRDS(runcode, "runcode.rds")``: the state, and the file the R tools read (BUG-065)."""
    state.runcode = runcode
    state.save(paths)
    write_rds_string(paths.runcode_rds, runcode)


def _select_scenarios(model_dir: Path, env: Mapping[str, str]) -> list[str]:
    """The scenarios of the AMT start group, chosen by REMIND's own ``selectScenarios()``."""
    completed = _run(_shell.rscript_argv(_SELECT_SCENARIOS_R), cwd=model_dir, env=env)
    lines = completed.stdout.splitlines()
    names = [line.removeprefix(_SCENARIO_PREFIX) for line in lines if line.startswith(_SCENARIO_PREFIX)]
    return [name for name in names if name]


# --- evaluateRuns -------------------------------------------------------------------------------


@dataclass
class _Evaluation:
    """What evaluateRuns collects on its way to the README and the Mattermost message."""

    readme: list[str] = field(default_factory=list)
    errors: list[str] = field(default_factory=list)
    git_info: list[str] = field(default_factory=list)
    runs_started: list[str] = field(default_factory=list)
    runs_not_started: list[str] | None = None
    testthat_result: str = ""
    changelog: Path | None = None

    def write(self, *lines: str) -> None:
        self.readme.extend(lines)


@dataclass
class _SqueueFailures:
    """How often squeue failed in a row; more than three times stops the step (R's ``errCount``)."""

    count: int = 0


def _squeue_jobs(user: str, failures: _SqueueFailures) -> list[str] | None:
    """The user's jobs (``SQUEUE_FORMAT``), or ``None`` when squeue failed (logged with its stderr).

    An empty user is the current user (BUG-090): ``squeue`` without ``-u`` would list everyone's jobs.
    """
    completed = _shell.run(_shell.squeue_argv(SQUEUE_FORMAT, user=user or _shell.current_user()))
    if completed.returncode != 0:
        _log_output(completed.stderr)
        failures.count += 1  # count how often squeue fails
        log.warning("squeue failed with exit status %d (%d in a row)", completed.returncode, failures.count)
        if failures.count > 3:
            raise ModelstatsError("squeue had exit status > 0 more than 3 times in a row.")
        return None
    failures.count = 0  # reset if squeue was successful
    return completed.stdout.splitlines()


def _wait_for_runs(paths: AmtPaths, user: str) -> None:
    """Poll squeue every ten minutes until no job of the user works inside the model folder."""
    log.info("%s - waiting for all AMT runs to finish.", _timestamp())
    inside = f"{paths.model_dir}/"
    failures = _SqueueFailures()
    while True:
        jobs = _squeue_jobs(user, failures)
        if jobs is not None and not any(inside in job for job in jobs):
            break
        _shell.sleep(600)
    log.info("%s - all AMT runs finished.", _timestamp())


def _git_info(paths: AmtPaths, last_commit: str, today: str) -> tuple[str, list[str]]:
    """The tested commit and the merges since the last test."""
    head = _run(["git", "log", "-1"], cwd=paths.model_dir).stdout.splitlines()
    if not head or not head[0].startswith("commit "):
        raise ModelstatsError(f"git log -1 in {paths.model_dir} did not name a commit")  # BUG-083
    commit = head[0].removeprefix("commit ")
    argv = ["git", "log", "--merges", "--pretty=oneline", f"{last_commit}..{commit}", "--abbrev-commit"]
    listed = _run(argv, cwd=paths.model_dir, required=False)  # R used the lines of a failed git too
    merges = [line for line in (listed.stdout.splitlines() if listed else []) if "Merge pull request" in line]
    info = [f"Tested commit: {commit}", f"The test of {today} contains these merges:", *merges]
    return commit, info


def _run_names(paths: AmtPaths, *, dot_folders: bool = False) -> list[str]:
    """``dir()`` of the output folder (``list.dirs()`` with ``dot_folders``): the run folders in run order."""
    entries = sorted(subdirs(paths.output_dir), key=run_sort_key)
    return [entry.name for entry in entries if dot_folders or not entry.name.startswith(".")]


def _status(run_dir: Path) -> RunStatus | None:
    """``getRunStatus(i)``: the first row, or ``None`` for a folder that vanished."""
    rows = get_run_status(run_dir)
    return rows[0] if rows else None


def _known_runs(paths: AmtPaths, state: AmtState) -> dict[str, RunStatus | Exception]:
    """Extend the table of known REMIND runs by the folders not seen before (R's ``gRS.rds``).

    Returns what was read per folder: the status, or the error (BUG-070), so that the evaluation
    does not read a folder twice. MAgPIE kept no table (R read every folder for nothing).
    """
    read: dict[str, RunStatus | Exception] = {}
    for name in _run_names(paths):
        if name in state.runs:
            continue
        try:
            row = _status(paths.output_dir / name)
        except (ModelstatsError, OSError) as error:
            log.warning("Skipping %s: %s", paths.output_dir / name, error)  # BUG-070
            read[name] = error
            continue
        if row is not None:
            read[name] = row
            state.runs[name] = {"Conv": row.conv, "Mif": row.mif}
    return read


def _creation_date(path: Path) -> date:
    """``file.info()$ctime`` as a date."""
    return datetime.fromtimestamp(path.stat().st_ctime).date()


def _runcode(paths: AmtPaths, state: AmtState) -> re.Pattern[str]:
    """The runcode of the current test as a pattern; checked before the wait for the runs (BUG-083)."""
    if not state.runcode:
        raise ModelstatsError(f"{paths.state_file}: no runcode; did the next:start step run?")  # BUG-083
    return compile_pattern(state.runcode, source=str(paths.state_file))


def _runs_started(paths: AmtPaths, runcode: re.Pattern[str] | None, today: date) -> list[str]:
    """The run folders of the current test: by runcode (REMIND) or created in the last 3 days (MAgPIE)."""
    if runcode is not None:
        return [name for name in _run_names(paths, dot_folders=True) if runcode.search(name)]  # list.dirs()
    # if model is MAgPIE ignore runcode and find run folders based on their creation time (last 3 days)
    three_days_ago = today - timedelta(days=3)
    return [name for name in _run_names(paths) if _creation_date(paths.output_dir / name) > three_days_ago]


def _readme_row(row: RunStatus) -> str:
    cells: dict[str, object] = {}
    if row.runtime is not None:
        cells["runtime"] = format_runtime(row.runtime)
    return format_row(row, _README_WIDTHS, "  ", CLUSTER_COLUMNS, cells=cells)


def _check_run(row: RunStatus, model: Model, evaluation: _Evaluation) -> None:
    """The error rules of one run."""
    if model == "REMIND":
        if not re.search("Calib_nash|testOneRegi", row.run_type) and row.conv not in _CONVERGED:
            evaluation.errors.append("Some run(s) did not converge")
        if row.run_type == "Calib_nash" and row.conv != "Clb_converged":
            evaluation.errors.append("Some run(s) did not converge")
        if "testOneRegi" in row.run_type and row.modelstat != "2: Locally Optimal":
            evaluation.errors.append("testOneRegi does not return an optimal solution")
        if row.mif == "sumErr":
            evaluation.errors.append("Summation checks for some run(s) revealed some gaps")
    else:
        digits = "".join(dict.fromkeys(re.sub("[^0-9]", "", row.modelstat)))
        if digits != "2":
            evaluation.errors.append("Some run(s) did not converge")
    if row.run_in_app_results != "yes":
        evaluation.errors.append("Some run(s) did not report correctly")


def _compare_with_last_run(
    paths: AmtPaths,
    state: AmtState,
    row: RunStatus,
    run_dir: Path,
    *,
    compare_scenarios: bool,
    evaluation: _Evaluation,
) -> None:
    """For a successful run compare runtime and results with the previous AMT run of its scenario."""
    log.info("Changed to %s", run_dir)
    # Use the fulldata.gdx of a successful SSP2-NPi-AMT to update the gdx on the
    # RSE server that is used for testing convGDX2MIF
    if "SSP2-NPi-AMT" in row.name and row.conv in _CONVERGED:
        log.info("Updating the gdx on the RSE server %s with the fulldata.gdx of %s", RSE_GDX, row.name)
        _run(["rsync", "-e", "ssh", "-av", "fulldata.gdx", RSE_GDX], cwd=run_dir, required=False)
    config = run_dir / "config.Rdata"
    cfg = load_config(config)
    title = _cfg_text(cfg, "title", config)
    scenario = compile_pattern(title, source=f"{config}: cfg$title")
    this_name = Path(_cfg_text(cfg, "results_folder", config)).name
    same_runs = [
        name
        for name, cells in state.runs.items()
        if cells.get("Conv") in _CONVERGED  # runs have to be converged
        and cells.get("Mif") in ("yes", "sumErr")  # need to have mifs
        and scenario.search(name)  # must be the same scenario
    ]
    earlier = [name for name in same_runs if name < this_name]  # not the current run, and before it
    if not earlier:
        return  # BUG-069
    last_run = max(earlier)
    last_dir = paths.output_dir / last_run
    # If it was too long ago, the lastRun may have been moved to the archive in the meantime.
    if not last_dir.exists():
        last_dir = paths.archive_dir / last_run  # BUG-082: the job keeps the run's name
    if not last_dir.is_dir():
        log.warning("%s is gone from output/ and archive/: not compared with", last_run)  # BUG-095
        return
    this_dir = run_dir.resolve()
    last_dir = last_dir.resolve()
    # compare runtime for converged run only (skip if not_converged)
    if row.conv in _CONVERGED:
        try:
            current_hours = _read_runtime(this_dir)
            last_hours = _read_runtime(last_dir)
        except RDataReadError as error:
            raise ModelstatsError(f"runtime of {this_dir} or {last_dir}: {error}") from error
        if current_hours is not None and last_hours is not None and current_hours > 1.25 * last_hours:
            evaluation.errors.append("Check runtime! Have some scenarios become slower?")
    # run compareScenarios also for runs that are not_converged
    mifs = [this_dir / f"REMIND_generic_{title}.mif", last_dir / f"REMIND_generic_{title}.mif"]
    has_pdf = any(  # dir(): no dot-files
        re.search("comp_with_.*.pdf", entry.name)
        for entry in run_dir.iterdir()
        if not entry.name.startswith(".")
    )
    if compare_scenarios and all(mif.exists() for mif in mifs) and not has_pdf:
        log.info("Calling compareScenarios2 with %s and %s", this_dir, last_dir)
        out_name = f"comp_with_{last_run}"
        wrap = (
            "Rscript scripts/cs2/run_compareScenarios2.R "
            f"outputdirs={shlex.quote(f'{this_dir},{last_dir}')} profileName=default "
            f"outFileName={shlex.quote(out_name)}; "
            f"mv {shlex.quote(out_name + '.pdf')} {shlex.quote(str(this_dir))}"
        )
        argv = [
            "sbatch",
            "--qos=standby",
            f"--job-name={out_name}",
            "--comment=compareScenarios2",
            f"--output={this_dir}/{out_name}.out",
            f"--error={this_dir}/{out_name}.out",
            "--mail-type=END",
            "--time=200",
            "--mem-per-cpu=8000",
            f"--wrap={wrap}",
        ]
        _console.out.print(Text(" ".join(argv[:-1]) + f' --wrap="{wrap}" '))  # cat(cs2com, "\n") (BUG-089)
        # run_compareScenarios2.R works only if called from the main folder
        _run(argv, cwd=run_dir / _cfg_text(cfg, "remind_folder", config), required=False)


def _runs_not_started(state: AmtState, evaluation: _Evaluation) -> None:
    """Find and print runs not started."""
    if len(evaluation.runs_started) < len(state.runs_to_start) + 1:
        scenarios_started = {_TIMESTAMP.sub("", name) for name in evaluation.runs_started}
        planned = ["default-AMT", *state.runs_to_start]
        # setdiff(): each missing scenario once
        evaluation.runs_not_started = [n for n in dict.fromkeys(planned) if n not in scenarios_started]
        evaluation.write(" ", "These scenarios did not start at all:", *evaluation.runs_not_started, " ")


def _testthat_result(paths: AmtPaths) -> str:
    """Evaluate the result of ``make test-full`` from its log, and file the log under ``tests/``."""
    current = paths.test_full_log
    if not current.exists():
        return "Could not check for the results of `make test-full`, test-full.log not found"
    date_tag = datetime.fromtimestamp(current.stat().st_mtime).strftime("%Y-%m-%d")
    lines = current.read_text(encoding="utf-8", errors="replace").splitlines()
    fails = [line for line in lines if "[ FAIL" in line]
    new_name = paths.model_dir / "tests" / f"test-full-{date_tag}.log"
    try:
        current.rename(new_name)
    except OSError as error:
        log.warning("Could not move %s to %s: %s", current, new_name, error)
    if len(fails) != 1:  # BUG-071: R's isTRUE() needs exactly one line
        return f"`make test-full` did not run properly. Check {new_name}"
    (status,) = fails
    if not ("FAIL 0" in status and "WARN 0" in status):
        return f"Not all tests pass in `make test-full`: {status}. Check `{new_name}`"
    return f"All tests pass in `make test-full`: {status}"


def _archive_old_runs(paths: AmtPaths, today: date) -> None:
    """Find (by date in folder name) and move runs older than ``ARCHIVE_AFTER_DAYS`` to archive."""
    cutoff = today - timedelta(days=ARCHIVE_AFTER_DAYS)
    old_runs = []
    for name in _run_names(paths):
        if "AMT" not in name:
            continue
        match = _DATE_IN_NAME.match(name)
        if match is None:
            continue  # BUG-087: R stopped on a name without a date
        try:
            run_date = date.fromisoformat(match.group(1))
        except ValueError:
            continue
        if run_date < cutoff:
            old_runs.append(name)
    if old_runs:
        log.info(
            "Moving %d runs with timestamp older than %d days (%s) to 'archive':",
            len(old_runs),
            ARCHIVE_AFTER_DAYS,
            cutoff.isoformat(),
        )
        log.info("%s", "\n".join(old_runs))
        try:
            paths.archive_dir.mkdir(exist_ok=True)
        except OSError as error:  # mv failed in R and the evaluation went on
            log.warning("Could not create %s: %s", paths.archive_dir, error)
            return
        for name in old_runs:  # mv: an existing target or any other failure is reported, not fatal
            target = paths.archive_dir / name
            if target.exists():
                log.warning("Could not move %s to archive: %s exists", name, target)
                continue
            try:
                shutil.move(paths.output_dir / name, target)
            except OSError as error:
                log.warning("Could not move %s to archive: %s", name, error)


def _publish(repo: Path, readme: str, changelog: Path | None) -> None:
    """Commit the README (and the data changelog) to the report repository and push."""
    _run(["git", "reset", "--hard", "origin/master"], cwd=repo, required=False)
    _run(["git", "pull"], cwd=repo, required=False)
    (repo / "README.md").write_text(readme, encoding="utf-8", errors="surrogateescape")  # names as bytes
    _run(["git", "add", "README.md"], cwd=repo, required=False)
    if changelog is not None and changelog.exists():
        shutil.copy(changelog, repo / "data-changelog.csv")
    if (repo / "data-changelog.csv").exists():
        _run(["git", "add", "data-changelog.csv"], cwd=repo, required=False)
    _run(["git", "commit", "-m", "Automated Test Results"], cwd=repo, required=False)
    _run(["git", "push"], cwd=repo, required=False)


def _mattermost_message(
    paths: AmtPaths, model: Model, today: str, summary: str, evaluation: _Evaluation
) -> str:
    """Compose the message; each element will appear in a new line in the final message."""
    if model == "MAgPIE":
        if evaluation.errors:
            return (
                "Some MAgPIE tests produce warnings. "
                "Please check https://gitlab.pik-potsdam.de/landuse/testing_suite"
            )
        return (
            "MAgPIE tests completed successfully. "
            "Find the results at https://gitlab.pik-potsdam.de/landuse/testing_suite."
        )
    table, table_text = _console.capture()
    try:
        print_run_table(
            [paths.output_dir / name for name in evaluation.runs_started],
            user=None,
            color=False,
            sort_by_time=False,
            out=table,
        )
    except (ModelstatsError, OSError) as error:  # BUG-070: the tables read the runs again
        log.warning("The run table of the message failed: %s", error)
        table.print(Text(f"Error: {error}"))
    sanity, sanity_text = _console.capture()
    try:
        print_sanity_checks(out=sanity)
    except (ModelstatsError, OSError) as error:  # BUG-055: a run that cannot be read stops the table
        log.warning("The sanity table of the message failed: %s", error)
        sanity.print(Text(f"Error: {error}"))
    lines = [
        f"Please find below the status of the REMIND automated model tests (AMT) of {today}. "
        "Runs are here: `/p/projects/remind/modeltests/remind`.",
        summary,
        evaluation.testthat_result,
        "```",
        *evaluation.git_info,
        "```",
        "`rs -A` returns:",
        "```",
        *table_text.getvalue().splitlines(),
        "```",
        "Sanity checks (`rs -s`) return:",
        "```",
        *sanity_text.getvalue().splitlines(),
        "```",
    ]
    if evaluation.runs_not_started is not None:
        lines += ["These scenarios did not start at all:", *evaluation.runs_not_started]
    return "\n".join(lines)


def _data_changelog(paths: AmtPaths, evaluation: _Evaluation, tmp: Path) -> None:
    """MAgPIE: add the default runs' reports to the data changelog of the report repository."""
    repo = paths.report_repo
    if repo is None:
        return
    for name in evaluation.runs_started:
        if not name.startswith("default_"):
            continue
        if evaluation.changelog is None:  # BUG-088: one copy, every default run adds to it
            evaluation.changelog = tmp / "data-changelog.csv"
            try:
                shutil.copy(repo / "data-changelog.csv", evaluation.changelog)
            except OSError as error:
                log.warning("Could not copy %s: %s", repo / "data-changelog.csv", error)
        argv = _shell.rscript_argv(
            _ADD_TO_CHANGELOG_R, str(paths.output_dir / name), str(evaluation.changelog), name
        )
        _run(
            argv, cwd=paths.model_dir, required=False
        )  # R: try(); the model folder's .Rprofile applies (BUG-099)


def evaluate_runs(
    paths: AmtPaths,
    *,
    model: Model,
    user: str,
    compare_scenarios: bool = True,
    publish: bool = True,
    mattermost_url: str | None = None,
) -> str:
    """Wait for the test runs, compile the README, publish it and notify.

    Args:
        paths: The model folder, its files and the report repository.
        model: ``REMIND`` or ``MAgPIE``.
        user: The user whose SLURM jobs are waited for.
        compare_scenarios: Whether compareScenarios jobs are submitted for REMIND runs.
        publish: Whether the README is committed and pushed to the report repository.
        mattermost_url: The webhook of the notification (``None``: no message).

    Returns:
        The README text.

    Raises:
        ModelstatsError: The state, the output folder, the runcode or the last tested commit are
            missing or unusable, ``publish`` without a report repository, ``squeue`` keeps failing,
            a run's runtime cannot be read, or a required program cannot start.
        subprocess.CalledProcessError: ``git log`` failed.
        OSError: ``.testsstatus``, the state file, ``README.md`` or ``test-full.log`` cannot be
            read or written.
    """
    if not paths.output_dir.is_dir():
        raise ModelstatsError(f"{paths.output_dir}: no such folder")  # R: with_dir("output") failed first
    if publish and paths.report_repo is None:
        raise ModelstatsError("publishing the README needs --report-repo")  # BUG-100
    log.info("Current working directory %s", paths.output_dir)
    log.info("Writing 'evaluateRuns() is running or stopped due to an error' to %s", paths.status_file)
    paths.status_file.write_text("evaluateRuns() is running or stopped due to an error\n", encoding="utf-8")
    state = AmtState.load(paths)
    if not state.last_commit:
        raise ModelstatsError(
            f"{paths.state_file}: no last tested commit (lastcommit.rds) to compare with"  # BUG-083
        )
    runcode = _runcode(paths, state) if model == "REMIND" else None
    today_text = _timefmt.today().isoformat()
    _wait_for_runs(paths, user)
    log.info("Compiling the README.md to be committed to testing_suite repo.")
    commit, git_info = _git_info(paths, state.last_commit, today_text)
    evaluation = _Evaluation(git_info=git_info)
    evaluation.write(
        "```",
        f"This is the result of the automated model tests for {model} on {today_text}.",
        f"Path to runs: {paths.model_dir}/output/",
        "Direct and interactive access to plots: open shinyResults::appResults, then use '"
        + ("weeklyTests" if model == "MAgPIE" else "AMT")
        + "' as keyword in the title search",
    )
    if model == "REMIND" and compare_scenarios:
        evaluation.write(
            "Each run folder below should contain a compareScenarios PDF comparing the output of the "
            "current and the last successful tests (comp_with_RUN-DATE.pdf)"
        )
    evaluation.write(
        "Note: 'Mif' = 'no' indicates a possible error in output generation, please check!",
        "If you are currently viewing the email: Overview of the last test is in red, "
        "and of the current test in green",
        *git_info,
    )
    statuses = _known_runs(paths, state) if model == "REMIND" else {}
    evaluation.runs_started = _runs_started(paths, runcode, _timefmt.today())  # BUG-086
    evaluation.write("  ".join(_README_TITLES))
    with tempfile.TemporaryDirectory() as tmp_name:  # for the MAgPIE changelog copy
        if model == "MAgPIE":
            _data_changelog(paths, evaluation, Path(tmp_name))
        log.info(
            "Starting analysis for the list of the following runs:\n%s", "\n".join(evaluation.runs_started)
        )
        for name in evaluation.runs_started:
            run_dir = paths.output_dir / name
            try:
                row = statuses.get(name) or _status(run_dir)
            except (ModelstatsError, OSError) as error:
                row = error
            if isinstance(row, Exception):  # BUG-070: R stopped here
                evaluation.write(f"{name}  could not be read: {row}")
                evaluation.errors.append("Some run(s) could not be read")
                continue
            if row is None:
                continue
            evaluation.write(_readme_row(row))
            _check_run(row, model, evaluation)
            # For a successful run compare runtime and results with previous AMT run
            # Since there is no column 'Conv' for MAgPIE runs the following is only done for REMIND runs
            if row.conv in (*_CONVERGED, "not_converged"):
                _compare_with_last_run(
                    paths, state, row, run_dir, compare_scenarios=compare_scenarios, evaluation=evaluation
                )
                log.info("Finished analysis for %s and changed back to %s", name, paths.output_dir)
            elif model == "REMIND":
                log.info("%s does not seem to have converged. Skipping!", name)
        if model == "REMIND":
            _runs_not_started(state, evaluation)
            evaluation.testthat_result = _testthat_result(paths)
            _archive_old_runs(paths, _timefmt.today())  # BUG-086
        if not evaluation.runs_started:
            evaluation.errors.append("No runs started")
        if evaluation.errors:
            summary = "Summary: " + ". ".join(dict.fromkeys(evaluation.errors))
        else:
            summary = "Summary: AMT runs look good."
        evaluation.write(summary, "```")
        log.info("Finished compiling README.md")
        readme = "\n".join(evaluation.readme) + "\n"
        if publish and paths.report_repo is not None:
            _publish(paths.report_repo, readme, evaluation.changelog)
    log.info("Composing message and sending it to mattermost channel")
    # for MAgPIE only if warnings/errors occur, for REMIND always display AMT status
    if mattermost_url:
        notify_mattermost(_mattermost_message(paths, model, today_text, summary, evaluation), mattermost_url)
    # only save this if everything went well
    state.last_commit = commit
    state.save(paths)
    log.info("Function 'evaluateRuns' finished.")
    return readme


# --- console script -----------------------------------------------------------------------------

app = typer.Typer(add_completion=False, rich_markup_mode=None)


@contextmanager
def _logging_to_stderr() -> Generator[None]:
    """The package's log (R's ``message()`` lines) on the stderr console's stream, for one command."""
    logger = logging.getLogger("modelstats")
    handler = logging.StreamHandler(_console.err.file)
    handler.setFormatter(logging.Formatter("%(message)s"))
    level = logger.level
    logger.addHandler(handler)
    logger.setLevel(logging.INFO)
    try:
        yield
    finally:
        logger.removeHandler(handler)
        logger.setLevel(level)
        handler.close()


@app.command()
def run(
    model: Annotated[Model, typer.Option("--model", help="Model name: REMIND or MAgPIE")],
    model_dir: Annotated[
        Path,
        typer.Option(
            "--model-dir", exists=True, file_okay=False, help="Path to the folder where the model is found"
        ),
    ] = Path(),
    user: Annotated[str | None, typer.Option("--user", help="the user that starts the jobs")] = None,
    report_repo: Annotated[
        Path | None,
        typer.Option("--report-repo", exists=True, file_okay=False, help="git clone that sends the report"),
    ] = None,
    publish: Annotated[
        bool, typer.Option("--publish/--no-publish", help="commit and push the README")
    ] = True,
    compare_scenarios: Annotated[
        bool, typer.Option("--compare-scenarios/--no-compare-scenarios", help="run compareScenarios")
    ] = True,
    mattermost_url: Annotated[
        str | None,
        typer.Option(
            "--mattermost-url",
            envvar=MATTERMOST_URL_ENV,
            show_envvar=True,
            help=f"Mattermost webhook (prefer the variable {MATTERMOST_URL_ENV}: arguments show in ps)",
        ),
    ] = None,
) -> None:
    """Run the next step of the automated model tests (start the runs, or evaluate them)."""
    if publish and report_repo is None:
        raise typer.BadParameter("--publish needs --report-repo (or pass --no-publish)")
    _console.tolerate_undecodable_names()
    with _logging_to_stderr():
        try:
            modeltests(
                model_dir,
                model=model,
                user=user,
                report_repo=report_repo,
                publish=publish,
                compare_scenarios=compare_scenarios,
                mattermost_url=mattermost_url,
            )
        except (ModelstatsError, subprocess.CalledProcessError, OSError) as error:
            _console.error(error)
            raise typer.Exit(1) from error


def main(argv: Sequence[str] | None = None) -> int:
    """Run ``modeltests`` with the given arguments and return its exit status."""
    return _console.run_app(app, argv, prog_name="modeltests")
