"""``startRuns``: the start half of the AMT (``R/modeltests.R`` lines 71-86 and 88-166), every effect through Effects.

The function runs with the process working directory set to ``mydir`` (``withr::local_dir(mydir)`` in
``modeltests()``, line 31) and composes its state-file paths from ``mydir`` as given (``paste0(mydir,
"/runcode.rds")``: a double slash when ``mydir`` ends in ``/``, harmless). Statement by statement:

- the runcode ``.*-AMT_<today>|.*-AMT_<tomorrow>`` (lines 92-96, ``Sys.Date()`` in the process time zone);
- ``system("git reset --hard origin/develop && git pull")`` (line 98): the identical command STRING through
  ``/bin/sh`` (:meth:`Effects.run_shell` without capture, the child writing to the process' own stdout and
  stderr like R's ``system()``); the exit status is ignored, status 127 only warns ``error in running command``;
- ``sed -i 's/cfg$force_download <- FALSE/cfg$force_download <- TRUE/' config/default.cfg`` (line 100) as a
  file edit (:func:`sed_replace`): the same bytes GNU sed produces (first occurrence per line, line endings
  kept, the mode kept), written atomically like ``sed -i``'s temporary file and rename. The R golden traces a
  ``sed`` call, the port makes none: the documented exception of the AMT trace comparison (plan 03 4.5);
- :func:`delete_empty_realization_folders` (lines 71-86): ``find modules/ -name 'module.gms'`` as
  :meth:`Effects.walk`, the ``realization.gms`` lines of each module file, R's two ``sub()`` calls, the
  ``setdiff`` against ``dir(<module directory>)`` and ``unlink(recursive = TRUE)``;
- REMIND (lines 106-146): ``Sys.setenv(autoRenvFixDeps = "TRUE")`` through :meth:`Effects.setenv` (seen by every
  later child), the two ``Rscript start.R ...`` command strings exactly as R pastes them, the second sed edit, the
  ``selectScenarios()`` call through the :mod:`modelstats.amt.bridges` ``select_scenarios`` bridge (the bridge
  does R's ``read.csv2`` and ``source()``, lines 127-136, and saves the data.frame to a file under
  ``tempdir()``), ``row.names(...) <- paste0(..., "-AMT")`` and ``saveRDS`` through :mod:`modelstats.amt.state`
  (line 137-138), ``git reset --hard origin/develop && git pull`` inside ``withr::with_dir("magpie", ...)`` as
  :meth:`Effects.chdir`, ``make test-full-slurm``;
- MAgPIE (lines 148-162): ``Rscript start.R runscripts=default submit='SLURM priority'``, then the wait loop:
  ``Sys.sleep(300)`` BEFORE every ``squeue -u <user> -h -o '%i %q %T %C %M %j %V %L %e %Z'`` (``intern = TRUE``:
  :func:`modelstats.slurm.system_intern`, so a failing squeue warns and yields no lines, which ends the loop)
  until no line matches ``grepl(paste0(mydir, "$"), jobsInSlurm)`` - with the trailing slash of the cron job's
  ``mydir`` the regex never matches a WorkDir, so the loop ends after one check (BUG-032, parity); then
  ``Rscript start.R runscripts=test_runs submit='SLURM standby'``;
- ``saveRDS(runcode, paste0(mydir, "/runcode.rds"))`` (line 164) and the closing message.

``model = NULL`` stops with ``Model cannot be NULL`` (line 90) before anything runs; any other model name runs
the common steps and the runcode write only, like R's ``if`` chain. The ``message()`` lines go to ``sys.stderr``
(resolved at call time). The deparsed R calls carried by the warnings are R 4.6.1's first deparse line, as
``Rscript`` prints them (``In system(paste0("squeue -u ", user, ...),  :`` keeps the trailing comma of a
wrapped deparse); probed 2026-10-01, pinned in ``tests/unit/test_amt_start.py``.

Verified R facts (R 4.6.1, GNU sed 4.10, 2026-10-01): ``paste0("squeue -u ", NULL, " -h")`` is ``squeue -u  -h``;
``any(grepl(p, character(0)))`` is ``FALSE``; ``system()`` returns the status and warns only on 127; ``sed -i`` on a
missing file prints ``sed: can't read <file>: No such file or directory`` with status 2 and R goes on; the
``sub()`` chain of line 80 leaves a line without ``modules/<name>/`` unchanged; ``dir("modules/01_macro/")`` with
the trailing slash lists the directory; ``setdiff`` keeps the first argument's order.
"""

from __future__ import annotations

import datetime as dt
import os
import re
import sys
import warnings

import pandas as pd

from modelstats.amt import state
from modelstats.amt.bridges import select_scenarios
from modelstats.env import Effects, PathLike, default_effects, shell_tokens
from modelstats.errors import RParityError, RWarning
from modelstats.rdata_io import read_rds
from modelstats.slurm import r_regex, system_intern

__all__ = [
    "AUTO_RENV_FIX_DEPS",
    "BRIDGE_RDS_NAME",
    "DEFAULT_CFG",
    "FORCE_DOWNLOAD_OFF",
    "FORCE_DOWNLOAD_ON",
    "GIT_RESET_PULL",
    "GIT_RESET_PULL_CALL",
    "MAGPIE_DEFAULT_RUN",
    "MAGPIE_DEFAULT_RUN_CALL",
    "MAGPIE_DIR",
    "MAGPIE_TEST_RUNS",
    "MAGPIE_TEST_RUNS_CALL",
    "MAGPIE_WAIT_SECONDS",
    "MAKE_TEST_FULL",
    "MAKE_TEST_FULL_CALL",
    "MODEL_NULL_MESSAGE",
    "MODULES_DIR",
    "MODULE_FILE",
    "MSG_BUNDLE",
    "MSG_FINISHED",
    "MSG_TEST_ONE_REGI",
    "REMIND_BUNDLE",
    "REMIND_BUNDLE_CALL",
    "REMIND_TEST_ONE_REGI",
    "REMIND_TEST_ONE_REGI_CALL",
    "SED_FORCE_DOWNLOAD_OFF",
    "SED_FORCE_DOWNLOAD_ON",
    "SQUEUE_WAIT_CALL",
    "SQUEUE_WAIT_FORMAT",
    "START_RUNS_CALL",
    "WAIT_GREPL_CALL",
    "delete_empty_realization_folders",
    "r_system",
    "runcode_for",
    "sed_replace",
    "select_runs_to_start",
    "squeue_wait_command",
    "start_runs",
    "wait_for_default_run",
]

# --- lines 90-100 -------------------------------------------------------------------------------------------

MODEL_NULL_MESSAGE = "Model cannot be NULL"
#: The condition call of ``stop("Model cannot be NULL")``: ``startRuns()`` as ``modeltests()`` calls it (line 39).
START_RUNS_CALL = "startRuns(model = model, user = user, mydir = mydir)"

GIT_RESET_PULL = "git reset --hard origin/develop && git pull"
GIT_RESET_PULL_CALL = 'system("git reset --hard origin/develop && git pull")'

DEFAULT_CFG = "config/default.cfg"
FORCE_DOWNLOAD_OFF = "cfg$force_download <- FALSE"
FORCE_DOWNLOAD_ON = "cfg$force_download <- TRUE"
#: The two sed command lines R runs (lines 100 and 117); the port edits the file instead (see :func:`sed_replace`).
SED_FORCE_DOWNLOAD_ON = f"sed -i 's/{FORCE_DOWNLOAD_OFF}/{FORCE_DOWNLOAD_ON}/' {DEFAULT_CFG}"
SED_FORCE_DOWNLOAD_OFF = f"sed -i 's/{FORCE_DOWNLOAD_ON}/{FORCE_DOWNLOAD_OFF}/' {DEFAULT_CFG}"

# --- lines 71-86 --------------------------------------------------------------------------------------------

#: ``find modules/ -name 'module.gms'`` (line 72), the path as R passes it.
MODULES_DIR = "modules/"
MODULE_FILE = "module.gms"
_MODULE_FILE_SUFFIX = "module.gms$"  # sub("module.gms$", "", moduleFile), line 74
_REALIZATION_LINE = "realization.gms"  # grep("realization.gms", readLines(moduleFile)), line 75
_MODULE_PREFIX = "^.*.modules/[0-9a-zA-Z_]{1,}/"  # line 80, inner sub()
_REALIZATION_SUFFIX = '/realization.gms"$'  # line 80, outer sub()
_ALWAYS_KEPT = ("module.gms", "input")  # line 78-79

# --- lines 106-146 (REMIND) ---------------------------------------------------------------------------------

AUTO_RENV_FIX_DEPS = ("autoRenvFixDeps", "TRUE")  # Sys.setenv(autoRenvFixDeps = "TRUE"), line 109
MSG_TEST_ONE_REGI = "Configuring and starting single testOneRegi-AMT"
REMIND_TEST_ONE_REGI = (
    "Rscript start.R --testOneRegi titletag=AMT "
    'slurmConfig="--qos=priority --nodes=1 --tasks-per-node=1 --wait --time=2:00:00"'
)
REMIND_TEST_ONE_REGI_CALL = (
    'system(paste("Rscript start.R --testOneRegi titletag=AMT", '
    '"slurmConfig=\\"--qos=priority --nodes=1 --tasks-per-node=1 --wait --time=2:00:00\\""))'
)
MSG_BUNDLE = "Configuring and starting bundle of AMT runs"
REMIND_BUNDLE = (
    "Rscript start.R startgroup=AMT titletag=AMT "
    'slurmConfig="--qos=standby --nodes=1 --tasks-per-node=12 --time=36:00:00" '
    "config/scenario_config.csv"
)
#: The first line of R's three-line deparse of the ``paste0()`` call (line 120), as ``Rscript`` prints it.
REMIND_BUNDLE_CALL = 'system(paste0("Rscript start.R ", "startgroup=AMT titletag=AMT ", '
#: Where the ``select_scenarios`` bridge saves ``selectScenarios()``'s data.frame: under ``Effects.tempdir()``.
BRIDGE_RDS_NAME = "runsToStart.rds"
MAGPIE_DIR = "magpie"  # withr::with_dir("magpie", ...), line 142
MAKE_TEST_FULL = "make test-full-slurm"
MAKE_TEST_FULL_CALL = 'system("make test-full-slurm")'

# --- lines 148-162 (MAgPIE) ---------------------------------------------------------------------------------

MAGPIE_DEFAULT_RUN = "Rscript start.R runscripts=default submit='SLURM priority'"
MAGPIE_DEFAULT_RUN_CALL = "system(\"Rscript start.R runscripts=default submit='SLURM priority'\")"
MAGPIE_WAIT_SECONDS = 300  # Sys.sleep(300), line 153
SQUEUE_WAIT_FORMAT = "%i %q %T %C %M %j %V %L %e %Z"
#: The first deparse line of the ``system(paste0("squeue -u ", user, ...), intern = TRUE)`` call (lines 154-155),
#: with the trailing comma and space R keeps when the deparse wraps (``In system(...),  :`` in the warning).
SQUEUE_WAIT_CALL = 'system(paste0("squeue -u ", user, " -h -o \'%i %q %T %C %M %j %V %L %e %Z\'"), '
WAIT_GREPL_CALL = 'grepl(paste0(mydir, "$"), jobsInSlurm)'
MAGPIE_TEST_RUNS = "Rscript start.R runscripts=test_runs submit='SLURM standby'"
MAGPIE_TEST_RUNS_CALL = "system(\"Rscript start.R runscripts=test_runs submit='SLURM standby'\")"

MSG_FINISHED = "Function 'startRuns' finished."


def _effects(effects: Effects | None) -> Effects:
    return effects if effects is not None else default_effects()


def _message(text: str) -> None:
    """``message(text)``: the line on stderr, resolved at call time so that captures and redirects work."""
    sys.stderr.write(text + "\n")
    sys.stderr.flush()


# ---------------------------------------------------------------------------------------------------------------
# R primitives
# ---------------------------------------------------------------------------------------------------------------


def runcode_for(today: dt.date) -> str:
    """Lines 92-96: ``.*-AMT_<today>|.*-AMT_<today + 1 day>`` (``%Y-%m-%d``)."""
    return f".*-AMT_{today.isoformat()}|.*-AMT_{(today + dt.timedelta(days=1)).isoformat()}"


def r_system(command: str, call: str, effects: Effects | None = None) -> int:
    """``system(command)`` without ``intern``: the command line through ``/bin/sh``, the status returned.

    The child inherits the process' stdout and stderr (nothing is captured, like R). R ignores the
    status everywhere in ``startRuns()``; only a command that cannot be run (status 127) makes ``system()``
    warn ``error in running command`` with the deparsed ``call`` and continue, reproduced here as an
    :class:`~modelstats.errors.RWarning`.
    """
    proc = _effects(effects).run_shell(command, capture=False)
    if proc.returncode == 127:
        warnings.warn(RWarning(call, "error in running command"), stacklevel=2)
    return int(proc.returncode)


def sed_replace(path: PathLike, old: str, new: str, effects: Effects | None = None) -> int:
    """The file edit of ``sed -i 's/<old>/<new>/' <path>`` for the literal patterns of lines 100 and 117.

    GNU sed replaces the first occurrence per line and keeps the line endings (a missing final newline
    stays missing); ``$`` inside ``cfg$force_download`` is a literal in a basic regular expression, so
    ``old`` is matched literally. The file is rewritten atomically (sed's temporary file and rename, the
    mode kept) even when nothing changed, as sed does. A file sed could not read makes sed print
    ``sed: can't read <path>: <reason>`` on stderr and exit with status 2, which R's ``system()`` ignores;
    the same happens here and the status is returned.
    """
    eff = _effects(effects)
    try:
        text = eff.read_text(path)
    except OSError as exc:
        reason = exc.strerror or "No such file or directory"
        sys.stderr.write(f"sed: can't read {os.fspath(path)}: {reason}\n")
        sys.stderr.flush()
        return 2
    edited = "\n".join(line.replace(old, new, 1) for line in text.split("\n"))
    eff.write_text(path, edited, atomic=True)
    return 0


def _r_readlines(text: str) -> list[str]:
    """``readLines()`` line splitting: LF, CRLF and CR end a line, an incomplete last line counts."""
    lines = re.split(r"\r\n|\r|\n", text)
    if lines and lines[-1] == "":
        lines.pop()
    return lines


def delete_empty_realization_folders(effects: Effects | None = None) -> list[str]:
    """``deleteEmptyRealizationFolders()`` (lines 71-86) in the current working directory; the unlinked paths.

    For every ``module.gms`` below ``modules/`` (``find``, here :meth:`Effects.walk`; the files are visited in
    sorted order, where ``find`` uses the directory order, because each module's deletions are independent)
    the realizations named in ``realization.gms`` lines are kept together with ``module.gms`` and ``input``;
    everything else ``dir()`` lists in the module directory (dot-files excluded, files included) is removed
    with ``unlink(recursive = TRUE)``.
    """
    eff = _effects(effects)
    module_files = sorted(
        os.path.join(dirpath, name)
        for dirpath, _dirnames, filenames in eff.walk(MODULES_DIR)
        for name in filenames
        if name == MODULE_FILE
    )
    deleted: list[str] = []
    for module_file in module_files:
        module_directory = re.sub(_MODULE_FILE_SUFFIX, "", module_file, count=1)
        realizations = [line for line in _r_readlines(eff.read_text(module_file)) if re.search(_REALIZATION_LINE, line)]
        kept = [
            *_ALWAYS_KEPT,
            *(
                re.sub(_REALIZATION_SUFFIX, "", re.sub(_MODULE_PREFIX, "", line, count=1), count=1)
                for line in realizations
            ),
        ]
        for name in eff.listdir_like_r(module_directory):  # setdiff(dir(moduleDirectory), ...)
            if name in kept:
                continue
            target = f"{module_directory}{name}"  # paste0(moduleDirectory, emptyRealizations)
            eff.delete(target, recursive=True)
            deleted.append(target)
    return deleted


def select_runs_to_start(mydir: PathLike, effects: Effects | None = None) -> pd.DataFrame:
    """Lines 127-136: ``selectScenarios(settings, interactive = FALSE, startgroup = "AMT")`` through the bridge.

    The bridge reads ``config/scenario_config.csv`` with R's ``read.csv2()`` call, sources ``scripts/start/*.R``,
    calls ``selectScenarios()`` and saves the data.frame to ``<tempdir>/runsToStart.rds``; that file is read
    back through :func:`modelstats.rdata_io.read_rds` and returned with its row names as ``selectScenarios()``
    gave them (the ``-AMT`` suffix is :func:`modelstats.amt.state.with_amt_suffix`, line 137). An R error in
    the bridged code raises :class:`~modelstats.errors.RParityError` with R's text, a bridge that cannot run
    :class:`~modelstats.amt.bridges.BridgeError`; nothing is written to ``mydir`` in either case.
    """
    eff = _effects(effects)
    out = os.path.join(eff.tempdir(), BRIDGE_RDS_NAME)
    select_scenarios(mydir, out, effects=eff)
    frame = read_rds(out, eff)
    if not isinstance(frame, pd.DataFrame):
        msg = f"the select_scenarios bridge wrote {type(frame).__name__} to {out}, not a data.frame"
        raise TypeError(msg)
    return frame


def squeue_wait_command(user: str | None) -> str:
    """``paste0("squeue -u ", user, " -h -o '<ten fields>'")`` of lines 154-155; a NULL ``user`` pastes as nothing."""
    return f"squeue -u {'' if user is None else user} -h -o '{SQUEUE_WAIT_FORMAT}'"


def wait_for_default_run(mydir: PathLike, user: str | None, effects: Effects | None = None) -> int:
    """Lines 152-159: sleep 300 s, then ``squeue`` until no job's line matches ``paste0(mydir, "$")``; the checks made.

    The regex is R's (TRE) through :func:`modelstats.slurm.r_regex`, compiled on every check as ``grepl()``
    does. A failing ``squeue`` (non-zero status) warns ``running command '...' had status N`` and yields no
    lines, so the loop ends; a ``squeue`` that cannot be run raises ``error in running command``.
    """
    eff = _effects(effects)
    command = squeue_wait_command(user)
    argv = shell_tokens(command)
    pattern = f"{os.fspath(mydir)}$"
    checks = 0
    while True:
        eff.sleep(MAGPIE_WAIT_SECONDS)
        jobs_in_slurm = system_intern(command, argv, SQUEUE_WAIT_CALL, eff)
        checks += 1
        regex = r_regex(pattern, call=WAIT_GREPL_CALL)
        if not any(regex.search(line) is not None for line in jobs_in_slurm):
            return checks


# ---------------------------------------------------------------------------------------------------------------
# startRuns
# ---------------------------------------------------------------------------------------------------------------


def start_runs(model: str | None, mydir: PathLike, user: str | None, effects: Effects | None = None) -> None:
    """``startRuns(model, mydir, user)`` (lines 88-166) with the process working directory at ``mydir``.

    ``mydir`` is used as given for the state-file paths (``paste0(mydir, "/runsToStart.rds")`` and
    ``paste0(mydir, "/runcode.rds")``) and for the MAgPIE wait regex; the relative paths of the R code
    (``config/default.cfg``, ``modules/``, ``magpie``) resolve against the working directory, which
    ``modeltests()`` has set to ``mydir`` (:meth:`Effects.chdir`). See the module docstring for the steps.
    """
    eff = _effects(effects)
    if model is None:
        raise RParityError(MODEL_NULL_MESSAGE, call=START_RUNS_CALL)
    mydir_text = os.fspath(mydir)
    runcode = runcode_for(eff.today())

    r_system(GIT_RESET_PULL, GIT_RESET_PULL_CALL, eff)
    # Force downloading of the input data in the first run
    sed_replace(DEFAULT_CFG, FORCE_DOWNLOAD_OFF, FORCE_DOWNLOAD_ON, eff)
    delete_empty_realization_folders(eff)

    if model == "REMIND":
        eff.setenv(*AUTO_RENV_FIX_DEPS)
        _message(MSG_TEST_ONE_REGI)
        r_system(REMIND_TEST_ONE_REGI, REMIND_TEST_ONE_REGI_CALL, eff)
        _message(MSG_BUNDLE)
        # do not download input data every run, reset force_download
        sed_replace(DEFAULT_CFG, FORCE_DOWNLOAD_ON, FORCE_DOWNLOAD_OFF, eff)
        r_system(REMIND_BUNDLE, REMIND_BUNDLE_CALL, eff)
        runs_to_start = state.with_amt_suffix(select_runs_to_start(mydir_text, eff))
        state.write_runs_to_start(f"{mydir_text}/{state.RUNS_TO_START_FILE}", runs_to_start, eff)
        with eff.chdir(MAGPIE_DIR):
            r_system(GIT_RESET_PULL, GIT_RESET_PULL_CALL, eff)
        r_system(MAKE_TEST_FULL, MAKE_TEST_FULL_CALL, eff)
    elif model == "MAgPIE":
        r_system(MAGPIE_DEFAULT_RUN, MAGPIE_DEFAULT_RUN_CALL, eff)
        wait_for_default_run(mydir_text, user, eff)
        r_system(MAGPIE_TEST_RUNS, MAGPIE_TEST_RUNS_CALL, eff)

    state.write_runcode(f"{mydir_text}/{state.RUNCODE_FILE}", runcode, eff)
    _message(MSG_FINISHED)
