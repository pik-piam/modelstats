"""``modeltests start``: start one REMIND AMT cycle (``R/modeltests.R:88-166``, ``startRuns``, REMIND branch).

The behaviour kept from the R version (``migration/06-user-requests.md`` section 4.1, "start"), as
:func:`start_cycle` performs it, with the R lines reproduced:

1. ``git reset --hard origin/develop && git pull`` in the checkout (``:98``): :func:`update_checkout`;
2. the ``selectScenarios()`` bridge (``:127-137``): :func:`modelstats.amt.bridges.select_scenarios`, run
   here before the first run is submitted (see "Order" below);
3. the transition ``idle -> started`` (``06`` 3.2) with the complete cycle record (the id and the runcode
   pattern of ``:92-96``, the start time and the tested commit, the selected scenario names with the
   ``-AMT`` suffix of ``:137``, the cycle log) and ``runcode.rds`` (``:164``, ``06`` 3.4), both through
   ``modelstats.amt.state.StateStore.save_started``, persisted *before* anything is submitted (see
   "Order");
4. ``cfg$force_download <- TRUE`` in ``config/default.cfg`` (``:100``, the ``sed -i`` as a file edit through
   ``Effects.replace_in_file``): :func:`set_force_download`;
5. the empty realization folders (``deleteEmptyRealizationFolders``, ``:71-86``, called at ``:104``):
   :func:`delete_empty_realization_folders`;
6. ``Rscript start.R --testOneRegi titletag=AMT slurmConfig="--qos=priority --nodes=1 --tasks-per-node=1
   --wait --time=2:00:00"`` (``:111-113``): :func:`start_test_one_regi`;
7. ``cfg$force_download <- FALSE`` (``:117``) and ``Rscript start.R startgroup=AMT titletag=AMT
   slurmConfig="--qos=standby --nodes=1 --tasks-per-node=12 --time=36:00:00" config/scenario_config.csv``
   (``:115-123``): :func:`start_bundle`;
8. ``magpie/``: ``git reset --hard origin/develop && git pull`` (``:142-144``, ``06`` section 2);
9. ``make test-full-slurm`` (``:146``): :func:`run_test_full`.

``runsToStart.rds`` (``:138``) and ``../.testsstatus`` (``:42``) are not written any more (``06`` 3.4); the
state file carries both. Every subprocess goes through :func:`modelstats.amt.logs.run_logged`: announced,
its lines in the cycle log with the ``[<tool>]`` prefix, a non-zero status reported. ``Rscript`` and ``make``
run with ``autoRenvFixDeps=TRUE`` (``:109``; R set it for the whole process, the bridge sets it for its own
child), nothing else is added to the environment.

**Failure semantics** (decision D-24: implementation artefacts are not reproduced). R ran every step with
``system()`` and ignored its status: a failed ``git reset`` skipped only the ``git pull`` behind the ``&&``
and the tests ran on a stale checkout (golden ``amt/remind-start-git-fail``), a failed ``make`` went
unnoticed until the evaluation (``amt/remind-start-make-fail``). Here the infrastructure commands are
checked: a non-zero ``git`` (checkout or ``magpie/``) or ``make`` status, a failed bridge, or any other
error inside the steps leaves the state ``failed`` with ``last_error`` and the cycle, sends the failure
notification and raises :class:`modelstats.amt.context.CycleFailedError`
(:func:`modelstats.amt.context.record_failure`). The persist step (3) is covered too, and it runs before
the first submission: a failed write of ``amt-state.json`` leaves the previous document in place (the
write is atomic) and the ``failed`` state is written instead with the cycle, nothing has been submitted,
and a plain ``start`` is refused until a human looks (``evaluate`` would report every scenario as missing,
``reset`` drops the cycle); a failed write of ``runcode.rds`` leaves the JSON rewritten as ``failed`` with
the cycle, likewise with nothing submitted, while ``rs-r -A`` may read a stale pattern until the next
successful start. A failure *after* the checkpoint (steps 4-9) leaves ``failed`` with the full cycle, so
``evaluate`` finds whatever was submitted. ``Rscript start.R`` keeps R's tolerance on purpose: its status
is reported as a warning and the start goes on, because the runs it submits are the test itself
(``--wait`` on the ``testOneRegi`` call makes ``start.R`` answer for a run that failed, and a run that
failed is a result for ``evaluate`` to report, not an infrastructure failure). The refused transitions
(``started`` without ``--force``, ``failed``) are checked before any effect
(``modelstats.amt.state.target_phase``) and change nothing.

**Order.** R selected the scenarios after both ``Rscript start.R`` calls (``:111-138``); the selection is a
pure read of ``config/scenario_config.csv``, so it is made right after ``git pull`` and before the first
run is submitted: a bridge failure then aborts before anything runs (``03`` 2.3, "no partial state"). The
cycle is persisted right after the selection, before the first ``Rscript start.R``, because the
submissions and ``make`` take hours (``--wait`` on the ``testOneRegi`` call) and a process killed in
between (SIGTERM, a node reboot, Ctrl-C) runs no handler: R set ``.testsstatus`` to ``next:evaluate``
only after ``startRuns`` returned (``:42``), so a killed start was repeated by the next cron invocation
and the day's runcode matched both submissions (Codex review 2026-10-11, finding 1). Here a termination
before the checkpoint has submitted nothing and a rerun is harmless; one after it leaves ``started`` with
the full cycle, which ``run`` evaluates (``No runs started`` and the missing scenarios when nothing ran)
or a human abandons with ``start --force``; never a second submission. The R goldens record the bridge's
``git log -1`` and ``Rscript select_scenarios.R`` nowhere at start (R read the commit at evaluate,
``:198``): both are additions on the allowed-difference list of ``06`` 4.4 item 3.
Under ``--dry-run`` the bridge is not run: the script sources the checkout's ``scripts/start/*.R`` after
R read its ``.Rprofile``, which is REMIND code, not a read of the configuration (a fresh clone's
``.Rprofile`` bootstraps ``renv``, with downloads and writes). :class:`modelstats.env.DryRunEffects`
announces the call (``would run Rscript ... select_scenarios.R AMT``) and answers it with an empty
selection, so the cycle a dry run reports records no scenario names.

**Empty realization folders** (``:71-86``). ``find modules/ -name 'module.gms'`` names every module file at
any depth, without following symbolic links to directories (``find``'s default ``-P``); for each,
``dir(<module directory>)`` minus ``module.gms``, ``input`` and the realization names parsed out of the
``realization.gms`` lines of the module file is unlinked recursively. As in R, nothing checks emptiness:
every other entry of a module directory, a regular file included, is removed (register candidate,
reported by this unit).
"""

from __future__ import annotations

import re
from collections.abc import Mapping
from dataclasses import replace
from datetime import datetime
from pathlib import Path
from typing import Final

from modelstats.amt.bridges import select_scenarios
from modelstats.amt.context import AmtContext, record_failure
from modelstats.amt.logs import run_logged
from modelstats.amt.state import AmtState, Command, Cycle, cycle_id, runcode_pattern, start, target_phase
from modelstats.env import Effects

__all__ = [
    "BUNDLE_ARGV",
    "DEFAULT_CFG",
    "FORCE_DOWNLOAD_FALSE",
    "FORCE_DOWNLOAD_TRUE",
    "GIT_PULL_ARGV",
    "GIT_RESET_ARGV",
    "KEPT_MODULE_ENTRIES",
    "MAGPIE_DIR",
    "MAKE_ARGV",
    "MODULES_DIR",
    "MODULE_FILE",
    "START_ENVIRONMENT",
    "TEST_ONE_REGI_ARGV",
    "delete_empty_realization_folders",
    "empty_realizations",
    "module_files",
    "new_cycle",
    "realization_name",
    "run_test_full",
    "set_force_download",
    "start_bundle",
    "start_cycle",
    "start_test_one_regi",
    "update_checkout",
]

GIT_RESET_ARGV: Final = ("git", "reset", "--hard", "origin/develop")
"""``git reset --hard origin/develop`` (``R/modeltests.R:98, 143``)."""

GIT_PULL_ARGV: Final = ("git", "pull")
"""``git pull`` (``R/modeltests.R:98, 143``), run only after a successful reset (the ``&&``)."""

TEST_ONE_REGI_ARGV: Final = (
    "Rscript",
    "start.R",
    "--testOneRegi",
    "titletag=AMT",
    "slurmConfig=--qos=priority --nodes=1 --tasks-per-node=1 --wait --time=2:00:00",
)
"""``R/modeltests.R:112-113`` after the shell removed the quotes around ``slurmConfig``'s value."""

BUNDLE_ARGV: Final = (
    "Rscript",
    "start.R",
    "startgroup=AMT",
    "titletag=AMT",
    "slurmConfig=--qos=standby --nodes=1 --tasks-per-node=12 --time=36:00:00",
    "config/scenario_config.csv",
)
"""``R/modeltests.R:120-123`` after the shell removed the quotes around ``slurmConfig``'s value."""

MAKE_ARGV: Final = ("make", "test-full-slurm")
"""``make test-full-slurm`` (``R/modeltests.R:146``)."""

START_ENVIRONMENT: Final[Mapping[str, str]] = {"autoRenvFixDeps": "TRUE"}
"""``Sys.setenv(autoRenvFixDeps = "TRUE")`` (``R/modeltests.R:109``): no question by ``piamenv::fixDeps``."""

DEFAULT_CFG: Final = "config/default.cfg"
"""The file the two ``sed -i`` calls edit (``R/modeltests.R:100, 117``)."""

FORCE_DOWNLOAD_FALSE: Final = "cfg$force_download <- FALSE"
FORCE_DOWNLOAD_TRUE: Final = "cfg$force_download <- TRUE"
"""The two sides of ``s/cfg$force_download <- FALSE/cfg$force_download <- TRUE/`` (literal in sed's BRE)."""

MAGPIE_DIR: Final = "magpie"
"""The MAgPIE submodule of the REMIND checkout that ``make test-full-slurm`` needs (``:142``)."""

MODULES_DIR: Final = "modules"
MODULE_FILE: Final = "module.gms"
KEPT_MODULE_ENTRIES: Final = ("module.gms", "input")
"""What ``deleteEmptyRealizationFolders`` keeps besides the realizations (``R/modeltests.R:78-79``)."""

_REALIZATION_LINE: Final = re.compile(r"realization.gms")
"""``grep("realization.gms", readLines(moduleFile))`` (``R/modeltests.R:75``; the dot is unescaped in R)."""
_MODULE_PREFIX: Final = re.compile(r"^.*.modules/[0-9a-zA-Z_]{1,}/")
"""``sub("^.*.modules/[0-9a-zA-Z_]{1,}/", "", ...)`` (``R/modeltests.R:80``)."""
_REALIZATION_SUFFIX: Final = re.compile(r'/realization.gms"$')
"""``sub("/realization.gms\\"$", "", ...)`` (``R/modeltests.R:80``)."""


# ---------------------------------------------------------------------------------------------
# The command
# ---------------------------------------------------------------------------------------------


def start_cycle(ctx: AmtContext) -> AmtState:
    """Start the AMT cycle in ``ctx.dir`` from ``ctx.state`` and return the saved ``started`` state.

    ``ctx.state`` is what the CLI loaded under the lock (``StateStore.load_or_migrate``). The transition is
    checked first (``06`` 3.2: a refused command is :class:`modelstats.amt.state.AmtStateError` with
    nothing changed), then the steps of the module docstring run; a failure among them is recorded as the
    ``failed`` state and raised as :class:`modelstats.amt.context.CycleFailedError`. The cycle's log path
    is :meth:`AmtContext.cycle_log_path` of the cycle id (the start date), the file the CLI opened for this
    command. The ``started`` state is on disk before the first ``Rscript start.R`` (module docstring,
    "Order"), so an interruption during the submissions never leads to a second one.
    """
    current = ctx.state
    force = ctx.options.force
    target_phase(current, Command.START, force=force)
    cycle = new_cycle(ctx, ctx.effects.now())
    ctx.logger.info(
        "Starting AMT cycle %s in %s (run pattern %s%s)",
        cycle.id,
        ctx.dir,
        cycle.runcode,
        ", dry run" if ctx.options.dry_run else "",
    )
    try:
        update_checkout(ctx, ctx.dir)
        selection = select_scenarios(ctx.effects, ctx.dir, logger=ctx.logger)
        if ctx.options.dry_run:
            ctx.logger.info(
                "dry run: the selectScenarios() bridge was not run (it executes the checkout's"
                " scripts/start/*.R); the cycle records no scenario names"
            )
        cycle = replace(cycle, started_commit=selection.commit, runs_to_start=selection.names)
        started = start(current, cycle, force=force)
        ctx.store.save_started(started)
        ctx.logger.info(
            "cycle %s recorded as 'started' in %s (pattern in %s) before the first submission",
            cycle.id,
            ctx.state_path,
            ctx.store.runcode_path,
        )
        set_force_download(ctx, enabled=True)
        delete_empty_realization_folders(ctx)
        start_test_one_regi(ctx)
        start_bundle(ctx)
        update_checkout(ctx, ctx.dir / MAGPIE_DIR)
        run_test_full(ctx)
    except Exception as error:
        raise record_failure(ctx, Command.START, error, cycle=cycle) from error
    ctx.logger.info(
        "Function 'startRuns' finished: cycle %s started with %d scenario%s, state in %s, pattern in %s",
        cycle.id,
        len(cycle.runs_to_start),
        "" if len(cycle.runs_to_start) == 1 else "s",
        ctx.state_path,
        ctx.store.runcode_path,
    )
    return started


def new_cycle(ctx: AmtContext, now: datetime) -> Cycle:
    """The cycle record of a start at ``now``: id and runcode from the date, the log path, nothing else yet.

    ``R/modeltests.R:92-96``: ``Sys.Date()`` is ``now.date()`` in the session's zone. ``started_commit`` and
    ``runs_to_start`` are filled from the bridge's answer; until then the record is what a failed start
    keeps.
    """
    today = now.date()
    identity = cycle_id(today)
    return Cycle(
        id=identity,
        runcode=runcode_pattern(today),
        started_at=now,
        log=str(ctx.cycle_log_path(identity)),
    )


# ---------------------------------------------------------------------------------------------
# The steps (R/modeltests.R:98-146)
# ---------------------------------------------------------------------------------------------


def update_checkout(ctx: AmtContext, directory: Path) -> None:
    """``git reset --hard origin/develop && git pull`` in ``directory`` (``R/modeltests.R:98, 143``).

    The ``&&`` is kept: ``pull`` runs only after a successful ``reset``. Either failure raises
    ``CommandFailedError`` (the start aborts, see the module docstring).
    """
    run_logged(ctx.effects, ctx.logger, GIT_RESET_ARGV, cwd=directory, check=True)
    run_logged(ctx.effects, ctx.logger, GIT_PULL_ARGV, cwd=directory, check=True)


def set_force_download(ctx: AmtContext, *, enabled: bool) -> int:
    """``sed -i 's/cfg$force_download <- FALSE/cfg$force_download <- TRUE/' config/default.cfg`` or back.

    ``R/modeltests.R:100`` (``enabled``: the first run downloads the input data) and ``:117`` (reset before
    the bundle so that the other runs do not download again). ``Effects.replace_in_file`` is the literal,
    first-occurrence-per-line replacement of ``sed``; the file is rewritten even without a match, as
    ``sed -i`` does (the golden shows ``default.cfg`` touched). Returns the number of lines changed; none
    is a warning in the log, not an error (``sed`` was silent about it).
    """
    old, new = (FORCE_DOWNLOAD_FALSE, FORCE_DOWNLOAD_TRUE)
    if not enabled:
        old, new = new, old
    path = ctx.dir / DEFAULT_CFG
    changed = ctx.effects.replace_in_file(path, old, new)
    if changed == 0:
        ctx.logger.warning("no line '%s' in %s: nothing replaced", old, path)
    else:
        ctx.logger.info("set '%s' in %s (%d line%s)", new, path, changed, "" if changed == 1 else "s")
    return changed


def delete_empty_realization_folders(ctx: AmtContext) -> list[Path]:
    """``deleteEmptyRealizationFolders()`` (``R/modeltests.R:71-86``): unlink what no module file names.

    For every ``module.gms`` below ``modules/`` (``find modules/ -name 'module.gms'``, ``:72``), the entries
    of its directory (``dir(moduleDirectory)``, ``:77``: no dot names, R's order) other than ``module.gms``,
    ``input`` and the realizations named in the file (:func:`empty_realizations`) are removed recursively
    (``unlink(..., recursive = TRUE)``, ``:83``). Returns the removed paths. "Empty realization folders
    break the models, but can be left over because git does not delete empty folders" (``:102-103``).
    """
    removed: list[Path] = []
    for module_file in module_files(ctx.effects, ctx.dir / MODULES_DIR):
        for entry in empty_realizations(ctx.effects, module_file):
            path = module_file.parent / entry
            ctx.effects.delete_tree(path)
            ctx.logger.info("deleted %s (not a realization named in %s)", path, module_file)
            removed.append(path)
    if not removed:
        ctx.logger.info("no empty realization folders below %s", ctx.dir / MODULES_DIR)
    return removed


def module_files(effects: Effects, modules_dir: Path) -> list[Path]:
    """``find modules/ -name 'module.gms'`` (``R/modeltests.R:72``): every ``module.gms`` below the directory.

    Descends into every subdirectory, dot names included, as ``find`` does (``Effects.list_dirs_like_r``),
    but not into a symbolic link to a directory: ``find`` runs with its default ``-P`` and never follows
    one below its starting point (``Effects.is_symlink``), so a module reachable only through a link is
    not swept and nothing outside the checkout can be removed. The link itself may still be unlinked as
    an entry of its module directory, as R's ``unlink`` removes the link, not the target. A missing
    ``modules/`` gives nothing, as ``find``'s error left R's loop with nothing to do. R's order is
    ``find``'s directory order; here each level is in R's collation, which changes nothing about what is
    removed.
    """
    found: list[Path] = []

    def walk(directory: Path) -> None:
        candidate = directory / MODULE_FILE
        if effects.exists(candidate) and not effects.is_dir(candidate):
            found.append(candidate)
        for sub in effects.list_dirs_like_r(directory, full_names=True):
            if effects.is_symlink(sub):
                continue
            walk(Path(sub))

    walk(modules_dir)
    return found


def empty_realizations(effects: Effects, module_file: Path) -> list[str]:
    """``setdiff(dir(moduleDirectory), c("module.gms", "input", <realizations>))`` (``R/modeltests.R:75-81``).

    The realizations are the ``realization.gms`` lines of the module file reduced by
    :func:`realization_name`; the result keeps the order of ``dir()`` (R's collation, dot names left out),
    files included: nothing in R checks that an entry is a directory or empty.
    """
    lines = effects.read_text(module_file).splitlines()
    named = {realization_name(line) for line in lines if _REALIZATION_LINE.search(line)}
    kept = {*KEPT_MODULE_ENTRIES, *named}
    return [entry for entry in effects.listdir_like_r(module_file.parent) if entry not in kept]


def realization_name(include_line: str) -> str:
    r"""The realization a ``module.gms`` line names (``R/modeltests.R:80-81``).

    ``sub("/realization.gms\\"$", "", sub("^.*.modules/[0-9a-zA-Z_]{1,}/", "", line))``: the first
    substitution drops everything up to and including ``modules/<module>/`` (greedy, so the last such
    occurrence), the second the trailing ``/realization.gms"``. For ``$include
    "./modules/01_macro/singleSectorGr/realization.gms"`` that is ``singleSectorGr``; a line that does not
    have that shape gives some other text, which only ever keeps an entry that would not have existed.
    """
    stripped = _MODULE_PREFIX.sub("", include_line, count=1)
    return _REALIZATION_SUFFIX.sub("", stripped, count=1)


def start_test_one_regi(ctx: AmtContext) -> None:
    """``Rscript start.R --testOneRegi titletag=AMT slurmConfig="..."`` (``R/modeltests.R:111-113``).

    The single region run with ``--wait``: ``start.R`` returns when the run has finished and the input data
    is downloaded (``force_download`` is on). A non-zero status is a warning in the log, not an abort (see
    the module docstring).
    """
    ctx.logger.info("Configuring and starting single testOneRegi-AMT")
    run_logged(ctx.effects, ctx.logger, TEST_ONE_REGI_ARGV, cwd=ctx.dir, env=START_ENVIRONMENT)


def start_bundle(ctx: AmtContext) -> None:
    """``force_download`` off, then the AMT bundle ``Rscript start.R startgroup=AMT ...`` (``:115-123``)."""
    ctx.logger.info("Configuring and starting bundle of AMT runs")
    set_force_download(ctx, enabled=False)
    run_logged(ctx.effects, ctx.logger, BUNDLE_ARGV, cwd=ctx.dir, env=START_ENVIRONMENT)


def run_test_full(ctx: AmtContext) -> None:
    """``make test-full-slurm`` in the checkout (``R/modeltests.R:146``), after ``magpie/`` was updated.

    Submits the ``testthat`` job whose ``test-full.log`` the evaluation reads (``:381-398``). A non-zero
    status aborts the start (an infrastructure failure, see the module docstring).
    """
    ctx.logger.info("Submitting make test-full-slurm")
    run_logged(ctx.effects, ctx.logger, MAKE_ARGV, cwd=ctx.dir, env=START_ENVIRONMENT, check=True)
