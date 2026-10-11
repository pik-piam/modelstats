"""``evaluate_cycle``: the evaluation of one AMT cycle (``R/modeltests.R:168-492``, ``06`` sections 3 and 4).

``evaluateRuns()`` waited for the runs of the cycle, wrote the README block, compared each converged run
with its predecessor, archived old runs, published the README and posted the Mattermost message. This
module is its REMIND-only rewrite (``migration/06-user-requests.md`` 4.1 keeps the behaviour, 4.2 lists
what changes under decision D-24). The steps, in R's order, with the lines they come from:

1. :func:`wait_for_runs` (``:178-195``): ``squeue -u USER -h -o '%i %q %T %C %M %j %V %L %e %Z'`` through
   ``Effects.run`` until no listed job has the checkout at or below its work directory (the ``%Z`` field,
   both sides canonicalised through ``Effects.normalize_path``; R applied ``grepl(mydir, line)`` to the
   whole line), 600 s between polls through ``Effects.sleep``; more than three consecutive ``squeue``
   failures abort with R's message, a success resets the counter.
2. :func:`tested_commit` (``:198``): ``git log -1`` parsed by :func:`modelstats.amt.bridges.checkout_commit`;
   a failed ``git`` is an :class:`EvaluateError` (R aborted on ``[[1]]`` of an empty vector).
3. :func:`merge_list` (``:199-201``): ``git log --merges --pretty=oneline <last>..<tested> --abbrev-commit``
   filtered to the ``Merge pull request`` lines, ``<last>`` being ``last_completed.tested_commit`` of the
   state (``lastcommit.rds``). Without a base (first cycle, migration without the file) the list is
   empty and the log says so; R aborted on the missing ``readRDS``.
4. :func:`started_runs` (``:238-239``): the directories of ``output/`` matching the cycle's ``runcode``.
5. :func:`review_run` per started run (``:273-364``): a :class:`modelstats.run.Run` for the rules and its
   parity record (``Run.to_status_record``) for the README row; :func:`rule_errors` applies the five
   rules of ``:280-298`` (``Calib_nash`` must be ``Clb_converged``, every other non-``testOneRegi`` run must
   be converged, ``testOneRegi`` must be ``2: Locally Optimal``, no ``sumErr``, ``runInAppResults`` must be
   ``yes``), rule-level defects included (BUG-001: ``Clb_converged`` needs more than ten calibration files,
   so the rule fires for every calibration run of today's REMIND). For a converged or not converged run:
   the RSE ``rsync`` of ``fulldata.gdx`` for a converged ``SSP2-NPi-AMT`` (``:306-310``), the previous
   run from :func:`modelstats.amt.previous.find_previous_run` (D-24, replacing the ``gRS.rds`` history),
   the runtime rule ``current > 1.25 x previous`` on ``runstatistics.rda`` for a converged run
   (``:326-331``), and the compareScenarios2 ``sbatch`` command line byte for byte
   (:func:`compare_scenarios_argv`, ``:339-352``) run from ``cfg$remind_folder`` when both mif files exist
   and no ``comp_with_*.pdf`` is there yet (``:334-337``).
6. :func:`scenarios_not_started` (``:367-378``): by set difference on the scenario names (D-24; R counted
   and missed a scenario hidden by one started twice, BUG-024); a non-empty list is written to the README
   and added to the error list as :data:`SCENARIOS_NOT_STARTED`.
7. :func:`testthat_result` (``:380-398``): ``<checkout>/test-full.log`` (R's ``../test-full.log`` from
   ``output/``), the last ``[ FAIL`` line, as a :class:`TestthatVerdict`; the rename to
   ``<checkout>/tests/test-full-<mtime date>.log`` (``:389``) is deferred to step 12
   (:func:`archive_testthat_log`) so that a retried evaluation reads the same log.
8. :func:`archive_old_runs` (``:400-418``): the ``*AMT*`` directories of ``output/`` whose folder date is
   older than 90 days are moved into ``output/archive/`` one by one through ``Effects.move``, the
   directory created when missing (D-24: BUG-016, BUG-033 gone).
9. ``No runs started`` (``:421``), the summary (``:423-427``) and the README through
   :mod:`modelstats.amt.readme`, written next to the cycle log (:func:`readme_path`; R used ``tempdir()``).
10. :func:`publish_readme` (``:433-445``) unless ``--no-publish``: in the report repository ``git reset
    --hard origin/master``, ``git pull``, the copy as ``README.md``, ``git add README.md``, ``git commit -m
    'Automated Test Results'``, ``git push``, each ``git`` checked (D-24, the evaluate counterpart of
    BUG-075: R's ``system()`` ignored every status, so a failed push left the README unpublished and
    nobody told); a non-zero status fails the cycle, which keeps the cycle for a retry. A ``README.md``
    already equal to the new one after the pull is not committed (:attr:`Publication.UNCHANGED`: the
    retry of a cycle whose push succeeded before the state was saved).
11. The Mattermost message (``:447-486``) through :mod:`modelstats.amt.notify`: the ``rs -A`` and sanity
    tables of the started runs, the summary, the testthat sentence, the git lines, the scenarios that
    did not start; composed and posted only when the webhook variable is set (``:449``: without a token
    R captured nothing either). The sanity table lists the cycle's started runs of the checkout's
    ``output/`` (D-24: ``getSanityChecks()`` read the hard-coded cluster path, a path-arithmetic
    artefact of the kind BUG-013 names).
12. The ``test-full.log`` rename of step 7 (:func:`archive_testthat_log`), then the transition ``started
    -> idle`` (``06`` 3.2): :func:`modelstats.amt.state.complete` with the tested commit and the summary,
    saved through the store (R saved ``lastcommit.rds`` last, ``:489``).

The command receives the :class:`modelstats.amt.context.AmtContext` that ``start`` receives too (``06``
4.3). Any exception after the pre-checks (:func:`cycle_to_evaluate`, the report repository) goes through
:func:`modelstats.amt.context.record_failure`: the state becomes ``failed`` with the error in
``last_error``, the failure notification of :func:`modelstats.amt.notify.failure_message` is sent (R sent
none) and :class:`modelstats.amt.context.CycleFailedError` is raised from the original error; the
pre-checks raise before any effect and leave the state untouched. Every R ``message()`` becomes a log
record of the cycle logger (``06`` 3.3) and every subprocess runs through
:func:`modelstats.amt.logs.run_logged`.

The R AMT goldens (``migration/goldens/amt/remind-evaluate*``) are reference material with an allowed
difference list (``06`` 4.4): ``tests/unit/test_amt_evaluate.py`` classifies every case as kept, changed by
D-24 or mapped onto the state machine.
"""

from __future__ import annotations

import io
import logging
import os
import re
from collections.abc import Callable, Iterable, Sequence
from dataclasses import dataclass
from datetime import date, timedelta
from enum import Enum
from pathlib import Path
from typing import Final, Protocol

from modelstats.amt.bridges import GIT, checkout_commit
from modelstats.amt.context import AmtContext, record_failure
from modelstats.amt.logs import run_logged
from modelstats.amt.notify import (
    Delivery,
    StatusReport,
    capture_tables,
    send_to,
    status_message,
    webhook_url,
)
from modelstats.amt.previous import ARCHIVE_DIR, find_previous_run
from modelstats.amt.readme import ReadmeReport, git_info_lines, render_readme, summary_line
from modelstats.amt.state import (
    AmtState,
    Command,
    Cycle,
    StateStore,
    evaluable_cycle,
    target_phase,
)
from modelstats.amt.state import complete as complete_state
from modelstats.env import Effects, PathLike
from modelstats.errors import ModelstatsError
from modelstats.run import Convergence, MifState, Run
from modelstats.run_status import RunStatus

__all__ = [
    "ARCHIVE_DAYS",
    "CHECK_RUNTIME",
    "COMMIT_MESSAGE",
    "DEFAULT_RUN",
    "GDX_ON_RSE_SERVER",
    "MERGE_MARK",
    "NOT_CONVERGED",
    "NOT_OPTIMAL",
    "NOT_REPORTED",
    "NO_RUNS_STARTED",
    "POLL_SECONDS",
    "README_FILE_TEMPLATE",
    "RSE_RUN",
    "RUNTIME_FACTOR",
    "SCENARIOS_NOT_STARTED",
    "SQUEUE_FAILED",
    "SQUEUE_FAILURES_ALLOWED",
    "SQUEUE_FORMAT",
    "SUMMATION_GAPS",
    "TESTTHAT_MISSING",
    "TEST_FULL_LOG",
    "TESTS_DIR",
    "EvaluateContext",
    "EvaluateError",
    "Evaluation",
    "Publication",
    "RunReview",
    "TestthatVerdict",
    "archive_old_runs",
    "archive_testthat_log",
    "compare_scenarios_argv",
    "comparison_pending",
    "cycle_to_evaluate",
    "evaluate_cycle",
    "expected_scenarios",
    "folder_date",
    "gams_hours",
    "job_in_checkout",
    "merge_list",
    "old_runs",
    "publish_readme",
    "readme_path",
    "review_run",
    "rule_errors",
    "runtime_is_slower",
    "scenario_of",
    "scenarios_not_started",
    "started_runs",
    "tested_commit",
    "testthat_result",
    "wait_for_runs",
]

# --- the wait loop (R/modeltests.R:178-195) ----------------------------------------------------------------

SQUEUE: Final = "squeue"
SQUEUE_FORMAT: Final = "%i %q %T %C %M %j %V %L %e %Z"
"""The ``-o`` format of ``R/modeltests.R:182``; ``%Z`` (the work directory) is the last field."""
POLL_SECONDS: Final = 600
"""``Sys.sleep(600)`` between two polls (``R/modeltests.R:193``)."""
SQUEUE_FAILURES_ALLOWED: Final = 3
"""``if (errCount > 3) stop(...)`` (``R/modeltests.R:186``): the fourth consecutive failure aborts."""
SQUEUE_FAILED: Final = "squeue had exit status > 0 more than 3 times in a row."
"""The ``stop()`` message of ``R/modeltests.R:186``."""

# --- the error texts (R/modeltests.R:282-298, 330, 421) ---------------------------------------------------

NOT_CONVERGED: Final = "Some run(s) did not converge"
NOT_OPTIMAL: Final = "testOneRegi does not return an optimal solution"
SUMMATION_GAPS: Final = "Summation checks for some run(s) revealed some gaps"
NOT_REPORTED: Final = "Some run(s) did not report correctly"
CHECK_RUNTIME: Final = "Check runtime! Have some scenarios become slower?"
NO_RUNS_STARTED: Final = "No runs started"
SCENARIOS_NOT_STARTED: Final = "Some scenario(s) did not start at all"
"""New (``06`` 4.2, D-24): the scenarios of the "did not start" block are an error of the cycle (BUG-024)."""

CALIBRATION_RUN_TYPE: Final = "Calib_nash"
"""``grsi[, "RunType"] == "Calib_nash"`` (``R/modeltests.R:284``)."""
EXEMPT_RUN_TYPES: Final = re.compile("Calib_nash|testOneRegi")
"""``grepl("Calib_nash|testOneRegi", RunType)`` (``:280``): the run types the convergence rule skips."""
TEST_ONE_REGI: Final = "testOneRegi"
"""``grepl("testOneRegi", RunType)`` (``:287``)."""
OPTIMAL_CODE: Final = 2
"""``modelstat == "2: Locally Optimal"`` (``:287``) as the code of :class:`modelstats.run.ModelStat`."""
CONVERGED: Final = frozenset({Convergence.CONVERGED, Convergence.CONVERGED_HAD_INFES})
"""``Conv %in% c("converged", "converged (had INFES)")`` (``:281, 306, 326``)."""
REVIEWED_CONV: Final = frozenset({"converged", "converged (had INFES)", "not_converged"})
"""``grsi[, "Conv"] %in% c("converged", "converged (had INFES)", "not_converged")`` (``R/modeltests.R:301``)
on the parity ``Conv`` string: the runs that get a previous run, the runtime rule and the comparison. The
p80_repy digit string of an interrupted run below ``cm_iteration_max`` (``R/getRunStatus.R:288-290``) is
not in the set, so such a run is not reviewed, although ``Run.convergence`` calls it ``NOT_CONVERGED``
(D-25); the gate is a rule of the model owners and is kept on R's vocabulary (``06`` 4.2)."""

# --- the RSE server and compareScenarios2 (R/modeltests.R:304-357) -------------------------------------

RSE_RUN: Final = "SSP2-NPi-AMT"
"""``grepl("SSP2-NPi-AMT", rownames(grsi))`` (``:306``): the run whose ``fulldata.gdx`` feeds RSE tests."""
GDX_ON_RSE_SERVER: Final = (
    "rse@rse.pik-potsdam.de:/webservice/data/example/remind2_test-convGDX2MIF_SSP2-NPi-AMT.gdx"
)
"""``gdxOnRseServer`` of ``R/modeltests.R:307-308``."""
RSYNC_ARGV: Final = ("rsync", "-e", "ssh", "-av", "fulldata.gdx", GDX_ON_RSE_SERVER)
"""``rsync -e ssh -av fulldata.gdx <server>`` (``:310``), run inside the run folder."""
RUNTIME_FACTOR: Final = 1.25
"""``currentRunTime > (1.25 * lastRunTime)`` (``R/modeltests.R:329``), in hours of GAMS wall time."""
COMPARISON_PREFIX: Final = "comp_with_"
"""``outFileName <- paste0("comp_with_", lastRun)`` (``:339``) and the PDF the check looks for (``:337``)."""
COMPARISON_PDF: Final = "comp_with_.*.pdf"
"""``grepl("comp_with_.*.pdf", dir())`` (``:337``)."""
SBATCH: Final = "sbatch"
COMPARE_SCRIPT: Final = "scripts/cs2/run_compareScenarios2.R"
"""Relative to ``cfg$remind_folder``, which is why ``sbatch`` runs from there (``:354-356``)."""

# --- the "did not start" block, test-full.log and the archive (R/modeltests.R:367-418) ---------------------

DEFAULT_RUN: Final = "default-AMT"
"""``setdiff(c("default-AMT", runsToStart), ...)`` (``:373``): the default run every cycle starts."""
DATETIME_PATTERN: Final = re.compile(r"_[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}\.[0-9]{2}\.[0-9]{2}")
"""``datetimepattern`` of ``R/modeltests.R:371``."""
TEST_FULL_LOG: Final = "test-full.log"
"""``../test-full.log`` seen from ``output/`` (``:381``): ``make test-full-slurm`` writes it there."""
TESTS_DIR: Final = "tests"
"""``tests/test-full-<date>.log`` (``:388``)."""
FAIL_MARK: Final = "[ FAIL"
"""``grep("\\\\[ FAIL", logStatus)`` (``:387``)."""
TESTTHAT_MISSING: Final = "Could not check for the results of `make test-full`, test-full.log not found"
"""``R/modeltests.R:384``."""
ARCHIVE_DAYS: Final = 90
"""``daysback`` (``:401``)."""
AMT_NAME: Final = ".*AMT.*"
"""``list.files(pattern = ".*AMT.*", include.dirs = TRUE)`` (``:404``)."""
FOLDER_DATE: Final = re.compile(r".*_([0-9]{4}-[0-9]{2}-[0-9]{2})_.*$")
"""``gsub(".*_([0-9]{4}-[0-9]{2}-[0-9]{2})_.*$", "\\\\1", runname)`` (``:409``): the last date in the name."""

# --- publishing and the README file (R/modeltests.R:202, 433-445) ------------------------------------------

COMMIT_MESSAGE: Final = "Automated Test Results"
"""``git commit -m 'Automated Test Results'`` (``:442``)."""
PUBLISHED_NAME: Final = "README.md"
"""The file the report repository receives (``:437-438``)."""
README_FILE_TEMPLATE: Final = "amt-{cycle_id}-README.md"
"""``<log-dir>/amt-<cycle id>-README.md``: where the README is kept (R: ``tempdir()``, ``:202``)."""
MERGE_MARK: Final = "Merge pull request"
"""``| grep 'Merge pull request'`` (``:201``)."""


class EvaluateError(ModelstatsError):
    """The evaluation stopped: R's ``stop()`` (the wait loop) or an R abort the rewrite reports by name."""


# ---------------------------------------------------------------------------------------------
# What the steps read of the context (06 section 4.3: one AmtContext for start and evaluate)
# ---------------------------------------------------------------------------------------------


class EvaluateContext(Protocol):
    """The part of :class:`modelstats.amt.context.AmtContext` the evaluation steps read.

    The steps (:func:`review_run`, the README, the notification) are typed against this interface so that
    a test can hand them a minimal context; :func:`evaluate_cycle` itself takes the concrete
    ``AmtContext`` because the failure path (:func:`modelstats.amt.context.record_failure`) is shared with
    ``start``. ``checkout`` is ``--dir`` (R's ``mydir``), ``output_dir`` its ``output/``, ``report_repo``
    ``--report-repo`` (``gitdir``, needed unless ``publish`` is off), ``user`` the SLURM user whose jobs are
    awaited, ``publish`` and ``compare_scenarios`` the two flags, ``mattermost_variable`` the environment
    variable holding the webhook URL (``--mattermost-url-env``; unset variable: no message), ``log_dir``
    where the cycle log and the README go, ``store`` and ``state`` the state machine as the command loaded
    it under the lock.
    """

    @property
    def effects(self) -> Effects:
        """The environment (``03`` 2.3)."""
        ...

    @property
    def logger(self) -> logging.Logger:
        """The cycle logger (``06`` 3.3)."""
        ...

    @property
    def checkout(self) -> Path:
        """The REMIND checkout (``--dir``)."""
        ...

    @property
    def output_dir(self) -> Path:
        """``<dir>/output``: the run folders."""
        ...

    @property
    def report_repo(self) -> Path | None:
        """The git clone receiving ``README.md`` (``--report-repo``)."""
        ...

    @property
    def user(self) -> str:
        """The SLURM user (``--user``)."""
        ...

    @property
    def publish(self) -> bool:
        """``--publish`` / ``--no-publish``."""
        ...

    @property
    def compare_scenarios(self) -> bool:
        """``--compare-scenarios`` / ``--no-compare-scenarios``."""
        ...

    @property
    def mattermost_variable(self) -> str:
        """``--mattermost-url-env``."""
        ...

    @property
    def log_dir(self) -> Path:
        """``--log-dir``."""
        ...

    @property
    def store(self) -> StateStore:
        """The state file of the checkout."""
        ...

    @property
    def state(self) -> AmtState:
        """The state as loaded by the command."""
        ...


# ---------------------------------------------------------------------------------------------
# The results
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True)
class RunReview:
    """One started run after the loop of ``R/modeltests.R:273-364``.

    ``record`` is its README row, ``errors`` the texts it added to the error list (the runtime rule
    included), ``previous`` the run it was compared with (``None`` when none qualified or the run was
    neither converged nor not converged), ``compared`` whether the compareScenarios2 job was submitted and
    ``rse_updated`` whether its ``fulldata.gdx`` went to the RSE server.
    """

    run: Run
    record: RunStatus
    errors: tuple[str, ...]
    previous: Run | None = None
    compared: bool = False
    rse_updated: bool = False


class Publication(Enum):
    """What :func:`publish_readme` did in the report repository."""

    COMMITTED = "committed"
    """``README.md`` differed from the repository's: copied, added, committed and pushed."""
    UNCHANGED = "unchanged"
    """``README.md`` already equal after ``git pull``: nothing to commit (a retry after a successful push)."""


@dataclass(frozen=True)
class TestthatVerdict:
    """The ``make test-full`` result of ``R/modeltests.R:380-398`` before the log is archived.

    ``sentence`` is the line of the Mattermost message; ``log`` the ``<checkout>/test-full.log`` that was
    read (``None`` when there is none, :data:`TESTTHAT_MISSING`); ``kept`` where
    :func:`archive_testthat_log` moves it (``tests/test-full-<mtime date>.log``, the path the sentence
    names).
    """

    sentence: str
    log: Path | None = None
    kept: Path | None = None


@dataclass(frozen=True)
class Evaluation:
    """What one ``evaluate`` produced, for the command line and the tests."""

    cycle: Cycle
    today: date
    tested_commit: str
    merges: tuple[str, ...]
    reviews: tuple[RunReview, ...]
    runs_not_started: tuple[str, ...]
    testthat_result: str
    testthat_log: Path | None
    archived: tuple[str, ...]
    errors: tuple[str, ...]
    summary: str
    readme: str
    readme_path: Path
    publication: Publication | None
    delivery: Delivery
    state: AmtState

    @property
    def runs_started(self) -> tuple[str, ...]:
        """The names of the started runs in README order."""
        return tuple(review.run.name for review in self.reviews)

    @property
    def published(self) -> bool:
        """Whether a new ``README.md`` reached the report repository (``None``: ``--no-publish``)."""
        return self.publication is Publication.COMMITTED


# ---------------------------------------------------------------------------------------------
# Pre-checks: no effect, no state change
# ---------------------------------------------------------------------------------------------


def cycle_to_evaluate(state: AmtState) -> Cycle:
    """The cycle ``evaluate`` works on, or :class:`AmtStateError` before anything happens (``06`` 3.2).

    ``started`` and ``failed`` may evaluate (:func:`modelstats.amt.state.target_phase`); a ``failed`` state
    without a cycle has nothing to retry (:func:`modelstats.amt.state.evaluable_cycle` points at ``reset``).
    """
    target_phase(state, Command.EVALUATE)
    return evaluable_cycle(state)


def _report_repo(ctx: EvaluateContext) -> Path | None:
    """``--report-repo`` when publishing; :class:`EvaluateError` when it is required and missing."""
    if not ctx.publish:
        return None
    if ctx.report_repo is None:
        raise EvaluateError("--report-repo is required to publish README.md (or pass --no-publish)")
    return ctx.report_repo


# ---------------------------------------------------------------------------------------------
# 1. The wait loop (R/modeltests.R:178-195)
# ---------------------------------------------------------------------------------------------


def _as_given(path: str) -> str:
    """The identity ``normalize`` of :func:`job_in_checkout`: paths compared as spelled."""
    return path


def job_in_checkout(line: str, checkout: PathLike, *, normalize: Callable[[str], str] = _as_given) -> bool:
    """Whether a ten-field ``squeue`` line's work directory (``%Z``, the last field) is the checkout or below.

    ``R/modeltests.R:187`` ran ``grepl(mydir, line)`` over the whole line with ``mydir`` as a regular
    expression ending in a slash and spelled as given; the rewrite reads the field (D-24) so that a job
    started *in* the checkout (``make test-full-slurm``) counts as well, and compares ``normalizePath``'d
    paths: ``normalize`` (``Effects.normalize_path`` from :func:`wait_for_runs`) is applied to the work
    directory, and ``checkout`` is expected canonical already, so a ``..`` or symlink alias on either side
    (``--dir /p/amt/remind/../remind``, a job submitted with ``--chdir`` through a link) still matches.
    """
    fields = line.split(" ")
    if len(fields) < 10:
        return False
    workdir = normalize(fields[-1]).rstrip("/")
    root = os.fspath(checkout).rstrip("/")
    return workdir == root or workdir.startswith(f"{root}/")


def wait_for_runs(effects: Effects, logger: logging.Logger, *, user: str, checkout: PathLike) -> int:
    """Poll ``squeue`` until no job of ``user`` works in the checkout (``R/modeltests.R:178-195``).

    Returns the number of polls. More than :data:`SQUEUE_FAILURES_ALLOWED` consecutive failures raise
    :class:`EvaluateError` with R's message; a successful poll resets the counter; :data:`POLL_SECONDS`
    pass between polls through ``Effects.sleep``. The checkout is canonicalised once
    (``Effects.normalize_path``: ``normalizePath``, the real path of an existing directory) and every
    work directory of the queue the same way before the comparison (R compared ``mydir`` as spelled,
    ``:187``); a work directory that does not exist or cannot be read is compared as spelled.
    """
    argv = [SQUEUE, "-u", user, "-h", "-o", SQUEUE_FORMAT]
    root = effects.normalize_path(checkout)
    logger.info("%s - waiting for all AMT runs to finish.", effects.now().strftime("%Y-%m-%d %H:%M:%S"))
    failures = 0
    polls = 0
    while True:
        result = run_logged(effects, logger, argv)
        polls += 1
        if not result.ok:
            failures += 1
            if failures > SQUEUE_FAILURES_ALLOWED:
                raise EvaluateError(SQUEUE_FAILED)
        elif not any(
            job_in_checkout(line, root, normalize=effects.normalize_path) for line in result.lines()
        ):
            break
        else:
            failures = 0
        effects.sleep(POLL_SECONDS)
    logger.info("%s - all AMT runs finished.", effects.now().strftime("%Y-%m-%d %H:%M:%S"))
    return polls


# ---------------------------------------------------------------------------------------------
# 2.-3. The git information (R/modeltests.R:197-201)
# ---------------------------------------------------------------------------------------------


def tested_commit(effects: Effects, logger: logging.Logger, checkout: PathLike) -> str:
    """``sub("commit ", "", system("git log -1", intern = TRUE)[[1]])`` (``R/modeltests.R:198``).

    R aborted with a subscript error when ``git`` printed nothing; here that is :class:`EvaluateError`.
    """
    commit = checkout_commit(effects, checkout, logger=logger)
    if commit is None:
        raise EvaluateError(f"git log -1 in {os.fspath(checkout)} did not report the tested commit")
    return commit


def merge_list(
    effects: Effects, logger: logging.Logger, checkout: PathLike, *, base: str | None, tested: str
) -> list[str]:
    """``git log --merges --pretty=oneline <base>..<tested> --abbrev-commit | grep 'Merge pull request'``.

    ``R/modeltests.R:199-201``. ``base`` is the commit of the previous evaluation; without one the list is
    empty (logged), where R failed on ``readRDS("lastcommit.rds")`` before the wait loop. A ``git`` that
    exits non-zero contributes whatever it printed, as ``system(intern = TRUE)`` did.
    """
    if base is None:
        logger.info("no previously tested commit is recorded: the README lists no merges")
        return []
    argv = [GIT, "log", "--merges", "--pretty=oneline", f"{base}..{tested}", "--abbrev-commit"]
    result = run_logged(effects, logger, argv, cwd=checkout)
    return [line for line in result.lines() if MERGE_MARK in line]


# ---------------------------------------------------------------------------------------------
# 4. The started runs (R/modeltests.R:238-239)
# ---------------------------------------------------------------------------------------------


def started_runs(effects: Effects, output_dir: PathLike, runcode: str) -> list[str]:
    """``grep(runcode, list.dirs(full.names = FALSE, recursive = FALSE), value = TRUE)`` in ``output/``."""
    pattern = re.compile(runcode)
    names = effects.list_dirs_like_r(output_dir, full_names=False)
    return [name for name in names if pattern.search(name)]


# ---------------------------------------------------------------------------------------------
# 5. One run (R/modeltests.R:273-364)
# ---------------------------------------------------------------------------------------------


def rule_errors(run: Run) -> list[str]:
    """The texts one run adds to the error list (``R/modeltests.R:280-298``), in R's order.

    The rules read the ``Run`` vocabulary where R read the parity strings: ``Conv`` converged or converged
    (had INFES) is :attr:`modelstats.run.Convergence.is_converged` without ``Clb_converged``,
    ``Clb_converged`` is :attr:`modelstats.run.Convergence.CALIBRATION_CONVERGED` (unreachable with ten
    calibration files, BUG-001, kept), ``modelstat == "2: Locally Optimal"`` is the code ``2``, ``Mif ==
    "sumErr"`` is :attr:`modelstats.run.MifState.SUMMATION_ERRORS`, ``runInAppResults != "yes"`` is a
    falsy :attr:`modelstats.run.Run.in_app_results` (``None`` off the cluster counts as not reported, as the
    missing column aborted R there).
    """
    run_type = run.run_type or ""
    errors: list[str] = []
    if not EXEMPT_RUN_TYPES.search(run_type) and run.convergence not in CONVERGED:
        errors.append(NOT_CONVERGED)
    if run_type == CALIBRATION_RUN_TYPE and run.convergence is not Convergence.CALIBRATION_CONVERGED:
        errors.append(NOT_CONVERGED)
    if TEST_ONE_REGI in run_type and run.modelstat.code != OPTIMAL_CODE:
        errors.append(NOT_OPTIMAL)
    if run.mif is MifState.SUMMATION_ERRORS:
        errors.append(SUMMATION_GAPS)
    if not run.in_app_results:
        errors.append(NOT_REPORTED)
    return errors


def gams_hours(run: Run) -> float | None:
    """``as.numeric(stats$timeGAMSEnd - stats$timeGAMSStart, units = "hours")`` (``R/modeltests.R:64-69``).

    ``None`` without ``runstatistics.rda`` or without both instants (R's ``length(x) > 0`` test, ``:329``).
    """
    stats = run.statistics
    seconds = None if stats is None else stats.gams_wall_seconds()
    return None if seconds is None else seconds / 3600


def runtime_is_slower(run: Run, previous: Run) -> bool:
    """``currentRunTime > (1.25 * lastRunTime)`` when both are known (``R/modeltests.R:327-329``)."""
    current = gams_hours(run)
    last = gams_hours(previous)
    return current is not None and last is not None and current > RUNTIME_FACTOR * last


def comparison_pending(effects: Effects, run: Run, previous: Run) -> bool:
    """``R/modeltests.R:335-337``: both ``REMIND_generic_<title>.mif`` exist and no ``comp_with_*.pdf`` yet.

    The title is the current run's ``cfg$title`` for both folders, as R pasted it.
    """
    title = run.title
    if title is None:
        return False
    mifs = (f"{run.path}/REMIND_generic_{title}.mif", f"{previous.path}/REMIND_generic_{title}.mif")
    if not all(effects.exists(mif) for mif in mifs):
        return False
    return not effects.listdir_like_r(run.path, pattern=COMPARISON_PDF)


def compare_scenarios_argv(this_run: PathLike, previous_run: PathLike, previous_name: str) -> list[str]:
    """``cs2com`` of ``R/modeltests.R:340-352`` as the argument vector the shell handed to ``sbatch``.

    The paths are the normalised run folders (``fullPathToThisRun``, ``fullPathToLastRun``); the output
    name is ``comp_with_<previous run name>``. R built one shell string with the ``--wrap`` value in double
    quotes; the vector is what ``sbatch`` received (the AMT goldens' ``trace.jsonl`` pins it).
    """
    this = os.fspath(this_run)
    last = os.fspath(previous_run)
    out = f"{COMPARISON_PREFIX}{previous_name}"
    wrap = (
        f"Rscript {COMPARE_SCRIPT} outputdirs={this},{last} profileName=default outFileName={out}"
        f"; mv {out}.pdf {this}"
    )
    return [
        SBATCH,
        "--qos=standby",
        f"--job-name={out}",
        "--comment=compareScenarios2",
        f"--output={this}/{out}.out",
        f"--error={this}/{out}.out",
        "--mail-type=END",
        "--time=200",
        "--mem-per-cpu=8000",
        f"--wrap={wrap}",
    ]


def _update_rse_gdx(effects: Effects, logger: logging.Logger, run: Run) -> bool:
    """``rsync -e ssh -av fulldata.gdx <server>`` in the folder of a converged SSP2-NPi-AMT (``:306-310``)."""
    if RSE_RUN not in run.name or run.convergence not in CONVERGED:
        return False
    logger.info(
        "Updating the gdx on the RSE server %s with the fulldata.gdx of %s", GDX_ON_RSE_SERVER, run.name
    )
    run_logged(effects, logger, list(RSYNC_ARGV), cwd=run.path)
    return True


def _submit_comparison(
    effects: Effects, logger: logging.Logger, run: Run, previous: Run, checkout: Path
) -> None:
    """``sbatch`` the compareScenarios2 job from ``cfg$remind_folder`` (``R/modeltests.R:338-356``)."""
    logger.info("Calling compareScenarios2 with %s and %s", run.path, previous.path)
    config = run.config
    remind_folder = config.remind_folder if config is not None and config.remind_folder else None
    if remind_folder is None:
        logger.warning("cfg$remind_folder of %s is not set; running sbatch from %s", run.name, checkout)
    argv = compare_scenarios_argv(run.path, previous.path, previous.name)
    run_logged(effects, logger, argv, cwd=remind_folder if remind_folder is not None else checkout)


def review_run(ctx: EvaluateContext, name: str) -> RunReview:
    """The loop body of ``R/modeltests.R:273-364`` for one started run.

    The row (``getRunStatus(i)``) is read first, in R's order, so that a run R could not read aborts the
    evaluation here too (:class:`modelstats.errors.RunReadError`); then the rules; then, for a converged
    or not converged run, the RSE update, the previous run, the runtime rule (converged runs only) and the
    comparison. Everything else is logged as R's "does not seem to have converged. Skipping!". That gate
    (``:301``) reads the parity ``Conv`` of the row, :data:`REVIEWED_CONV`, because the rule's vocabulary is
    R's (``06`` 4.2, rules kept): an interrupted run below ``cm_iteration_max`` carries its modelstat
    digits there and is skipped, as R skipped it, although the ``Run`` view calls it not converged (D-25).
    """
    effects, logger = ctx.effects, ctx.logger
    output_dir = ctx.output_dir
    run = Run(output_dir / name, effects, user=ctx.user)
    record = run.to_status_record(detailed=True)
    errors = rule_errors(run)
    if record.Conv not in REVIEWED_CONV:
        logger.info("%s does not seem to have converged. Skipping!", name)
        return RunReview(run, record, tuple(errors))
    logger.info("Analysing %s", run.path)
    rse_updated = _update_rse_gdx(effects, logger, run)
    previous = find_previous_run(run, output_dir, effects, user=ctx.user, logger=logger)
    compared = False
    if previous is not None:
        logger.info("previous run of %s: %s", run.name, previous.path)
        if run.convergence in CONVERGED and runtime_is_slower(run, previous):
            errors.append(CHECK_RUNTIME)
        if ctx.compare_scenarios and comparison_pending(effects, run, previous):
            _submit_comparison(effects, logger, run, previous, ctx.checkout)
            compared = True
    else:
        logger.info("no earlier converged run of %s with a mif in output/ or output/archive/", run.name)
    logger.info("Finished analysis for %s", name)
    return RunReview(
        run, record, tuple(errors), previous=previous, compared=compared, rse_updated=rse_updated
    )


# ---------------------------------------------------------------------------------------------
# 6. The scenarios that did not start (R/modeltests.R:367-378, D-24)
# ---------------------------------------------------------------------------------------------


def scenario_of(run_name: str) -> str:
    """``gsub(datetimepattern, "", runsStarted)`` (``R/modeltests.R:372``): the name without its stamp."""
    return DATETIME_PATTERN.sub("", run_name)


def expected_scenarios(cycle: Cycle) -> list[str]:
    """``c("default-AMT", runsToStart)`` (``:373``) without repeats, in order."""
    return list(dict.fromkeys([DEFAULT_RUN, *cycle.runs_to_start]))


def scenarios_not_started(started: Iterable[str], expected: Iterable[str]) -> list[str]:
    """``setdiff(expected, scenariosStarted)`` (``:373``) on the scenario names, whatever the count (D-24)."""
    seen = {scenario_of(name) for name in started}
    return [scenario for scenario in dict.fromkeys(expected) if scenario not in seen]


# ---------------------------------------------------------------------------------------------
# 7. make test-full (R/modeltests.R:380-398)
# ---------------------------------------------------------------------------------------------


def testthat_result(effects: Effects, checkout: Path) -> TestthatVerdict:
    """The ``make test-full`` sentence of the message (``R/modeltests.R:380-398``), a pure read.

    ``<checkout>/test-full.log`` is read and its last ``[ FAIL`` line is the status; the sentence names
    ``<checkout>/tests/test-full-<mtime date>.log``, where :func:`archive_testthat_log` moves the file
    at the end of the evaluation (R renamed it here, ``:389``, before the README, the publication and the
    message: a crash in any of them made the retry of ``06`` 3.2 report ``test-full.log not found``, Codex
    review 2026-10-11, finding 4). Without the file the sentence says so and nothing is to be moved. R took
    the last *six* ``[ FAIL`` lines (``tail()``) and ``isTRUE()`` of a vector, which is ``FALSE`` for two or
    more: the rewrite reads the last line.
    """
    log_path = checkout / TEST_FULL_LOG
    info = effects.stat(log_path)
    if info is None:
        return TestthatVerdict(TESTTHAT_MISSING)
    date_tag = info.mtime.strftime("%Y-%m-%d")
    lines = effects.read_text(log_path).splitlines()
    fails = [line for line in lines if FAIL_MARK in line]
    status = fails[-1] if fails else None
    kept = _kept_log_path(effects, checkout, date_tag)
    if status is None:
        sentence = f"`make test-full` did not run properly. Check {kept}"
    elif not ("FAIL 0" in status and "WARN 0" in status):
        sentence = f"Not all tests pass in `make test-full`: {status}. Check `{kept}`"
    else:
        sentence = f"All tests pass in `make test-full`: {status}"
    return TestthatVerdict(sentence, log_path, kept)


def _kept_log_path(effects: Effects, checkout: Path, date_tag: str) -> Path:
    """``normalizePath("../tests/test-full-<date>.log")`` (``R/modeltests.R:388-391``), before the move.

    R normalised the moved file; the same real path is the canonical ``tests/`` directory plus the name
    (``tests/`` of the checkout when it exists, else the canonical checkout plus ``tests``, which the
    archive step creates).
    """
    name = f"test-full-{date_tag}.log"
    tests_dir = checkout / TESTS_DIR
    if effects.is_dir(tests_dir):
        return Path(effects.normalize_path(tests_dir)) / name
    return Path(effects.normalize_path(checkout)) / TESTS_DIR / name


def archive_testthat_log(effects: Effects, logger: logging.Logger, verdict: TestthatVerdict) -> None:
    """Move ``test-full.log`` to ``tests/test-full-<date>.log`` (``R/modeltests.R:389``), last of all.

    ``tests/`` is created when missing (R's ``file.rename`` silently failed without it, BUG-079). Nothing
    happens for a verdict without a log. Called right before the state is saved so that an evaluation
    that fails after the sentence was composed leaves the log where a retry reads it again.
    """
    if verdict.log is None or verdict.kept is None:
        return
    effects.mkdir(verdict.kept.parent)
    effects.rename(verdict.log, verdict.kept)
    logger.info("renamed %s to %s", verdict.log, verdict.kept)


# ---------------------------------------------------------------------------------------------
# 8. The archive (R/modeltests.R:400-418, D-24)
# ---------------------------------------------------------------------------------------------


def folder_date(name: str) -> date | None:
    """The day in ``<...>_YYYY-MM-DD_<...>`` (``R/modeltests.R:409``), ``None`` without one or when invalid.

    R's ``as.Date()`` of a name without the pattern aborted when that name came first and was ``NA``
    otherwise; the rewrite skips such names.
    """
    match = FOLDER_DATE.match(name)
    if match is None:
        return None
    try:
        return date.fromisoformat(match.group(1))
    except ValueError:
        return None


def old_runs(effects: Effects, output_dir: PathLike, today: date) -> list[str]:
    """The ``*AMT*`` directories of ``output/`` with a folder date before ``today - 90 days`` (``:403-411``).

    ``list.files(pattern = ".*AMT.*", include.dirs = TRUE)`` then ``filter(isdir)``: files drop out, the
    names come in R's order; ``ctimeDay < (Sys.Date() - daysback)`` is strict.
    """
    cutoff = today - timedelta(days=ARCHIVE_DAYS)
    output = os.fspath(output_dir)
    names = effects.listdir_like_r(output, pattern=AMT_NAME)
    old: list[str] = []
    for name in names:
        day = folder_date(name)
        if day is not None and day < cutoff and effects.is_dir(f"{output}/{name}"):
            old.append(name)
    return old


def archive_old_runs(
    effects: Effects, logger: logging.Logger, output_dir: PathLike, today: date
) -> list[str]:
    """Move the old runs into ``output/archive/`` one by one (``R/modeltests.R:413-418``, ``06`` 4.2).

    ``Effects.move`` per run replaces ``system("mv <runs> archive")``, and the directory is created when it
    is missing (BUG-016, BUG-033 gone by design). Returns the names moved.
    """
    old = old_runs(effects, output_dir, today)
    if not old:
        return []
    cutoff = today - timedelta(days=ARCHIVE_DAYS)
    logger.info(
        "Moving %d runs with timestamp older than %d days (%s) to 'archive':", len(old), ARCHIVE_DAYS, cutoff
    )
    for name in old:
        logger.info("%s", name)
    archive = f"{os.fspath(output_dir)}/{ARCHIVE_DIR}"
    if not effects.is_dir(archive):
        effects.mkdir(archive)
    for name in old:
        effects.move(f"{os.fspath(output_dir)}/{name}", archive)
    return old


# ---------------------------------------------------------------------------------------------
# 9.-10. The README file and its publication (R/modeltests.R:202, 433-445)
# ---------------------------------------------------------------------------------------------


def readme_path(log_dir: Path, cycle_id: str) -> Path:
    """``<log-dir>/amt-<cycle id>-README.md``: the README kept next to the cycle log."""
    return log_dir / README_FILE_TEMPLATE.format(cycle_id=cycle_id)


def publish_readme(
    effects: Effects, logger: logging.Logger, report_repo: PathLike, readme: PathLike
) -> Publication:
    """``R/modeltests.R:434-444`` in the report repository: reset, pull, copy, add, commit, push.

    Each ``git`` runs through :func:`modelstats.amt.logs.run_logged` with ``check=True``: a non-zero
    status raises :class:`modelstats.env.CommandFailedError`, which ``evaluate_cycle`` records as the
    ``failed`` state with the cycle kept, so that ``evaluate`` can retry once the repository is reachable
    (D-24; R's ``system()`` calls went on and the README silently never reached ``testing_suite``, the
    evaluate counterpart of BUG-075). After ``git pull`` a ``README.md`` equal to the new one is left
    alone (:attr:`Publication.UNCHANGED`): that is the retry of a cycle whose push succeeded before the
    state was saved, where ``git commit`` would exit 1 with ``nothing to commit``.
    """
    run_logged(effects, logger, [GIT, "reset", "--hard", "origin/master"], cwd=report_repo, check=True)
    run_logged(effects, logger, [GIT, "pull"], cwd=report_repo, check=True)
    target = f"{os.fspath(report_repo)}/{PUBLISHED_NAME}"
    if effects.exists(target) and effects.read_text(target) == effects.read_text(readme):
        logger.info("%s unchanged in %s: nothing to commit or push", PUBLISHED_NAME, os.fspath(report_repo))
        return Publication.UNCHANGED
    effects.copy(readme, target)
    run_logged(effects, logger, [GIT, "add", PUBLISHED_NAME], cwd=report_repo, check=True)
    run_logged(effects, logger, [GIT, "commit", "-m", COMMIT_MESSAGE], cwd=report_repo, check=True)
    run_logged(effects, logger, [GIT, "push"], cwd=report_repo, check=True)
    return Publication.COMMITTED


# ---------------------------------------------------------------------------------------------
# 11. The notifications (R/modeltests.R:447-486, 06 section 3.2)
# ---------------------------------------------------------------------------------------------


def _notify_status(
    ctx: EvaluateContext, report: ReadmeReport, testthat: str, started: Sequence[str]
) -> Delivery:
    """Compose and post the REMIND status message (``R/modeltests.R:449-485``).

    The webhook is resolved first: without one nothing is captured and nothing is composed
    (``if (!is.null(mattermostToken))``, ``:449``), so a checkout evaluated without a webhook never runs
    the ``rs -A`` and sanity tables. The tables are the cycle's started runs in ``ctx.output_dir``.
    """
    logger = ctx.logger
    url = webhook_url(ctx.effects, ctx.mattermost_variable)
    if url is None:
        logger.info("No Mattermost webhook URL in $%s: notification not sent", ctx.mattermost_variable)
        return Delivery.NOT_CONFIGURED
    logger.info("Composing message and sending it to mattermost channel")
    diagnostics = io.StringIO()
    tables = capture_tables(
        [ctx.output_dir / name for name in started],
        ctx.effects,
        output_dir=ctx.output_dir,
        user=ctx.user,
        err=diagnostics,
    )
    for line in diagnostics.getvalue().splitlines():
        logger.warning("[rs] %s", line)
    message = status_message(
        StatusReport(
            today=report.today,
            runs_path=str(ctx.checkout),
            summary=report.summary,
            testthat_result=testthat,
            git_info=report.git_info,
            tables=tables,
            runs_not_started=report.runs_not_started or (),
        )
    )
    return send_to(ctx.effects, url, message, variable=ctx.mattermost_variable, log=logger)


# ---------------------------------------------------------------------------------------------
# The command
# ---------------------------------------------------------------------------------------------


def evaluate_cycle(ctx: AmtContext) -> Evaluation:
    """``evaluateRuns()`` for the cycle in ``ctx.state`` (``R/modeltests.R:168-492``, ``06`` 4.1-4.2).

    The pre-checks (a cycle to evaluate, the report repository when publishing) raise before any effect.
    Afterwards any exception moves the state to ``failed`` with ``last_error`` filled and the cycle kept,
    posts the failure notification and is raised as :class:`modelstats.amt.context.CycleFailedError` from
    the original error (:func:`modelstats.amt.context.record_failure`, shared with ``start``); on success
    the state is ``idle`` with ``last_completed`` filled and the :class:`Evaluation` says what happened.
    """
    cycle = cycle_to_evaluate(ctx.state)
    report_repo = _report_repo(ctx)
    try:
        return _evaluate(ctx, cycle, report_repo)
    except Exception as error:
        raise record_failure(ctx, Command.EVALUATE, error) from error


def _evaluate(ctx: EvaluateContext, cycle: Cycle, report_repo: Path | None) -> Evaluation:
    effects, logger = ctx.effects, ctx.logger
    output_dir = ctx.output_dir
    today = effects.now().date()
    logger.info("Evaluating AMT cycle %s (runs matching %s) in %s", cycle.id, cycle.runcode, ctx.checkout)
    wait_for_runs(effects, logger, user=ctx.user, checkout=ctx.checkout)
    logger.info("Compiling the README.md to be committed to testing_suite repo.")
    commit = tested_commit(effects, logger, ctx.checkout)
    base = ctx.state.last_completed.tested_commit if ctx.state.last_completed is not None else None
    merges = merge_list(effects, logger, ctx.checkout, base=base, tested=commit)
    started = started_runs(effects, output_dir, cycle.runcode)
    logger.info("Starting analysis for the list of the following runs:\n%s", "\n".join(started))
    reviews = [review_run(ctx, name) for name in started]
    errors = [text for review in reviews for text in review.errors]
    not_started = scenarios_not_started(started, expected_scenarios(cycle))
    if not_started:
        logger.info("These scenarios did not start at all: %s", ", ".join(not_started))
        errors.append(SCENARIOS_NOT_STARTED)
    verdict = testthat_result(effects, ctx.checkout)
    logger.info("%s", verdict.sentence)
    archived = archive_old_runs(effects, logger, output_dir, effects.now().date())
    if not started:
        errors.append(NO_RUNS_STARTED)
    summary = summary_line(errors)
    report = ReadmeReport(
        today=today,
        runs_path=f"{ctx.checkout}/output/",
        compare_scenarios=ctx.compare_scenarios,
        git_info=git_info_lines(commit, today, merges),
        runs=[review.record for review in reviews],
        runs_not_started=not_started or None,
        summary=summary,
    )
    readme = render_readme(report)
    readme_file = readme_path(ctx.log_dir, cycle.id)
    effects.mkdir(ctx.log_dir)
    effects.write_text(readme_file, readme)
    logger.info("Finished compiling README.md (%s)", readme_file)
    publication = None
    if report_repo is not None:
        publication = publish_readme(effects, logger, report_repo, readme_file)
    delivery = _notify_status(ctx, report, verdict.sentence, started)
    archive_testthat_log(effects, logger, verdict)
    state = complete_state(ctx.state, evaluated_at=effects.now(), tested_commit=commit, summary=summary)
    ctx.store.save(state)
    logger.info("Evaluation of cycle %s finished: %s", cycle.id, summary)
    return Evaluation(
        cycle=cycle,
        today=today,
        tested_commit=commit,
        merges=tuple(merges),
        reviews=tuple(reviews),
        runs_not_started=tuple(not_started),
        testthat_result=verdict.sentence,
        testthat_log=verdict.kept,
        archived=tuple(archived),
        errors=tuple(errors),
        summary=summary,
        readme=readme,
        readme_path=readme_file,
        publication=publication,
        delivery=delivery,
        state=state,
    )
