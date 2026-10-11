"""``modelstats.amt.evaluate``: every evaluation step and every ``remind-evaluate*`` case of the R goldens.

Two tiers. The unit tier builds synthetic REMIND runs (``tests/unit/test_run_status.py`` builders) in a
``tmp_path`` checkout and drives the steps and the whole command with ``RecordingEffects``: the wait loop,
the git information, the rules, the runtime comparison, the compareScenarios2 command line (pinned against
the R golden's ``sbatch`` argv, which is tracked in git), the "did not start" set difference, the
``test-full.log`` branches, the archive sweep, the publication, the two transitions of ``06`` 3.2 and the
pre-checks that leave the state untouched.

The golden tier (``pytest -m golden``, the fixture snapshot and the synthetic layer required) replays every
``remind-evaluate*`` case of ``migration/cases/amt/`` against the R golden under ``migration/goldens/amt/``
(``06`` 4.4 item 3): the fixture AMT directory is cloned into ``tmp_path`` (small files copied, large ones
linked, never written through), the case's ``prepare.sh`` is replicated in Python, the state is migrated
from the R files exactly as the command would do it, and ``evaluate_cycle`` runs under ``RecordingEffects``
with the case's canned answers. For the kept behaviour the README rows and blocks, the git/rsync/sbatch argv,
the set of archived runs, the Mattermost text and the final state equal the R golden; the D-24 changes are
asserted explicitly; the cases that only exercised an RDS state file or the toggle map onto the state
machine. The allowed differences (``06`` 4.4) are: the summary line carries the new "did not start" error
(D-24, BUG-024); the crayon escapes R's sandbox emitted inside the message's code fences (a recording
artefact); the clone's path prefix in place of ``/p``; ``sed``/``mv``/``curl`` being effects instead of
subprocesses; the failure notification R never sent.

The mapping of the cases (``06`` 4.4 item 4), also the table :data:`EXPECTATIONS`:

=================================== =============== ====================================================
case (remind-evaluate-...)          classification  what the Python evaluation does
=================================== =============== ====================================================
remind-evaluate                     kept            baseline: five runs, rsync, no previous run, message
archive                             kept            one run older than 90 days moved into archive/
archive-boundary                    kept            strict cutoff: 07-01 moves, 07-02 stays
archive-missing                     changed by D-24 archive/ is created and the run moved into it
                                                    (BUG-033: R renamed the run to ``archive``)
archive-missing-two                 changed by D-24 both runs moved (R's ``mv`` failed, nothing moved)
curlfail                            kept            HTTP 500: delivery failed, the cycle completes
duplicate                           changed by D-24 the second copy is compared with the first; R
                                                    aborted (BUG-014) and the count-based check would
                                                    have hidden a missing scenario (BUG-024)
email                               kept            reset, pull, copy, add, commit, push in the repo
git-fail                            kept            ``git log -1`` fails: the cycle is ``failed``
git-merges-empty                    kept            no merge lines
grs-corrupt, grs-existing, grs-stale mapped         ``gRS.rds`` is never read: the baseline result
                                                    (BUG-039 gone by design)
nocompscen                          kept            no sbatch, README without the compareScenarios line
notoken                             kept            variable unset: no post
previous-in-archive                 changed by D-24 the archived converged run is found and compared
                                                    with (R's history never saw the archive here)
previous-run                        kept            sbatch byte for byte, no runtime warning
runcode-nomatch                     kept            "No runs started", 25 scenarios did not start
runstostart-missing                 mapped          a cycle without ``runs_to_start``: nothing missing
sbatch-fail                         kept            the failing sbatch is logged and tolerated
slow                                kept            1 h previous run: "Check runtime!"
squeue-3fail                        kept            three failures, a success resets the counter
squeue-4fail                        kept            the fourth failure stops: the cycle is ``failed``
state-corrupt                       mapped          garbage ``runcode.rds`` migrates to ``failed``
                                                    without a cycle: refused before any effect
state-missing                       mapped          no ``lastcommit.rds``: no merge list, completes
testfull-fail, -missing, -noresult, kept            R's own cases planted ``test-full.log`` one
-ok                                                 directory above the checkout, so R (and the port,
                                                    which reads the same ``<checkout>/test-full.log``)
                                                    never found it; the branches are unit-tested
=================================== =============== ====================================================
"""

from __future__ import annotations

import io
import json
import logging
import os
import re
import shutil
from collections.abc import Callable, Iterator, Mapping, Sequence
from dataclasses import dataclass, replace
from datetime import datetime, timedelta
from pathlib import Path
from typing import Any

import pytest

from modelstats.amt.context import AmtContext, AmtOptions, CycleFailedError
from modelstats.amt.evaluate import (
    ARCHIVE_DAYS,
    CHECK_RUNTIME,
    COMMIT_MESSAGE,
    DEFAULT_RUN,
    GDX_ON_RSE_SERVER,
    NO_RUNS_STARTED,
    NOT_CONVERGED,
    NOT_OPTIMAL,
    NOT_REPORTED,
    POLL_SECONDS,
    SCENARIOS_NOT_STARTED,
    SQUEUE_FAILED,
    SQUEUE_FORMAT,
    SUMMATION_GAPS,
    TEST_FULL_LOG,
    TESTS_DIR,
    TESTTHAT_MISSING,
    EvaluateError,
    Evaluation,
    archive_old_runs,
    compare_scenarios_argv,
    comparison_pending,
    cycle_to_evaluate,
    evaluate_cycle,
    expected_scenarios,
    folder_date,
    gams_hours,
    job_in_checkout,
    merge_list,
    old_runs,
    publish_readme,
    readme_path,
    review_run,
    rule_errors,
    runtime_is_slower,
    scenario_of,
    scenarios_not_started,
    started_runs,
    wait_for_runs,
)
from modelstats.amt.evaluate import tested_commit as commit_tested
from modelstats.amt.evaluate import testthat_result as make_test_full_sentence
from modelstats.amt.logs import cycle_log
from modelstats.amt.notify import MATTERMOST_URL_VARIABLE, NEXT_STEPS, SANITY_HEADER, Delivery
from modelstats.amt.previous import ARCHIVE_DIR
from modelstats.amt.readme import LOOK_GOOD, NOT_STARTED_TITLE, SUMMARY_PREFIX, summary_line
from modelstats.amt.state import (
    AmtState,
    AmtStateError,
    Completed,
    Cycle,
    LastError,
    Phase,
    StateStore,
    rds_bytes,
    start,
)
from modelstats.env import (
    CannedAmt,
    CannedSlurm,
    ClusterPaths,
    EffectCall,
    LineCallback,
    PathLike,
    RecordingEffects,
    RunResult,
    Stream,
)
from modelstats.run import Convergence, Run
from tests.conftest import (
    CASES,
    GOLDENS,
    SANDBOX_ENVIRON,
    SANDBOX_FROZEN,
    SANDBOX_TZ,
    resolve_frozen,
    slurm_case_dir,
)
from tests.unit.test_facts import REGIONS, make_run, remind_cfg, remind_stats, write_stats
from tests.unit.test_run_status import remind_run

NOW = datetime(2026, 9, 30, 12, 0, 0, tzinfo=SANDBOX_TZ)
USER = "pascalfu"
URL = "https://mattermost.example.org/hooks/fake-amt-token"
PREVIOUS_COMMIT = "13f60fdd4fff366bebcc7dfe522897dedeb43cc7"
TESTED_COMMIT = "3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f"
RUNCODE = ".*-AMT_2026-09-30|.*-AMT_2026-10-01"
CONVERGED = {"o_iterationNumber": 26, "o_modelstat": 2, "s80_bool": 1}
NOT_CONVERGED_GDX = {"o_iterationNumber": 50, "o_modelstat": 2, "s80_bool": 0}
TEN = SQUEUE_FORMAT
PASSING_LOG = "testthat results\n[ FAIL 0 | WARN 0 | SKIP 3 | PASS 120 ]\n"
ESCAPE = re.compile(r"\x1b\[[0-9;]*m")
AMT_CASES = CASES / "amt"
AMT_GOLDENS = GOLDENS / "amt"
BASELINE = "remind-evaluate"
HOUR = 3600.0


# ---------------------------------------------------------------------------------------------
# The context and the checkout of the unit tier
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True)
class Checkout:
    """A synthetic AMT directory: the checkout, its output, a report repository and a results archive."""

    root: Path

    @property
    def checkout(self) -> Path:
        return self.root / "remind"

    @property
    def output(self) -> Path:
        return self.checkout / "output"

    @property
    def archive(self) -> Path:
        return self.output / ARCHIVE_DIR

    @property
    def report_repo(self) -> Path:
        return self.root / "testing_suite"

    @property
    def results_archive(self) -> Path:
        return self.root / "results"

    @property
    def log_dir(self) -> Path:
        return self.checkout / "logs"

    def cluster_paths(self) -> ClusterPaths:
        return ClusterPaths(
            amt_dir=str(self.checkout),
            amt_output=f"{self.output}/",
            amt_archive=str(self.archive),
            amt_runcode=str(self.checkout / "runcode.rds"),
            remind_results_archive=f"{self.results_archive}/",
        )


def make_checkout(root: Path) -> Checkout:
    tree = Checkout(root)
    for directory in (tree.output, tree.report_repo, tree.results_archive):
        directory.mkdir(parents=True, exist_ok=True)
    return tree


def recording(
    tree: Checkout,
    *,
    amt: CannedAmt | None = None,
    slurm: CannedSlurm | None = None,
    webhook: str | None = URL,
    now: datetime = NOW,
    user: str = USER,
    cluster_paths: ClusterPaths | None = None,
) -> RecordingEffects:
    """The recorder on ``tree``.

    ``cluster_paths`` defaults to the tree's (``ClusterPaths()``: the cluster's).
    """
    environ = {"TZ": "Europe/Berlin", "USER": user}
    if webhook is not None:
        environ[MATTERMOST_URL_VARIABLE] = webhook
    return RecordingEffects(
        now=now,
        user=user,
        on_cluster=True,
        amt=amt,
        slurm=slurm,
        cwd=tree.root,
        environ=environ,
        cluster_paths=cluster_paths if cluster_paths is not None else tree.cluster_paths(),
    )


def amt_run(
    tree: Checkout,
    name: str,
    *,
    title: str | None = None,
    gdx: Mapping[str, float] | None = None,
    repy: Mapping[str, float] | None = None,
    gams_hours_: float = 2.3,
    run_id: float = 123456.0,
    reported: bool = True,
    mif: bool = True,
    cfg: dict[str, Any] | None = None,
    where: Path | None = None,
) -> Path:
    """A REMIND AMT run named ``name`` whose title is the name without its stamp (or ``title``).

    ``repy`` is the ``p80_repy`` modelstat per region (the parity ``Conv`` of an interrupted run).
    """
    scenario = title if title is not None else scenario_of(name)
    config = cfg if cfg is not None else remind_cfg(title=scenario)
    config = {**config, "remind_folder": str(tree.checkout), "results_folder": f"output/{name}"}
    run = remind_run(
        where if where is not None else tree.output,
        name,
        cfg=config,
        stats=remind_stats(gams_start=1000.0, gams_end=1000.0 + gams_hours_ * HOUR, run_id=run_id),
        gdx=gdx if gdx is not None else CONVERGED,
        repy=repy,
        mif=False,
        extra={f"REMIND_generic_{scenario}.mif": "mif"} if mif else None,
    )
    if reported:
        (tree.results_archive / f"{int(run_id)}.rds").write_bytes(b"")
    return run


def started_state(
    runs_to_start: Sequence[str] = ("SSP2-NPi-AMT", "SSP2-never-AMT"),
    *,
    tested_commit: str | None = PREVIOUS_COMMIT,
) -> AmtState:
    cycle = Cycle(
        id="2026-09-30",
        runcode=RUNCODE,
        started_at=NOW - timedelta(hours=14),
        started_commit=TESTED_COMMIT,
        runs_to_start=tuple(runs_to_start),
        log="logs/amt-2026-09-30.log",
    )
    completed = None if tested_commit is None else Completed(id="2026-09-23", tested_commit=tested_commit)
    return start(AmtState(last_completed=completed), cycle)


@pytest.fixture
def log_stream() -> Iterator[tuple[logging.Logger, io.StringIO]]:
    stream = io.StringIO()
    with cycle_log(None, "evaluate", clock=lambda: NOW, stream=stream) as logger:
        yield logger, stream


def context(
    tree: Checkout,
    effects: RecordingEffects,
    logger: logging.Logger,
    *,
    state: AmtState | None = None,
    publish: bool = True,
    compare_scenarios: bool = True,
    report_repo: Path | None | str = "default",
) -> AmtContext:
    """The ``AmtContext`` of the command line with the state saved as the command would find it."""
    store = StateStore.at(effects, tree.checkout)
    current = state if state is not None else started_state()
    if current.phase is Phase.STARTED:
        store.save_started(current)
    else:
        store.save(current)
    repo = tree.report_repo if report_repo == "default" else report_repo
    return AmtContext(
        dir=tree.checkout,
        report_repo=repo if isinstance(repo, Path) else None,
        user=USER,
        options=AmtOptions(publish=publish, compare_scenarios=compare_scenarios, log_dir=tree.log_dir),
        effects=effects,
        logger=logger,
        state=current,
    )


def runs_of(effects: RecordingEffects, tool: str) -> list[tuple[list[str], str]]:
    """The argv (without the program) and cwd of every recorded call of ``tool``."""
    return [(list(record.argv), record.cwd) for record in effects.trace if record.tool == tool]


def moved(effects: RecordingEffects) -> list[tuple[str, str]]:
    return [(call.args[0], call.args[1]) for call in effects.calls if call.name == "move"]


def set_mtime(path: Path, moment: datetime) -> None:
    os.utime(path, (moment.timestamp(), moment.timestamp()))


# ---------------------------------------------------------------------------------------------
# 1. The wait loop
# ---------------------------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("line", "expected"),
    [
        (
            "1 standby RUNNING 12 1:00 SSP2-NPi-AMT 2026-09-30T00:00:00 N/A N/A /p/remind/output/NPi_x",
            True,
        ),
        ("1 standby RUNNING 12 1:00 remind-tests 2026-09-30T00:00:00 N/A N/A /p/remind", True),
        ("1 standby RUNNING 12 1:00 other 2026-09-30T00:00:00 N/A N/A /p/remind-other/output/x", False),
        ("1 standby RUNNING 12 1:00 other 2026-09-30T00:00:00 N/A N/A /home/pascalfu", False),
        ("too few fields /p/remind", False),
    ],
)
def test_job_in_checkout_reads_the_work_directory_field(line: str, expected: bool) -> None:
    assert job_in_checkout(line, "/p/remind/") is expected
    assert job_in_checkout(line, Path("/p/remind")) is expected


def squeue_line(workdir: PathLike) -> str:
    return f"1 standby RUNNING 12 1:00 SSP2-NPi-AMT 2026-09-30T00:00:00 N/A N/A {os.fspath(workdir)}"


def test_job_in_checkout_matches_aliases_of_the_checkout_when_normalised(tmp_path: Path) -> None:
    """A ``..`` or symlink spelling on either side matches once both are ``normalizePath``'d (D-24)."""
    checkout = tmp_path / "remind"
    (checkout / "output" / "run").mkdir(parents=True)
    link = tmp_path / "alias"
    link.symlink_to(checkout)
    real = os.path.realpath(checkout)
    dotted = f"{tmp_path}/other/../remind"
    assert not job_in_checkout(squeue_line(f"{real}/output/run"), dotted)  # as spelled: R's false negative
    assert job_in_checkout(
        squeue_line(f"{real}/output/run"), os.path.realpath(dotted), normalize=os.path.realpath
    )
    assert job_in_checkout(squeue_line(f"{link}/output/run"), real, normalize=os.path.realpath)
    assert job_in_checkout(squeue_line(f"{dotted}/output/run"), real, normalize=os.path.realpath)
    assert job_in_checkout(squeue_line(f"{link}/"), real, normalize=os.path.realpath)
    assert not job_in_checkout(squeue_line(f"{real}-other/output/run"), real, normalize=os.path.realpath)
    assert not job_in_checkout(squeue_line(f"{tmp_path}/other"), real, normalize=os.path.realpath)


class ScriptedSqueue(RecordingEffects):
    """``RecordingEffects`` whose ``squeue`` answers come from a list, one per call (the last one repeats)."""

    def __init__(self, answers: Sequence[RunResult], **kwargs: Any) -> None:
        super().__init__(**kwargs)
        self._answers = list(answers)
        self._next = 0

    def run(
        self,
        argv: Sequence[str],
        *,
        cwd: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        check: bool = False,
        capture: bool = True,
        on_line: LineCallback | None = None,
    ) -> RunResult:
        if argv[0] != "squeue":
            return super().run(argv, cwd=cwd, env=env, check=check, capture=capture, on_line=on_line)
        answer = self._answers[min(self._next, len(self._answers) - 1)]
        self._next += 1
        result = RunResult(tuple(argv), answer.returncode, answer.stdout, answer.stderr)
        if on_line is not None:
            for line in result.lines():
                on_line(Stream.STDOUT, line)
        self.calls.append(EffectCall("run", tuple(argv), result.returncode))
        return result


def squeue_answer(status: int, *lines: str) -> RunResult:
    return RunResult(("squeue",), status, "".join(f"{line}\n" for line in lines), "")


def test_wait_loop_polls_until_no_job_works_in_the_checkout(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO]
) -> None:
    logger, _ = log_stream
    tree = make_checkout(tmp_path)
    busy = squeue_answer(
        0, f"1 standby RUNNING 12 1:00 SSP2-NPi-AMT 2026-09-30T00:00:00 N/A N/A {tree.output}/x"
    )
    effects = ScriptedSqueue(
        [busy, busy, squeue_answer(0)],
        now=NOW,
        user=USER,
        on_cluster=True,
        cwd=tree.root,
        advance_on_sleep=True,
    )
    assert wait_for_runs(effects, logger, user=USER, checkout=tree.checkout) == 3
    sleeps = [call for call in effects.calls if call.name == "sleep"]
    assert [call.args for call in sleeps] == [(str(POLL_SECONDS),)] * 2
    assert effects.now() == NOW + timedelta(seconds=2 * POLL_SECONDS)


def test_wait_loop_recognises_the_checkout_under_an_alias(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO]
) -> None:
    """An alias of ``--dir`` (``..``, a symlink), or a job submitted through one, still makes the loop wait.

    Both sides are ``normalizePath``'d (D-24); R compared ``mydir`` as spelled.
    """
    logger, _ = log_stream
    tree = make_checkout(tmp_path)
    real_output = Path(os.path.realpath(tree.output))
    (real_output / "x").mkdir()
    link = tmp_path / "alias"
    link.symlink_to(tree.checkout)
    dotted = tree.root / ".." / tree.root.name / "remind"
    busy = squeue_answer(0, squeue_line(real_output / "x"))
    via_link = squeue_answer(0, squeue_line(link / "output" / "x"))
    cases: list[tuple[str | Path, RunResult]] = [
        (dotted, busy),
        (link, busy),
        (tree.checkout, via_link),
        (f"{link}/", via_link),
    ]
    for checkout, first in cases:
        effects = ScriptedSqueue(
            [first, squeue_answer(0)], now=NOW, user=USER, on_cluster=True, cwd=tree.root
        )
        assert wait_for_runs(effects, logger, user=USER, checkout=checkout) == 2, checkout
        assert [c.args for c in effects.calls if c.name == "normalize_path"][0] == (os.fspath(checkout),)


def test_wait_loop_argv_is_r_s_squeue_call(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO]
) -> None:
    logger, _ = log_stream
    tree = make_checkout(tmp_path)
    effects = recording(tree)
    wait_for_runs(effects, logger, user="alice", checkout=tree.checkout)
    assert runs_of(effects, "squeue") == [(["-u", "alice", "-h", "-o", TEN], str(tree.root))]


def test_wait_loop_stops_after_the_fourth_consecutive_failure(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO]
) -> None:
    logger, _ = log_stream
    tree = make_checkout(tmp_path)
    effects = recording(tree, slurm=CannedSlurm({"squeue_sequence": "1\n1\n1\n1\n"}))
    with pytest.raises(EvaluateError, match=re.escape(SQUEUE_FAILED)):
        wait_for_runs(effects, logger, user=USER, checkout=tree.checkout)
    assert [call.status for call in effects.runs] == [1, 1, 1, 1]
    assert len([call for call in effects.calls if call.name == "sleep"]) == 3


def test_wait_loop_success_resets_the_failure_counter(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO]
) -> None:
    logger, _ = log_stream
    tree = make_checkout(tmp_path)
    busy = squeue_answer(0, f"1 standby RUNNING 12 1:00 x 2026-09-30T00:00:00 N/A N/A {tree.checkout}")
    failed = squeue_answer(1)
    effects = ScriptedSqueue(
        [failed, failed, failed, busy, failed, failed, failed, squeue_answer(0)],
        now=NOW,
        user=USER,
        on_cluster=True,
        cwd=tree.root,
    )
    assert wait_for_runs(effects, logger, user=USER, checkout=tree.checkout) == 8


# ---------------------------------------------------------------------------------------------
# 2.-4. git and the started runs
# ---------------------------------------------------------------------------------------------


def test_tested_commit_is_git_log_1_in_the_checkout(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO]
) -> None:
    logger, _ = log_stream
    tree = make_checkout(tmp_path)
    effects = recording(tree)
    assert commit_tested(effects, logger, tree.checkout) == TESTED_COMMIT
    assert runs_of(effects, "git") == [(["log", "-1"], str(tree.checkout))]


def test_tested_commit_fails_when_git_prints_nothing(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO]
) -> None:
    logger, _ = log_stream
    tree = make_checkout(tmp_path)
    effects = recording(tree, amt=CannedAmt({"git-log-1.txt": "", "git-log-1.exit": "128"}))
    with pytest.raises(EvaluateError, match="git log -1"):
        commit_tested(effects, logger, tree.checkout)


def test_merge_list_filters_the_pull_request_lines(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO]
) -> None:
    logger, stream = log_stream
    tree = make_checkout(tmp_path)
    effects = recording(tree)
    merges = merge_list(effects, logger, tree.checkout, base=PREVIOUS_COMMIT, tested=TESTED_COMMIT)
    assert merges == [
        "3f5e2a1 Merge pull request #999 from example/feature",
        "1a2b3c4 Merge pull request #998 from example/bugfix",
    ]
    assert runs_of(effects, "git") == [
        (
            ["log", "--merges", "--pretty=oneline", f"{PREVIOUS_COMMIT}..{TESTED_COMMIT}", "--abbrev-commit"],
            str(tree.checkout),
        )
    ]
    other = recording(tree, amt=CannedAmt({"git-log-merges.txt": "abc1234 Merge branch 'x'\n"}))
    assert merge_list(other, logger, tree.checkout, base=PREVIOUS_COMMIT, tested=TESTED_COMMIT) == []
    assert merge_list(recording(tree), logger, tree.checkout, base=None, tested=TESTED_COMMIT) == []
    assert "no previously tested commit" in stream.getvalue()


def test_started_runs_are_the_output_directories_matching_the_runcode(tmp_path: Path) -> None:
    tree = make_checkout(tmp_path)
    for name in (
        "SSP2-NPi-AMT_2026-09-30_10.00.00",
        "default-AMT_2026-10-01_00.10.00",
        "SSP2-NPi-AMT_2026-09-23_10.00.00",
    ):
        make_run(tree.output, name)
    (tree.output / "testOneRegi-AMT_2026-09-30_09.00.00").write_text("a file")
    effects = recording(tree)
    assert started_runs(effects, tree.output, RUNCODE) == [
        "default-AMT_2026-10-01_00.10.00",
        "SSP2-NPi-AMT_2026-09-30_10.00.00",
    ]
    assert started_runs(effects, tree.output, "^nothing-matches$") == []


# ---------------------------------------------------------------------------------------------
# 5. The rules, the runtime and the comparison
# ---------------------------------------------------------------------------------------------


def test_rule_errors_follow_r_s_five_rules_in_order(tmp_path: Path) -> None:
    tree = make_checkout(tmp_path)
    effects = recording(tree)
    fine = Run(amt_run(tree, "SSP2-NPi-AMT_2026-09-30_10.00.00"), effects, user=USER)
    assert rule_errors(fine) == []
    unreported = Run(amt_run(tree, "SSP2-NDC-AMT_2026-09-30_10.00.00", run_id=7.0, reported=False), effects)
    assert rule_errors(unreported) == [NOT_REPORTED]
    not_converged = Run(amt_run(tree, "SSP3-NPi-AMT_2026-09-30_10.00.00", gdx=NOT_CONVERGED_GDX), effects)
    assert rule_errors(not_converged) == [NOT_CONVERGED]
    gaps = amt_run(tree, "SSP2-EU21-AMT_2026-09-30_10.00.00")
    (gaps / "REMIND_generic_SSP2-EU21-AMT_summation_errors.csv").write_text("x")
    assert rule_errors(Run(gaps, effects)) == [SUMMATION_GAPS]
    calibrating = amt_run(
        tree,
        "SSP2-calibrate-AMT_2026-09-30_10.00.00",
        cfg=remind_cfg(title="SSP2-calibrate-AMT", ces="calibrate"),
    )
    run = Run(calibrating, effects)
    assert run.run_type == "Calib_nash"
    assert rule_errors(run) == [NOT_CONVERGED]  # BUG-001 kept: converged is not Clb_converged
    one_regi = amt_run(
        tree,
        "testOneRegi-AMT_2026-09-30_10.00.00",
        cfg=remind_cfg(title="testOneRegi-AMT", optimization="testOneRegi"),
        gdx={"o_iterationNumber": 1, "o_modelstat": 2, "s80_bool": 1},
    )
    assert rule_errors(Run(one_regi, effects)) == []
    infeasible = amt_run(
        tree,
        "testOneRegi-AMT_2026-09-30_11.00.00",
        cfg=remind_cfg(title="testOneRegi-AMT", optimization="testOneRegi"),
        gdx={"o_iterationNumber": 1, "o_modelstat": 5, "s80_bool": 1},
    )
    assert rule_errors(Run(infeasible, effects)) == [NOT_OPTIMAL]
    bare = Run(make_run(tree.output, "stray-AMT_2026-09-30_10.00.00"), effects)
    assert rule_errors(bare) == [NOT_CONVERGED, NOT_REPORTED]


def test_runtime_rule_compares_gams_hours_with_the_factor(tmp_path: Path) -> None:
    tree = make_checkout(tmp_path)
    effects = recording(tree)
    current = Run(amt_run(tree, "SSP2-NPi-AMT_2026-09-30_10.00.00", gams_hours_=2.3), effects)
    assert gams_hours(current) == pytest.approx(2.3)
    slow = Run(amt_run(tree, "SSP2-NPi-AMT_2026-09-23_10.00.00", gams_hours_=1.0), effects)
    fine = Run(amt_run(tree, "SSP2-NPi-AMT_2026-09-16_10.00.00", gams_hours_=2.0), effects)
    assert runtime_is_slower(current, slow)
    assert not runtime_is_slower(current, fine)
    exact = Run(amt_run(tree, "SSP2-NPi-AMT_2026-09-30_11.00.00", gams_hours_=2.5), effects)
    boundary = Run(amt_run(tree, "SSP2-NPi-AMT_2026-09-09_10.00.00", gams_hours_=2.0), effects)
    assert not runtime_is_slower(exact, boundary)  # 2.5 == 1.25 * 2.0: not slower
    below = Run(amt_run(tree, "SSP2-NPi-AMT_2026-09-08_10.00.00", gams_hours_=1.75), effects)
    assert runtime_is_slower(exact, below)
    no_stats = Run(make_run(tree.output, "SSP2-NPi-AMT_2026-09-02_10.00.00"), effects)
    assert gams_hours(no_stats) is None
    assert not runtime_is_slower(current, no_stats) and not runtime_is_slower(no_stats, slow)


def test_an_interrupted_run_below_the_maximum_is_skipped_like_r(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO]
) -> None:
    """``R/modeltests.R:301`` gates on the parity ``Conv``, not on ``Run.convergence``.

    The ``p80_repy`` digits of a run interrupted below ``cm_iteration_max`` (``R/getRunStatus.R:288-290``)
    are not in the set, although ``Run.convergence`` calls that run not converged (D-25); at the maximum
    the parity ``Conv`` is ``not_converged`` and R compared it ("run compareScenarios also for runs that
    are not_converged", ``:333``).
    """
    logger, stream = log_stream
    tree = make_checkout(tmp_path)
    interrupted = {"o_iterationNumber": 35, "o_modelstat": 2, "s80_bool": 0}
    current = amt_run(
        tree, "SSP2-NPi-AMT_2026-09-30_10.00.00", gdx=interrupted, repy=dict.fromkeys(REGIONS, 2.0)
    )
    previous = amt_run(tree, "SSP2-NPi-AMT_2026-09-23_10.00.00", gams_hours_=1.0, run_id=111.0)
    effects = recording(tree)
    ctx = context(tree, effects, logger)
    review = review_run(ctx, current.name)
    assert review.record.Conv == "2" * len(REGIONS)
    assert review.run.convergence is Convergence.NOT_CONVERGED
    assert review.previous is None and not review.compared and not review.rse_updated
    assert review.errors == (NOT_CONVERGED,)
    assert runs_of(effects, "sbatch") == [] and runs_of(effects, "rsync") == []
    assert f"{current.name} does not seem to have converged. Skipping!" in stream.getvalue()
    at_maximum = amt_run(tree, "SSP2-NPi-AMT_2026-09-30_11.00.00", gdx=NOT_CONVERGED_GDX)
    review = review_run(ctx, at_maximum.name)
    assert review.record.Conv == "not_converged" and review.run.convergence is Convergence.NOT_CONVERGED
    assert review.previous is not None and review.previous.name == previous.name
    assert review.compared and review.errors == (NOT_CONVERGED,)
    assert len(runs_of(effects, "sbatch")) == 1
    assert f"{at_maximum.name} does not seem to have converged" not in stream.getvalue()


def test_comparison_needs_both_mifs_and_no_pdf_yet(tmp_path: Path) -> None:
    tree = make_checkout(tmp_path)
    effects = recording(tree)
    current = Run(amt_run(tree, "SSP2-NPi-AMT_2026-09-30_10.00.00"), effects)
    previous = Run(amt_run(tree, "SSP2-NPi-AMT_2026-09-23_10.00.00"), effects)
    assert comparison_pending(effects, current, previous)
    (current.path / "comp_with_SSP2-NPi-AMT_2026-09-16_10.00.00.pdf").write_bytes(b"")
    assert not comparison_pending(effects, current, previous)
    without_mif = Run(amt_run(tree, "SSP2-NPi-AMT_2026-09-09_10.00.00", mif=False), effects)
    fresh = Run(amt_run(tree, "SSP2-NPi-AMT_2026-09-30_11.00.00"), effects)
    assert not comparison_pending(effects, fresh, without_mif)


def golden_sbatch_argv(case_id: str) -> list[str]:
    trace = (AMT_GOLDENS / case_id / "trace.jsonl").read_text(encoding="utf-8")
    records = [json.loads(line) for line in trace.splitlines() if line.strip()]
    (argv,) = [["sbatch", *record["argv"]] for record in records if record["tool"] == "sbatch"]
    return argv


def test_compare_scenarios_argv_equals_the_r_command_line_of_the_golden() -> None:
    """``cs2com`` of ``R/modeltests.R:340-352`` as the fake ``sbatch`` of case ``previous-run`` saw it."""
    output = "/p/projects/remind/modeltests/remind/output"
    this = f"{output}/SSP2-NPi-AMT_2026-09-28_10.30.27"
    last = f"{output}/SSP2-NPi-AMT_2026-09-21_10.30.27"
    argv = compare_scenarios_argv(this, last, "SSP2-NPi-AMT_2026-09-21_10.30.27")
    assert argv == golden_sbatch_argv("remind-evaluate-previous-run")
    assert argv == golden_sbatch_argv("remind-evaluate-slow")


# ---------------------------------------------------------------------------------------------
# 6. The scenarios that did not start
# ---------------------------------------------------------------------------------------------


def test_scenario_of_strips_the_stamp() -> None:
    assert scenario_of("SSP2-NPi-AMT_2026-09-28_10.30.27") == "SSP2-NPi-AMT"
    assert scenario_of("testOneRegi-AMT") == "testOneRegi-AMT"


def test_not_started_is_a_set_difference_whatever_the_counts() -> None:
    """BUG-024 gone by design: a scenario started twice does not hide a missing one."""
    cycle = Cycle(
        id="x", runcode="r", runs_to_start=("SSP2-NPi-AMT", "SSP2-NDC-AMT", "default-AMT", "SSP2-NPi-AMT")
    )
    assert expected_scenarios(cycle) == [DEFAULT_RUN, "SSP2-NPi-AMT", "SSP2-NDC-AMT"]
    started = [
        "default-AMT_2026-09-28_13.23.51",
        "SSP2-NPi-AMT_2026-09-28_10.30.27",
        "SSP2-NPi-AMT_2026-09-28_11.00.00",
    ]
    assert scenarios_not_started(started, expected_scenarios(cycle)) == ["SSP2-NDC-AMT"]
    assert scenarios_not_started([], expected_scenarios(cycle)) == [
        DEFAULT_RUN,
        "SSP2-NPi-AMT",
        "SSP2-NDC-AMT",
    ]
    assert scenarios_not_started(started, expected_scenarios(Cycle(id="x", runcode="r"))) == []


# ---------------------------------------------------------------------------------------------
# 7. make test-full
# ---------------------------------------------------------------------------------------------


def test_testthat_result_without_the_log(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO]
) -> None:
    logger, _ = log_stream
    tree = make_checkout(tmp_path)
    effects = recording(tree)
    assert make_test_full_sentence(effects, logger, tree.checkout) == TESTTHAT_MISSING
    assert not [call for call in effects.calls if call.name in ("rename", "mkdir")]


@pytest.mark.parametrize(
    ("content", "expected"),
    [
        (PASSING_LOG, "All tests pass in `make test-full`: [ FAIL 0 | WARN 0 | SKIP 3 | PASS 120 ]"),
        (
            "testthat results\n[ FAIL 2 | WARN 1 | SKIP 0 | PASS 10 ]\n",
            "Not all tests pass in `make test-full`: [ FAIL 2 | WARN 1 | SKIP 0 | PASS 10 ]. Check `{kept}`",
        ),
        ("Error: could not start\n", "`make test-full` did not run properly. Check {kept}"),
        (
            "[ FAIL 1 | WARN 0 | SKIP 0 | PASS 1 ]\n[ FAIL 0 | WARN 0 | SKIP 0 | PASS 2 ]\n",
            "All tests pass in `make test-full`: [ FAIL 0 | WARN 0 | SKIP 0 | PASS 2 ]",
        ),
    ],
)
def test_testthat_result_reads_the_last_fail_line_and_renames_the_log(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO], content: str, expected: str
) -> None:
    logger, _ = log_stream
    tree = make_checkout(tmp_path)
    log = tree.checkout / TEST_FULL_LOG
    log.write_text(content)
    set_mtime(log, datetime(2026, 9, 29, 16, 0, tzinfo=SANDBOX_TZ))
    effects = recording(tree)
    kept = tree.checkout / TESTS_DIR / "test-full-2026-09-29.log"
    assert make_test_full_sentence(effects, logger, tree.checkout) == expected.format(kept=kept)
    assert not log.exists() and kept.read_text() == content


# ---------------------------------------------------------------------------------------------
# 8. The archive
# ---------------------------------------------------------------------------------------------


def test_folder_date_is_the_last_date_in_the_name() -> None:
    assert folder_date("SSP2-NPi-AMT_2026-05-01_00.00.00") == datetime(2026, 5, 1).date()
    assert folder_date("x_2026-01-01_2026-02-02_y") == datetime(2026, 2, 2).date()
    assert folder_date("testOneRegi-AMT") is None
    assert folder_date("t_2026-13-45_00.00.00") is None


def test_old_runs_are_the_amt_directories_older_than_90_days(tmp_path: Path) -> None:
    tree = make_checkout(tmp_path)
    today = NOW.date()
    cutoff = today - timedelta(days=ARCHIVE_DAYS)
    old = make_run(tree.output, "SSP2-NPi-AMT_2026-05-01_00.00.00")
    make_run(tree.output, f"default-AMT_{(cutoff - timedelta(days=1)).isoformat()}_23.59.59")
    make_run(tree.output, f"SSP2-NPi-AMT_{cutoff.isoformat()}_00.00.00")  # at the cutoff: stays
    make_run(tree.output, "testOneRegi-AMT")  # no stamp
    make_run(tree.output, "other_2026-01-01_00.00.00")  # no AMT in the name
    (tree.output / "notes-AMT_2026-01-01_00.00.00").write_text("a file")
    assert old_runs(recording(tree), tree.output, today) == [
        f"default-AMT_{(cutoff - timedelta(days=1)).isoformat()}_23.59.59",
        old.name,
    ]


def test_archive_old_runs_moves_each_run_and_creates_the_directory(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO]
) -> None:
    """BUG-016 and BUG-033 gone by design: one ``move`` per run into a directory created when missing."""
    logger, stream = log_stream
    tree = make_checkout(tmp_path)
    make_run(tree.output, "SSP2-NPi-AMT_2026-05-01_00.00.00", {"full.log": "x"})
    make_run(tree.output, "default-AMT_2026-04-30_00.00.00")
    effects = recording(tree)
    assert archive_old_runs(effects, logger, tree.output, NOW.date()) == [
        "default-AMT_2026-04-30_00.00.00",
        "SSP2-NPi-AMT_2026-05-01_00.00.00",
    ]
    assert (tree.archive / "SSP2-NPi-AMT_2026-05-01_00.00.00" / "full.log").read_text() == "x"
    assert (tree.archive / "default-AMT_2026-04-30_00.00.00").is_dir()
    assert not (tree.output / "default-AMT_2026-04-30_00.00.00").exists()
    assert [call.name for call in effects.calls if call.name in ("mkdir", "move")] == [
        "mkdir",
        "move",
        "move",
    ]
    assert "Moving 2 runs with timestamp older than 90 days (2026-07-02) to 'archive':" in stream.getvalue()
    assert archive_old_runs(effects, logger, tree.output, NOW.date()) == []


# ---------------------------------------------------------------------------------------------
# 9.-10. The README file and the publication
# ---------------------------------------------------------------------------------------------


def test_readme_path_is_next_to_the_cycle_log(tmp_path: Path) -> None:
    assert readme_path(tmp_path / "logs", "2026-09-30") == tmp_path / "logs" / "amt-2026-09-30-README.md"


def test_publish_readme_runs_r_s_git_sequence_in_the_report_repository(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO]
) -> None:
    logger, _ = log_stream
    tree = make_checkout(tmp_path)
    readme = tree.root / "README-draft.md"
    readme.write_text("```\nreport\n```\n")
    effects = recording(tree, amt=CannedAmt({"git-push.exit": "1"}))
    publish_readme(effects, logger, tree.report_repo, readme)
    repo = str(tree.report_repo)
    assert runs_of(effects, "git") == [
        (["reset", "--hard", "origin/master"], repo),
        (["pull"], repo),
        (["add", "README.md"], repo),
        (["commit", "-m", COMMIT_MESSAGE], repo),
        (["push"], repo),
    ]
    assert (tree.report_repo / "README.md").read_text() == "```\nreport\n```\n"


# ---------------------------------------------------------------------------------------------
# The pre-checks
# ---------------------------------------------------------------------------------------------


def test_cycle_to_evaluate_refuses_idle_and_a_failed_state_without_a_cycle() -> None:
    with pytest.raises(AmtStateError, match="cannot 'evaluate'"):
        cycle_to_evaluate(AmtState())
    failed = AmtState(phase=Phase.FAILED, last_error=LastError(command="evaluate", message="boom"))
    with pytest.raises(AmtStateError, match="run 'modeltests reset'"):
        cycle_to_evaluate(failed)
    state = started_state()
    assert cycle_to_evaluate(state) is state.cycle
    assert cycle_to_evaluate(replace(state, phase=Phase.FAILED)) is state.cycle


def test_publishing_without_a_report_repository_is_refused_before_any_effect(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO]
) -> None:
    logger, _ = log_stream
    tree = make_checkout(tmp_path)
    effects = recording(tree)
    ctx = context(tree, effects, logger, report_repo=None)
    saved = ctx.store.load()
    with pytest.raises(EvaluateError, match="--report-repo"):
        evaluate_cycle(ctx)
    assert effects.runs == [] and effects.posts == []
    assert ctx.store.load() == saved


def test_evaluate_without_a_cycle_changes_nothing(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO]
) -> None:
    logger, _ = log_stream
    tree = make_checkout(tmp_path)
    effects = recording(tree)
    ctx = context(tree, effects, logger, state=AmtState())
    with pytest.raises(AmtStateError):
        evaluate_cycle(ctx)
    assert effects.runs == [] and ctx.store.load() == AmtState()


# ---------------------------------------------------------------------------------------------
# The whole command on a synthetic checkout
# ---------------------------------------------------------------------------------------------


def full_checkout(tmp_path: Path) -> tuple[Checkout, Path, Path]:
    """A checkout with a slow converged run, its previous run, an old run and a passing test-full.log."""
    tree = make_checkout(tmp_path)
    current = amt_run(tree, "SSP2-NPi-AMT_2026-09-30_10.00.00", gams_hours_=2.3)
    previous = amt_run(tree, "SSP2-NPi-AMT_2026-09-23_10.00.00", gams_hours_=1.0, run_id=111.0)
    make_run(tree.output, "default-AMT_2026-05-01_00.00.00", {"full.log": "old"})
    log = tree.checkout / TEST_FULL_LOG
    log.write_text(PASSING_LOG)
    set_mtime(log, datetime(2026, 9, 29, 16, 0, tzinfo=SANDBOX_TZ))
    return tree, current, previous


def test_evaluate_cycle_runs_every_step_and_completes_the_cycle(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO]
) -> None:
    logger, stream = log_stream
    tree, current, previous = full_checkout(tmp_path)
    effects = recording(tree)
    ctx = context(tree, effects, logger)
    result = evaluate_cycle(ctx)
    # the review of the one started run
    assert result.runs_started == ("SSP2-NPi-AMT_2026-09-30_10.00.00",)
    (review,) = result.reviews
    assert review.previous is not None and review.previous.name == previous.name
    assert review.compared and review.rse_updated and review.errors == (CHECK_RUNTIME,)
    assert result.runs_not_started == (DEFAULT_RUN, "SSP2-never-AMT")
    assert result.errors == (CHECK_RUNTIME, SCENARIOS_NOT_STARTED)
    assert result.summary == f"{SUMMARY_PREFIX}{CHECK_RUNTIME}. {SCENARIOS_NOT_STARTED}"
    assert (
        result.testthat_result
        == "All tests pass in `make test-full`: [ FAIL 0 | WARN 0 | SKIP 3 | PASS 120 ]"
    )
    assert result.archived == ("default-AMT_2026-05-01_00.00.00",)
    assert result.tested_commit == TESTED_COMMIT and len(result.merges) == 2
    # the subprocesses, byte for byte
    assert runs_of(effects, "rsync") == [
        (["-e", "ssh", "-av", "fulldata.gdx", GDX_ON_RSE_SERVER], str(current.resolve()))
    ]
    expected_sbatch = compare_scenarios_argv(current.resolve(), previous.resolve(), previous.name)
    assert runs_of(effects, "sbatch") == [(expected_sbatch[1:], str(tree.checkout))]
    git = [argv for argv, _ in runs_of(effects, "git")]
    assert git[:2] == [
        ["log", "-1"],
        ["log", "--merges", "--pretty=oneline", f"{PREVIOUS_COMMIT}..{TESTED_COMMIT}", "--abbrev-commit"],
    ]
    assert [argv[0] for argv in git[2:]] == ["reset", "pull", "add", "commit", "push"]
    # the file effects
    assert moved(effects) == [(str(tree.output / "default-AMT_2026-05-01_00.00.00"), str(tree.archive))]
    assert (tree.archive / "default-AMT_2026-05-01_00.00.00" / "full.log").read_text() == "old"
    assert (tree.checkout / TESTS_DIR / "test-full-2026-09-29.log").exists()
    assert result.readme_path == tree.log_dir / "amt-2026-09-30-README.md"
    assert result.readme_path.read_text() == result.readme
    assert (tree.report_repo / "README.md").read_text() == result.readme
    lines = result.readme.splitlines()
    assert lines[0] == "```" and lines[-1] == "```" and lines[-2] == result.summary
    assert lines[2] == f"Path to runs: {tree.checkout}/output/"
    assert any(line.startswith("SSP2-NPi-AMT_2026-09-30_10.00.00 ") for line in lines)
    assert lines[lines.index(NOT_STARTED_TITLE) + 1 : lines.index(NOT_STARTED_TITLE) + 3] == [
        DEFAULT_RUN,
        "SSP2-never-AMT",
    ]
    # the notification and the state
    assert result.delivery is Delivery.SENT and result.published
    (post,) = effects.posts
    text = post.to_json()["payload_json"]
    assert isinstance(text, dict)
    message = text["text"]
    assert message.splitlines()[:3] == [
        "Please find below the status of the REMIND automated model tests (AMT) of 2026-09-30. "
        f"Runs are here: `{tree.checkout}`.",
        result.summary,
        result.testthat_result,
    ]
    assert "`rs -A` returns:" in message and "SSP2-NPi-AMT_2026-09-30_10.00.00" in message
    assert message.endswith(f"{NOT_STARTED_TITLE}\n{DEFAULT_RUN}\nSSP2-never-AMT")
    saved = ctx.store.load()
    assert saved is not None and saved == result.state
    assert saved.phase is Phase.IDLE and saved.cycle is None
    assert saved.last_completed == Completed(
        id="2026-09-30",
        evaluated_at=NOW,
        tested_commit=TESTED_COMMIT,
        summary=result.summary,
        runcode=RUNCODE,
    )
    assert "Evaluation of cycle 2026-09-30 finished" in stream.getvalue()


def test_evaluate_cycle_without_publishing_comparison_or_webhook(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO]
) -> None:
    logger, _ = log_stream
    tree, _, _ = full_checkout(tmp_path)
    effects = recording(tree, webhook=None)
    ctx = context(tree, effects, logger, publish=False, compare_scenarios=False, report_repo=None)
    result = evaluate_cycle(ctx)
    assert not result.published and result.delivery is Delivery.NOT_CONFIGURED
    assert effects.posts == [] and runs_of(effects, "sbatch") == []
    assert [argv[0] for argv, _ in runs_of(effects, "git")] == ["log", "log"]
    assert "comp_with_RUN-DATE.pdf" not in result.readme
    assert result.errors == (CHECK_RUNTIME, SCENARIOS_NOT_STARTED)  # the runtime rule needs no comparison
    assert not (tree.report_repo / "README.md").exists()


def test_without_a_webhook_nothing_is_captured_and_the_cluster_paths_are_not_read(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO]
) -> None:
    """``R/modeltests.R:449``: no token, no tables; the production cluster paths play no part in a cycle."""
    logger, stream = log_stream
    tree, _, _ = full_checkout(tmp_path)
    effects = recording(tree, webhook=None, cluster_paths=ClusterPaths())
    ctx = context(tree, effects, logger, publish=False, compare_scenarios=False, report_repo=None)
    result = evaluate_cycle(ctx)
    assert result.delivery is Delivery.NOT_CONFIGURED and result.state.phase is Phase.IDLE
    assert effects.posts == []
    assert not any(a.startswith(ClusterPaths().amt_dir) for c in effects.calls for a in c.args)
    assert (
        f"No Mattermost webhook URL in ${MATTERMOST_URL_VARIABLE}: notification not sent" in stream.getvalue()
    )
    assert "Composing message" not in stream.getvalue()


def test_the_sanity_table_describes_the_cycle_s_runs_in_the_checkout(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO]
) -> None:
    """The message's sanity block is ``Results from <dir>/output/`` plus the started runs.

    D-24: R's ``getSanityChecks()`` read the cluster constant and its ``runcode.rds``, so another
    ``--dir`` got another checkout's table.
    """
    logger, _ = log_stream
    tree, current, _ = full_checkout(tmp_path)
    effects = recording(tree, cluster_paths=ClusterPaths())
    ctx = context(tree, effects, logger, publish=False, compare_scenarios=False, report_repo=None)
    result = evaluate_cycle(ctx)
    assert result.delivery is Delivery.SENT
    (post,) = effects.posts
    body = post.to_json()["payload_json"]
    assert isinstance(body, dict)
    lines = body["text"].splitlines()
    sanity = lines[lines.index(SANITY_HEADER) + 1 :]
    assert sanity[:3] == ["```", f"Results from {tree.output}/ ", ""]
    assert sanity[3].startswith("For column explanations see: ") and sanity[4].startswith("Folder")
    assert sanity[5] == "" and sanity[6].startswith(current.name) and sanity[7] == "```"
    assert not any(a.startswith(ClusterPaths().amt_dir) for c in effects.calls for a in c.args)


def test_evaluate_cycle_with_nothing_started(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO]
) -> None:
    logger, _ = log_stream
    tree = make_checkout(tmp_path)
    effects = recording(tree)
    ctx = context(tree, effects, logger, state=started_state(runs_to_start=()))
    result = evaluate_cycle(ctx)
    assert result.reviews == () and result.runs_not_started == (DEFAULT_RUN,)
    assert result.errors == (SCENARIOS_NOT_STARTED, NO_RUNS_STARTED)
    assert result.summary == f"{SUMMARY_PREFIX}{SCENARIOS_NOT_STARTED}. {NO_RUNS_STARTED}"
    assert result.testthat_result == TESTTHAT_MISSING


def test_a_failure_moves_the_cycle_to_failed_and_notifies(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO]
) -> None:
    logger, stream = log_stream
    tree, _, _ = full_checkout(tmp_path)
    effects = recording(tree, slurm=CannedSlurm({"squeue_sequence": "1\n1\n1\n1\n"}))
    ctx = context(tree, effects, logger)
    with pytest.raises(CycleFailedError, match=re.escape(SQUEUE_FAILED)) as raised:
        evaluate_cycle(ctx)
    assert isinstance(raised.value.__cause__, EvaluateError) and str(raised.value.__cause__) == SQUEUE_FAILED
    saved = ctx.store.load()
    assert saved is not None and saved.phase is Phase.FAILED and raised.value.state == saved
    assert saved.cycle == ctx.state.cycle
    assert saved.last_error == LastError(
        command="evaluate", message=SQUEUE_FAILED, at=NOW, log="logs/amt-2026-09-30.log"
    )
    (post,) = effects.posts
    body = post.to_json()["payload_json"]
    assert isinstance(body, dict)
    assert (
        body["text"]
        .splitlines()[0]
        .startswith("The REMIND automated model tests (AMT) failed: `modeltests evaluate` stopped")
    )
    assert SQUEUE_FAILED in body["text"] and body["text"].endswith(NEXT_STEPS)
    assert "'evaluate' failed: " + SQUEUE_FAILED in stream.getvalue()
    assert runs_of(effects, "git") == [] and moved(effects) == []
    assert (tree.checkout / TEST_FULL_LOG).exists()


def test_a_failed_cycle_can_be_evaluated_again(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO]
) -> None:
    logger, _ = log_stream
    tree, _, _ = full_checkout(tmp_path)
    failed = replace(
        started_state(), phase=Phase.FAILED, last_error=LastError(command="evaluate", message=SQUEUE_FAILED)
    )
    effects = recording(tree)
    ctx = context(tree, effects, logger, state=failed)
    result = evaluate_cycle(ctx)
    assert result.state.phase is Phase.IDLE and result.state.last_error == failed.last_error
    assert (
        result.state.last_completed is not None and result.state.last_completed.tested_commit == TESTED_COMMIT
    )


def test_an_unreadable_started_run_fails_the_cycle(
    tmp_path: Path, log_stream: tuple[logging.Logger, io.StringIO]
) -> None:
    logger, _ = log_stream
    tree, current, _ = full_checkout(tmp_path)
    (current / "fulldata.gdx").write_bytes(b"not a gdx")
    effects = recording(tree)
    ctx = context(tree, effects, logger)
    with pytest.raises(CycleFailedError, match="gdx"):
        evaluate_cycle(ctx)
    saved = ctx.store.load()
    assert saved is not None and saved.phase is Phase.FAILED and saved.last_error is not None
    assert "gdx" in saved.last_error.message


# ---------------------------------------------------------------------------------------------
# The R AMT goldens: every remind-evaluate* case (06 section 4.4, items 3 and 4)
# ---------------------------------------------------------------------------------------------

KEPT = "kept"
CHANGED = "changed by D-24"
MAPPED = "mapped onto the state machine"
OK, ERROR, REFUSED = "ok", "error", "refused"
LINK_ABOVE = 1_000_000
"""Files above this size are linked into the clone, not copied (the GDX files); nothing writes through."""
OLD_NPI = "SSP2-NPi-AMT_2026-08-28_22.06.57"
CURRENT_NPI = "SSP2-NPi-AMT_2026-09-28_10.30.27"
EARLIER_NPI = "SSP2-NPi-AMT_2026-09-21_10.30.27"
GIT_LOG_1 = ["log", "-1"]
GIT_MERGES = ["log", "--merges", "--pretty=oneline", f"{PREVIOUS_COMMIT}..{TESTED_COMMIT}", "--abbrev-commit"]
PUBLISH_GIT = [
    ["reset", "--hard", "origin/master"],
    ["pull"],
    ["add", "README.md"],
    ["commit", "-m", COMMIT_MESSAGE],
    ["push"],
]
RSYNC = ["-e", "ssh", "-av", "fulldata.gdx", GDX_ON_RSE_SERVER]
CLUSTER_OUTPUT = "/p/projects/remind/modeltests/remind/output"
NO_RUNS_FOUND_PRINTED = '[1] "No runs found"'
"""What ``capture.output(loopRuns(character(0)))`` recorded: R auto-printed the returned string."""


@dataclass(frozen=True)
class Expectation:
    """What the Python evaluation of a case does, next to the R golden."""

    classification: str
    outcome: str
    error: str | None = None
    comparisons: tuple[tuple[str, str], ...] = ()
    """(current run, previous run relative to ``output/``) pairs that get a compareScenarios2 job."""
    rsync_runs: tuple[str, ...] = (CURRENT_NPI,)
    archived: tuple[str, ...] = ()
    merges: bool = True
    readme_reference: str = ""
    """The case whose golden README/message is the reference when the case's own golden has none."""
    not_started_block: bool = True
    squeue_exits: tuple[int, ...] = (0,)


EXPECTATIONS: dict[str, Expectation] = {
    "remind-evaluate": Expectation(KEPT, OK),
    "remind-evaluate-archive": Expectation(KEPT, OK, archived=("SSP2-NPi-AMT_2026-05-01_00.00.00",)),
    "remind-evaluate-archive-boundary": Expectation(KEPT, OK, archived=("default-AMT_2026-07-01_23.59.59",)),
    "remind-evaluate-archive-missing": Expectation(
        CHANGED, OK, archived=("SSP2-NPi-AMT_2026-05-01_00.00.00",)
    ),
    "remind-evaluate-archive-missing-two": Expectation(
        CHANGED, OK, archived=("default-AMT_2026-04-30_00.00.00", "SSP2-NPi-AMT_2026-05-01_00.00.00")
    ),
    "remind-evaluate-curlfail": Expectation(KEPT, OK),
    "remind-evaluate-duplicate": Expectation(
        CHANGED,
        OK,
        comparisons=(("SSP2-NPi-AMT_2026-09-28_11.00.00", CURRENT_NPI),),
        rsync_runs=(CURRENT_NPI, "SSP2-NPi-AMT_2026-09-28_11.00.00"),
        readme_reference=BASELINE,
    ),
    "remind-evaluate-email": Expectation(KEPT, OK),
    "remind-evaluate-git-fail": Expectation(
        KEPT, ERROR, error="git log -1 in {checkout} did not report the tested commit"
    ),
    "remind-evaluate-git-merges-empty": Expectation(KEPT, OK),
    "remind-evaluate-grs-corrupt": Expectation(MAPPED, OK, readme_reference=BASELINE),
    "remind-evaluate-grs-existing": Expectation(MAPPED, OK),
    "remind-evaluate-grs-stale": Expectation(MAPPED, OK, readme_reference=BASELINE),
    "remind-evaluate-nocompscen": Expectation(KEPT, OK),
    "remind-evaluate-notoken": Expectation(KEPT, OK),
    "remind-evaluate-previous-in-archive": Expectation(
        CHANGED, OK, comparisons=((CURRENT_NPI, f"{ARCHIVE_DIR}/{EARLIER_NPI}"),)
    ),
    "remind-evaluate-previous-run": Expectation(KEPT, OK, comparisons=((CURRENT_NPI, EARLIER_NPI),)),
    "remind-evaluate-runcode-nomatch": Expectation(KEPT, OK, rsync_runs=()),
    "remind-evaluate-runstostart-missing": Expectation(
        MAPPED, OK, readme_reference=BASELINE, not_started_block=False
    ),
    "remind-evaluate-sbatch-fail": Expectation(KEPT, OK, comparisons=((CURRENT_NPI, EARLIER_NPI),)),
    "remind-evaluate-slow": Expectation(KEPT, OK, comparisons=((CURRENT_NPI, EARLIER_NPI),)),
    "remind-evaluate-squeue-3fail": Expectation(KEPT, OK, squeue_exits=(1, 1, 1, 0)),
    "remind-evaluate-squeue-4fail": Expectation(KEPT, ERROR, error=SQUEUE_FAILED, squeue_exits=(1, 1, 1, 1)),
    "remind-evaluate-state-corrupt": Expectation(MAPPED, REFUSED),
    "remind-evaluate-state-missing": Expectation(MAPPED, OK, merges=False, readme_reference=BASELINE),
    "remind-evaluate-testfull-fail": Expectation(KEPT, OK),
    "remind-evaluate-testfull-missing": Expectation(KEPT, OK),
    "remind-evaluate-testfull-noresult": Expectation(KEPT, OK),
    "remind-evaluate-testfull-ok": Expectation(KEPT, OK),
}


def evaluate_cases() -> list[str]:
    return sorted(path.name for path in AMT_CASES.glob("remind-evaluate*") if path.is_dir())


def test_every_remind_evaluate_case_has_an_expectation() -> None:
    assert evaluate_cases() == sorted(EXPECTATIONS)


# --- the clone of the fixture AMT directory ------------------------------------------------------------


@dataclass(frozen=True)
class Clone:
    """The fixture AMT tree reproduced under ``tmp_path``: ``p`` is the clone's ``/p``."""

    p: Path

    @property
    def amt_root(self) -> Path:
        return self.p / "projects" / "remind" / "modeltests"

    @property
    def checkout(self) -> Path:
        return self.amt_root / "remind"

    @property
    def output(self) -> Path:
        return self.checkout / "output"

    @property
    def archive(self) -> Path:
        return self.output / ARCHIVE_DIR

    @property
    def report_repo(self) -> Path:
        return self.amt_root / "testing_suite"

    @property
    def log_dir(self) -> Path:
        return self.checkout / "logs"

    def cluster_paths(self) -> ClusterPaths:
        return ClusterPaths(
            amt_dir=str(self.checkout),
            amt_output=f"{self.output}/",
            amt_archive=str(self.archive),
            amt_runcode=str(self.checkout / "runcode.rds"),
            remind_results_archive=f"{self.p}/projects/rd3mod/models/results/remind/",
        )

    def unprefix(self, text: str) -> str:
        """The text with the clone's ``/p`` replaced by the cluster's."""
        return text.replace(str(self.p), "/p")


def link_tree(src: Path, dst: Path) -> None:
    """Copy ``src`` below ``dst``: small files copied with their mtimes, large files linked, no overwrite."""
    for root, _dirs, files in os.walk(src):
        relative = Path(root).relative_to(src)
        target_dir = dst / relative
        target_dir.mkdir(parents=True, exist_ok=True)
        for name in files:
            source = Path(root) / name
            target = target_dir / name
            if target.exists() or target.is_symlink():
                continue
            if source.is_symlink():
                os.symlink(os.readlink(source), target)
            elif source.stat().st_size > LINK_ABOVE:
                os.symlink(source.resolve(), target)
            else:
                shutil.copy2(source, target)
    for root, _dirs, _files in sorted(os.walk(src), key=lambda item: -len(item[0])):
        relative = Path(root).relative_to(src)
        stamp = os.stat(root).st_mtime
        os.utime(dst / relative, (stamp, stamp))


def clone_tree(tmp_path: Path, fixtures: Path, synthetic: Path) -> Clone:
    clone = Clone((tmp_path / "p").resolve())
    link_tree(fixtures / "p" / "projects" / "remind" / "modeltests", clone.amt_root)
    link_tree(fixtures / "p" / "projects" / "rd3mod", clone.p / "projects" / "rd3mod")
    link_tree(synthetic / "p" / "projects" / "remind" / "modeltests", clone.amt_root)
    replace_file(clone.amt_root / ".testsstatus", b"next:evaluate\n")
    return clone


def replace_file(path: Path, content: bytes) -> None:
    """Write ``path`` as a new file (a link to a canonical file is removed first, never written through)."""
    if path.exists() or path.is_symlink():
        path.unlink()
    path.write_bytes(content)


def copy_run(clone: Clone, source: str, target: str) -> None:
    """``cp -a`` of a run folder inside the clone (links stay links)."""
    shutil.copytree(clone.output / source, clone.output / target, symlinks=True)


def earlier_npi(clone: Clone, hours: float) -> None:
    """The earlier SSP2-NPi-AMT run of ``prepare.sh``: a copy of the current one with ``hours`` GAMS hours."""
    copy_run(clone, CURRENT_NPI, EARLIER_NPI)
    run = clone.output / EARLIER_NPI
    (run / "runstatistics.rda").unlink()
    write_stats(run, remind_stats(gams_start=1000.0, gams_end=1000.0 + hours * HOUR, run_id=777.0))


def plant_test_full_log(clone: Clone, content: str) -> None:
    """The ``test-full.log`` of the testfull cases: at the AMT root, as ``prepare.sh`` wrote it."""
    (clone.checkout / TESTS_DIR).mkdir(exist_ok=True)
    replace_file(clone.amt_root / TEST_FULL_LOG, content.encode())
    set_mtime(clone.amt_root / TEST_FULL_LOG, datetime(2026, 9, 29, 16, 0, tzinfo=SANDBOX_TZ))


def prepare_archive(clone: Clone) -> None:
    copy_run(clone, OLD_NPI, "SSP2-NPi-AMT_2026-05-01_00.00.00")


def prepare_archive_boundary(clone: Clone) -> None:
    copy_run(clone, OLD_NPI, "SSP2-NPi-AMT_2026-07-02_00.00.00")
    copy_run(clone, OLD_NPI, "default-AMT_2026-07-01_23.59.59")


def prepare_archive_missing(clone: Clone) -> None:
    copy_run(clone, OLD_NPI, "SSP2-NPi-AMT_2026-05-01_00.00.00")
    shutil.rmtree(clone.archive)


def prepare_archive_missing_two(clone: Clone) -> None:
    copy_run(clone, OLD_NPI, "SSP2-NPi-AMT_2026-05-01_00.00.00")
    copy_run(clone, OLD_NPI, "default-AMT_2026-04-30_00.00.00")
    shutil.rmtree(clone.archive)


def prepare_previous_in_archive(clone: Clone) -> None:
    shutil.copytree(clone.output / CURRENT_NPI, clone.archive / EARLIER_NPI, symlinks=True)


PREPARE: dict[str, Callable[[Clone], None]] = {
    "remind-evaluate-archive": prepare_archive,
    "remind-evaluate-archive-boundary": prepare_archive_boundary,
    "remind-evaluate-archive-missing": prepare_archive_missing,
    "remind-evaluate-archive-missing-two": prepare_archive_missing_two,
    "remind-evaluate-duplicate": lambda clone: copy_run(
        clone, CURRENT_NPI, "SSP2-NPi-AMT_2026-09-28_11.00.00"
    ),
    "remind-evaluate-grs-corrupt": lambda clone: replace_file(clone.output / "gRS.rds", b"garbage\n"),
    "remind-evaluate-previous-in-archive": prepare_previous_in_archive,
    "remind-evaluate-previous-run": lambda clone: earlier_npi(clone, 3.0),
    "remind-evaluate-runcode-nomatch": lambda clone: replace_file(
        clone.checkout / "runcode.rds", rds_bytes("^nothing-matches$")
    ),
    "remind-evaluate-runstostart-missing": lambda clone: (clone.checkout / "runsToStart.rds").unlink(),
    "remind-evaluate-sbatch-fail": lambda clone: copy_run(clone, CURRENT_NPI, EARLIER_NPI),
    "remind-evaluate-slow": lambda clone: earlier_npi(clone, 1.0),
    "remind-evaluate-state-corrupt": lambda clone: replace_file(clone.checkout / "runcode.rds", b"garbage\n"),
    "remind-evaluate-state-missing": lambda clone: (clone.checkout / "lastcommit.rds").unlink(),
    "remind-evaluate-testfull-fail": lambda clone: plant_test_full_log(
        clone, "testthat results\n[ FAIL 2 | WARN 1 | SKIP 0 | PASS 10 ]\n"
    ),
    "remind-evaluate-testfull-missing": lambda clone: (clone.amt_root / TEST_FULL_LOG).unlink(),
    "remind-evaluate-testfull-noresult": lambda clone: plant_test_full_log(clone, "Error: could not start\n"),
    "remind-evaluate-testfull-ok": lambda clone: plant_test_full_log(clone, PASSING_LOG),
}


# --- the golden files ----------------------------------------------------------------------------------


def case_definition(case_id: str) -> dict[str, Any]:
    data: dict[str, Any] = json.loads((AMT_CASES / case_id / "case.json").read_text(encoding="utf-8"))
    return data


def golden_result(case_id: str) -> str:
    status: str = json.loads((AMT_GOLDENS / case_id / "result.json").read_text(encoding="utf-8"))["status"]
    return status


def golden_readme(case_id: str) -> list[str] | None:
    path = AMT_GOLDENS / case_id / "README.md"
    return path.read_text(encoding="utf-8").splitlines() if path.is_file() else None


def golden_message(case_id: str) -> str | None:
    """The text R pasted into the ``curl`` body (BUG-015: not JSON), escape sequences removed."""
    path = AMT_GOLDENS / case_id / "mattermost.json"
    if not path.is_file():
        return None
    body: str = json.loads(path.read_text(encoding="utf-8"))["payload"]
    prefix, suffix = '{"text": "', '"}'
    assert body.startswith(prefix) and body.endswith(suffix)
    return ESCAPE.sub("", body[len(prefix) : -len(suffix)])


def golden_calls(case_id: str, tool: str) -> list[list[str]]:
    trace = (AMT_GOLDENS / case_id / "trace.jsonl").read_text(encoding="utf-8")
    records = [json.loads(line) for line in trace.splitlines() if line.strip()]
    return [list(record["argv"]) for record in records if record["tool"] == tool]


def golden_squeue_exits(case_id: str) -> list[int]:
    trace = (AMT_GOLDENS / case_id / "trace.jsonl").read_text(encoding="utf-8")
    records = [json.loads(line) for line in trace.splitlines() if line.strip()]
    return [
        int(record["exit"])
        for record in records
        if record["tool"] == "squeue" and "%i" in " ".join(record["argv"])
    ]


def summary_parts(summary: str) -> list[str]:
    if summary == LOOK_GOOD:
        return []
    return summary.removeprefix(SUMMARY_PREFIX).split(". ")


def expected_summary(golden: str, *, not_started: bool) -> str:
    """The R summary with the D-24 error for the scenarios that did not start, before ``No runs started``."""
    parts = summary_parts(golden)
    if not_started:
        at = parts.index(NO_RUNS_STARTED) if NO_RUNS_STARTED in parts else len(parts)
        parts.insert(at, SCENARIOS_NOT_STARTED)
    return summary_line(parts)


def readme_sections(lines: Sequence[str]) -> tuple[list[str], str]:
    """The README lines without the summary, and the summary line."""
    summary = next(line for line in lines if line.startswith(SUMMARY_PREFIX))
    return [line for line in lines if line != summary], summary


def reference_readme(case_id: str, expectation: Expectation) -> list[str]:
    """The golden README the Python README is compared with, transformed for the mapped and changed cases."""
    lines = golden_readme(expectation.readme_reference or case_id)
    assert lines is not None, f"{case_id}: no README golden to compare with"
    if case_id == "remind-evaluate-state-missing":
        lines = [line for line in lines if MERGE_LINE.match(line) is None]
    if case_id == "remind-evaluate-duplicate":
        current = next(line for line in lines if line.startswith(f"{CURRENT_NPI} "))
        lines.insert(
            lines.index(current) + 1, current.replace(CURRENT_NPI, "SSP2-NPi-AMT_2026-09-28_11.00.00")
        )
    if not expectation.not_started_block:
        start = lines.index(" ")
        end = lines.index(" ", start + 1)
        lines = lines[:start] + lines[end + 1 :]
    return lines


MERGE_LINE = re.compile(r"^[0-9a-f]{7} Merge pull request ")


# --- the replay ----------------------------------------------------------------------------------------


@dataclass(frozen=True)
class Replay:
    clone: Clone
    effects: RecordingEffects
    ctx: AmtContext
    result: Evaluation | None
    error: BaseException | None
    log: str
    r_files_before: dict[str, bytes | None]


def replay(case_id: str, tmp_path: Path, fixtures: Path, synthetic: Path) -> Replay:
    case = case_definition(case_id)
    clone = clone_tree(tmp_path, fixtures, synthetic)
    prepare = PREPARE.get(case_id)
    if prepare is not None:
        prepare(clone)
    environ = {**SANDBOX_ENVIRON, "USER": case["user"]}
    if case["mattermostToken"] is not None:
        environ[MATTERMOST_URL_VARIABLE] = case["mattermostToken"]
    effects = RecordingEffects(
        now=resolve_frozen(case["frozen"] or SANDBOX_FROZEN),
        user=case["user"],
        on_cluster=True,
        amt=CannedAmt.from_case_dir(AMT_CASES / case_id),
        slurm=CannedSlurm.from_case_dir(slurm_case_dir(case["slurm_case"])),
        cwd=clone.p,
        environ=environ,
        cluster_paths=clone.cluster_paths(),
    )
    stream = io.StringIO()
    with cycle_log(None, "evaluate", clock=effects.now, stream=stream) as logger:
        store = StateStore.at(effects, clone.checkout)
        state = store.load_or_migrate(logger.info)
        ctx = AmtContext(
            dir=clone.checkout,
            report_repo=clone.report_repo,
            user=case["user"],
            options=AmtOptions(
                publish=bool(case["email"]), compare_scenarios=bool(case["compScen"]), log_dir=clone.log_dir
            ),
            effects=effects,
            logger=logger,
            state=state,
        )
        r_files_before = state_files(clone)
        result: Evaluation | None = None
        error: BaseException | None = None
        try:
            result = evaluate_cycle(ctx)
        except (CycleFailedError, AmtStateError) as raised:
            error = raised
    return Replay(clone, effects, ctx, result, error, stream.getvalue(), r_files_before)


def state_files(clone: Clone) -> dict[str, bytes | None]:
    """The four R state files of ``06`` 3.2 as bytes (``None`` when absent)."""
    names = (
        clone.amt_root / ".testsstatus",
        clone.checkout / "runcode.rds",
        clone.checkout / "runsToStart.rds",
        clone.checkout / "lastcommit.rds",
    )
    return {path.name: path.read_bytes() if path.exists() else None for path in names}


@pytest.mark.golden
@pytest.mark.parametrize("case_id", sorted(EXPECTATIONS))
def test_remind_evaluate_case(case_id: str, tmp_path: Path, fixtures_dir: Path, synthetic_dir: Path) -> None:
    expectation = EXPECTATIONS[case_id]
    played = replay(case_id, tmp_path, fixtures_dir, synthetic_dir)
    clone, effects, ctx = played.clone, played.effects, played.ctx
    # the R state files are only read (06 3.2): the clone keeps them as the prepare step left them
    assert state_files(clone) == played.r_files_before
    if expectation.outcome == REFUSED:
        assert_refused(case_id, played)
        return
    if expectation.outcome == ERROR:
        assert_failed(case_id, expectation, played)
        return
    assert played.error is None, f"{case_id}: {played.error!r}\n{played.log}"
    assert played.result is not None
    result = played.result
    if expectation.classification == KEPT:
        assert golden_result(case_id) == "ok"
    assert_readme(case_id, expectation, played)
    assert_subprocesses(case_id, expectation, played)
    assert_archive(case_id, expectation, played)
    assert_message(case_id, expectation, played)
    # the transition started -> idle (06 3.2)
    saved = ctx.store.load()
    assert saved is not None and saved == result.state and saved.phase is Phase.IDLE and saved.cycle is None
    assert saved.last_completed is not None
    assert saved.last_completed.tested_commit == TESTED_COMMIT
    assert saved.last_completed.summary == result.summary
    assert ctx.state.cycle is not None and saved.last_completed.runcode == ctx.state.cycle.runcode
    assert saved.last_completed.evaluated_at == effects.now()
    assert result.testthat_result == TESTTHAT_MISSING  # the R cases planted the log above the checkout


def assert_refused(case_id: str, played: Replay) -> None:
    """``state-corrupt``: the migration yields ``failed`` without a cycle; refused, nothing runs."""
    assert isinstance(played.error, AmtStateError), played.log
    assert "run 'modeltests reset'" in str(played.error)
    assert played.ctx.state.phase is Phase.FAILED and played.ctx.state.cycle is None
    assert played.effects.runs == [] and played.effects.posts == []
    assert played.ctx.store.load() == played.ctx.state
    assert golden_result(case_id) == "error"


def assert_failed(case_id: str, expectation: Expectation, played: Replay) -> None:
    """``git-fail`` and ``squeue-4fail``: the cycle is ``failed`` with the message, kept, and notified."""
    assert expectation.error is not None
    message = expectation.error.format(checkout=played.clone.checkout)
    assert isinstance(played.error, CycleFailedError), played.log
    assert isinstance(played.error.__cause__, EvaluateError) and str(played.error.__cause__) == message
    saved = played.ctx.store.load()
    assert played.error.state == saved
    assert saved is not None and saved.phase is Phase.FAILED
    assert saved.cycle == played.ctx.state.cycle and saved.cycle is not None
    assert saved.last_error is not None and saved.last_error.message == message
    assert saved.last_error.command == "evaluate" and saved.last_error.at == played.effects.now()
    assert golden_result(case_id) == "error"
    assert golden_squeue_exits(case_id) == list(expectation.squeue_exits)
    assert [call.status for call in played.effects.runs if call.tool == "squeue"][
        : len(expectation.squeue_exits)
    ] == list(expectation.squeue_exits)
    assert [argv for argv, _ in runs_of(played.effects, "git")] == golden_calls(case_id, "git")
    assert moved(played.effects) == [] and runs_of(played.effects, "sbatch") == []
    # the failure notification is new (06 3.2): R sent nothing
    assert golden_message(case_id) is None
    (post,) = played.effects.posts
    body = post.to_json()["payload_json"]
    assert isinstance(body, dict) and message in body["text"] and body["text"].endswith(NEXT_STEPS)


def assert_readme(case_id: str, expectation: Expectation, played: Replay) -> None:
    assert played.result is not None
    reference = reference_readme(case_id, expectation)
    ours = played.clone.unprefix(played.result.readme).splitlines()
    expected_lines, golden_summary = readme_sections(reference)
    our_lines, our_summary = readme_sections(ours)
    assert our_lines == expected_lines
    assert our_summary == expected_summary(golden_summary, not_started=bool(played.result.runs_not_started))
    assert played.result.readme_path.read_text(encoding="utf-8") == played.result.readme
    if expectation.classification == KEPT and expectation.readme_reference == "":
        assert golden_readme(case_id) is not None


def assert_subprocesses(case_id: str, expectation: Expectation, played: Replay) -> None:
    clone, effects = played.clone, played.effects
    git_expected = [GIT_LOG_1] + ([GIT_MERGES] if expectation.merges else [])
    if played.ctx.publish:
        git_expected += PUBLISH_GIT
    git = runs_of(effects, "git")
    assert [argv for argv, _ in git] == git_expected
    assert all(
        clone.unprefix(cwd)
        in ("/p/projects/remind/modeltests/remind", "/p/projects/remind/modeltests/testing_suite")
        for _, cwd in git
    )
    if expectation.classification == KEPT:
        assert [argv for argv, _ in git] == golden_calls(case_id, "git")
    rsync = runs_of(effects, "rsync")
    assert rsync == [(RSYNC, f"{clone.output}/{name}") for name in expectation.rsync_runs]
    if expectation.classification == KEPT:
        assert [argv for argv, _ in rsync] == golden_calls(case_id, "rsync")
    sbatch = [
        ([clone.unprefix(a) for a in argv], clone.unprefix(cwd)) for argv, cwd in runs_of(effects, "sbatch")
    ]
    assert sbatch == [
        (
            compare_scenarios_argv(
                f"{CLUSTER_OUTPUT}/{current}", f"{CLUSTER_OUTPUT}/{previous}", previous.rsplit("/", 1)[-1]
            )[1:],
            "/p/projects/remind/modeltests/remind",
        )
        for current, previous in expectation.comparisons
    ]
    if expectation.classification == KEPT:
        assert [argv for argv, _ in sbatch] == golden_calls(case_id, "sbatch")
    else:
        assert golden_calls(case_id, "sbatch") == []  # R compared nothing in the changed and mapped cases
    assert [call.status for call in effects.runs if call.tool == "squeue"][
        : len(expectation.squeue_exits)
    ] == list(expectation.squeue_exits)
    if expectation.classification == KEPT:
        assert golden_squeue_exits(case_id) == list(expectation.squeue_exits)
    assert len([call for call in effects.calls if call.name == "sleep"]) == len(expectation.squeue_exits) - 1


def assert_archive(case_id: str, expectation: Expectation, played: Replay) -> None:
    clone, effects = played.clone, played.effects
    assert played.result is not None and played.result.archived == expectation.archived
    assert moved(effects) == [(str(clone.output / name), str(clone.archive)) for name in expectation.archived]
    for name in expectation.archived:
        assert (clone.archive / name).is_dir() and not (clone.output / name).exists()
    golden_mv = golden_calls(case_id, "mv")
    if expectation.classification == KEPT:
        assert [argv[:-1] for argv in golden_mv] == (
            [list(expectation.archived)] if expectation.archived else []
        )
    elif case_id == "remind-evaluate-archive-missing":
        # BUG-033: R's ``mv run archive`` renamed the run to ``archive``; here the directory is created first
        assert golden_mv == [["SSP2-NPi-AMT_2026-05-01_00.00.00", "archive"]]
        assert [call.args for call in effects.calls if call.name == "mkdir"][0] == (str(clone.archive),)
        assert (clone.archive / "SSP2-NPi-AMT_2026-05-01_00.00.00" / "config.Rdata").exists()
    elif case_id == "remind-evaluate-archive-missing-two":
        # BUG-033: R's ``mv a b archive`` failed and moved nothing; here both runs land in the new directory
        assert golden_mv == [
            ["default-AMT_2026-04-30_00.00.00", "SSP2-NPi-AMT_2026-05-01_00.00.00", "archive"]
        ]


def assert_message(case_id: str, expectation: Expectation, played: Replay) -> None:
    clone, effects, result = played.clone, played.effects, played.result
    assert result is not None
    case = case_definition(case_id)
    if case["mattermostToken"] is None:
        assert effects.posts == [] and result.delivery is Delivery.NOT_CONFIGURED
        assert golden_message(case_id) is None
        return
    (post,) = effects.posts
    body = post.to_json()["payload_json"]
    assert isinstance(body, dict)
    ours = clone.unprefix(body["text"]).splitlines()
    assert result.delivery is (Delivery.FAILED if case_id == "remind-evaluate-curlfail" else Delivery.SENT)
    golden = golden_message(expectation.readme_reference or case_id)
    assert golden is not None
    expected = golden.splitlines()
    if case_id == "remind-evaluate-state-missing":
        expected = [line for line in expected if MERGE_LINE.match(line) is None]
    if case_id in ("remind-evaluate-duplicate", "remind-evaluate-runstostart-missing"):
        # the tables differ by the duplicate row / the block by the missing scenarios: the frame is checked
        assert ours[0] == expected[0] and ours[2] == expected[2]
        assert ours[1] == expected_summary(expected[1], not_started=bool(result.runs_not_started))
        assert ("SSP2-NPi-AMT_2026-09-28_11.00.00" in body["text"]) == (
            case_id == "remind-evaluate-duplicate"
        )
        assert (NOT_STARTED_TITLE in body["text"]) == expectation.not_started_block
        return
    assert ours[1] == expected_summary(expected[1], not_started=bool(result.runs_not_started))
    if case_id == "remind-evaluate-runcode-nomatch":
        # R's capture.output() caught the auto-printed return value of loopRuns(character(0)); the Python
        # loop_runs returns "No runs found" without printing it (phase 3), so the rs -A fence is empty here
        assert NO_RUNS_FOUND_PRINTED in expected and NO_RUNS_FOUND_PRINTED not in ours
        expected = [line for line in expected if line != NO_RUNS_FOUND_PRINTED]
    assert [line for line in ours if line != ours[1]] == [line for line in expected if line != expected[1]]
