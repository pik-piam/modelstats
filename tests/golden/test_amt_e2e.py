"""``modeltests`` end to end: every ``remind-*`` AMT case inside ``migration/harness/sandbox.sh`` (06 4.4).

``migration/harness/make_goldens.sh`` ran the R ``modeltests()`` once per case of ``migration/cases/amt/``
on a fresh overlay of the fixture AMT directory and the fake REMIND checkout, with the case's prepare
scripts, squeue case and frozen clock, and recorded the goldens under ``migration/goldens/amt/<case>/``.
``migration/harness/run_amt_case.sh`` drives the Python ``modeltests`` the same way (``06`` 4.4 item 2): the
console entry :func:`modelstats.amt.cli.main` runs inside the sandbox with an injected ``Effects`` that
captures the webhook into ``/out/mattermost.jsonl`` and makes ``sleep`` a no-op (as ``make_goldens.R`` did for
``Sys.sleep``); everything else is real inside the overlay (the fake binaries on ``PATH`` write
``trace.jsonl``, the bridge runs the real ``Rscript``, the file effects show in ``clone-diff.json``).

The comparison with the R goldens is informative with an explicit allowed-difference list (``06`` 4.4
item 3): for the kept behaviour the README rows and blocks, the summary (plus the D-24 error for the
scenarios that did not start), the ``git``/``rsync``/``sbatch`` argument vectors of the trace, the set of
archived runs and the Mattermost text equal the R golden; the changes of ``06`` 4.2 are asserted
explicitly; the cases that only exercised the toggle file or an RDS state file map onto the state
machine (item 4). The case table is :data:`EXPECTATIONS` of ``tests/unit/test_amt_evaluate.py`` plus the
four start/toggle cases of :data:`START_CASES`; every difference is classified in
``migration/reviews/2026-10-11-phase5-amt-differences.md``. The round trips (``start`` then ``evaluate``,
``run`` twice, ``status`` and ``reset``, the refused second ``start`` and ``--force``, ``--dry-run``) run
several commands on one clone and check the one log per cycle (``06`` 3.3).

The tier is ``pytest -m sandbox``: bubblewrap, libfaketime, the fixture snapshot, the synthetic layer, the
venv's Python and ``Rscript`` are required (skipped otherwise, never relaxed).
"""

from __future__ import annotations

import io
import json
import re
import shutil
import subprocess
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any

import pytest

from modelstats.amt.cli import (
    CANCELLED_EXIT_STATUS,
    CRASHED_EXIT_STATUS,
    REFUSED_EXIT_STATUS,
    USAGE_EXIT_STATUS,
    main,
)
from modelstats.amt.evaluate import (
    NO_RUNS_STARTED,
    SCENARIOS_NOT_STARTED,
    TESTTHAT_MISSING,
    compare_scenarios_argv,
)
from modelstats.amt.notify import NEXT_STEPS
from modelstats.amt.readme import NOT_STARTED_TITLE, summary_line
from modelstats.amt.state import AmtState, Cycle, LastError, Phase, StateStore
from modelstats.env import CannedSlurm, RecordingEffects
from tests.conftest import CASES, MIGRATION, REPO, SANDBOX_TZ, record_path
from tests.unit.test_amt_bridges import golden_runs_to_start
from tests.unit.test_amt_evaluate import (
    CHANGED,
    CLUSTER_OUTPUT,
    ERROR,
    EXPECTATIONS,
    GIT_LOG_1,
    GIT_MERGES,
    KEPT,
    MAPPED,
    MERGE_LINE,
    NO_RUNS_FOUND_PRINTED,
    OK,
    PUBLISH_GIT,
    REFUSED,
    RSYNC,
    TESTED_COMMIT,
    Expectation,
    case_definition,
    expected_summary,
    golden_calls,
    golden_message,
    golden_result,
    golden_squeue_exits,
    readme_sections,
    reference_readme,
)
from tests.unit.test_amt_start import R_MYDIR, SUBPROCESS_TOOLS, golden_effects, golden_runcode, golden_trace
from tests.unit.test_env import held_by_another_process

RUNNER = MIGRATION / "harness" / "run_amt_case.sh"
FAKETIME = MIGRATION / "harness" / "vendor" / "libfaketime" / "src" / "libfaketime.so.1"
PYTHON = REPO / ".venv" / "bin" / "python"
SANDBOX_TIMEOUT = 900
CHECKOUT = R_MYDIR
"""The checkout inside the sandbox (``/p/projects/remind/modeltests/remind``, ``case.json`` ``mydir``)."""
CHECKOUT_RELATIVE = "projects/remind/modeltests/remind"
"""The same path relative to the fixture root, as ``clone-diff.json`` records paths."""
MIGRATED_CYCLE = "2026-09-28"
"""The cycle id derived from the fixture's ``runcode.rds`` (``.*-AMT_2026-09-28|...``)."""
NO_MATCH_RUNCODE = "^nothing-matches$"
"""``runcode-nomatch``: a pattern that is not R's, so the migrated cycle's id is the pattern itself."""
NEW_CYCLE = "2026-09-30"
"""The cycle id of a ``start`` under the frozen clock."""
R_STATE_FILES = (
    "projects/remind/modeltests/.testsstatus",
    *(f"{CHECKOUT_RELATIVE}/{n}" for n in ("lastcommit.rds", "runsToStart.rds", "output/gRS.rds")),
)
"""The R files the rewrite only reads (``06`` 3.2, 3.4); ``runcode.rds`` is rewritten by ``start``."""
TEN_FIELD = "%i %q %T %C %M %j %V %L %e %Z"
LOG_LINE = re.compile(r"^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2} (\w+) +([A-Z]+) +(.*)$")
STATUS_BY_OUTCOME = {OK: 0, ERROR: 1, REFUSED: 2}
NOW = datetime(2026, 9, 30, 12, 0, 0, tzinfo=SANDBOX_TZ)


@dataclass(frozen=True)
class StartCase:
    """A start or toggle case: what ``modeltests run`` does there, next to the R golden."""

    classification: str
    exit_status: int
    phase: Phase
    r_status: str


START_CASES: dict[str, StartCase] = {
    "remind-start": StartCase(KEPT, 0, Phase.STARTED, "ok"),
    "remind-start-git-fail": StartCase(CHANGED, 1, Phase.FAILED, "ok"),
    "remind-start-make-fail": StartCase(CHANGED, 1, Phase.FAILED, "ok"),
    "remind-nothing": StartCase(MAPPED, 2, Phase.FAILED, "ok"),
}
"""``06`` 4.4: ``remind-start`` is kept; the two failure cases are D-24 (R went on, the port stops with a
``failed`` state and a notification); ``remind-nothing`` (``next:foo``) maps onto the ``failed`` migration."""

ALL_CASES = sorted([*EXPECTATIONS, *START_CASES])


def test_every_remind_case_is_run_end_to_end() -> None:
    cases = sorted(p.name for p in record_path(CASES, "amt").glob("remind-*") if p.is_dir())
    assert cases == ALL_CASES


# ---------------------------------------------------------------------------------------------
# The sandbox run and its artefacts
# ---------------------------------------------------------------------------------------------


@pytest.fixture(scope="session")
def sandbox_runner(fixtures_dir: Path, synthetic_dir: Path) -> Path:
    """``run_amt_case.sh`` with its prerequisites, or a skip."""
    if shutil.which("bwrap") is None:
        pytest.skip("bubblewrap (bwrap) is not installed")
    if not FAKETIME.is_file() and shutil.which("faketime") is None:
        pytest.skip(f"libfaketime not built: {FAKETIME} (migration/harness/README.md)")
    if not PYTHON.is_file():
        pytest.skip(f"venv interpreter absent: {PYTHON} (uv sync)")
    if shutil.which("Rscript") is None:
        pytest.skip("Rscript is not installed (the selectScenarios bridge and the prepare scripts need it)")
    assert RUNNER.is_file(), RUNNER
    return RUNNER


@dataclass(frozen=True)
class SandboxRun:
    """What ``run_amt_case.sh`` left in its case directory."""

    case_dir: Path

    @property
    def statuses(self) -> list[int]:
        """The exit status of every segment (``out/statuses.json``)."""
        records = json.loads((self.case_dir / "out" / "statuses.json").read_text(encoding="utf-8"))
        return [int(record["status"]) for record in records]

    @property
    def stdout(self) -> str:
        return (self.case_dir / "amt.out").read_text(encoding="utf-8")

    @property
    def stderr(self) -> str:
        return (self.case_dir / "amt.err").read_text(encoding="utf-8")

    @property
    def trace(self) -> list[dict[str, Any]]:
        """The fake binaries' ``trace.jsonl`` (the subprocesses the command ran)."""
        text = (self.case_dir / "out" / "trace.jsonl").read_text(encoding="utf-8")
        return [json.loads(line) for line in text.splitlines() if line.strip()]

    @property
    def posts(self) -> list[dict[str, Any]]:
        """The webhook calls captured by the injected effects (``out/mattermost.jsonl``)."""
        path = self.case_dir / "out" / "mattermost.jsonl"
        if not path.is_file():
            return []
        return [json.loads(line) for line in path.read_text(encoding="utf-8").splitlines() if line.strip()]

    @property
    def state(self) -> AmtState | None:
        """``amt-state.json`` of the checkout after the last segment."""
        path = self.case_dir / "out" / "state" / "amt-state.json"
        if not path.is_file():
            return None
        return AmtState.from_json(path.read_text(encoding="utf-8"))

    @property
    def logs(self) -> list[Path]:
        """The cycle logs of the checkout's log directory."""
        directory = self.case_dir / "out" / "state" / "logs"
        return sorted(directory.glob("*.log")) if directory.is_dir() else []

    def readme(self, cycle_id: str) -> list[str] | None:
        path = self.case_dir / "out" / "state" / "logs" / f"amt-{cycle_id}-README.md"
        return path.read_text(encoding="utf-8").splitlines() if path.is_file() else None

    def log_lines(self, log: Path) -> list[tuple[str, str, str]]:
        """``(command, level, message)`` of every single-line record of a cycle log."""
        matched = [LOG_LINE.match(line) for line in log.read_text(encoding="utf-8").splitlines()]
        return [(m.group(1), m.group(2), m.group(3)) for m in matched if m is not None]

    @property
    def clone_diff(self) -> dict[str, list[dict[str, Any]]]:
        return json.loads((self.case_dir / "clone-diff.json").read_text(encoding="utf-8"))  # type: ignore[no-any-return]

    def changed_paths(self, kind: str) -> list[str]:
        return [entry["path"] for entry in self.clone_diff[kind]]

    def calls(self, tool: str) -> list[tuple[list[str], str]]:
        """The argv (without the program) and cwd of every traced call of ``tool``."""
        return [(list(r["argv"]), r["cwd"]) for r in self.trace if r["tool"] == tool]

    def ten_field_squeue_exits(self) -> list[int]:
        return [int(r["exit"]) for r in self.trace if r["tool"] == "squeue" and TEN_FIELD in r["argv"]]

    def archived(self) -> list[str]:
        """The runs that moved into ``output/archive/`` according to the clone diff (created directories)."""
        prefix = f"{CHECKOUT_RELATIVE}/output/archive/"
        created = [
            entry["path"][len(prefix) :]
            for entry in self.clone_diff["created"]
            if entry["path"].startswith(prefix) and entry.get("kind") == "dir"
        ]
        return sorted(name for name in created if "/" not in name)


def run_case(runner: Path, case_dir: Path, case_id: str, *segments: list[str]) -> SandboxRun:
    """``run_amt_case.sh`` for ``case_id`` with the segments joined by ``+``."""
    case_dir.mkdir(parents=True, exist_ok=True)
    argv: list[str] = []
    for index, segment in enumerate(segments):
        if index:
            argv.append("+")
        argv.extend(segment)
    command = [str(runner), str(case_dir), "--amt-case", case_id, "--", *argv]
    subprocess.run(command, check=False, timeout=SANDBOX_TIMEOUT, capture_output=True)
    played = SandboxRun(case_dir)
    assert (case_dir / "amt.status").is_file(), f"{case_id}: the sandbox did not run"
    return played


# ---------------------------------------------------------------------------------------------
# The evaluate cases (the table of tests/unit/test_amt_evaluate.py, item 3 of 06 4.4)
# ---------------------------------------------------------------------------------------------


def assert_r_files_untouched(played: SandboxRun, *, runcode_rewritten: bool) -> None:
    """``06`` 3.2 and 3.4: the R state files are only read; ``runcode.rds`` is rewritten by ``start``."""
    changed = set(played.changed_paths("modified")) | set(played.changed_paths("deleted"))
    assert not changed & set(R_STATE_FILES), changed
    assert (f"{CHECKOUT_RELATIVE}/runcode.rds" in changed) is runcode_rewritten


def assert_one_log_per_cycle(played: SandboxRun, cycle_id: str, *commands: str) -> None:
    """``06`` 3.3: one ``amt-<cycle id>.log`` holding the records of every command of the cycle."""
    assert [log.name for log in played.logs] == [f"amt-{cycle_id}.log"]
    labels = {command for command, _, _ in played.log_lines(played.logs[0])}
    assert labels == set(commands), labels


def failure_text(played: SandboxRun) -> str:
    (post,) = played.posts
    text: str = post["payload_json"]["text"]
    return text


def assert_refused(case_id: str, played: SandboxRun) -> None:
    """``state-corrupt``: the migration yields ``failed`` without a cycle; ``run`` is refused."""
    assert played.statuses == [2]
    state = played.state
    assert state is not None and state.phase is Phase.FAILED and state.cycle is None
    assert "cannot 'run'" in played.stderr and "'modeltests reset' clears the state" in played.stderr
    assert played.trace == [] and played.posts == [] and played.logs == []
    assert golden_result(case_id) == "error"


def assert_failed(case_id: str, expectation: Expectation, played: SandboxRun) -> None:
    """``git-fail`` and ``squeue-4fail``: the cycle is ``failed`` with the message, kept, and notified."""
    assert expectation.error is not None
    message = expectation.error.format(checkout=CHECKOUT)
    assert played.statuses == [1]
    state = played.state
    assert state is not None and state.phase is Phase.FAILED and state.cycle is not None
    assert state.cycle.id == MIGRATED_CYCLE and state.last_error is not None
    assert state.last_error.command == "evaluate" and state.last_error.message == message
    assert state.last_error.log == f"{CHECKOUT}/logs/amt-{MIGRATED_CYCLE}.log"
    assert_one_log_per_cycle(played, MIGRATED_CYCLE, "evaluate")
    assert any(level == "ERROR" and message in text for _, level, text in played.log_lines(played.logs[0]))
    assert golden_result(case_id) == "error"
    assert played.ten_field_squeue_exits() == list(expectation.squeue_exits) == golden_squeue_exits(case_id)
    assert [argv for argv, _ in played.calls("git")] == golden_calls(case_id, "git")
    assert played.calls("sbatch") == [] and played.archived() == []
    # the failure notification is new (06 3.2): R sent nothing
    assert golden_message(case_id) is None
    text = failure_text(played)
    assert message in text and text.endswith(NEXT_STEPS)


def cycle_of(case_id: str) -> str:
    """The id of the cycle migrated from the case's R files (the start date inside the R pattern)."""
    return NO_MATCH_RUNCODE if case_id == "remind-evaluate-runcode-nomatch" else MIGRATED_CYCLE


def assert_readme(case_id: str, expectation: Expectation, played: SandboxRun) -> str:
    """The README rows and blocks equal the (transformed) golden; returns the summary line."""
    ours = played.readme(cycle_of(case_id))
    assert ours is not None, f"{case_id}: no README written"
    expected_lines, golden_summary = readme_sections(reference_readme(case_id, expectation))
    our_lines, our_summary = readme_sections(ours)
    assert our_lines == expected_lines
    assert our_summary == expected_summary(golden_summary, not_started=NOT_STARTED_TITLE in ours)
    return our_summary


def assert_subprocesses(case_id: str, expectation: Expectation, played: SandboxRun, *, publish: bool) -> None:
    git_expected = [GIT_LOG_1] + ([GIT_MERGES] if expectation.merges else [])
    if publish:
        git_expected += PUBLISH_GIT
    git = played.calls("git")
    assert [argv for argv, _ in git] == git_expected
    # R ran git from output/ (withr::with_dir), the port from the checkout: the same repository
    assert all(cwd in (CHECKOUT, "/p/projects/remind/modeltests/testing_suite") for _, cwd in git)
    if expectation.classification == KEPT:
        assert [argv for argv, _ in git] == golden_calls(case_id, "git")
    rsync = played.calls("rsync")
    assert rsync == [(RSYNC, f"{CHECKOUT}/output/{name}") for name in expectation.rsync_runs]
    if expectation.classification == KEPT:
        assert [argv for argv, _ in rsync] == golden_calls(case_id, "rsync")
    sbatch = played.calls("sbatch")
    assert sbatch == [
        (
            compare_scenarios_argv(
                f"{CLUSTER_OUTPUT}/{current}", f"{CLUSTER_OUTPUT}/{previous}", previous.rsplit("/", 1)[-1]
            )[1:],
            CHECKOUT,
        )
        for current, previous in expectation.comparisons
    ]
    if expectation.classification == KEPT:
        assert [argv for argv, _ in sbatch] == golden_calls(case_id, "sbatch")
    else:
        assert golden_calls(case_id, "sbatch") == []  # R compared nothing in the changed and mapped cases
    assert played.ten_field_squeue_exits() == list(expectation.squeue_exits)
    if expectation.classification == KEPT:
        assert golden_squeue_exits(case_id) == list(expectation.squeue_exits)


def assert_archive(case_id: str, expectation: Expectation, played: SandboxRun) -> None:
    assert played.archived() == sorted(expectation.archived)
    deleted = {
        p[len(f"{CHECKOUT_RELATIVE}/output/") :]
        for p in played.changed_paths("deleted")
        if p.startswith(f"{CHECKOUT_RELATIVE}/output/")
        and "/" not in p[len(f"{CHECKOUT_RELATIVE}/output/") :]
    }
    assert deleted == set(expectation.archived)
    golden_mv = golden_calls(case_id, "mv")
    if expectation.classification == KEPT:
        assert [argv[:-1] for argv in golden_mv] == (
            [list(expectation.archived)] if expectation.archived else []
        )
    elif case_id == "remind-evaluate-archive-missing":
        # BUG-033: R's ``mv run archive`` renamed the run to ``archive``; here archive/ is created first
        assert golden_mv == [["SSP2-NPi-AMT_2026-05-01_00.00.00", "archive"]]
        assert f"{CHECKOUT_RELATIVE}/output/archive" in played.changed_paths("created")
    elif case_id == "remind-evaluate-archive-missing-two":
        # BUG-033: R's ``mv a b archive`` failed and moved nothing; here both runs land in the new directory
        assert golden_mv == [
            ["default-AMT_2026-04-30_00.00.00", "SSP2-NPi-AMT_2026-05-01_00.00.00", "archive"]
        ]


def assert_message(case_id: str, expectation: Expectation, played: SandboxRun, summary: str) -> None:
    case = case_definition(case_id)
    if case["mattermostToken"] is None:
        assert played.posts == [] and golden_message(case_id) is None
        assert "notification not sent" in played.stderr
        return
    (post,) = played.posts
    assert post["url"] == case["mattermostToken"]
    text: str = post["payload_json"]["text"]
    ours = text.splitlines()
    assert ours[1] == summary and ours[2] == TESTTHAT_MISSING
    golden = golden_message(expectation.readme_reference or case_id)
    assert golden is not None
    expected = golden.splitlines()
    if case_id == "remind-evaluate-state-missing":
        expected = [line for line in expected if MERGE_LINE.match(line) is None]
    if case_id in ("remind-evaluate-duplicate", "remind-evaluate-runstostart-missing"):
        assert ours[0] == expected[0] and ours[2] == expected[2]
        assert ours[1] == expected_summary(expected[1], not_started=NOT_STARTED_TITLE in ours)
        assert ("SSP2-NPi-AMT_2026-09-28_11.00.00" in text) == (case_id == "remind-evaluate-duplicate")
        assert (NOT_STARTED_TITLE in text) == expectation.not_started_block
        return
    assert ours[1] == expected_summary(expected[1], not_started=NOT_STARTED_TITLE in ours)
    if case_id == "remind-evaluate-runcode-nomatch":
        # R's capture.output() caught the auto-printed value of loopRuns(character(0)); the port prints none
        assert NO_RUNS_FOUND_PRINTED in expected and NO_RUNS_FOUND_PRINTED not in ours
        expected = [line for line in expected if line != NO_RUNS_FOUND_PRINTED]
    assert [line for line in ours if line != ours[1]] == [line for line in expected if line != expected[1]]
    delivery = "notification failed" if case_id == "remind-evaluate-curlfail" else "notification sent"
    assert delivery in played.stderr


@pytest.mark.sandbox
@pytest.mark.parametrize("case_id", sorted(EXPECTATIONS))
def test_remind_evaluate_case_end_to_end(case_id: str, sandbox_runner: Path, tmp_path: Path) -> None:
    expectation = EXPECTATIONS[case_id]
    played = run_case(sandbox_runner, tmp_path / case_id, case_id, ["run"])
    assert_r_files_untouched(played, runcode_rewritten=False)
    if expectation.outcome == REFUSED:
        assert_refused(case_id, played)
        return
    if expectation.outcome == ERROR:
        assert_failed(case_id, expectation, played)
        return
    assert played.statuses == [0], played.stderr
    if expectation.classification == KEPT:
        assert golden_result(case_id) == "ok"
    case = case_definition(case_id)
    summary = assert_readme(case_id, expectation, played)
    assert_subprocesses(case_id, expectation, played, publish=bool(case["email"]))
    assert_archive(case_id, expectation, played)
    assert_message(case_id, expectation, played, summary)
    assert_one_log_per_cycle(played, cycle_of(case_id), "evaluate")
    # the transition started -> idle (06 3.2), through the migration of the R files
    state = played.state
    assert state is not None and state.phase is Phase.IDLE and state.cycle is None
    assert state.last_completed is not None and state.last_completed.id == cycle_of(case_id)
    assert state.last_completed.tested_commit == TESTED_COMMIT
    assert state.last_completed.summary == summary
    assert state.last_completed.runcode == (
        NO_MATCH_RUNCODE
        if case_id == "remind-evaluate-runcode-nomatch"
        else ".*-AMT_2026-09-28|.*-AMT_2026-09-29"
    )
    assert state.last_completed.evaluated_at is not None
    assert state.last_completed.evaluated_at.isoformat() == "2026-09-30T12:00:00+02:00"
    assert played.stdout == f"evaluated AMT cycle {cycle_of(case_id)}: {summary}\n"
    if bool(case["email"]):
        assert "projects/remind/modeltests/testing_suite/README.md" in played.changed_paths("modified")


# ---------------------------------------------------------------------------------------------
# The start and toggle cases (06 4.4 items 3 and 4)
# ---------------------------------------------------------------------------------------------


def golden_subprocess_steps(case_id: str) -> list[tuple[str, list[str], str]]:
    """The R trace's subprocess records (``sed`` became ``replace_in_file``, ``mv``/``curl`` effects)."""
    return [
        (r["tool"], list(r["argv"]), r["cwd"]) for r in golden_trace(case_id) if r["tool"] in SUBPROCESS_TOOLS
    ]


def our_start_steps(played: SandboxRun) -> list[tuple[str, list[str], str]]:
    """The traced subprocesses of a start without the bridge's ``git log -1`` (the R start had none)."""
    return [
        (r["tool"], list(r["argv"]), r["cwd"])
        for r in played.trace
        if r["tool"] in SUBPROCESS_TOOLS and not (r["tool"] == "git" and r["argv"] == ["log", "-1"])
    ]


@pytest.mark.sandbox
@pytest.mark.parametrize("case_id", sorted(START_CASES))
def test_remind_start_and_toggle_cases_end_to_end(case_id: str, sandbox_runner: Path, tmp_path: Path) -> None:
    case = START_CASES[case_id]
    played = run_case(sandbox_runner, tmp_path / case_id, case_id, ["run"])
    assert played.statuses == [case.exit_status], played.stderr
    assert golden_result(case_id) == case.r_status
    state = played.state
    assert state is not None and state.phase is case.phase
    # runcode.rds is written at the checkpoint before the first submission: a start that fails at make has
    # rewritten it, one that fails at the first git reset has not (start.py, "Order")
    assert_r_files_untouched(played, runcode_rewritten=case_id in ("remind-start", "remind-start-make-fail"))
    if case_id == "remind-nothing":
        assert state.cycle is not None and state.cycle.id == MIGRATED_CYCLE
        assert state.last_error is not None and "'next:foo'" in state.last_error.message
        assert "cannot 'run'" in played.stderr and "Doing nothing" not in played.stderr
        assert played.trace == [] and played.posts == [] and played.logs == []
        return
    assert_one_log_per_cycle(played, NEW_CYCLE, "start")
    assert state.cycle is not None and state.cycle.id == NEW_CYCLE
    assert state.cycle.runcode == golden_runcode("remind-start")
    assert state.cycle.log == f"{CHECKOUT}/logs/amt-{NEW_CYCLE}.log"
    assert (
        state.cycle.started_at is not None
        and state.cycle.started_at.isoformat() == "2026-09-30T12:00:00+02:00"
    )
    for record in played.trace:
        if record["tool"] in ("Rscript", "make"):
            assert record["env"].get("autoRenvFixDeps") == "TRUE"
    if case_id == "remind-start":
        assert list(state.cycle.runs_to_start) == golden_runs_to_start()[0]
        assert state.cycle.started_commit == TESTED_COMMIT
        assert our_start_steps(played) == golden_subprocess_steps(case_id)
        effects = golden_effects(case_id)
        assert played.changed_paths("deleted") == [e["path"] for e in effects["deleted"]]
        touched = {e["path"]: e["sha256"] for e in played.clone_diff["touched"]}
        assert touched == {e["path"]: e["sha256"] for e in effects["touched"]}
        assert played.posts == []
        assert played.stdout == f"started AMT cycle {NEW_CYCLE} with 9 scenarios; log {state.cycle.log}\n"
        return
    # the two failure cases: R recorded status ok and went on; D-24: an infrastructure failure stops the start
    assert state.last_error is not None and state.last_error.command == "start"
    assert state.last_error.log == state.cycle.log
    text = failure_text(played)
    assert state.last_error.message in text and text.endswith(NEXT_STEPS)
    assert "the AMT state is now 'failed'" in played.stderr
    if case_id == "remind-start-git-fail":
        assert our_start_steps(played) == [("git", ["reset", "--hard", "origin/develop"], CHECKOUT)]
        assert state.cycle.runs_to_start == () and state.cycle.started_commit is None
        assert "git reset --hard origin/develop exited with status" in state.last_error.message
    else:
        assert our_start_steps(played) == golden_subprocess_steps(case_id)
        assert list(state.cycle.runs_to_start) == golden_runs_to_start()[0]
        assert "make test-full-slurm exited with status" in state.last_error.message


# ---------------------------------------------------------------------------------------------
# Round trips on one clone (06 3.2, 3.3)
# ---------------------------------------------------------------------------------------------


def json_documents(text: str) -> list[Any]:
    """Every JSON document concatenated in ``text`` (``status`` prints one per call)."""
    decoder = json.JSONDecoder()
    documents: list[Any] = []
    index = 0
    while index < len(text):
        while index < len(text) and text[index].isspace():
            index += 1
        if index >= len(text):
            break
        document, index = decoder.raw_decode(text, index)
        documents.append(document)
    return documents


@pytest.mark.sandbox
def test_start_then_evaluate_then_status_on_one_clone(sandbox_runner: Path, tmp_path: Path) -> None:
    """``start``, ``evaluate`` and ``status`` of one cycle write one log file and end idle."""
    played = run_case(
        sandbox_runner, tmp_path / "round-trip", "remind-start", ["start"], ["evaluate"], ["status"]
    )
    assert played.statuses == [0, 0, 0], played.stderr
    assert_one_log_per_cycle(played, NEW_CYCLE, "start", "evaluate")
    state = played.state
    assert state is not None and state.phase is Phase.IDLE and state.cycle is None
    assert state.last_completed is not None and state.last_completed.id == NEW_CYCLE
    assert state.last_completed.tested_commit == TESTED_COMMIT
    assert state.last_completed.runcode == golden_runcode("remind-start")
    readme = played.readme(NEW_CYCLE)
    assert readme is not None
    summary = summary_line([SCENARIOS_NOT_STARTED, NO_RUNS_STARTED])
    assert readme[-2] == summary and state.last_completed.summary == summary
    assert readme[readme.index(NOT_STARTED_TITLE) + 1] == "default-AMT"
    assert played.calls("sbatch") == [] and played.calls("rsync") == []
    # status: the state document on stdout, the whereabouts on stderr
    lines = played.stdout.splitlines()
    assert lines[0].startswith(f"started AMT cycle {NEW_CYCLE} with 9 scenarios")
    assert lines[1] == f"evaluated AMT cycle {NEW_CYCLE}: {summary}"
    (document,) = json_documents("\n".join(lines[2:]))
    assert AmtState.from_dict(document) == state
    assert f"state file: {CHECKOUT}/amt-state.json" in played.stderr
    assert "the AMT state is 'idle' (last completed cycle 2026-09-30)" in played.stderr
    # the R files: runcode.rds rewritten by start, the others only read
    assert_r_files_untouched(played, runcode_rewritten=True)


@pytest.mark.sandbox
def test_run_twice_starts_then_evaluates(sandbox_runner: Path, tmp_path: Path) -> None:
    """``run`` is the cron entry point: idle starts, started evaluates (``06`` 3.1)."""
    played = run_case(sandbox_runner, tmp_path / "run-twice", "remind-start", ["run"], ["run"])
    assert played.statuses == [0, 0], played.stderr
    assert_one_log_per_cycle(played, NEW_CYCLE, "start", "evaluate")
    messages = [text for _, _, text in played.log_lines(played.logs[0]) if text.startswith("'run' found")]
    assert messages == [
        "'run' found the AMT state is 'idle': running 'start'",
        f"'run' found a cycle is 'started' (cycle {NEW_CYCLE}, started 2026-09-30T12:00:00+02:00): "
        "running 'evaluate'",
    ]
    state = played.state
    assert state is not None and state.phase is Phase.IDLE and state.last_completed is not None
    assert state.last_completed.id == NEW_CYCLE


@pytest.mark.sandbox
def test_status_reset_status_on_a_failed_migration(sandbox_runner: Path, tmp_path: Path) -> None:
    """``remind-nothing``: ``run`` is refused, ``status`` shows the failed migration, ``reset`` clears it."""
    played = run_case(
        sandbox_runner,
        tmp_path / "reset",
        "remind-nothing",
        ["run"],
        ["status"],
        ["reset", "--yes"],
        ["status"],
    )
    assert played.statuses == [2, 0, 0, 0], played.stderr
    reset_line = f"the AMT state is now 'idle' ({CHECKOUT}/amt-state.json)"
    documents = json_documents(played.stdout.replace(reset_line, ""))
    before, after = (AmtState.from_dict(document) for document in documents)
    assert before.phase is Phase.FAILED and before.cycle is not None and before.cycle.id == MIGRATED_CYCLE
    assert before.last_error is not None and "'next:foo'" in before.last_error.message
    assert after.phase is Phase.IDLE and after.cycle is None and after.last_error == before.last_error
    assert played.state == after
    assert reset_line in played.stdout
    assert played.stderr.count("the AMT state is 'failed'") >= 2  # the refusal of run and the first status
    assert played.trace == [] and played.posts == [] and played.logs == []


@pytest.mark.sandbox
def test_second_start_is_refused_until_force(sandbox_runner: Path, tmp_path: Path) -> None:
    """A second ``start`` is refused with status 2 and nothing changes; ``--force`` abandons the cycle."""
    played = run_case(
        sandbox_runner, tmp_path / "force", "remind-start", ["start"], ["start"], ["--force", "start"]
    )
    assert played.statuses == [0, 2, 0], played.stderr
    assert "cannot 'start': a cycle is 'started'" in played.stderr
    assert "'modeltests start --force' to abandon the cycle" in played.stderr
    state = played.state
    assert state is not None and state.phase is Phase.STARTED and state.cycle is not None
    assert state.last_error is not None and state.last_error.command == "start"
    assert "was abandoned by 'start --force'" in state.last_error.message
    # the refused start ran nothing: two starts' worth of subprocesses, not three
    assert len([r for r in played.trace if r["tool"] == "make"]) == 2
    assert_one_log_per_cycle(played, NEW_CYCLE, "start")


@pytest.mark.sandbox
def test_dry_run_writes_nothing_into_the_clone(sandbox_runner: Path, tmp_path: Path) -> None:
    """``--dry-run run``: every mutation is announced, no file is written, no program runs (``06`` 3.1)."""
    played = run_case(sandbox_runner, tmp_path / "dry-run", "remind-start", ["--dry-run", "run"])
    assert played.statuses == [0], played.stderr
    assert {kind: played.changed_paths(kind) for kind in ("created", "modified", "deleted", "touched")} == {
        "created": [],
        "modified": [],
        "deleted": [],
        "touched": [],
    }
    assert played.state is None and played.logs == []
    assert "dry run: would write /p/projects/remind/modeltests/remind/amt-state.json" in played.stderr
    assert "dry run: would run make test-full-slurm" in played.stderr
    assert (
        "Starting AMT cycle 2026-09-30 in /p/projects/remind/modeltests/remind (run pattern" in played.stderr
    )
    assert ", dry run)" in played.stderr
    # no program runs, the selectScenarios() bridge included (it would execute the checkout's R code):
    # it is announced and answered with an empty selection; the fake start.R, git and make are not run
    assert "dry run: would run Rscript" in played.stderr and "select_scenarios.R AMT in" in played.stderr
    assert 'selectScenarios(startgroup = "AMT") selected 0 scenarios: (none)' in played.stderr
    tools = [r["tool"] for r in played.trace]
    assert "make" not in tools and "Rscript" not in tools and "git" not in tools
    assert played.posts == []


# ---------------------------------------------------------------------------------------------
# In process (the default tier): the command frame of modelstats.amt.cli over RecordingEffects
# ---------------------------------------------------------------------------------------------


def in_process(
    tmp_path: Path,
    *argv: str,
    state: AmtState | None = None,
    answer: str | None = None,
    slurm: CannedSlurm | None = None,
) -> tuple[int, str, str, StateStore]:
    """``main(argv)`` on a fresh checkout below ``tmp_path``; ``answer`` feeds the ``reset`` prompt."""
    checkout = tmp_path / "modeltests" / "remind"
    checkout.mkdir(parents=True, exist_ok=True)
    effects = RecordingEffects(now=NOW, user="pascalfu", on_cluster=False, cwd=tmp_path, slurm=slurm)
    store = StateStore.at(effects, checkout)
    if state is not None:
        store.save(state)
    out, err = io.StringIO(), io.StringIO()
    status = main(
        ["--dir", str(checkout), "--no-publish", *argv],
        effects=effects,
        out=out,
        err=err,
        read_line=lambda: answer,
    )
    return status, out.getvalue(), err.getvalue(), store


def failed_state(tmp_path: Path) -> AmtState:
    """A ``failed`` evaluate of a cycle whose log lives below the checkout of :func:`in_process`."""
    log = str(tmp_path / "modeltests" / "remind" / "logs" / "amt-2026-09-23.log")
    return AmtState(
        phase=Phase.FAILED,
        cycle=Cycle(id="2026-09-23", runcode=".*-AMT_2026-09-23|.*-AMT_2026-09-24", log=log),
        last_error=LastError(command="evaluate", message="boom", at=NOW, log=log),
    )


def test_help_and_usage_errors_follow_click(capsys: pytest.CaptureFixture[str]) -> None:
    assert main(["--help"]) == 0
    assert capsys.readouterr().out.startswith("Usage: modeltests [OPTIONS] COMMAND [ARGS]...")
    assert main([]) == USAGE_EXIT_STATUS
    assert "Usage: modeltests [OPTIONS] COMMAND [ARGS]..." in capsys.readouterr().err
    assert main(["bogus"]) == USAGE_EXIT_STATUS
    assert "No such command 'bogus'" in capsys.readouterr().err
    assert main(["start", "--bogus"]) == USAGE_EXIT_STATUS
    assert "No such option: --bogus" in capsys.readouterr().err


def test_status_of_a_fresh_checkout_is_idle_and_writes_nothing(tmp_path: Path) -> None:
    status, out, err, store = in_process(tmp_path, "status")
    assert status == 0
    assert AmtState.from_json(out) == AmtState()
    assert err.splitlines() == ["the AMT state is 'idle'", f"state file: {store.path}"]
    assert not store.exists()


def test_a_missing_checkout_is_refused(tmp_path: Path) -> None:
    out, err = io.StringIO(), io.StringIO()
    status = main(
        ["--dir", str(tmp_path / "nowhere"), "status"],
        effects=RecordingEffects(now=NOW, user="u", on_cluster=False, cwd=tmp_path),
        out=out,
        err=err,
    )
    assert status == REFUSED_EXIT_STATUS and out.getvalue() == ""
    assert (
        err.getvalue()
        == f"modeltests: --dir {tmp_path / 'nowhere'} is not a directory (the REMIND checkout)\n"
    )


def test_run_and_start_are_refused_while_failed_and_nothing_changes(tmp_path: Path) -> None:
    for command in ("run", "start"):
        status, out, err, store = in_process(tmp_path, command, state=failed_state(tmp_path))
        assert status == REFUSED_EXIT_STATUS and out == "", command
        assert f"modeltests: cannot '{command}': the AMT state is 'failed'" in err
        assert store.load() == failed_state(tmp_path)
    four_failures = CannedSlurm({"squeue_sequence": "1\n1\n1\n1\n"})
    status, out, err, store = in_process(
        tmp_path, "evaluate", state=failed_state(tmp_path), slurm=four_failures
    )
    # evaluate retries a failed cycle; here squeue fails four times (R's abort) and the crash is recorded
    assert status == CRASHED_EXIT_STATUS
    saved = store.load()
    assert saved is not None and saved.phase is Phase.FAILED and saved.cycle == failed_state(tmp_path).cycle
    assert saved.last_error is not None and saved.last_error.command == "evaluate"
    assert "'evaluate' failed:" in err and "the AMT state is now 'failed'" in err
    assert (tmp_path / "modeltests" / "remind" / "logs" / "amt-2026-09-23.log").is_file()


def test_reset_asks_and_declines_without_a_yes(tmp_path: Path) -> None:
    status, out, err, store = in_process(tmp_path, "reset", state=failed_state(tmp_path), answer="n")
    assert status == CANCELLED_EXIT_STATUS and out == ""
    assert "Clear the 'failed' state and drop the recorded cycle? [y/N] " in err
    assert "reset cancelled; the state is unchanged" in err
    assert store.load() == failed_state(tmp_path)
    status, out, err, store = in_process(tmp_path, "reset", state=failed_state(tmp_path), answer=None)  # EOF
    assert status == CANCELLED_EXIT_STATUS and store.load() == failed_state(tmp_path)


def test_reset_clears_a_failed_state_on_yes_or_flag(tmp_path: Path) -> None:
    for argv, answer in ((["reset"], "Y"), (["reset", "--yes"], None), (["reset", "-y"], "n")):
        status, out, err, store = in_process(tmp_path, *argv, state=failed_state(tmp_path), answer=answer)
        assert status == 0, (argv, err)
        assert out == f"the AMT state is now 'idle' ({store.path})\n"
        saved = store.load()
        assert saved is not None and saved.phase is Phase.IDLE and saved.cycle is None
        assert saved.last_error == failed_state(tmp_path).last_error
        log = tmp_path / "modeltests" / "remind" / "logs" / "amt-2026-09-23.log"
        assert "the AMT state is now 'idle'" in log.read_text(encoding="utf-8")
    status, _, err, store = in_process(tmp_path, "reset", "--yes")
    assert status == REFUSED_EXIT_STATUS and "cannot 'reset': the AMT state is 'idle'" in err


def test_evaluate_without_a_report_repository_is_refused_before_any_effect(tmp_path: Path) -> None:
    started = AmtState(phase=Phase.STARTED, cycle=failed_state(tmp_path).cycle)
    checkout = tmp_path / "modeltests" / "remind"
    checkout.mkdir(parents=True)
    effects = RecordingEffects(now=NOW, user="pascalfu", on_cluster=False, cwd=tmp_path)
    StateStore.at(effects, checkout).save(started)
    out, err = io.StringIO(), io.StringIO()
    status = main(["--dir", str(checkout), "evaluate"], effects=effects, out=out, err=err)
    assert status == REFUSED_EXIT_STATUS and out.getvalue() == ""
    assert "--report-repo is required to publish README.md (or pass --no-publish)" in err.getvalue()
    assert StateStore.at(effects, checkout).load() == started
    assert effects.runs == [] and effects.posts == []


def test_no_wait_fails_fast_when_the_lock_is_held(tmp_path: Path) -> None:
    checkout = tmp_path / "modeltests" / "remind"
    checkout.mkdir(parents=True)
    with held_by_another_process(checkout / "amt-state.lock") as release:
        status, out, err, _ = in_process(tmp_path, "--no-wait", "status")
        release()
    assert status == REFUSED_EXIT_STATUS and out == ""
    assert "another modeltests command holds" in err and "not waiting (--no-wait)" in err


def test_dry_run_status_reads_but_writes_nothing(tmp_path: Path) -> None:
    checkout = tmp_path / "modeltests" / "remind"
    checkout.mkdir(parents=True)
    (tmp_path / "modeltests" / ".testsstatus").write_text("next:start\n")
    status, out, err, store = in_process(tmp_path, "--dry-run", "status")
    assert status == 0 and AmtState.from_json(out) == AmtState()
    assert "dry run: would write" in err and "deriving the state from" in err
    assert not store.exists() and not (checkout / "amt-state.lock").exists()
