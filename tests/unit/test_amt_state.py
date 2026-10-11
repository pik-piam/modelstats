"""``modelstats.amt.state``: the document, every transition of ``06`` 3.2, the store, the lock, the migration.

The transition table is written out literally from ``migration/06-user-requests.md`` section 3.2 and every
(phase, command, --force) combination is checked against it, the refused ones included. The migration tests
write the R files with ``rdata_io.write_rds`` (what ``saveRDS`` produces) for each ``.testsstatus`` value and
assert that the R files keep their bytes; four of them rebuild the starting state of the R cases
``remind-nothing``, ``remind-evaluate-state-missing``, ``remind-evaluate-state-corrupt`` and
``remind-evaluate-runstostart-missing`` from the goldens' ``state/*.json`` records (``06`` 4.4 item 4), and
one reads the canonical fixture checkout through the overlay when the fixture snapshot is present. The store
tests run ``FakeEffects`` and ``RecordingEffects`` on a ``tmp_path`` checkout and assert the calls that reach
the effects (``write_text``, ``write_bytes``, ``lock``), the atomic write (an interrupted rename leaves the
old state), the lock contention with a real ``flock`` holder, and that ``DryRunEffects`` writes nothing.
"""

from __future__ import annotations

import fcntl
import hashlib
import json
import os
import threading
import time
from collections.abc import Mapping
from dataclasses import dataclass
from datetime import UTC, date, datetime, timedelta
from pathlib import Path, PurePosixPath
from typing import Any
from zoneinfo import ZoneInfo

import pandas as pd
import pytest

from modelstats.amt.state import (
    AMT_STATE_FILE,
    LOCK_FILE,
    R_ERROR_SENTENCE,
    R_NEXT_EVALUATE,
    R_NEXT_START,
    RUNCODE_FILE,
    SCHEMA,
    AmtLockError,
    AmtState,
    AmtStateError,
    Command,
    Completed,
    Cycle,
    LastError,
    Phase,
    StateStore,
    complete,
    cycle_id,
    describe,
    fail,
    migrate_r_state,
    next_command,
    rds_bytes,
    reset,
    runcode_pattern,
    start,
    target_phase,
)
from modelstats.env import (
    DryRunEffects,
    EffectCall,
    Effects,
    FakeEffects,
    LockHeldError,
    Mode,
    OverlayPaths,
    RecordingEffects,
)
from modelstats.rdata_io import read_rds, scalar, write_rds
from modelstats.sanity import amt_runcode
from tests.conftest import FIXTURES, GOLDENS, SYNTHETIC
from tests.unit.test_rdata_io import r_identical, requires_r

BERLIN = ZoneInfo("Europe/Berlin")
NOW = datetime(2026, 9, 30, 12, 0, 0, tzinfo=BERLIN)
TODAY = NOW.date()
RUNCODE = ".*-AMT_2026-09-30|.*-AMT_2026-10-01"
R_RUNCODE = ".*-AMT_2026-09-28|.*-AMT_2026-09-29"
R_COMMIT = "13f60fdd4fff366bebcc7dfe522897dedeb43cc7"
R_RUNS = ("SSP2-NPi2025-calibrate-AMT", "SSP2-NPi-AMT", "testOneRegi-AMT")
FIXTURE_CHECKOUT = PurePosixPath("/fixtures/p/projects/remind/modeltests/remind")

CYCLE = Cycle(
    id="2026-09-30",
    runcode=RUNCODE,
    started_at=NOW,
    started_commit="abc1234abc1234abc1234abc1234abc1234abc12",
    runs_to_start=("testOneRegi-AMT", "SSP2-NPi-AMT"),
    log="logs/amt-2026-09-30.log",
)
OLD_CYCLE = Cycle(
    id="2026-09-23",
    runcode=".*-AMT_2026-09-23|.*-AMT_2026-09-24",
    started_at=NOW - timedelta(days=7),
    started_commit="0ld0ld0",
    runs_to_start=("SSP2-NPi-AMT",),
    log="logs/amt-2026-09-23.log",
)
COMPLETED = Completed(
    id="2026-09-16",
    evaluated_at=NOW - timedelta(days=13),
    tested_commit="c0mpl3t3",
    summary="Summary: AMT runs look good.",
    runcode=".*-AMT_2026-09-16|.*-AMT_2026-09-17",
)
ERROR = LastError(
    command="evaluate",
    message="squeue had exit status > 0 more than 3 times in a row.",
    at=NOW,
    log=CYCLE.log,
)

IDLE = AmtState(phase=Phase.IDLE, last_completed=COMPLETED)
STARTED = AmtState(phase=Phase.STARTED, cycle=CYCLE, last_completed=COMPLETED)
FAILED = AmtState(phase=Phase.FAILED, cycle=CYCLE, last_completed=COMPLETED, last_error=ERROR)
STATES = {Phase.IDLE: IDLE, Phase.STARTED: STARTED, Phase.FAILED: FAILED}

# The table of 06 section 3.2, row by row: (from, command, needs --force) -> to. Nothing else is a move.
TABLE_06_3_2 = {
    (Phase.IDLE, Command.START, False): Phase.STARTED,
    (Phase.STARTED, Command.EVALUATE, False): Phase.IDLE,
    (Phase.STARTED, Command.START, True): Phase.STARTED,
    (Phase.FAILED, Command.EVALUATE, False): Phase.IDLE,
    (Phase.FAILED, Command.RESET, False): Phase.IDLE,
}
MOVES = (Command.START, Command.EVALUATE, Command.RESET)


def expected_target(phase: Phase, command: Command, force: bool) -> Phase | None:
    """What the table allows for the combination; ``None`` for a refusal."""
    plain = TABLE_06_3_2.get((phase, command, False))
    if plain is not None:
        return plain
    return TABLE_06_3_2.get((phase, command, True)) if force else None


# ---------------------------------------------------------------------------------------------
# Builders
# ---------------------------------------------------------------------------------------------


def checkout(tmp_path: Path) -> Path:
    """``<tmp>/modeltests/remind``: the R ``mydir`` with its parent for ``../.testsstatus``."""
    directory = tmp_path / "modeltests" / "remind"
    directory.mkdir(parents=True)
    return directory


def fake(tmp_path: Path, now: datetime = NOW) -> FakeEffects:
    """``FakeEffects`` on the real filesystem below ``tmp_path`` (no overlay), frozen clock."""
    return FakeEffects(now=now, user="pascalfu", on_cluster=False, cwd=tmp_path)


def store_at(tmp_path: Path) -> tuple[StateStore, Path]:
    directory = checkout(tmp_path)
    return StateStore.at(fake(tmp_path), directory), directory


def write_r_files(
    directory: Path,
    *,
    tests_status: str | None,
    runcode: str | None = R_RUNCODE,
    commit: str | None = R_COMMIT,
    runs: tuple[str, ...] | None = R_RUNS,
) -> None:
    """The R state as ``saveRDS``/``writeLines`` leave it; ``None`` leaves a file out."""
    if tests_status is not None:
        (directory.parent / ".testsstatus").write_text(tests_status + "\n")
    if runcode is not None:
        write_rds(directory / "runcode.rds", runcode)
    if commit is not None:
        write_rds(directory / "lastcommit.rds", commit)
    if runs is not None:
        frame = pd.DataFrame(
            {"start": ["AMT"] * len(runs), "slurmConfig": [14] * len(runs)}, index=list(runs)
        )
        write_rds(directory / "runsToStart.rds", frame)


def digests(directory: Path) -> dict[str, str]:
    """sha256 of every file below ``directory``'s parent (the R files and ``.testsstatus``)."""
    root = directory.parent
    return {
        str(path.relative_to(root)): hashlib.sha256(path.read_bytes()).hexdigest()
        for path in sorted(root.rglob("*"))
        if path.is_file()
    }


def fake_calls(effects: Effects) -> list[EffectCall]:
    """The call log of the store's ``FakeEffects``."""
    assert isinstance(effects, FakeEffects)
    return effects.calls


def calls(effects: Effects, name: str) -> list[str]:
    return [call.args[0] for call in fake_calls(effects) if call.name == name]


# ---------------------------------------------------------------------------------------------
# The R pattern and the cycle id (R/modeltests.R:92-96)
# ---------------------------------------------------------------------------------------------


def test_runcode_pattern_is_today_or_tomorrow_as_r_pastes_it() -> None:
    assert runcode_pattern(date(2026, 9, 30)) == RUNCODE
    assert runcode_pattern(date(2026, 12, 31)) == ".*-AMT_2026-12-31|.*-AMT_2027-01-01"
    assert runcode_pattern(date(2028, 2, 28)) == ".*-AMT_2028-02-28|.*-AMT_2028-02-29"


def test_runcode_pattern_equals_the_r_golden_of_remind_start() -> None:
    """``saveRDS(runcode)`` of the R run frozen at 2026-09-30 (``goldens/amt/remind-start``)."""
    golden = GOLDENS / "amt" / "remind-start" / "state" / "runcode_rds.json"
    recorded = json.loads(golden.read_text())
    assert recorded["value"] == [runcode_pattern(TODAY)]


def test_cycle_id_is_the_start_date() -> None:
    assert cycle_id(TODAY) == "2026-09-30"


# ---------------------------------------------------------------------------------------------
# The document
# ---------------------------------------------------------------------------------------------


def test_json_document_has_the_shape_of_06_3_2() -> None:
    document = FAILED.to_dict()
    assert list(document) == ["schema", "phase", "cycle", "last_completed", "last_error"]
    assert document["schema"] == SCHEMA == 1
    assert document["phase"] == "failed"
    assert document["cycle"] == {
        "id": "2026-09-30",
        "runcode": RUNCODE,
        "started_at": "2026-09-30T12:00:00+02:00",
        "started_commit": CYCLE.started_commit,
        "runs_to_start": ["testOneRegi-AMT", "SSP2-NPi-AMT"],
        "log": "logs/amt-2026-09-30.log",
    }
    assert document["last_completed"] == {
        "id": "2026-09-16",
        "evaluated_at": "2026-09-17T12:00:00+02:00",
        "tested_commit": "c0mpl3t3",
        "summary": "Summary: AMT runs look good.",
        "runcode": COMPLETED.runcode,
    }
    assert document["last_error"] == {
        "command": "evaluate",
        "at": "2026-09-30T12:00:00+02:00",
        "message": ERROR.message,
        "log": "logs/amt-2026-09-30.log",
    }


@pytest.mark.parametrize(
    "state", [AmtState(), IDLE, STARTED, FAILED], ids=["fresh", "idle", "started", "failed"]
)
def test_json_round_trip(state: AmtState) -> None:
    text = state.to_json()
    assert text.endswith("}\n")
    assert AmtState.from_json(text) == state
    assert AmtState.from_json(text).to_json() == text


def test_fresh_state_is_idle_with_null_blocks() -> None:
    assert json.loads(AmtState().to_json()) == {
        "schema": 1,
        "phase": "idle",
        "cycle": None,
        "last_completed": None,
        "last_error": None,
    }


def test_timestamps_keep_their_offset() -> None:
    cycle = Cycle("x", RUNCODE, started_at=datetime(2026, 9, 30, 10, tzinfo=UTC))
    parsed = AmtState.from_json(AmtState(phase=Phase.STARTED, cycle=cycle).to_json())
    assert parsed.cycle is not None and parsed.cycle.started_at == cycle.started_at
    assert parsed.cycle.started_at is not None and parsed.cycle.started_at.utcoffset() == timedelta(0)


@pytest.mark.parametrize(
    ("text", "fragment"),
    [
        ("{not json", "not valid JSON"),
        ("[]", "must be a JSON object"),
        ('{"schema": 2, "phase": "idle"}', "unsupported schema 2"),
        ('{"schema": 1, "phase": "running"}', "'phase' must be one of idle, started, failed"),
        ('{"schema": 1, "phase": "started", "cycle": "x"}', "'cycle' must be a JSON object"),
        ('{"schema": 1, "phase": "started", "cycle": {"runcode": "a"}}', "'id' must be a string"),
        (
            '{"schema": 1, "phase": "started", "cycle": {"id": "a", "runcode": "b", "started_at": "then"}}',
            "not an ISO 8601",
        ),
        (
            '{"schema": 1, "phase": "started", "cycle": {"id": "a", "runcode": "b", "runs_to_start": "x"}}',
            "list of strings",
        ),
        (
            '{"schema": 1, "phase": "failed", "last_error": {"command": "evaluate"}}',
            "'message' must be a string",
        ),
    ],
)
def test_damaged_documents_are_state_errors_naming_the_file(text: str, fragment: str) -> None:
    with pytest.raises(AmtStateError, match=fragment) as info:
        AmtState.from_json(text, source="/amt/amt-state.json")
    assert str(info.value).startswith("/amt/amt-state.json: ")
    assert info.value.exit_status == 2


# ---------------------------------------------------------------------------------------------
# The transition table of 06 section 3.2: every combination
# ---------------------------------------------------------------------------------------------


@pytest.mark.parametrize("force", [False, True], ids=["plain", "force"])
@pytest.mark.parametrize("command", MOVES, ids=[c.value for c in MOVES])
@pytest.mark.parametrize("phase", list(Phase), ids=[p.value for p in Phase])
def test_target_phase_follows_the_table(phase: Phase, command: Command, force: bool) -> None:
    state = STATES[phase]
    expected = expected_target(phase, command, force)
    if expected is None:
        with pytest.raises(AmtStateError, match=f"cannot '{command.value}'") as info:
            target_phase(state, command, force=force)
        assert info.value.exit_status == 2
    else:
        assert target_phase(state, command, force=force) is expected


def test_the_table_has_exactly_the_five_rows_of_06() -> None:
    allowed = {
        (phase, command, force)
        for phase in Phase
        for command in MOVES
        for force in (False, True)
        if expected_target(phase, command, force) is not None
    }
    assert allowed == {
        (Phase.IDLE, Command.START, False),
        (Phase.IDLE, Command.START, True),
        (Phase.STARTED, Command.EVALUATE, False),
        (Phase.STARTED, Command.EVALUATE, True),
        (Phase.STARTED, Command.START, True),
        (Phase.FAILED, Command.EVALUATE, False),
        (Phase.FAILED, Command.EVALUATE, True),
        (Phase.FAILED, Command.RESET, False),
        (Phase.FAILED, Command.RESET, True),
    }


def test_idle_start_fills_the_cycle() -> None:
    after = start(IDLE, CYCLE)
    assert after == AmtState(phase=Phase.STARTED, cycle=CYCLE, last_completed=COMPLETED)
    assert start(IDLE, CYCLE, force=True) == after  # --force is a no-op when idle


def test_started_start_without_force_is_refused_with_the_hint() -> None:
    with pytest.raises(AmtStateError) as info:
        start(STARTED, CYCLE)
    assert str(info.value) == (
        "cannot 'start': a cycle is 'started' (cycle 2026-09-30, started 2026-09-30T12:00:00+02:00); "
        "run 'modeltests evaluate', or 'modeltests start --force' to abandon the cycle"
    )


def test_started_start_force_records_the_abandoned_cycle() -> None:
    old = AmtState(phase=Phase.STARTED, cycle=OLD_CYCLE, last_completed=COMPLETED)
    after = start(old, CYCLE, force=True)
    assert after.phase is Phase.STARTED
    assert after.cycle == CYCLE
    assert after.last_completed == COMPLETED
    assert after.last_error == LastError(
        command="start",
        message="cycle 2026-09-23 started 2026-09-23T12:00:00+02:00 was abandoned by 'start --force'"
        " before it was evaluated",
        at=CYCLE.started_at,
        log="logs/amt-2026-09-23.log",
    )


def test_started_evaluate_completes_the_cycle() -> None:
    after = complete(
        STARTED, evaluated_at=NOW, tested_commit="t3st3d", summary="Summary: AMT runs look good."
    )
    assert after.phase is Phase.IDLE
    assert after.cycle is None
    assert after.last_completed == Completed(
        id="2026-09-30",
        evaluated_at=NOW,
        tested_commit="t3st3d",
        summary="Summary: AMT runs look good.",
        runcode=RUNCODE,
    )
    assert after.last_error is None


def test_failed_evaluate_retries_and_keeps_the_error_as_history() -> None:
    after = complete(FAILED, evaluated_at=NOW, tested_commit="t3st3d", summary="Summary: AMT runs look good.")
    assert after.phase is Phase.IDLE
    assert after.cycle is None
    assert after.last_completed is not None and after.last_completed.runcode == RUNCODE
    assert after.last_error == ERROR


def test_failed_evaluate_without_a_cycle_points_at_reset() -> None:
    orphan = AmtState(phase=Phase.FAILED, last_error=ERROR)
    with pytest.raises(AmtStateError, match="no cycle is recorded to retry, run 'modeltests reset'"):
        complete(orphan, evaluated_at=NOW, tested_commit=None, summary=None)


def test_idle_evaluate_and_idle_reset_are_refused() -> None:
    with pytest.raises(
        AmtStateError, match="cannot 'evaluate': the AMT state is 'idle' .*run 'modeltests start' first"
    ):
        complete(IDLE, evaluated_at=NOW, tested_commit=None, summary=None)
    with pytest.raises(
        AmtStateError, match="cannot 'reset': the AMT state is 'idle' \\(last completed cycle 2026-09-16\\)"
    ):
        reset(IDLE)


def test_started_reset_is_refused() -> None:
    with pytest.raises(AmtStateError, match="cannot 'reset': a cycle is 'started'.*start --force"):
        reset(STARTED)


def test_failed_start_is_refused_even_with_force() -> None:
    expected = (
        "cannot 'start': the AMT state is 'failed': 'evaluate' failed at 2026-09-30T12:00:00+02:00: "
        f"{ERROR.message}, log logs/amt-2026-09-30.log; a human decides: 'modeltests evaluate' retries the "
        "cycle, 'modeltests reset' clears the state"
    )
    for force in (False, True):
        with pytest.raises(AmtStateError) as info:
            start(FAILED, CYCLE, force=force)
        assert str(info.value) == expected


def test_failed_reset_drops_the_cycle_and_keeps_the_history() -> None:
    after = reset(FAILED)
    assert after == AmtState(phase=Phase.IDLE, cycle=None, last_completed=COMPLETED, last_error=ERROR)


def test_fail_from_idle_keeps_the_cycle_start_had_built() -> None:
    crashed = fail(IDLE, ERROR, cycle=CYCLE)
    assert crashed == AmtState(phase=Phase.FAILED, cycle=CYCLE, last_completed=COMPLETED, last_error=ERROR)


def test_fail_from_started_keeps_the_running_cycle() -> None:
    crashed = fail(STARTED, ERROR)
    assert crashed.phase is Phase.FAILED
    assert crashed.cycle == CYCLE
    assert crashed.last_error == ERROR


def test_fail_from_failed_replaces_the_error() -> None:
    again = LastError(command="evaluate", message="later", at=NOW + timedelta(hours=1))
    assert fail(FAILED, again).last_error == again
    assert fail(FAILED, again).cycle == CYCLE


def test_run_maps_the_phases_of_06() -> None:
    assert next_command(IDLE) is Command.START
    assert next_command(STARTED) is Command.EVALUATE
    with pytest.raises(
        AmtStateError, match="cannot 'run': the AMT state is 'failed'.*a human decides"
    ) as info:
        next_command(FAILED)
    assert info.value.exit_status == 2


def test_describe_each_phase() -> None:
    assert describe(AmtState()) == "the AMT state is 'idle'"
    assert describe(IDLE) == "the AMT state is 'idle' (last completed cycle 2026-09-16)"
    assert describe(STARTED) == "a cycle is 'started' (cycle 2026-09-30, started 2026-09-30T12:00:00+02:00)"
    migrated = AmtState(phase=Phase.STARTED, cycle=Cycle("2026-09-28", R_RUNCODE))
    assert describe(migrated) == "a cycle is 'started' (cycle 2026-09-28, started an unknown time)"
    assert describe(FAILED) == (
        "the AMT state is 'failed': 'evaluate' failed at 2026-09-30T12:00:00+02:00: "
        f"{ERROR.message}, log logs/amt-2026-09-30.log"
    )


# ---------------------------------------------------------------------------------------------
# runcode.rds bytes (06 section 3.4)
# ---------------------------------------------------------------------------------------------


def test_rds_bytes_read_back_as_the_string_and_are_deterministic(tmp_path: Path) -> None:
    data = rds_bytes(RUNCODE)
    assert data == rds_bytes(RUNCODE)
    assert data[:2] == b"\x1f\x8b"  # gzip, as saveRDS writes by default
    path = tmp_path / "runcode.rds"
    path.write_bytes(data)
    assert scalar(read_rds(path)) == RUNCODE


def test_rds_bytes_equal_write_rds_payload(tmp_path: Path) -> None:
    """Same serialisation as ``rdata_io.write_rds`` (only the gzip header's timestamp can differ)."""
    import gzip

    path = tmp_path / "runcode.rds"
    write_rds(path, RUNCODE)
    assert gzip.decompress(path.read_bytes()) == gzip.decompress(rds_bytes(RUNCODE))


@requires_r
def test_rds_bytes_are_identical_in_r(tmp_path: Path) -> None:
    path = tmp_path / "runcode.rds"
    path.write_bytes(rds_bytes(RUNCODE))
    assert r_identical(path, f'"{RUNCODE}"')


# ---------------------------------------------------------------------------------------------
# The store: load, save, runcode.rds, atomic write, lock
# ---------------------------------------------------------------------------------------------


def test_store_paths(tmp_path: Path) -> None:
    store, directory = store_at(tmp_path)
    assert store.path == PurePosixPath(directory) / AMT_STATE_FILE
    assert store.lock_path == PurePosixPath(directory) / LOCK_FILE
    assert store.runcode_path == PurePosixPath(directory) / RUNCODE_FILE
    assert StateStore.at(store.effects, str(directory) + "/").directory == PurePosixPath(directory)


def test_load_is_none_without_a_file_and_round_trips_a_saved_state(tmp_path: Path) -> None:
    store, directory = store_at(tmp_path)
    assert store.load() is None
    assert not store.exists()
    store.save(STARTED)
    assert (directory / AMT_STATE_FILE).read_text() == STARTED.to_json()
    assert store.load() == STARTED
    assert calls(store.effects, "write_text") == [str(store.path)]


def test_load_names_the_file_in_its_error(tmp_path: Path) -> None:
    store, directory = store_at(tmp_path)
    (directory / AMT_STATE_FILE).write_text("{}")
    with pytest.raises(AmtStateError, match=f"{directory}/{AMT_STATE_FILE}: unsupported schema None"):
        store.load()


def test_save_started_writes_the_json_then_runcode_rds(tmp_path: Path) -> None:
    store, directory = store_at(tmp_path)
    store.save_started(STARTED)
    assert store.load() == STARTED
    assert scalar(read_rds(directory / RUNCODE_FILE)) == RUNCODE
    mutations = [
        (c.name, c.args[0]) for c in fake_calls(store.effects) if c.name in {"write_text", "write_bytes"}
    ]
    assert mutations == [("write_text", str(store.path)), ("write_bytes", str(store.runcode_path))]


def test_save_started_feeds_rs_a_through_the_state_file(tmp_path: Path) -> None:
    """D-26: ``rs -A`` reads ``cycle.runcode`` from the JSON before ``runcode.rds``."""
    from modelstats.env import ClusterPaths

    directory = checkout(tmp_path)
    effects = FakeEffects(
        now=NOW,
        user="pascalfu",
        on_cluster=False,
        cwd=tmp_path,
        cluster_paths=ClusterPaths(amt_dir=str(directory), amt_runcode=str(directory / RUNCODE_FILE)),
    )
    store = StateStore.at(effects, directory)
    store.save_started(STARTED)
    assert amt_runcode(effects) == RUNCODE
    store.save(complete(STARTED, evaluated_at=NOW, tested_commit="t", summary="s"))
    assert amt_runcode(effects) == RUNCODE  # last_completed.runcode while idle


def test_save_started_refuses_a_state_without_a_started_cycle(tmp_path: Path) -> None:
    store, _ = store_at(tmp_path)
    with pytest.raises(AmtStateError, match="save_started needs a 'started' state"):
        store.save_started(IDLE)
    assert not store.exists()


def test_interrupted_save_leaves_the_old_state(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    """Atomic write: a crash between the temp file and the rename keeps the previous document."""
    store, directory = store_at(tmp_path)
    store.save(IDLE)
    before = (directory / AMT_STATE_FILE).read_bytes()

    def broken_replace(*_args: object, **_kwargs: object) -> None:
        raise OSError("disk full")

    monkeypatch.setattr(os, "replace", broken_replace)
    with pytest.raises(OSError, match="disk full"):
        store.save(STARTED)
    monkeypatch.undo()
    assert (directory / AMT_STATE_FILE).read_bytes() == before
    assert store.load() == IDLE


def test_locked_takes_the_flock_on_the_lock_file(tmp_path: Path) -> None:
    store, directory = store_at(tmp_path)
    with store.locked():
        assert calls(store.effects, "lock") == [str(store.lock_path)]
        assert (directory / LOCK_FILE).exists()
        # the lock is held: a non-blocking flock from another descriptor fails
        fd = os.open(directory / LOCK_FILE, os.O_RDWR)
        try:
            with pytest.raises(BlockingIOError):
                fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
        finally:
            os.close(fd)


def test_lock_contention_fails_fast_with_no_wait(tmp_path: Path) -> None:
    store, directory = store_at(tmp_path)
    holder = os.open(directory / LOCK_FILE, os.O_RDWR | os.O_CREAT, 0o644)
    fcntl.flock(holder, fcntl.LOCK_EX)
    try:
        expected = f"another modeltests command holds {store.lock_path}; not waiting"
        with pytest.raises(AmtLockError, match=expected) as info, store.locked(wait=False):
            pytest.fail("the lock was taken although another descriptor holds it")
        assert info.value.exit_status == 2
        assert isinstance(info.value, AmtStateError)
        assert isinstance(info.value.__cause__, LockHeldError)
    finally:
        os.close(holder)


def test_lock_contention_waits_by_default(tmp_path: Path) -> None:
    store, directory = store_at(tmp_path)
    holder = os.open(directory / LOCK_FILE, os.O_RDWR | os.O_CREAT, 0o644)
    fcntl.flock(holder, fcntl.LOCK_EX)
    released_at: list[float] = []

    def release_later() -> None:
        time.sleep(0.2)
        released_at.append(time.monotonic())
        os.close(holder)

    threading.Thread(target=release_later).start()
    with store.locked():
        acquired_at = time.monotonic()
    assert released_at and acquired_at >= released_at[0]


# ---------------------------------------------------------------------------------------------
# The store on RecordingEffects (the AMT double) and DryRunEffects (--dry-run)
# ---------------------------------------------------------------------------------------------


def recording(tmp_path: Path) -> RecordingEffects:
    """``RecordingEffects`` whose working directory is ``tmp_path`` (relative paths resolve against it)."""
    return RecordingEffects(now=NOW, user="pascalfu", on_cluster=False, cwd=tmp_path)


def test_recording_store_resolves_a_relative_dir_against_the_injected_cwd(tmp_path: Path) -> None:
    """``--dir modeltests/remind`` as the CLI would pass it: the files land below the effects' cwd."""
    directory = checkout(tmp_path)
    effects = recording(tmp_path)
    store = StateStore.at(effects, "modeltests/remind")
    assert store.path == PurePosixPath("modeltests/remind/amt-state.json")
    store.save_started(STARTED)
    assert (directory / AMT_STATE_FILE).read_text() == STARTED.to_json()
    assert scalar(read_rds(directory / RUNCODE_FILE)) == RUNCODE
    assert store.load() == STARTED
    with store.locked(wait=False):
        assert (directory / LOCK_FILE).exists()
    names = [c.name for c in effects.calls]
    assert names == ["write_text", "write_bytes", "exists", "read_text", "lock"]


def test_recording_migration_reads_the_r_files_through_the_effects(tmp_path: Path) -> None:
    directory = checkout(tmp_path)
    write_r_files(directory, tests_status=R_NEXT_EVALUATE)
    effects = recording(tmp_path)
    store = StateStore.at(effects, "modeltests/remind")
    notes: list[str] = []
    state = store.load_or_migrate(notes.append)
    assert state == AmtState(
        phase=Phase.STARTED,
        cycle=Cycle(id="2026-09-28", runcode=R_RUNCODE, runs_to_start=R_RUNS),
        last_completed=Completed(tested_commit=R_COMMIT),
    )
    assert notes[0] == (
        "amt-state.json is absent; deriving the state from modeltests/.testsstatus ('next:evaluate')"
    )
    existence = [c.args[0] for c in effects.calls if c.name == "exists"]
    assert existence[:2] == ["modeltests/remind/amt-state.json", "modeltests/.testsstatus"]
    assert set(existence[2:]) == {
        "modeltests/remind/runcode.rds",
        "modeltests/remind/runsToStart.rds",
        "modeltests/remind/lastcommit.rds",
    }
    assert [c.args[0] for c in effects.calls if c.name == "read_text"] == ["modeltests/.testsstatus"]
    assert [c.args[0] for c in effects.calls if c.name == "write_text"] == [
        "modeltests/remind/amt-state.json"
    ]
    assert store.load() == state


def test_dry_run_store_writes_nothing(tmp_path: Path) -> None:
    """``--dry-run`` (``06`` 4.4 item 1): mutations are logged, the tree keeps its digests, no lock file."""
    directory = checkout(tmp_path)
    write_r_files(directory, tests_status=R_NEXT_START)
    before = digests(directory)
    messages: list[str] = []
    effects = DryRunEffects(fake(tmp_path), log=messages.append)
    store = StateStore.at(effects, directory)
    notes: list[str] = []
    state = store.load_or_migrate(notes.append)
    assert state.phase is Phase.IDLE and state.last_completed == Completed(
        tested_commit=R_COMMIT, runcode=R_RUNCODE
    )
    store.save_started(start(state, CYCLE))
    with store.locked(wait=False):
        pass
    assert messages == [
        f"would write {store.path}",
        f"would write {store.path}",
        f"would write {store.runcode_path}",
        f"would lock {store.lock_path}",
    ]
    assert digests(directory) == before
    assert not store.exists()
    assert not (directory / LOCK_FILE).exists()
    assert notes[-1] == f"the derived state is kept in {store.path}; the R files are left untouched"


# ---------------------------------------------------------------------------------------------
# Migration from the R files (06 section 3.2)
# ---------------------------------------------------------------------------------------------


def test_no_testsstatus_means_nothing_to_migrate(tmp_path: Path) -> None:
    store, directory = store_at(tmp_path)
    write_r_files(directory, tests_status=None)
    assert migrate_r_state(store.effects, directory) is None
    assert store.load_or_migrate() == AmtState()
    assert not store.exists()  # a fresh checkout is not written until the first transition


def test_next_start_migrates_to_idle_with_the_last_cycle(tmp_path: Path) -> None:
    store, directory = store_at(tmp_path)
    write_r_files(directory, tests_status=R_NEXT_START)
    migration = migrate_r_state(store.effects, directory)
    assert migration is not None
    assert migration.state == AmtState(
        phase=Phase.IDLE, last_completed=Completed(tested_commit=R_COMMIT, runcode=R_RUNCODE)
    )
    assert migration.notes == (
        f"amt-state.json is absent; deriving the state from {directory.parent}/.testsstatus ('next:start')",
        "derived state: the AMT state is 'idle'",
    )


def test_next_evaluate_migrates_to_started_with_the_cycle_from_the_rds_files(tmp_path: Path) -> None:
    store, directory = store_at(tmp_path)
    write_r_files(directory, tests_status=R_NEXT_EVALUATE)
    migration = migrate_r_state(store.effects, directory)
    assert migration is not None
    assert migration.state == AmtState(
        phase=Phase.STARTED,
        cycle=Cycle(id="2026-09-28", runcode=R_RUNCODE, runs_to_start=R_RUNS),
        last_completed=Completed(tested_commit=R_COMMIT),
    )
    assert (
        migration.notes[-1]
        == "derived state: a cycle is 'started' (cycle 2026-09-28, started an unknown time)"
    )
    assert target_phase(migration.state, Command.EVALUATE) is Phase.IDLE


def test_error_sentence_migrates_to_failed_with_the_cycle_kept(tmp_path: Path) -> None:
    store, directory = store_at(tmp_path)
    write_r_files(directory, tests_status=R_ERROR_SENTENCE)
    migration = migrate_r_state(store.effects, directory)
    assert migration is not None
    state = migration.state
    assert state.phase is Phase.FAILED
    assert state.cycle == Cycle(id="2026-09-28", runcode=R_RUNCODE, runs_to_start=R_RUNS)
    assert state.last_completed == Completed(tested_commit=R_COMMIT)
    assert state.last_error == LastError(
        command="evaluate",
        message=f"{directory.parent}/.testsstatus holds {R_ERROR_SENTENCE!r} (the R version would do"
        " nothing)",
    )
    assert target_phase(state, Command.EVALUATE) is Phase.IDLE  # retry
    assert target_phase(state, Command.RESET) is Phase.IDLE
    with pytest.raises(AmtStateError, match="cannot 'run'"):
        next_command(state)


def test_unknown_sentence_migrates_to_failed_like_the_r_doing_nothing(tmp_path: Path) -> None:
    """``next:foo`` (case ``remind-nothing``): R says "Doing nothing"; the rewrite says ``failed``."""
    store, directory = store_at(tmp_path)
    write_r_files(directory, tests_status="next:foo")
    migration = migrate_r_state(store.effects, directory)
    assert migration is not None
    assert migration.state.phase is Phase.FAILED
    assert migration.state.last_error is not None
    assert "holds 'next:foo' (the R version would do nothing)" in migration.state.last_error.message


def test_migration_without_runcode_rds_is_failed_without_a_cycle(tmp_path: Path) -> None:
    store, directory = store_at(tmp_path)
    write_r_files(directory, tests_status=R_NEXT_EVALUATE, runcode=None)
    migration = migrate_r_state(store.effects, directory)
    assert migration is not None
    state = migration.state
    assert state.phase is Phase.FAILED
    assert state.cycle is None
    assert state.last_completed == Completed(tested_commit=R_COMMIT)
    assert state.last_error == LastError(
        command="evaluate",
        message="runcode.rds is missing or unreadable, the running R cycle cannot be derived",
    )
    assert "runcode.rds is absent" in migration.notes
    with pytest.raises(AmtStateError, match="no cycle is recorded to retry"):
        complete(state, evaluated_at=NOW, tested_commit=None, summary=None)
    assert reset(state).phase is Phase.IDLE


def test_migration_with_a_damaged_runcode_rds(tmp_path: Path) -> None:
    store, directory = store_at(tmp_path)
    write_r_files(directory, tests_status=R_NEXT_EVALUATE, runcode=None)
    (directory / "runcode.rds").write_bytes(b"not an rds file")
    migration = migrate_r_state(store.effects, directory)
    assert migration is not None
    assert migration.state.phase is Phase.FAILED and migration.state.cycle is None
    assert any(note.startswith("runcode.rds could not be read (") for note in migration.notes)


def test_migration_with_a_runcode_that_is_not_one_string(tmp_path: Path) -> None:
    store, directory = store_at(tmp_path)
    write_r_files(directory, tests_status=R_NEXT_START, runcode=None)
    write_rds(directory / "runcode.rds", ["a", "b"])
    migration = migrate_r_state(store.effects, directory)
    assert migration is not None
    assert migration.state == AmtState(phase=Phase.IDLE, last_completed=Completed(tested_commit=R_COMMIT))
    assert "runcode.rds does not hold one character string" in migration.notes


def test_migration_notes_the_optional_files(tmp_path: Path) -> None:
    store, directory = store_at(tmp_path)
    write_r_files(directory, tests_status=R_NEXT_EVALUATE, commit=None, runs=None)
    migration = migrate_r_state(store.effects, directory)
    assert migration is not None
    assert migration.state.cycle == Cycle(id="2026-09-28", runcode=R_RUNCODE, runs_to_start=())
    assert migration.state.last_completed is None
    assert "runsToStart.rds is absent" in migration.notes
    assert "lastcommit.rds is absent" in migration.notes


def test_migration_of_an_idle_checkout_without_rds_files(tmp_path: Path) -> None:
    store, directory = store_at(tmp_path)
    write_r_files(directory, tests_status=R_NEXT_START, runcode=None, commit=None, runs=None)
    migration = migrate_r_state(store.effects, directory)
    assert migration is not None
    assert migration.state == AmtState()


def test_cycle_id_of_a_runcode_that_is_not_the_r_pattern(tmp_path: Path) -> None:
    store, directory = store_at(tmp_path)
    write_r_files(directory, tests_status=R_NEXT_EVALUATE, runcode="SSP2.*custom")
    migration = migrate_r_state(store.effects, directory)
    assert migration is not None
    assert migration.state.cycle == Cycle(id="SSP2.*custom", runcode="SSP2.*custom", runs_to_start=R_RUNS)


@pytest.mark.parametrize("tests_status", [R_NEXT_START, R_NEXT_EVALUATE, R_ERROR_SENTENCE, "next:foo"])
def test_migration_leaves_the_r_files_untouched_and_happens_once(tmp_path: Path, tests_status: str) -> None:
    store, directory = store_at(tmp_path)
    write_r_files(directory, tests_status=tests_status)
    before = digests(directory)
    notes: list[str] = []
    first = store.load_or_migrate(notes.append)
    assert store.exists()
    assert notes[0].startswith("amt-state.json is absent; deriving the state from ")
    assert notes[-1] == f"the derived state is kept in {store.path}; the R files are left untouched"
    after = digests(directory)
    assert {k: v for k, v in after.items() if k != f"remind/{AMT_STATE_FILE}"} == before
    assert (directory.parent / ".testsstatus").read_text() == tests_status + "\n"
    second_notes: list[str] = []
    assert store.load_or_migrate(second_notes.append) == first
    assert second_notes == []
    assert store.load() == first


def test_testsstatus_without_a_final_newline(tmp_path: Path) -> None:
    store, directory = store_at(tmp_path)
    write_r_files(directory, tests_status=R_NEXT_START)
    (directory.parent / ".testsstatus").write_text(R_NEXT_EVALUATE)
    migration = migrate_r_state(store.effects, directory)
    assert migration is not None and migration.state.phase is Phase.STARTED


def golden_state(case: str, name: str) -> dict[str, Any]:
    """One ``state/<file>.json`` record of an R AMT golden (``migration/goldens/README.md``)."""
    record: dict[str, Any] = json.loads((GOLDENS / "amt" / case / "state" / name).read_text())
    return record


def golden_string(record: Mapping[str, Any]) -> str:
    """The one string a ``character`` record holds (``value: ["..."]``)."""
    (value,) = record["value"]
    return str(value)


@dataclass(frozen=True)
class RCaseFiles:
    """What a golden's ``state/*.json`` records say the R run left: ``.testsstatus`` and the run names."""

    tests_status: str
    runs: tuple[str, ...]


def write_r_files_from_golden(directory: Path, case: str) -> RCaseFiles:
    """Rebuild the R files the case's run left behind (the goldens record them after the run)."""
    content = golden_string(golden_state(case, "_testsstatus.json"))
    (directory.parent / ".testsstatus").write_text(content + "\n")
    runcode = golden_state(case, "runcode_rds.json")
    if "read_error" in runcode:
        (directory / "runcode.rds").write_text("garbage\n")  # the case's prepare.sh
    elif runcode["exists"]:
        write_rds(directory / "runcode.rds", golden_string(runcode))
    commit = golden_state(case, "lastcommit_rds.json")
    if commit["exists"]:
        write_rds(directory / "lastcommit.rds", golden_string(commit))
    runs = golden_state(case, "runsToStart_rds.json")
    names = tuple(str(row["_row"]) for row in runs["value"]["rows"]) if runs["exists"] else ()
    if runs["exists"]:
        frame = pd.DataFrame({"start": ["AMT"] * len(names)}, index=list(names))
        write_rds(directory / "runsToStart.rds", frame)
    return RCaseFiles(content, names)


R_STATE_CASES = (
    "remind-nothing",
    "remind-evaluate-state-missing",
    "remind-evaluate-state-corrupt",
    "remind-evaluate-runstostart-missing",
)
"""The R cases whose purpose was the toggle file or an RDS state file (``06`` 4.4 item 4)."""


def expected_after_r_case(case: str, tests_status: str, runs: tuple[str, ...]) -> AmtState:
    """The state-machine counterpart of each case of :data:`R_STATE_CASES`.

    ``remind-nothing`` left ``next:foo`` ("Doing nothing"); the three ``evaluate`` cases crashed after
    writing the error sentence: one without ``lastcommit.rds``, one with a garbage ``runcode.rds`` (no
    cycle can be derived) and one without ``runsToStart.rds``.
    """
    cycle = Cycle(id="2026-09-28", runcode=R_RUNCODE, runs_to_start=runs)
    completed = Completed(tested_commit=R_COMMIT)

    def refused(content: str) -> LastError:
        message = f"{tests_status} holds {content!r} (the R version would do nothing)"
        return LastError(command="evaluate", message=message)

    if case == "remind-nothing":
        return AmtState(Phase.FAILED, cycle=cycle, last_completed=completed, last_error=refused("next:foo"))
    crashed = refused(R_ERROR_SENTENCE)
    if case == "remind-evaluate-state-missing":
        return AmtState(Phase.FAILED, cycle=cycle, last_completed=None, last_error=crashed)
    if case == "remind-evaluate-state-corrupt":
        return AmtState(Phase.FAILED, cycle=None, last_completed=completed, last_error=crashed)
    assert case == "remind-evaluate-runstostart-missing" and runs == ()
    return AmtState(Phase.FAILED, cycle=cycle, last_completed=completed, last_error=crashed)


@pytest.mark.parametrize("case", R_STATE_CASES)
def test_migration_of_the_state_the_r_cases_left_behind(tmp_path: Path, case: str) -> None:
    """The R files recorded by the case's golden (after the run) migrate once; the R files stay untouched."""
    store, directory = store_at(tmp_path)
    left = write_r_files_from_golden(directory, case)
    assert (len(left.runs) == 24) == (case != "remind-evaluate-runstostart-missing")
    before = digests(directory)
    expected = expected_after_r_case(case, f"{directory.parent}/.testsstatus", left.runs)
    assert store.load_or_migrate() == expected
    assert store.load() == expected
    assert target_phase(expected, Command.RESET) is Phase.IDLE
    if expected.cycle is not None:
        assert target_phase(expected, Command.EVALUATE) is Phase.IDLE  # the retry of 06 3.2
    assert (directory.parent / ".testsstatus").read_text() == left.tests_status + "\n"
    assert {k: v for k, v in digests(directory).items() if k != f"remind/{AMT_STATE_FILE}"} == before


def test_migration_of_the_fixture_checkout_through_the_overlay() -> None:
    """The canonical AMT checkout (``.testsstatus`` ``next:start``, the three RDS files) in local mode."""
    if not (FIXTURES / "p").is_dir():
        pytest.skip("fixture snapshot absent")
    effects = FakeEffects(
        now=NOW,
        user="pascalfu",
        on_cluster=False,
        paths=OverlayPaths.sandbox(FIXTURES / "p", SYNTHETIC / "p", Mode.LOCAL),
    )
    migration = migrate_r_state(effects, FIXTURE_CHECKOUT)
    assert migration is not None
    assert migration.state == AmtState(
        phase=Phase.IDLE, last_completed=Completed(tested_commit=R_COMMIT, runcode=R_RUNCODE)
    )
    assert migration.notes[0] == (
        "amt-state.json is absent; deriving the state from"
        " /fixtures/p/projects/remind/modeltests/.testsstatus ('next:start')"
    )
    assert not [c for c in effects.calls if c.name.startswith("write")]
