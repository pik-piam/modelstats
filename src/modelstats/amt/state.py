"""The AMT state machine: ``amt-state.json``, its transitions, the lock and the migration from the R files.

``migration/06-user-requests.md`` sections 3.2 and 3.4 (request 2, decision D-24). The R version toggles
between ``startRuns`` and ``evaluateRuns`` through the sentence in ``../.testsstatus``
(``R/modeltests.R:37-54``) and keeps its cycle in three RDS files (``runcode.rds`` ``:164``,
``runsToStart.rds`` ``:138``, ``lastcommit.rds`` ``:489``). The rewrite keeps one JSON document per
checkout, :class:`AmtState`, with three phases and the transitions of the table in ``06`` 3.2:

======= =============== ======= ==================================================================
from    command         to      notes
======= =============== ======= ==================================================================
idle    start           started ``cycle`` filled; ``runcode.rds`` also written (3.4)
started evaluate        idle    ``last_completed`` filled, ``cycle`` cleared
started start --force   started the abandoned cycle is recorded in ``last_error``
failed  evaluate        idle    retry after a failed evaluate (the ``cycle`` is still there)
failed  reset           idle
any     crash           failed  ``last_error`` filled; the cycle is kept
======= =============== ======= ==================================================================

Anything else raises :class:`AmtStateError`, whose message the CLI prints before exiting with status 2
(:data:`AmtStateError.exit_status`), and changes nothing. ``run`` maps ``idle`` to ``start``, ``started``
to ``evaluate`` and ``failed`` to that error (:func:`next_command`).

The transitions (:func:`start`, :func:`complete`, :func:`fail`, :func:`reset`) are pure functions on the
frozen dataclasses; :class:`StateStore` is the one place that touches files, always through
:class:`modelstats.env.Effects`: the JSON is written atomically (``write_text``: temp file and
``os.replace`` in the same directory), the whole command runs under an exclusive ``flock`` on
``amt-state.lock`` (:meth:`StateStore.locked`, ``--no-wait`` fails fast with :class:`AmtLockError`), and
``runcode.rds`` keeps being written next to the JSON on ``start`` (``06`` 3.4) so that ``rs-r -A`` and
``remindstatus`` see the running cycle; the Python ``rs -A`` reads the JSON first
(:func:`modelstats.sanity.amt_runcode`, D-26). The R pattern of ``runcode`` is :func:`runcode_pattern`
(``R/modeltests.R:92-96``).

**Migration of the R state** (``06`` 3.2, :func:`migrate_r_state`): when ``amt-state.json`` is absent and
``../.testsstatus`` exists, the first command derives the state once and :meth:`StateStore.load_or_migrate`
reports each derived fact so the caller can log it. ``next:start`` becomes ``idle``; ``next:evaluate``
becomes ``started`` with ``runcode`` from ``runcode.rds``, ``runs_to_start`` from the row names of
``runsToStart.rds`` and the previously tested commit from ``lastcommit.rds`` (``last_completed``, the base
of the next merge list, ``R/modeltests.R:174, 199-201``); the error sentence of ``R/modeltests.R:172``, and
any other content R answers with "Doing nothing" (``:53``), becomes ``failed`` with the content in
``last_error`` and the cycle derived the same way, so that ``evaluate`` can retry or ``reset`` can clear
it. The R files are only read (the R package may still be the rollback); ``gRS.rds`` is never read.

The R AMT cases whose purpose was the toggle file or an RDS state file map onto this module's tests
(``06`` 4.4 item 4): ``remind-nothing`` (``next:foo``, "Doing nothing") is the ``failed`` migration,
``remind-evaluate-state-missing`` (no ``lastcommit.rds``) a ``started`` cycle without a tested commit,
``remind-evaluate-state-corrupt`` (``runcode.rds`` is garbage) ``failed`` without a cycle, and
``remind-evaluate-runstostart-missing`` a cycle with no ``runs_to_start``; the ``gRS.rds`` cases have no
counterpart because the history file is not read.

``runcode.rds`` is serialised in memory (:func:`rds_bytes`, the conversion of
:func:`modelstats.rdata_io.write_rds`) and written through ``Effects.write_bytes`` so that a dry run writes
nothing and a recording run sees the bytes.
"""

from __future__ import annotations

import gzip
import io
import json
import os
from collections.abc import Callable, Iterator, Mapping
from contextlib import ExitStack, contextmanager
from dataclasses import dataclass, field, replace
from datetime import date, datetime, timedelta
from enum import Enum
from pathlib import PurePosixPath
from typing import Any, ClassVar, Final

from rdata.conversion import convert_python_to_r_data
from rdata.unparser import unparse_fileobj

from modelstats.env import Effects, LockHeldError, PathLike
from modelstats.errors import ModelstatsError
from modelstats.rdata_io import WRITE_CLASS_MAP, RDataError, read_rds, vector
from modelstats.sanity import AMT_STATE_FILE

__all__ = [
    "AMT_STATE_FILE",
    "LOCK_FILE",
    "RUNCODE_FILE",
    "R_ERROR_SENTENCE",
    "R_LAST_COMMIT_FILE",
    "R_NEXT_EVALUATE",
    "R_NEXT_START",
    "R_RUNS_TO_START_FILE",
    "R_TESTS_STATUS_FILE",
    "SCHEMA",
    "AmtLockError",
    "AmtState",
    "AmtStateError",
    "Command",
    "Completed",
    "Cycle",
    "LastError",
    "Migration",
    "Phase",
    "StateStore",
    "complete",
    "cycle_id",
    "describe",
    "evaluable_cycle",
    "fail",
    "migrate_r_state",
    "next_command",
    "rds_bytes",
    "reset",
    "runcode_pattern",
    "start",
    "target_phase",
]

SCHEMA: Final = 1
"""The ``schema`` field of ``amt-state.json`` (``06`` 3.2)."""

LOCK_FILE: Final = "amt-state.lock"
"""The ``flock`` file next to the state file (``06`` 3.2)."""

RUNCODE_FILE: Final = "runcode.rds"
"""The R cycle pattern file kept for ``rs-r -A`` (``R/modeltests.R:164``, ``06`` 3.4)."""

R_TESTS_STATUS_FILE: Final = ".testsstatus"
"""The R toggle file, one directory above the checkout (``R/modeltests.R:37``)."""

R_RUNS_TO_START_FILE: Final = "runsToStart.rds"
"""The scenarios ``startRuns`` meant to start (``R/modeltests.R:138``)."""

R_LAST_COMMIT_FILE: Final = "lastcommit.rds"
"""The commit the previous evaluate tested (``R/modeltests.R:489``)."""

R_NEXT_START: Final = "next:start"
"""``.testsstatus`` content that makes the R version start runs (``R/modeltests.R:37``)."""

R_NEXT_EVALUATE: Final = "next:evaluate"
"""``.testsstatus`` content that makes the R version evaluate runs (``R/modeltests.R:43``)."""

R_ERROR_SENTENCE: Final = "evaluateRuns() is running or stopped due to an error"
"""What ``evaluateRuns`` writes while it runs and leaves behind when it crashes (``R/modeltests.R:172``)."""

_RUNCODE_PREFIX: Final = ".*-AMT_"
_JSON_INDENT: Final = 2


class AmtStateError(ModelstatsError):
    """A command is not allowed in the current state, or the state cannot be read or derived.

    ``str(error)`` is the message the CLI prints; the process exits with :attr:`exit_status` (2) and the
    state file is unchanged (``06`` 3.2).
    """

    exit_status: ClassVar[int] = 2


class AmtLockError(AmtStateError):
    """``amt-state.lock`` is held by another invocation and waiting was disabled (``--no-wait``)."""


# ---------------------------------------------------------------------------------------------
# The state document
# ---------------------------------------------------------------------------------------------


class Phase(Enum):
    """The ``phase`` field: where the AMT cycle stands (``06`` 3.2)."""

    IDLE = "idle"
    STARTED = "started"
    FAILED = "failed"


class Command(Enum):
    """The ``modeltests`` commands of ``06`` 3.1."""

    START = "start"
    EVALUATE = "evaluate"
    RUN = "run"
    STATUS = "status"
    RESET = "reset"


@dataclass(frozen=True)
class Cycle:
    """The ``cycle`` block: one started AMT cycle.

    ``id`` is the start date (``2026-09-30``, :func:`cycle_id`), ``runcode`` the R pattern naming its runs
    (:func:`runcode_pattern`), ``started_at`` and ``started_commit`` what ``start`` recorded (``None`` for a
    cycle migrated from the R files, which never recorded them), ``runs_to_start`` the scenario names the
    ``selectScenarios()`` bridge returned with the ``-AMT`` suffix (the row names of ``runsToStart.rds``,
    ``R/modeltests.R:136-137``) and ``log`` the cycle log as the CLI names it (``06`` 3.3).
    """

    id: str
    runcode: str
    started_at: datetime | None = None
    started_commit: str | None = None
    runs_to_start: tuple[str, ...] = ()
    log: str | None = None


@dataclass(frozen=True)
class Completed:
    """The ``last_completed`` block: the last evaluated cycle.

    ``tested_commit`` is the base of the next merge list (``R/modeltests.R:199-201``); ``runcode`` lets
    ``rs -A`` show the last cycle's runs while idle (D-26). A block migrated from the R files knows only
    what ``lastcommit.rds`` and ``runcode.rds`` hold.
    """

    id: str | None = None
    evaluated_at: datetime | None = None
    tested_commit: str | None = None
    summary: str | None = None
    runcode: str | None = None


@dataclass(frozen=True)
class LastError:
    """The ``last_error`` block: the command that failed, when, with what message, and its log."""

    command: str
    message: str
    at: datetime | None = None
    log: str | None = None


@dataclass(frozen=True)
class AmtState:
    """``amt-state.json`` (``06`` 3.2): schema, phase and the three optional blocks."""

    phase: Phase = Phase.IDLE
    cycle: Cycle | None = None
    last_completed: Completed | None = None
    last_error: LastError | None = None
    schema: int = SCHEMA

    def to_dict(self) -> dict[str, Any]:
        """The JSON document as nested dictionaries (datetimes as ISO 8601 with offset)."""
        return {
            "schema": self.schema,
            "phase": self.phase.value,
            "cycle": None if self.cycle is None else _cycle_dict(self.cycle),
            "last_completed": None if self.last_completed is None else _completed_dict(self.last_completed),
            "last_error": None if self.last_error is None else _error_dict(self.last_error),
        }

    def to_json(self) -> str:
        """The file content: indented JSON and a final newline."""
        return json.dumps(self.to_dict(), indent=_JSON_INDENT, ensure_ascii=False) + "\n"

    @classmethod
    def from_dict(cls, document: object, *, source: str = AMT_STATE_FILE) -> AmtState:
        """Parse a document; every violation of the schema is :class:`AmtStateError` naming ``source``."""
        reader = _Reader(source)
        top = reader.mapping(document, "the document")
        schema = top.get("schema")
        if schema != SCHEMA:
            raise AmtStateError(f"{source}: unsupported schema {schema!r} (this version writes {SCHEMA})")
        return cls(
            phase=reader.phase(top.get("phase")),
            cycle=reader.cycle(top.get("cycle")),
            last_completed=reader.completed(top.get("last_completed")),
            last_error=reader.error(top.get("last_error")),
            schema=SCHEMA,
        )

    @classmethod
    def from_json(cls, text: str, *, source: str = AMT_STATE_FILE) -> AmtState:
        """Parse the file content."""
        try:
            document = json.loads(text)
        except ValueError as error:
            raise AmtStateError(f"{source}: not valid JSON ({error}); fix or remove the file") from error
        return cls.from_dict(document, source=source)


def _iso(value: datetime | None) -> str | None:
    return None if value is None else value.isoformat()


def _cycle_dict(cycle: Cycle) -> dict[str, Any]:
    return {
        "id": cycle.id,
        "runcode": cycle.runcode,
        "started_at": _iso(cycle.started_at),
        "started_commit": cycle.started_commit,
        "runs_to_start": list(cycle.runs_to_start),
        "log": cycle.log,
    }


def _completed_dict(completed: Completed) -> dict[str, Any]:
    return {
        "id": completed.id,
        "evaluated_at": _iso(completed.evaluated_at),
        "tested_commit": completed.tested_commit,
        "summary": completed.summary,
        "runcode": completed.runcode,
    }


def _error_dict(error: LastError) -> dict[str, Any]:
    return {"command": error.command, "at": _iso(error.at), "message": error.message, "log": error.log}


@dataclass(frozen=True)
class _Reader:
    """Typed access to a parsed JSON document, every mismatch an :class:`AmtStateError` naming the file."""

    source: str

    def mapping(self, value: object, what: str) -> Mapping[str, Any]:
        if not isinstance(value, Mapping):
            raise AmtStateError(f"{self.source}: {what} must be a JSON object, not {type(value).__name__}")
        return value

    def text(self, block: Mapping[str, Any], key: str, *, required: bool = False) -> str | None:
        value = block.get(key)
        if value is None and not required:
            return None
        if not isinstance(value, str):
            raise AmtStateError(f"{self.source}: {key!r} must be a string, not {type(value).__name__}")
        return value

    def required_text(self, block: Mapping[str, Any], key: str) -> str:
        value = self.text(block, key, required=True)
        assert value is not None  # ``required`` raised otherwise
        return value

    def moment(self, block: Mapping[str, Any], key: str) -> datetime | None:
        value = self.text(block, key)
        if value is None:
            return None
        try:
            return datetime.fromisoformat(value)
        except ValueError as error:
            raise AmtStateError(f"{self.source}: {key!r} is not an ISO 8601 timestamp: {value!r}") from error

    def names(self, block: Mapping[str, Any], key: str) -> tuple[str, ...]:
        value = block.get(key)
        if value is None:
            return ()
        if not isinstance(value, list) or not all(isinstance(item, str) for item in value):
            raise AmtStateError(f"{self.source}: {key!r} must be a list of strings")
        return tuple(value)

    def phase(self, value: object) -> Phase:
        try:
            return Phase(value)
        except ValueError:
            allowed = ", ".join(phase.value for phase in Phase)
            raise AmtStateError(f"{self.source}: 'phase' must be one of {allowed}, not {value!r}") from None

    def cycle(self, value: object) -> Cycle | None:
        if value is None:
            return None
        block = self.mapping(value, "'cycle'")
        return Cycle(
            id=self.required_text(block, "id"),
            runcode=self.required_text(block, "runcode"),
            started_at=self.moment(block, "started_at"),
            started_commit=self.text(block, "started_commit"),
            runs_to_start=self.names(block, "runs_to_start"),
            log=self.text(block, "log"),
        )

    def completed(self, value: object) -> Completed | None:
        if value is None:
            return None
        block = self.mapping(value, "'last_completed'")
        return Completed(
            id=self.text(block, "id"),
            evaluated_at=self.moment(block, "evaluated_at"),
            tested_commit=self.text(block, "tested_commit"),
            summary=self.text(block, "summary"),
            runcode=self.text(block, "runcode"),
        )

    def error(self, value: object) -> LastError | None:
        if value is None:
            return None
        block = self.mapping(value, "'last_error'")
        return LastError(
            command=self.required_text(block, "command"),
            message=self.required_text(block, "message"),
            at=self.moment(block, "at"),
            log=self.text(block, "log"),
        )


# ---------------------------------------------------------------------------------------------
# The cycle identity: R/modeltests.R:92-96
# ---------------------------------------------------------------------------------------------


def runcode_pattern(today: date) -> str:
    """``paste0(".*-AMT_", today, "|", ".*-AMT_", today + 1)`` of ``R/modeltests.R:92-96``.

    The regular expression matching the run folders of a cycle started on ``today``: runs started
    before midnight carry the start date, runs the scheduler starts after midnight the next one.
    ``today`` is ``Sys.Date()`` in the session's zone, i.e. ``effects.now().date()``.
    """
    tomorrow = today + timedelta(days=1)
    return f"{_RUNCODE_PREFIX}{today.isoformat()}|{_RUNCODE_PREFIX}{tomorrow.isoformat()}"


def cycle_id(today: date) -> str:
    """The cycle id of ``06`` 3.2: the start date as ``YYYY-MM-DD``."""
    return today.isoformat()


# ---------------------------------------------------------------------------------------------
# Transitions (06 section 3.2): pure functions, invalid moves raise with the CLI's message
# ---------------------------------------------------------------------------------------------

_TRANSITIONS: Final[Mapping[tuple[Phase, Command, bool], Phase]] = {
    (Phase.IDLE, Command.START, False): Phase.STARTED,
    (Phase.STARTED, Command.EVALUATE, False): Phase.IDLE,
    (Phase.STARTED, Command.START, True): Phase.STARTED,
    (Phase.FAILED, Command.EVALUATE, False): Phase.IDLE,
    (Phase.FAILED, Command.RESET, False): Phase.IDLE,
}
"""The table of ``06`` 3.2 as ``(from, command, needs --force) -> to``; ``run``/``status`` are not moves."""


def describe(state: AmtState) -> str:
    """One clause saying where the AMT stands, for messages: ``the AMT state is 'idle' (...)``."""
    if state.phase is Phase.STARTED and state.cycle is not None:
        return f"a cycle is 'started' (cycle {state.cycle.id}, started {_when(state.cycle.started_at)})"
    if state.phase is Phase.FAILED and state.last_error is not None:
        error = state.last_error
        where = f", log {error.log}" if error.log is not None else ""
        failed = f"'{error.command}' failed at {_when(error.at)}: {error.message}{where}"
        return f"the AMT state is 'failed': {failed}"
    if state.phase is Phase.IDLE and state.last_completed is not None and state.last_completed.id is not None:
        return f"the AMT state is 'idle' (last completed cycle {state.last_completed.id})"
    return f"the AMT state is '{state.phase.value}'"


def _when(moment: datetime | None) -> str:
    return "an unknown time" if moment is None else moment.isoformat()


def _hint(phase: Phase) -> str:
    """What a human can do in ``phase`` when a command is refused."""
    if phase is Phase.IDLE:
        return "run 'modeltests start' first"
    if phase is Phase.STARTED:
        return "run 'modeltests evaluate', or 'modeltests start --force' to abandon the cycle"
    return "a human decides: 'modeltests evaluate' retries the cycle, 'modeltests reset' clears the state"


def _refused(state: AmtState, command: Command) -> AmtStateError:
    return AmtStateError(f"cannot '{command.value}': {describe(state)}; {_hint(state.phase)}")


def target_phase(state: AmtState, command: Command, *, force: bool = False) -> Phase:
    """The phase ``command`` leads to from ``state`` by the table, or :class:`AmtStateError`.

    This is the pre-check a command runs before it does anything (``no state change`` for a refused
    command). ``force`` is ``start --force``; it is ignored where the plain move is allowed.
    """
    plain = _TRANSITIONS.get((state.phase, command, False))
    if plain is not None:
        return plain
    forced = _TRANSITIONS.get((state.phase, command, True)) if force else None
    if forced is not None:
        return forced
    raise _refused(state, command)


def next_command(state: AmtState) -> Command:
    """What ``run`` does: ``idle`` starts, ``started`` evaluates, ``failed`` is refused (a human decides)."""
    if state.phase is Phase.IDLE:
        return Command.START
    if state.phase is Phase.STARTED:
        return Command.EVALUATE
    raise _refused(state, Command.RUN)


def start(state: AmtState, cycle: Cycle, *, force: bool = False) -> AmtState:
    """``idle -> started`` with ``cycle``; ``started --force -> started`` recording the abandoned cycle.

    The abandoned cycle goes into ``last_error`` as a ``start`` entry dated by the new cycle's start,
    so ``status`` shows what was dropped and the old log stays findable.
    """
    target_phase(state, Command.START, force=force)
    abandoned = state.cycle if state.phase is Phase.STARTED else None
    last_error = state.last_error if abandoned is None else _abandonment(abandoned, cycle.started_at)
    return replace(state, phase=Phase.STARTED, cycle=cycle, last_error=last_error)


def _abandonment(abandoned: Cycle, at: datetime | None) -> LastError:
    return LastError(
        command=Command.START.value,
        message=(
            f"cycle {abandoned.id} started {_when(abandoned.started_at)} was abandoned by 'start --force'"
            " before it was evaluated"
        ),
        at=at,
        log=abandoned.log,
    )


def complete(
    state: AmtState, *, evaluated_at: datetime | None, tested_commit: str | None, summary: str | None
) -> AmtState:
    """``started -> idle`` and ``failed -> idle`` (retry): ``last_completed`` filled, the cycle cleared.

    The cycle's id and runcode travel into ``last_completed`` so that the next merge list has its base
    (``tested_commit``) and ``rs -A`` keeps its pattern while idle. ``last_error`` stays as history.
    """
    target_phase(state, Command.EVALUATE)
    cycle = evaluable_cycle(state)
    completed = Completed(
        id=cycle.id,
        evaluated_at=evaluated_at,
        tested_commit=tested_commit,
        summary=summary,
        runcode=cycle.runcode,
    )
    return replace(state, phase=Phase.IDLE, cycle=None, last_completed=completed)


def evaluable_cycle(state: AmtState) -> Cycle:
    """The cycle ``evaluate`` works on; a ``failed`` state without one has nothing to retry.

    The pre-check of ``modelstats.amt.evaluate.cycle_to_evaluate`` and the cycle :func:`complete` closes:
    one message points at ``reset`` in both places.
    """
    if state.cycle is None:
        raise AmtStateError(
            f"cannot 'evaluate': {describe(state)}; no cycle is recorded to retry, run 'modeltests reset'"
            " and then 'modeltests start'"
        )
    return state.cycle


def fail(state: AmtState, error: LastError, *, cycle: Cycle | None = None) -> AmtState:
    """``any -> failed`` after a crash inside ``start`` or ``evaluate``: ``last_error`` filled, cycle kept.

    ``cycle`` is the record ``start`` had built before it crashed (the state still says ``idle`` then);
    without it the cycle already in the state is kept.
    """
    kept = cycle if cycle is not None else state.cycle
    return replace(state, phase=Phase.FAILED, cycle=kept, last_error=error)


def reset(state: AmtState) -> AmtState:
    """``failed -> idle`` after a human looked at it: the failed cycle is dropped, the history kept."""
    target_phase(state, Command.RESET)
    return replace(state, phase=Phase.IDLE, cycle=None)


# ---------------------------------------------------------------------------------------------
# runcode.rds for rs-r (06 section 3.4)
# ---------------------------------------------------------------------------------------------


def rds_bytes(value: str) -> bytes:
    """What ``saveRDS(value, file)`` writes for one character string, as bytes (``R/modeltests.R:164``).

    The same conversion as :func:`modelstats.rdata_io.write_rds` (``rdata``'s XDR serialisation, format
    version 3, gzip), produced in memory so that the file reaches the disk through
    :class:`modelstats.env.Effects` like every other mutation (nothing is written under ``--dry-run``, a
    recording run sees the bytes). The gzip header carries no timestamp, so equal input gives equal bytes.
    """
    r_data = convert_python_to_r_data(value, constructor_dict=WRITE_CLASS_MAP)
    buffer = io.BytesIO()
    with gzip.GzipFile(fileobj=buffer, mode="wb", mtime=0) as stream:
        unparse_fileobj(stream, r_data, file_format="xdr", file_type="rds")
    return buffer.getvalue()


# ---------------------------------------------------------------------------------------------
# Migration from the R files (06 section 3.2)
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True)
class Migration:
    """The state derived from the R files and one line per derived fact, for the log."""

    state: AmtState
    notes: tuple[str, ...] = ()


@dataclass
class _RFiles:
    """What the R files of a checkout hold, read once, with a note for everything that was not there."""

    effects: Effects
    directory: PurePosixPath
    notes: list[str] = field(default_factory=list)

    def tests_status(self) -> str | None:
        """``readLines("../.testsstatus")`` joined, without the final newline; ``None`` without the file."""
        path = self.directory.parent / R_TESTS_STATUS_FILE
        if not self.effects.exists(path):
            return None
        text = self.effects.read_text(path)
        return text[:-1] if text.endswith("\n") else text

    def string(self, name: str) -> str | None:
        """The one string of ``<dir>/<name>`` (``readRDS``); ``None`` and a note when absent or damaged."""
        path = self.directory / name
        if not self.effects.exists(path):
            self.notes.append(f"{name} is absent")
            return None
        try:
            values = vector(read_rds(self.effects.physical_path(path)))
        except (RDataError, OSError) as error:
            self.notes.append(f"{name} could not be read ({error})")
            return None
        if len(values) != 1 or not isinstance(values[0], str):
            self.notes.append(f"{name} does not hold one character string")
            return None
        return values[0]

    def row_names(self, name: str) -> tuple[str, ...] | None:
        """The row names of the data frame in ``<dir>/<name>``; ``None`` and a note when absent or damaged."""
        path = self.directory / name
        if not self.effects.exists(path):
            self.notes.append(f"{name} is absent")
            return None
        try:
            frame = read_rds(self.effects.physical_path(path))
        except (RDataError, OSError) as error:
            self.notes.append(f"{name} could not be read ({error})")
            return None
        index = getattr(frame, "index", None)
        if index is None:
            self.notes.append(f"{name} does not hold a data frame")
            return None
        return tuple(str(row) for row in index)


def migrate_r_state(effects: Effects, directory: PathLike) -> Migration | None:
    """Derive :class:`AmtState` from ``../.testsstatus`` and the RDS files of ``directory``.

    ``None`` when there is no ``.testsstatus`` (nothing to migrate).

    ``next:start`` is ``idle`` (``R/modeltests.R:37``); ``next:evaluate`` is ``started`` (``:43``); the error
    sentence (``:172``) and any other content (``:53``, "Doing nothing") is ``failed`` with the content as the
    message. ``runcode.rds`` and ``lastcommit.rds`` fill ``last_completed`` when idle and the cycle plus
    ``last_completed.tested_commit`` otherwise; ``runsToStart.rds`` fills ``runs_to_start``. A missing or
    unreadable ``runcode.rds`` for a cycle that should exist makes the state ``failed`` without a cycle (there
    is nothing to retry); the other files are optional and only noted. Nothing is written.
    """
    checkout = PurePosixPath(os.fspath(directory))
    files = _RFiles(effects, checkout)
    content = files.tests_status()
    if content is None:
        return None
    source = str(checkout.parent / R_TESTS_STATUS_FILE)
    notes = [f"amt-state.json is absent; deriving the state from {source} ({content!r})"]
    if content == R_NEXT_START:
        state = _migrated_idle(files)
    elif content == R_NEXT_EVALUATE:
        state = _migrated_cycle(files, Phase.STARTED, None)
    else:
        error = LastError(
            command=Command.EVALUATE.value,
            message=f"{source} holds {content!r} (the R version would do nothing)",
        )
        state = _migrated_cycle(files, Phase.FAILED, error)
    notes.extend(files.notes)
    notes.append(f"derived state: {describe(state)}")
    return Migration(state, tuple(notes))


def _migrated_idle(files: _RFiles) -> AmtState:
    runcode = files.string(RUNCODE_FILE)
    commit = files.string(R_LAST_COMMIT_FILE)
    completed = (
        None if runcode is None and commit is None else Completed(tested_commit=commit, runcode=runcode)
    )
    return AmtState(phase=Phase.IDLE, last_completed=completed)


def _migrated_cycle(files: _RFiles, phase: Phase, error: LastError | None) -> AmtState:
    runcode = files.string(RUNCODE_FILE)
    runs_to_start = files.row_names(R_RUNS_TO_START_FILE)
    commit = files.string(R_LAST_COMMIT_FILE)
    completed = None if commit is None else Completed(tested_commit=commit)
    if runcode is None:
        message = f"{RUNCODE_FILE} is missing or unreadable, the running R cycle cannot be derived"
        return AmtState(
            phase=Phase.FAILED,
            last_completed=completed,
            last_error=LastError(command=Command.EVALUATE.value, message=message) if error is None else error,
        )
    cycle = Cycle(id=_cycle_id_of_runcode(runcode), runcode=runcode, runs_to_start=runs_to_start or ())
    return AmtState(phase=phase, cycle=cycle, last_completed=completed, last_error=error)


def _cycle_id_of_runcode(runcode: str) -> str:
    """The start date inside an R pattern (``.*-AMT_2026-09-28|.*-AMT_2026-09-29``), else the pattern."""
    first = runcode.split("|", 1)[0]
    if first.startswith(_RUNCODE_PREFIX):
        candidate = first[len(_RUNCODE_PREFIX) :]
        try:
            return date.fromisoformat(candidate).isoformat()
        except ValueError:
            pass
    return runcode


# ---------------------------------------------------------------------------------------------
# The store: the only place that touches files, through Effects
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True)
class StateStore:
    """``amt-state.json``, ``amt-state.lock`` and ``runcode.rds`` of one REMIND checkout (``--dir``).

    Every access goes through ``effects``; ``directory`` is the checkout as the CLI received it (relative
    paths are resolved by the effects against their working directory, logical paths by an overlay).
    """

    effects: Effects
    directory: PurePosixPath

    @classmethod
    def at(cls, effects: Effects, directory: PathLike) -> StateStore:
        """The store of the checkout ``directory``."""
        return cls(effects, PurePosixPath(os.fspath(directory)))

    @property
    def path(self) -> PurePosixPath:
        """``<dir>/amt-state.json``."""
        return self.directory / AMT_STATE_FILE

    @property
    def lock_path(self) -> PurePosixPath:
        """``<dir>/amt-state.lock``."""
        return self.directory / LOCK_FILE

    @property
    def runcode_path(self) -> PurePosixPath:
        """``<dir>/runcode.rds``."""
        return self.directory / RUNCODE_FILE

    def exists(self) -> bool:
        """Whether the state file has been written."""
        return self.effects.exists(self.path)

    def load(self) -> AmtState | None:
        """The state file parsed, ``None`` when there is none; a damaged file is :class:`AmtStateError`."""
        if not self.exists():
            return None
        return AmtState.from_json(self.effects.read_text(self.path), source=str(self.path))

    def load_or_migrate(self, note: Callable[[str], None] | None = None) -> AmtState:
        """The state file, else the state derived once from the R files and saved, else a fresh ``idle``.

        ``note`` receives one line per derived fact (the caller logs them, ``06`` 3.2: "says so in the
        log"). A fresh checkout without any R state is not written until the first transition.
        """
        loaded = self.load()
        if loaded is not None:
            return loaded
        migration = migrate_r_state(self.effects, self.directory)
        if migration is None:
            return AmtState()
        for line in migration.notes:
            if note is not None:
                note(line)
        self.save(migration.state)
        if note is not None:
            note(f"the derived state is kept in {self.path}; the R files are left untouched")
        return migration.state

    def save(self, state: AmtState) -> None:
        """Write the state atomically (``Effects.write_text``: temp file and rename in the directory)."""
        self.effects.write_text(self.path, state.to_json())

    def save_started(self, state: AmtState) -> None:
        """``start``'s persist step: the state, then ``runcode.rds`` with the cycle's pattern (``06`` 3.4)."""
        if state.phase is not Phase.STARTED or state.cycle is None:
            raise AmtStateError(f"save_started needs a 'started' state with a cycle, got {describe(state)}")
        self.save(state)
        self.effects.write_bytes(self.runcode_path, rds_bytes(state.cycle.runcode))

    @contextmanager
    def locked(self, *, wait: bool = True) -> Iterator[None]:
        """Hold the exclusive ``flock`` on ``amt-state.lock`` for the whole command (``06`` 3.2).

        ``wait=False`` is ``--no-wait``: a held lock is :class:`AmtLockError` instead of blocking.
        """
        with ExitStack() as stack:
            try:
                stack.enter_context(self.effects.lock(self.lock_path, wait=wait))
            except LockHeldError as error:
                raise AmtLockError(
                    f"another modeltests command holds {self.lock_path}; not waiting (--no-wait)"
                ) from error
            yield
