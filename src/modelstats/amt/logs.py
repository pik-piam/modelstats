"""One log per AMT cycle: the cycle log setup and the subprocess line sink (``06`` section 3.3).

``migration/06-user-requests.md`` section 3.3 (request 3). The R version got one log per *stage* from the
bash redirects of the cron script, so a start and its evaluate were two files and two overlapping
invocations interleaved. Here ``start`` creates ``<log-dir>/amt-<cycle id>.log`` (:func:`cycle_log_path`),
``evaluate`` appends to it and ``run`` follows; the path is recorded in the state file
(``modelstats.amt.state.Cycle.log``) so ``modeltests status`` can print it.

:func:`setup_cycle_log` configures Python ``logging``: one ``FileHandler`` in append mode and one
``StreamHandler`` on stderr (cron mail and ``nohup`` keep seeing everything), a formatter putting the
timestamp and the command (``start``/``evaluate``) on every line, timestamps from an injectable clock
(``Effects.now``, so a frozen sandbox clock shows in the log). The R ``message()`` lines of
``R/modeltests.R`` become ``INFO`` records. Subprocess output (``Rscript start.R``, ``make``, ``git``,
``sbatch``, ``rsync``, ``squeue``) arrives line by line through :func:`line_sink`, the ``on_line`` callback
of ``Effects.run``, which writes both pipes to the same log with a ``[<tool>]`` prefix; :func:`run_logged`
is ``Effects.run`` with the command announced, every line sunk and a non-zero status reported, where the R
code let ``system()`` print to the inherited streams (``R/modeltests.R:98-146, 182-201, 310, 355, 435-443``).

A line looks like::

    2026-09-30 22:04:11 start    INFO     Configuring and starting single testOneRegi-AMT
    2026-09-30 22:04:12 start    INFO     [git] HEAD is now at 3f5e2a1 Merge pull request #2041
"""

from __future__ import annotations

import logging
import os
import shlex
import sys
from collections.abc import Callable, Iterator, Mapping, Sequence
from contextlib import contextmanager
from datetime import datetime
from pathlib import Path
from typing import Final, TextIO

from modelstats.env import CommandFailedError, Effects, LineCallback, PathLike, RunResult, Stream

__all__ = [
    "LOGGER_NAME",
    "LOG_FILE_TEMPLATE",
    "CycleFormatter",
    "close_cycle_log",
    "cycle_log",
    "cycle_log_path",
    "line_sink",
    "run_logged",
    "setup_cycle_log",
    "tool_name",
]

LOGGER_NAME: Final = "modelstats.amt"
"""The logger every AMT module uses (``logging.getLogger(LOGGER_NAME)``)."""

LOG_FILE_TEMPLATE: Final = "amt-{cycle_id}.log"
"""``<log-dir>/amt-<cycle id>.log`` (``06`` 3.3)."""

LINE_FORMAT: Final = "%(asctime)s %(command)-8s %(levelname)-8s %(message)s"
"""Timestamp, the command padded to ``evaluate``'s width, the level, the message."""

TIME_FORMAT: Final = "%Y-%m-%d %H:%M:%S"
"""The timestamp as the R messages printed it (``R/modeltests.R:34, 179, 195``)."""

_ENCODING: Final = "utf-8"
_ENCODING_ERRORS: Final = "backslashreplace"
_MOMENT_ATTRIBUTE: Final = "cycle_moment"


def cycle_log_path(log_dir: Path, cycle_id: str) -> Path:
    """``<log-dir>/amt-<cycle id>.log``."""
    return log_dir / LOG_FILE_TEMPLATE.format(cycle_id=cycle_id)


class CycleFormatter(logging.Formatter):
    """The line format with the command on every record and timestamps from an injected clock.

    ``clock`` replaces the record's creation time (``time.time()``) so that a frozen ``Effects.now`` is
    what the log shows; without it the wall clock in the local zone is used.
    """

    def __init__(self, command: str, clock: Callable[[], datetime] | None = None) -> None:
        """``command`` is ``start`` or ``evaluate``; ``clock`` an aware-datetime source (``Effects.now``)."""
        super().__init__(LINE_FORMAT, datefmt=TIME_FORMAT)
        self._command = command
        self._clock = clock

    def format(self, record: logging.LogRecord) -> str:
        """Stamp the command and the moment onto the record, then format as usual (tracebacks included).

        The clock is read once per record, at the first handler that formats it, so the file and the
        stderr mirror show the same timestamp.
        """
        record.command = self._command
        if self._clock is not None and not hasattr(record, _MOMENT_ATTRIBUTE):
            setattr(record, _MOMENT_ATTRIBUTE, self._clock())
        return super().format(record)

    def formatTime(self, record: logging.LogRecord, datefmt: str | None = None) -> str:  # noqa: N802
        """The stamped moment, else the record's creation time, in :data:`TIME_FORMAT`."""
        stamped = getattr(record, _MOMENT_ATTRIBUTE, None)
        moment = stamped if isinstance(stamped, datetime) else datetime.fromtimestamp(record.created)
        return moment.strftime(datefmt or TIME_FORMAT)


def setup_cycle_log(
    log_path: Path | None,
    command: str,
    *,
    clock: Callable[[], datetime] | None = None,
    stream: TextIO | None = None,
    level: int = logging.INFO,
) -> logging.Logger:
    """The AMT logger writing to ``log_path`` (appended, created with its directory) and to ``stream``.

    ``stream`` defaults to the process's standard error at call time. ``log_path`` ``None`` means stderr
    only (``--dry-run`` may use it). Handlers configured by an earlier call are closed and replaced, so a
    process serving two commands in a row (``run`` round trips in tests) still writes one file in order;
    the logger does not propagate to the root logger, so nothing is printed twice.
    """
    logger = logging.getLogger(LOGGER_NAME)
    close_cycle_log(logger)
    logger.setLevel(level)
    logger.propagate = False
    formatter = CycleFormatter(command, clock)
    handlers: list[logging.Handler] = [logging.StreamHandler(stream if stream is not None else sys.stderr)]
    if log_path is not None:
        log_path.parent.mkdir(parents=True, exist_ok=True)
        handlers.insert(
            0, logging.FileHandler(log_path, mode="a", encoding=_ENCODING, errors=_ENCODING_ERRORS)
        )
    for handler in handlers:
        handler.setFormatter(formatter)
        logger.addHandler(handler)
    return logger


def close_cycle_log(logger: logging.Logger) -> None:
    """Flush, close and remove every handler of ``logger`` (the file is complete when the command ends)."""
    for handler in list(logger.handlers):
        logger.removeHandler(handler)
        handler.close()


@contextmanager
def cycle_log(
    log_path: Path | None,
    command: str,
    *,
    clock: Callable[[], datetime] | None = None,
    stream: TextIO | None = None,
    level: int = logging.INFO,
) -> Iterator[logging.Logger]:
    """:func:`setup_cycle_log` as a context manager that closes the handlers on exit."""
    logger = setup_cycle_log(log_path, command, clock=clock, stream=stream, level=level)
    try:
        yield logger
    finally:
        close_cycle_log(logger)


def tool_name(argv: Sequence[str]) -> str:
    """The ``<tool>`` of the prefix: the program's basename (``Rscript``, ``make``, ``git``, ``squeue``)."""
    return os.path.basename(argv[0]) if argv else ""


def line_sink(logger: logging.Logger, tool: str, *, level: int = logging.INFO) -> LineCallback:
    """The ``on_line`` callback of ``Effects.run``: each line of either pipe logged as ``[<tool>] <line>``.

    Both pipes land in the one log at ``level`` in arrival order (``git`` and ``make`` write their progress
    to stderr; the exit status, not the pipe, says whether something went wrong). One trailing line break
    is dropped (``Effects.run`` splits the output into lines and the record adds its own); anything else
    stays as the program printed it.
    """

    def sink(_stream: Stream, line: str) -> None:
        logger.log(level, "[%s] %s", tool, line.removesuffix("\n").removesuffix("\r"))

    return sink


def run_logged(
    effects: Effects,
    logger: logging.Logger,
    argv: Sequence[str],
    *,
    cwd: PathLike | None = None,
    env: Mapping[str, str] | None = None,
    check: bool = False,
) -> RunResult:
    """``Effects.run`` with the command announced, both pipes sunk into the log and a failure reported.

    The announcement is ``running: <command>`` (``shlex.join``, with ``in <cwd>`` when one is given), the
    lines follow with the ``[<tool>]`` prefix, and a non-zero status is a ``WARNING`` record (``<tool> exited
    with status N``) before the result is returned, or before ``check`` raises ``CommandFailedError``. The R
    code ran these through ``system()`` and let the child print to the cron redirect (``06`` 3.3).
    """
    tool = tool_name(argv)
    where = f" in {os.fspath(cwd)}" if cwd is not None else ""
    logger.info("running: %s%s", shlex.join(argv), where)
    result = effects.run(argv, cwd=cwd, env=env, check=False, on_line=line_sink(logger, tool))
    if not result.ok:
        logger.warning("%s exited with status %d", tool, result.returncode)
        if check:
            raise CommandFailedError(result)
    return result
