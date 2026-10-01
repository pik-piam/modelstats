"""Automated model tests (AMT): the ``modeltests`` port of ``R/modeltests.R`` (phase 5).

:func:`modeltests` is the cron entry point (lines 24-55): it enters ``mydir`` (``withr::local_dir``), prints the
banner, reads ``../.testsstatus`` and dispatches to :func:`~modelstats.amt.start.start_runs` (``next:start``) or
:func:`~modelstats.amt.evaluate.evaluate_runs` inside ``output/`` (``next:evaluate``), writing the next state
afterwards, or reports ``Doing nothing``. Every effect goes through :class:`~modelstats.env.Effects`; the
``message()`` lines go to ``sys.stderr`` resolved at call time. The submodules are ``start``, ``evaluate``,
``readme``, ``state``, ``bridges``, ``notify`` and ``cli`` (console script ``modeltests``).

R facts reproduced here (R 4.6.1, verified 2026-10-01 against the amt goldens and ``Rscript`` probes):

- ``readLines("../.testsstatus")`` of a missing file warns ``cannot open file '../.testsstatus': No such file or
  directory`` (call ``file(con, "r")``) and fails with ``cannot open the connection`` (same call); a file without a
  final newline adds the deferred warning ``incomplete final line found on '../.testsstatus'`` (call
  ``readLines("../.testsstatus")``) at every read, and R reads the file once per ``readLines`` call (up to three
  times in this function);
- ``if (readLines(...) == "next:start")`` on an empty file is ``argument is of length zero`` and on a file with
  two or more lines ``the condition has length > 1`` (call ``if (readLines("../.testsstatus") == "next:start") {``);
- ``normalizePath("../.testsstatus")`` is the resolved absolute path (the file exists at that point);
- ``message(a, b, ...)`` pastes its arguments with no separator and appends a newline; the banner message therefore
  prints an empty line, the two ``=`` lines around ``Begin of AMT procedure <%Y-%m-%d %H:%M:%S> in <mydir>`` and an
  empty line, ``mydir`` pasted as given (the cron job passes it with a trailing slash, BUG-013);
- ``withr::local_dir`` / ``with_dir`` fail with ``cannot change working directory`` (call ``setwd(dir = new)``).
"""

from __future__ import annotations

import contextlib
import os
import sys
import warnings
from collections.abc import Iterator

from modelstats.amt.evaluate import evaluate_runs
from modelstats.amt.start import start_runs
from modelstats.env import Effects, PathLike, default_effects
from modelstats.errors import RParityError, RWarning
from modelstats.slurm import normalize_path

__all__ = [
    "BANNER",
    "NEXT_EVALUATE",
    "NEXT_START",
    "OUTPUT_DIR",
    "STATUS_FILE",
    "modeltests",
    "read_status_lines",
]

#: Line 37 / 43 / 42 / 51: the two states of ``../.testsstatus`` the dispatcher knows.
NEXT_START = "next:start"
NEXT_EVALUATE = "next:evaluate"
#: ``"../.testsstatus"`` relative to ``mydir`` (R uses the literal relative path throughout).
STATUS_FILE = "../.testsstatus"
#: ``withr::with_dir("output", ...)`` of line 45.
OUTPUT_DIR = "output"
BANNER = "========================================================="

_CALL_FILE = 'file(con, "r")'
_CALL_READLINES = f'readLines("{STATUS_FILE}")'
_CALL_IF = f'if (readLines("{STATUS_FILE}") == "{NEXT_START}") {{'
_CALL_SETWD = "setwd(dir = new)"
_ERR_CANNOT_OPEN = "cannot open the connection"
_ERR_ZERO_LENGTH = "argument is of length zero"
_ERR_LENGTH_GT_1 = "the condition has length > 1"
_ERR_CHDIR = "cannot change working directory"


def _message(*parts: object) -> None:
    """``message(...)``: the pasted parts plus a newline on stderr (resolved at call time)."""
    sys.stderr.write("".join(str(part) for part in parts) + "\n")
    sys.stderr.flush()


def _effects(effects: Effects | None) -> Effects:
    return default_effects() if effects is None else effects


def _r_lines(text: str) -> list[str]:
    """``readLines()`` line splitting: LF, CRLF and CR end a line, an incomplete last line counts."""
    if text == "":
        return []
    lines = text.replace("\r\n", "\n").replace("\r", "\n").split("\n")
    if lines[-1] == "":
        lines.pop()
    return lines


def read_status_lines(effects: Effects | None = None) -> list[str]:
    """``readLines("../.testsstatus")`` relative to the current working directory, with R's conditions.

    A missing file raises ``RParityError('cannot open the connection')`` after the deferred warning R emits;
    a final line without a newline adds R's ``incomplete final line`` warning. Returns the lines.
    """
    eff = _effects(effects)
    try:
        text = eff.read_text(STATUS_FILE)
    except OSError as exc:
        reason = exc.strerror or "No such file or directory"
        warnings.warn(RWarning(_CALL_FILE, f"cannot open file '{STATUS_FILE}': {reason}"), stacklevel=2)
        raise RParityError(_ERR_CANNOT_OPEN, call=_CALL_FILE) from exc
    if text and not text.endswith(("\n", "\r")):
        warnings.warn(RWarning(_CALL_READLINES, f"incomplete final line found on '{STATUS_FILE}'"), stacklevel=2)
    return _r_lines(text)


def _status_is(expected: str, effects: Effects) -> bool:
    """``readLines("../.testsstatus") == <expected>`` as an ``if`` condition, with R's length checks."""
    lines = read_status_lines(effects)
    if len(lines) == 0:
        raise RParityError(_ERR_ZERO_LENGTH, call=_CALL_IF)
    if len(lines) > 1:
        raise RParityError(_ERR_LENGTH_GT_1, call=_CALL_IF)
    return lines[0] == expected


@contextlib.contextmanager
def _enter(effects: Effects, path: str) -> Iterator[None]:
    """``withr::local_dir`` / ``with_dir``: ``Effects.chdir`` with R's call text when the directory is not enterable."""
    with contextlib.ExitStack() as stack:
        try:
            stack.enter_context(effects.chdir(path))
        except RParityError as exc:
            if str(exc) == _ERR_CHDIR:
                raise RParityError(_ERR_CHDIR, call=_CALL_SETWD) from exc
            raise
        yield


def modeltests(
    mydir: PathLike = ".",
    gitdir: PathLike | None = None,
    model: str | None = None,
    user: str | None = None,
    email: bool = True,
    comp_scen: bool = True,
    mattermost_token: str | None = None,
    effects: Effects | None = None,
) -> None:
    """``modeltests(mydir, gitdir, model, user, email, compScen, mattermostToken)``, ``R/modeltests.R`` lines 24-55.

    Runs with the process working directory set to ``mydir`` for the whole call (``withr::local_dir``) and inside
    ``output/`` for the evaluation. ``mydir`` is pasted into the messages and handed to the two steps as given
    (the cron job passes a trailing slash, which the state-file paths of the steps rely on, BUG-013). R errors
    escape as :class:`~modelstats.errors.RParityError` (with R's call where R reports one); the deferred R
    warnings are raised with ``warnings.warn`` as the steps produce them.
    """
    eff = _effects(effects)
    mydir_text = os.fspath(mydir)
    with _enter(eff, mydir_text):  # withr::local_dir(mydir), line 31
        _message(
            f"\n{BANNER}\n",
            "Begin of AMT procedure ",
            eff.now().strftime("%Y-%m-%d %H:%M:%S"),
            " in ",
            mydir_text,
            f"\n{BANNER}\n",
        )
        if _status_is(NEXT_START, eff):
            _message(f"Found '{NEXT_START}' in ", normalize_path(STATUS_FILE, eff), "\nCalling 'startRuns'")
            start_runs(model, mydir_text, user, effects=eff)
            # make sure next call will evaluate runs
            _message(f"Writing '{NEXT_EVALUATE}' to ", normalize_path(STATUS_FILE, eff))
            eff.write_text(STATUS_FILE, NEXT_EVALUATE + "\n")
        elif _status_is(NEXT_EVALUATE, eff):
            _message(f"Found '{NEXT_EVALUATE}' in ", normalize_path(STATUS_FILE, eff), "\nCalling 'evaluateRuns'")
            with _enter(eff, OUTPUT_DIR):  # withr::with_dir("output", ...), line 45
                evaluate_runs(
                    model,
                    mydir_text,
                    comp_scen,
                    email,
                    mattermost_token,
                    None if gitdir is None else os.fspath(gitdir),
                    user,
                    effects=eff,
                )
            # make sure next call will start runs
            _message(f"Writing '{NEXT_START}' to ", normalize_path(STATUS_FILE, eff))
            eff.write_text(STATUS_FILE, NEXT_START + "\n")
        else:
            content = "".join(read_status_lines(eff))  # message() pastes the vector elements without a separator
            _message("Found '", content, "' in ", normalize_path(STATUS_FILE, eff), ". Doing nothing")
