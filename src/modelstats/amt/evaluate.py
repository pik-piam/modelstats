"""``evaluateRuns`` of ``R/modeltests.R`` (lines 167-491), line by line, behind the Effects boundary (plan 03 2.3).

The function runs with the working directory set to ``<mydir>/output`` by ``modeltests()`` (``withr::local_dir(mydir)``
and ``withr::with_dir("output", ...)``, lines 31 and 45), so every relative path here is relative to that directory,
exactly as in R. ``mydir`` is pasted as R pastes it: ``paste0(mydir, "../.testsstatus")``,
``paste0(mydir, "/lastcommit.rds")`` and ``paste0(mydir, "runsToStart.rds")`` all expect the trailing slash the cron
script passes (BUG-013, kept).

What each part of the R function becomes here:

- ``message()`` is a line on ``sys.stderr``; ``cat(cs2com, "\\n")`` and ``print(oldRuns)`` go to ``sys.stdout``, both
  resolved at call time (captures and redirects work);
- ``system(cmd, intern = TRUE)`` is :func:`system_intern` (``Effects.run_shell`` of R's exact command string: the
  stdout lines, the child's stderr passed through, status 127 the error ``error in running command``, any other
  non-zero status the deferred warning ``running command '<cmd>' had status <n>``) and ``system(cmd)`` is
  :func:`system_run` (``capture=False``: the child inherits the process streams, only 127 warns);
- R's deferred warnings are :class:`~modelstats.errors.RWarning` conditions raised with ``warnings.warn``; the
  function collects them while it runs and re-raises them for its caller at the end (the ``modeltests`` CLI prints
  them as ``Rscript`` does), because the one ``try()`` of the function (line 230) prints and clears every warning
  deferred so far in its ``In addition:`` block (``printDeferredWarnings``), as the ``remind-evaluate-git-merges-empty``
  and ``squeue-3fail`` goldens show;
- ``writeLines`` / ``readRDS`` / ``saveRDS`` / ``file.copy`` / ``file.rename`` / ``Sys.sleep`` / ``setwd`` go through
  ``Effects`` (:mod:`modelstats.amt.state` for the RDS files); the README accumulates in
  :class:`~modelstats.amt.readme.Readme` and is flushed to ``<tempdir()>/README.md`` after every ``write()`` step, so
  a failure leaves the partial README R leaves; ``magpie4::addToDataChangelog`` is the
  :func:`~modelstats.amt.bridges.add_to_data_changelog` bridge; the Mattermost message is
  :func:`~modelstats.amt.notify.send_notification`.

R facts reproduced here (R 4.6.1, verified 2026-10-01; the unit tests pin them):

- ``system(intern = TRUE)[[1]]`` of an empty output is ``subscript out of bounds`` (call ``system("git log -1",
  intern = TRUE)[[1]]``); ``if (NULL != "MAgPIE")`` is ``argument is of length zero``; ``stop()`` inside the function
  reports the call ``evaluateRuns(model = model, mydir = mydir, compScen = compScen, `` (the first deparse line);
- ``max(character(0))`` is ``NA_character_`` with the warning ``no non-missing arguments, returning NA`` (BUG-014), so
  ``lastRun`` becomes ``NA``, ``file.path("..", NA)`` is ``../NA``, the archive fallback gives ``archive/NA``,
  ``normalizePath`` warns ``path[1]="../archive/NA": No such file or directory`` and ``.readRuntime()`` fails on
  ``load()`` with ``cannot open the connection`` (call ``readChar(con, 5L, useBytes = TRUE)``, after the warning
  ``cannot open compressed file '<path>', probable reason '...'``);
- ``stats$timeGAMSEnd - stats$timeGAMSStart``: ``NULL - POSIXct`` is the error ``can only subtract from "POSIXt"
  objects`` (call ```-.POSIXt`(stats$timeGAMSEnd, stats$timeGAMSStart)``), ``POSIXct - NULL`` and ``NULL - NULL``
  have length zero (the ``length() > 0`` guards skip the comparison), an ``NA`` POSIXct gives an ``NA`` difftime and
  ``if (NA)`` fails with ``missing value where TRUE/FALSE needed``; ``as.numeric(difftime, units = "hours")`` first
  picks the automatic unit (secs/mins/hours/days by magnitude) and then multiplies by ``sc[from] / sc[to]``;
- strings compare and ``max()`` by the session collation (ICU root under the sandbox's ``C.utf8``,
  :func:`modelstats.env.r_collate_key`);
- ``as.Date(x)`` decides the format on the first element (``%Y-%m-%d`` then ``%Y/%m/%d``; ``%Y`` takes one to four
  digits, trailing text is ignored, an invalid date is ``NA``) and fails with ``character string is not in a standard
  unambiguous format`` (call ``charToDate(x)``) when the first element parses with neither; later elements that do not
  parse are ``NA`` and ``dplyr::filter`` drops them;
- ``readLines`` accepts LF, CRLF and CR, a terminated empty line is an element, a trailing terminator adds none;
  ``tail()`` keeps six; ``isTRUE(grepl(...))`` on two or more lines is ``FALSE`` (``did not run properly``);
- ``print(<character>)`` pads every element to the common quoted width (trailing blanks included), prefixes the lines
  with right-justified ``[k]`` labels and wraps at ``getOption("width")`` = 80 (``Rscript`` does not read ``COLUMNS``);
- ``!grepl(p, NA)`` is ``NA``; ``NA && FALSE`` is ``FALSE``, ``NA && TRUE`` is ``NA`` and ``if (NA)`` fails with
  ``missing value where TRUE/FALSE needed``; ``NA %in% x`` is ``FALSE``; ``&&`` does not evaluate its right side
  after a ``FALSE`` left side (so ``grsi[, "Conv"]`` is only read when needed); ``grsi[, "<absent>"]`` is
  ``undefined columns selected``;
- ``paste0`` drops a ``NULL`` (``user = NULL`` gives ``squeue -u  -h -o ...``, ``cfg$title = NULL`` an empty title);
  ``file.copy(character(0), to)`` does nothing (a ``NULL`` gitdir); ``setwd(NULL)`` is ``character argument expected``;
- ``file.rename()`` of a failing move returns ``FALSE`` with the warning ``cannot rename file '<from>' to '<to>', reason
  '<strerror>'`` and the function continues; ``system()`` ignores the exit status of ``sbatch``, ``rsync``, ``mv`` and
  the ``git`` calls of the e-mail step (only 127 warns ``error in running command``).
"""

from __future__ import annotations

import dataclasses
import datetime as dt
import math
import os
import re
import sys
import warnings
from collections.abc import Callable, Collection, Mapping, Sequence

from modelstats.amt import readme as readme_mod
from modelstats.amt import state
from modelstats.amt.bridges import add_to_data_changelog
from modelstats.amt.notify import send_notification
from modelstats.config import load_config
from modelstats.env import Effects, PathLike, default_effects, r_collate_key
from modelstats.errors import RParityError, RWarning
from modelstats.formatting import r_num_str
from modelstats.loop_runs import r_try_message, r_warnings_text
from modelstats.rdata_io import read_rda
from modelstats.run_status import RunStatus, StatusTable, get_run_status
from modelstats.runfolders import r_basename
from modelstats.runstats import RunStatistics
from modelstats.slurm import normalize_path, r_regex
from modelstats.textscan import r_intern_lines

__all__ = [
    "ARCHIVE_DAYSBACK",
    "CONVERGED",
    "CONVERGED_OR_NOT",
    "ERR_NOT_CONVERGED",
    "ERR_NOT_REPORTED",
    "ERR_NO_RUNS",
    "ERR_SLOWER",
    "ERR_SUM_ERR",
    "ERR_TEST_ONE_REGI",
    "GDX_ON_RSE_SERVER",
    "R_PRINT_WIDTH",
    "SQUEUE_FAILED",
    "TESTFULL_NOT_FOUND",
    "EvaluateResult",
    "archive_candidates",
    "cs2_command",
    "difftime_hours",
    "evaluate_runs",
    "r_as_date",
    "r_print_character",
    "r_read_lines",
    "read_runtime_hours",
    "runs_started_magpie",
    "runs_started_remind",
    "system_intern",
    "system_run",
    "wait_for_runs",
]

# ---------------------------------------------------------------------------
# texts of R/modeltests.R
# ---------------------------------------------------------------------------

#: Line 172.
RUNNING_STATUS = "evaluateRuns() is running or stopped due to an error"
#: Line 186.
SQUEUE_FAILED = "squeue had exit status > 0 more than 3 times in a row."
#: Lines 282, 285, 288, 291, 298, 330, 421.
ERR_NOT_CONVERGED = "Some run(s) did not converge"
ERR_TEST_ONE_REGI = "testOneRegi does not return an optimal solution"
ERR_SUM_ERR = "Summation checks for some run(s) revealed some gaps"
ERR_NOT_REPORTED = "Some run(s) did not report correctly"
ERR_SLOWER = "Check runtime! Have some scenarios become slower?"
ERR_NO_RUNS = "No runs started"
#: Line 384.
TESTFULL_NOT_FOUND = "Could not check for the results of `make test-full`, test-full.log not found"
#: Lines 307-308.
GDX_ON_RSE_SERVER = "rse@rse.pik-potsdam.de:/webservice/data/example/remind2_test-convGDX2MIF_SSP2-NPi-AMT.gdx"
#: Line 401.
ARCHIVE_DAYSBACK = 90
#: Lines 281, 301, 306, 314, 326.
CONVERGED: tuple[str, ...] = ("converged", "converged (had INFES)")
CONVERGED_OR_NOT: tuple[str, ...] = ("converged", "converged (had INFES)", "not_converged")
#: ``getOption("width")`` of ``Rscript`` (``COLUMNS`` is not consulted), the wrap width of ``print(oldRuns)``.
R_PRINT_WIDTH = 80

_SQUEUE_TEN = "%i %q %T %C %M %j %V %L %e %Z"
_WAIT_SECONDS = 600
_THREE_DAYS = 3

# Deparsed R calls (first deparse line, as R prints them in ``Error in`` / ``In <call> :``), pinned with R 4.6.1.
_CALL_EVALUATE_RUNS = "evaluateRuns(model = model, mydir = mydir, compScen = compScen, "
_CALL_SQUEUE = 'system(paste0("squeue -u ", user, " -h -o \'%i %q %T %C %M %j %V %L %e %Z\'"), '
_CALL_GIT_LOG_1 = 'system("git log -1", intern = TRUE)'
_CALL_GIT_LOG_1_SUBSCRIPT = 'system("git log -1", intern = TRUE)[[1]]'
_CALL_GIT_MERGES = 'system(paste0("git log --merges --pretty=oneline ", lastCommit, '
_CALL_GREPL_MYDIR = "grepl(mydir, jobsInSlurm)"
_CALL_GREP_RUNCODE = "grep(runcode, list.dirs(full.names = FALSE, recursive = FALSE), value = TRUE)"
_CALL_RSYNC = 'system(paste("rsync -e ssh -av fulldata.gdx", gdxOnRseServer))'
_CALL_CS2 = "system(cs2com)"
_CALL_MV = 'system(paste("mv", paste(oldRuns, collapse = " "), "archive"))'
_CALL_MAX = "max(sameRuns[sameRuns < basename(cfg$results_folder)])"
_CALL_GREPL_TITLE = "grepl(cfg$title, rownames(gRS))"
_CALL_NORMALIZE_LAST = 'normalizePath(file.path("..", lastRun))'
_CALL_NORMALIZE_NEW = 'normalizePath(paste0("../", newName))'
_CALL_FILE_RENAME = 'file.rename(from = currentName, to = paste0("../", newName))'
_CALL_READCHAR = "readChar(con, 5L, useBytes = TRUE)"
_CALL_LOAD_RUNSTATS = 'load(paste0(x, "/runstatistics.rda"))'
_CALL_LOAD_CONFIG = 'load("config.Rdata")'
_CALL_MINUS_POSIXT = "`-.POSIXt`(stats$timeGAMSEnd, stats$timeGAMSStart)"
_CALL_SETWD = "setwd(dir = new)"
_CALL_CHAR_TO_DATE = "charToDate(x)"
_CALL_GIT_RESET = 'system("git reset --hard origin/master")'
_CALL_GIT_PULL = 'system("git pull")'
_CALL_GIT_ADD_README = 'system("git add README.md")'
_CALL_GIT_ADD_CHANGELOG = 'system("git add data-changelog.csv")'
_CALL_GIT_COMMIT = "system(\"git commit -m 'Automated Test Results'\")"
_CALL_GIT_PUSH = 'system("git push")'

_ERR_NA_CONDITION = "missing value where TRUE/FALSE needed"
_ERR_ZERO_LENGTH = "argument is of length zero"
_ERR_UNDEFINED_COLUMNS = "undefined columns selected"
_ERR_SUBSCRIPT = "subscript out of bounds"
_ERR_CANNOT_RUN = "error in running command"
_ERR_CANNOT_OPEN = "cannot open the connection"
_ERR_SUBTRACT_POSIXT = 'can only subtract from "POSIXt" objects'
_ERR_CHAR_EXPECTED = "character argument expected"
_ERR_DATE_FORMAT = "character string is not in a standard unambiguous format"
_ERR_PATTERN = "invalid 'pattern' argument"

_COMP_WITH_PDF = re.compile(r"comp_with_.*.pdf")
_AMT_NAME = re.compile(r".*AMT.*")
_RUN_DATE = re.compile(r".*_([0-9]{4}-[0-9]{2}-[0-9]{2})_.*$")
_FAIL_LINE = re.compile(r"\[ FAIL")
_DATE_DASH = re.compile(r"^([0-9]{1,4})-([0-9]{1,2})-([0-9]{1,2})")
_DATE_SLASH = re.compile(r"^([0-9]{1,4})/([0-9]{1,2})/([0-9]{1,2})")
_CALIB_OR_TEST_ONE_REGI = re.compile("Calib_nash|testOneRegi")
_TEST_ONE_REGI = re.compile("testOneRegi")
_SSP2_NPI_AMT = re.compile("SSP2-NPi-AMT")


@dataclasses.dataclass(frozen=True)
class EvaluateResult:
    """What ``evaluateRuns`` computed (R returns the invisible ``NULL`` of its last ``message()``).

    ``runs_not_started`` is ``None`` when the block of lines 370-378 was not written (R: ``exists("runsNotStarted")``
    is ``FALSE``); ``message`` is the Mattermost text that was sent, ``None`` when nothing was sent.
    """

    commit_tested: str
    runs_started: list[str]
    error_list: list[str]
    summary: str
    testthat_result: str | None
    runs_not_started: list[str] | None
    readme_path: str
    message: str | None


# ---------------------------------------------------------------------------
# streams and conditions
# ---------------------------------------------------------------------------


def _message(*parts: object) -> None:
    """``message(...)``: the pasted parts and a newline on stderr."""
    sys.stderr.write("".join(str(part) for part in parts) + "\n")
    sys.stderr.flush()


def _stdout(text: str) -> None:
    sys.stdout.write(text)
    sys.stdout.flush()


def _warn(call: str, text: str) -> None:
    """A deferred R warning (``Warning message: In <call> : <text>``)."""
    warnings.warn(RWarning(call, text), stacklevel=3)


def _effects(effects: Effects | None) -> Effects:
    return default_effects() if effects is None else effects


def _r_warnings(caught: Sequence[warnings.WarningMessage]) -> list[warnings.WarningMessage]:
    return [item for item in caught if isinstance(item.message, RWarning)]


def _r_try[T](deferred: list[warnings.WarningMessage], thunk: Callable[[], T]) -> T | None:
    """``try(expr)``: the value, or ``None`` after printing R's error text and the deferred warnings to stderr.

    ``try()`` prints ``Error in <call> : <message>`` and then ``printDeferredWarnings()``: every warning deferred
    so far (not only those raised inside) in an ``In addition:`` block, and clears them.
    """
    try:
        return thunk()
    except Exception as exc:  # noqa: BLE001 - try() catches every R error condition
        text = r_try_message(exc) + r_warnings_text(_r_warnings(deferred), in_addition=True)
        sys.stderr.write(text)
        sys.stderr.flush()
        del deferred[:]
        return None


# ---------------------------------------------------------------------------
# system()
# ---------------------------------------------------------------------------


def system_intern(command: str, call: str, effects: Effects | None = None, cwd: PathLike | None = None) -> list[str]:
    """``system(command, intern = TRUE)`` for R's exact command string (``/bin/sh`` splits it).

    Returns the stdout lines (:func:`modelstats.textscan.r_intern_lines`); the child's stderr is passed through
    to ``sys.stderr``. Status 127 raises ``RParityError('error in running command')`` with ``call``; any other
    non-zero status raises the deferred warning ``running command '<command>' had status <n>``.
    """
    proc = _effects(effects).run_shell(command, cwd=cwd)
    if proc.stderr:
        sys.stderr.write(proc.stderr)
        sys.stderr.flush()
    status = proc.returncode
    if status == 127:
        raise RParityError(_ERR_CANNOT_RUN, call=call)
    if status > 0:
        _warn(call, f"running command '{command}' had status {status}")
    return r_intern_lines(proc.stdout)


def system_run(command: str, call: str, effects: Effects | None = None, cwd: PathLike | None = None) -> int:
    """``system(command)``: the child inherits stdout and stderr, the status comes back and is ignored by R.

    Only a command that cannot be run (status 127) raises the deferred warning ``error in running command``.
    """
    proc = _effects(effects).run_shell(command, cwd=cwd, capture=False)
    if proc.returncode == 127:
        _warn(call, _ERR_CANNOT_RUN)
    return int(proc.returncode)


# ---------------------------------------------------------------------------
# R primitives
# ---------------------------------------------------------------------------


def r_read_lines(text: str) -> list[str]:
    """``readLines(con, warn = FALSE)`` of a file's text: LF, CRLF and CR end a line, a final partial line counts."""
    if text == "":
        return []
    parts = re.split(r"\r\n|\r|\n", text)
    if parts[-1] == "":
        parts.pop()
    return parts


def _tail(values: Sequence[str], n: int = 6) -> list[str]:
    """``tail(x)``: the last six elements."""
    return list(values[-n:]) if n > 0 else []


def _r_strptime_date(text: str, pattern: re.Pattern[str]) -> dt.date | None:
    """``as.Date(strptime(text, format, tz = "GMT"))`` for ``%Y-%m-%d`` or ``%Y/%m/%d``: ``None`` is ``NA``.

    ``%Y`` reads one to four digits, ``%m`` and ``%d`` one or two, text after the day is ignored and an impossible
    date is ``NA``.
    """
    match = pattern.match(text)
    if match is None:
        return None
    try:
        return dt.date(int(match.group(1)), int(match.group(2)), int(match.group(3)))
    except ValueError:
        return None


def r_as_date(texts: Sequence[str]) -> list[dt.date | None]:
    """``as.Date(<character>)`` with the default ``tryFormats``: the format is chosen on the first element.

    Raises ``RParityError('character string is not in a standard unambiguous format')`` (call ``charToDate(x)``)
    when the first element parses with neither ``%Y-%m-%d`` nor ``%Y/%m/%d``; other elements that do not parse
    are ``None`` (``NA``).
    """
    if not texts:
        return []
    first = texts[0]
    for pattern in (_DATE_DASH, _DATE_SLASH):
        if _r_strptime_date(first, pattern) is not None:
            return [_r_strptime_date(text, pattern) for text in texts]
    raise RParityError(_ERR_DATE_FORMAT, call=_CALL_CHAR_TO_DATE)


def _encode_string(value: str) -> str:
    """``encodeString(value, quote = '"')`` for the characters that occur in run folder names."""
    escaped = (
        value.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t")
    )
    return f'"{escaped}"'


def r_print_character(values: Sequence[str], width: int = R_PRINT_WIDTH) -> str:
    """What ``print(x)`` writes for a character vector ``x`` (``printStringVector``).

    Every element is quoted and left-justified to the common width (the last one of a line included), elements
    are separated by one blank, each line starts with the index label ``[k]`` right-justified to the label width
    of the vector, and a new line starts when the next element would exceed ``width``; ``character(0)`` for an
    empty vector.
    """
    n = len(values)
    if n == 0:
        return "character(0)\n"
    encoded = [_encode_string(value) for value in values]
    w = max(len(item) for item in encoded)
    labwidth = len(str(n)) + 2
    lines: list[str] = []
    line = "[1]".rjust(labwidth)
    used = labwidth
    for k, item in enumerate(encoded, start=1):
        if k > 1 and used + w + 1 > width:
            lines.append(line)
            line = f"[{k}]".rjust(labwidth)
            used = labwidth
        line += " " + item.ljust(w)
        used += w + 1
    lines.append(line)
    return "".join(f"{text}\n" for text in lines)


def _setdiff(x: Sequence[str], y: Collection[str]) -> list[str]:
    """``setdiff(x, y)``: the distinct elements of ``x`` not in ``y``, in the order of ``x``."""
    seen: set[str] = set()
    out: list[str] = []
    for item in x:
        if item in y or item in seen:
            continue
        seen.add(item)
        out.append(item)
    return out


def _normalize_path_warn(path: str, call: str, effects: Effects) -> str:
    """``normalizePath(path)`` with its warning for a path that does not exist (not suppressed here)."""
    normalized = normalize_path(path, effects)
    if normalized == "" or not effects.exists(normalized):
        _warn(call, f'path[1]="{normalized}": No such file or directory')
    return normalized


def _local_date(epoch: float) -> dt.date:
    """``as.Date(format(<POSIXct>, "%Y-%m-%d"))``: the calendar date in the process time zone."""
    return dt.datetime.fromtimestamp(epoch).date()


# ---------------------------------------------------------------------------
# three-valued logic over status cells (NA is None)
# ---------------------------------------------------------------------------

type _Logical = bool | None


def _col(row: Mapping[str, object], name: str) -> object:
    """``grsi[, name]``: the cell, or R's ``undefined columns selected`` for a column the record lacks."""
    if name not in row:
        raise RParityError(_ERR_UNDEFINED_COLUMNS)
    return row[name]


def _as_text(value: object) -> str | None:
    """``as.character()`` of one cell; ``None`` stays ``NA``."""
    if value is None:
        return None
    if isinstance(value, str):
        return value
    if isinstance(value, bool):
        return "TRUE" if value else "FALSE"
    if isinstance(value, int | float):
        return r_num_str(value)
    return str(value)


def _r_grepl(pattern: re.Pattern[str], value: object) -> _Logical:
    text = _as_text(value)
    return None if text is None else pattern.search(text) is not None


def _r_eq(value: object, other: str) -> _Logical:
    text = _as_text(value)
    return None if text is None else text == other


def _r_ne(value: object, other: str) -> _Logical:
    equal = _r_eq(value, other)
    return None if equal is None else not equal


def _r_in(value: object, options: Collection[str]) -> bool:
    """``value %in% options``: never ``NA``."""
    text = _as_text(value)
    return text is not None and text in options


def _r_not(value: _Logical) -> _Logical:
    return None if value is None else not value


def _r_and(left: _Logical, right: Callable[[], _Logical]) -> _Logical:
    """``left && right()``: the right side is not evaluated after ``FALSE``; ``NA && FALSE`` is ``FALSE``."""
    if left is False:
        return False
    value = right()
    if left is True:
        return value
    return False if value is False else None


def _r_if(condition: _Logical) -> bool:
    """``if (condition)``: ``NA`` is the error ``missing value where TRUE/FALSE needed``."""
    if condition is None:
        raise RParityError(_ERR_NA_CONDITION)
    return condition


# ---------------------------------------------------------------------------
# the pieces of evaluateRuns
# ---------------------------------------------------------------------------


def wait_for_runs(mydir: str, user: str | None, effects: Effects) -> None:
    """Lines 180-194: poll ``squeue -u <user> -h -o '<ten fields>'`` until no line matches ``mydir``.

    A failing ``squeue`` (exit status above zero) counts; the fourth consecutive failure is the error
    ``squeue had exit status > 0 more than 3 times in a row.``, a success resets the counter, and the function
    sleeps 600 seconds between two polls (never before the first one).
    """
    user_text = "" if user is None else user
    command = f"squeue -u {user_text} -h -o '{_SQUEUE_TEN}'"
    pattern = r_regex(mydir, call=_CALL_GREPL_MYDIR)
    err_count = 0
    while True:
        proc = effects.run_shell(command)
        if proc.stderr:
            sys.stderr.write(proc.stderr)
            sys.stderr.flush()
        status = proc.returncode
        if status == 127:
            raise RParityError(_ERR_CANNOT_RUN, call=_CALL_SQUEUE)
        if status > 0:
            _warn(_CALL_SQUEUE, f"running command '{command}' had status {status}")
            err_count += 1
            if err_count > 3:
                raise RParityError(SQUEUE_FAILED, call=_CALL_EVALUATE_RUNS)
        elif not any(pattern.search(line) is not None for line in r_intern_lines(proc.stdout)):
            break
        else:
            err_count = 0
        effects.sleep(_WAIT_SECONDS)


def runs_started_remind(mydir: str, effects: Effects) -> list[str]:
    """Lines 238-239: the directories of the working directory whose names match ``runcode.rds``."""
    runcode = state.read_runcode(f"{mydir}/runcode.rds", effects)
    pattern = r_regex(runcode, call=_CALL_GREP_RUNCODE)
    dirs = [name for name in effects.listdir_all(".") if effects.is_dir(name)]  # list.dirs(recursive = FALSE)
    return [name for name in dirs if pattern.search(name) is not None]


def runs_started_magpie(effects: Effects) -> list[str]:
    """Lines 244-247: the directories of the working directory whose ctime date is within the last three days."""
    three_days_ago = effects.today() - dt.timedelta(days=_THREE_DAYS)
    started: list[str] = []
    for name in effects.listdir_like_r("."):
        if not effects.is_dir(name):
            continue
        try:
            ctime = effects.stat(name).ctime
        except OSError:
            continue  # file.info() gives an NA row, which() drops it
        if _local_date(ctime) > three_days_ago:
            started.append(name)
    return started


def difftime_hours(seconds: float) -> float:
    """``as.numeric(end - start, units = "hours")`` for a difference of ``seconds``, computed as R computes it.

    ``difftime()`` picks the unit by magnitude (``secs`` below 60, ``mins`` below 3600, ``hours`` below 86400,
    else ``days``), divides, and ``units<-`` multiplies by ``sc[from] / sc[to]`` to get hours.
    """
    magnitude = abs(seconds)
    if not math.isfinite(magnitude) or magnitude < 60:
        unit = 1.0
    elif magnitude < 3600:
        unit = 60.0
    elif magnitude < 86400:
        unit = 3600.0
    else:
        unit = 86400.0
    return (seconds / unit) * (unit / 3600.0)


def read_runtime_hours(path: str, effects: Effects) -> float | None:
    """``as.numeric(.readRuntime(path), units = "hours")`` (lines 64-69, 327-328).

    ``load(<path>/runstatistics.rda)`` fails like R for a missing file (``cannot open the connection`` after the
    ``cannot open compressed file`` warning, BUG-039: the whole evaluation aborts) or a corrupt one. The result is
    the hours as a float, ``None`` for a zero-length difference (no ``stats`` object, or no ``timeGAMSStart``) and
    ``nan`` for an ``NA`` difference; a missing ``timeGAMSEnd`` with a ``timeGAMSStart`` is R's error
    ``can only subtract from "POSIXt" objects``.
    """
    file = f"{path}/runstatistics.rda"
    try:
        objects = read_rda(file, effects)
    except RParityError as exc:
        if str(exc) == _ERR_CANNOT_OPEN:
            cause = exc.__cause__
            reason = (
                os.strerror(cause.errno)
                if isinstance(cause, OSError) and cause.errno is not None
                else "No such file or directory"
            )
            _warn(_CALL_READCHAR, f"cannot open compressed file '{file}', probable reason '{reason}'")
            raise RParityError(_ERR_CANNOT_OPEN, call=_CALL_READCHAR) from exc
        raise RParityError(str(exc), call=_CALL_LOAD_RUNSTATS) from exc
    stats_obj = objects.get("stats")
    if stats_obj is None:
        return None  # NULL - NULL: integer(0)
    if not isinstance(stats_obj, Mapping):
        raise RParityError("$ operator is invalid for atomic vectors")
    stats = RunStatistics({str(key): value for key, value in stats_obj.items()})
    end, start = stats.timeGAMSEnd, stats.timeGAMSStart
    if end is None and start is None:
        return None
    if end is None:
        raise RParityError(_ERR_SUBTRACT_POSIXT, call=_CALL_MINUS_POSIXT)
    if start is None:
        return None  # POSIXct - NULL: a POSIXct of length zero
    seconds = end.elapsed_since(start)
    if seconds is None:
        return math.nan
    return difftime_hours(seconds)


def cs2_command(out_file_name: str, full_path_this: str, full_path_last: str) -> str:
    """Lines 340-352: the ``sbatch`` command line of compareScenarios2, byte for byte."""
    return (
        "sbatch --qos=standby"
        f" --job-name={out_file_name}"
        " --comment=compareScenarios2"
        f" --output={full_path_this}/{out_file_name}.out"
        f" --error={full_path_this}/{out_file_name}.out"
        " --mail-type=END --time=200 --mem-per-cpu=8000"
        ' --wrap="Rscript scripts/cs2/run_compareScenarios2.R'
        f" outputdirs={full_path_this},{full_path_last}"
        " profileName=default"
        f" outFileName={out_file_name}"
        f"; mv {out_file_name}.pdf {full_path_this}"
        '"'
    )


def archive_candidates(today: dt.date, effects: Effects) -> list[str]:
    """Lines 403-411: the ``.*AMT.*`` directories whose folder date is older than 90 days.

    ``as.Date(gsub(...))`` decides its format on the first candidate (an error when it carries no date), later
    candidates without a date are ``NA`` and dropped.
    """
    names = [name for name in effects.listdir_like_r(".") if _AMT_NAME.search(name) is not None]
    names = [name for name in names if effects.is_dir(name)]
    dates = r_as_date([_RUN_DATE.sub(r"\1", name) for name in names])
    cutoff = today - dt.timedelta(days=ARCHIVE_DAYSBACK)
    return [name for name, date in zip(names, dates, strict=True) if date is not None and date < cutoff]


# ---------------------------------------------------------------------------
# evaluateRuns
# ---------------------------------------------------------------------------


def evaluate_runs(
    model: str | None,
    mydir: PathLike,
    comp_scen: bool,
    email: bool,
    mattermost_token: str | None,
    gitdir: PathLike | None,
    user: str | None,
    effects: Effects | None = None,
) -> EvaluateResult:
    """``evaluateRuns(model, mydir, compScen, email, mattermostToken, gitdir, user)``, lines 167-491.

    Call it with the working directory set to ``<mydir>/output`` (``modeltests()`` does). Every R error escapes as
    :class:`~modelstats.errors.RParityError` with R's message and, where R reports one, the deparsed call; the
    deferred R warnings of the run are re-raised (``warnings.warn``) when the function returns or fails, after the
    ``try()`` of line 230 has printed and cleared the ones deferred before it.
    """
    eff = _effects(effects)
    deferred: list[warnings.WarningMessage] = []
    try:
        with warnings.catch_warnings(record=True) as caught:
            warnings.simplefilter("always")
            deferred = caught
            return _evaluate_runs(
                model, os.fspath(mydir), comp_scen, email, mattermost_token, gitdir, user, eff, deferred
            )
    finally:
        for item in list(deferred):
            warnings.warn(item.message, stacklevel=2)


def _evaluate_runs(
    model: str | None,
    mydir: str,
    comp_scen: bool,
    email: bool,
    mattermost_token: str | None,
    gitdir: PathLike | None,
    user: str | None,
    eff: Effects,
    deferred: list[warnings.WarningMessage],
) -> EvaluateResult:
    # lines 170-172
    _message("Current working directory ", normalize_path(".", eff))
    status_file = f"{mydir}../.testsstatus"
    _message(f"Writing '{RUNNING_STATUS}' to ", normalize_path(status_file, eff))
    eff.write_text(status_file, RUNNING_STATUS + "\n")

    # lines 174-176
    last_commit = state.read_lastcommit(f"{mydir}/lastcommit.rds", eff)
    error_list: list[str] = []
    today = eff.now().strftime("%Y-%m-%d")

    # lines 178-195
    _message(eff.now().strftime("%Y-%m-%d %H:%M:%S"), " - waiting for all AMT runs to finish.")
    wait_for_runs(mydir, user, eff)
    _message(eff.now().strftime("%Y-%m-%d %H:%M:%S"), " - all AMT runs finished.")

    # lines 197-201
    _message("Compiling the README.md to be committed to testing_suite repo.")
    log_lines = system_intern("git log -1", _CALL_GIT_LOG_1, eff)
    if not log_lines:
        raise RParityError(_ERR_SUBSCRIPT, call=_CALL_GIT_LOG_1_SUBSCRIPT)
    commit_tested = log_lines[0].replace("commit ", "", 1)
    merges = system_intern(
        f"git log --merges --pretty=oneline {last_commit}..{commit_tested} --abbrev-commit | grep 'Merge pull request'",
        _CALL_GIT_MERGES,
        eff,
    )

    # lines 202-222: the README header and the git information
    readme_path = os.path.join(eff.tempdir(), "README.md")
    readme = readme_mod.Readme()

    def flush() -> None:
        eff.write_text(readme_path, readme.text)

    try:
        readme.begin(model, today, mydir, comp_scen)
    finally:
        flush()
    git_info = readme_mod.git_info(commit_tested, today, merges)
    readme.add_git_info(git_info)
    flush()

    # lines 224-248
    if model is None:
        raise RParityError(_ERR_ZERO_LENGTH)  # if (NULL != "MAgPIE")
    grs: StatusTable
    if model != "MAgPIE":
        grs_old = state.read_grs("gRS.rds", eff) if eff.exists("gRS.rds") else None

        def rbind_new() -> StatusTable:
            old_names = grs_old.rownames if grs_old is not None else []
            new = get_run_status(_setdiff(eff.listdir_like_r("."), old_names), effects=eff)
            return state.rbind_status(grs_old, new)

        bound = _r_try(deferred, rbind_new)
        if bound is not None:
            grs = bound
        else:
            grs = get_run_status(eff.listdir_like_r("."), effects=eff)
        state.write_grs("gRS.rds", grs, eff)
        runs_started = runs_started_remind(mydir, eff)
    else:
        grs = get_run_status(eff.listdir_like_r("."), effects=eff)
        runs_started = runs_started_magpie(eff)

    # lines 250-256
    readme.add_column_titles()
    flush()

    # lines 258-270
    changelog: str | None = None
    if model == "MAgPIE":
        for run in [name for name in runs_started if name.startswith("default_")]:
            changelog = os.path.join(eff.tempdir(), "data-changelog.csv")
            if gitdir is not None:  # file.path(NULL, ...) is character(0): file.copy() does nothing
                eff.copy(f"{os.fspath(gitdir)}/data-changelog.csv", changelog)  # file.path(gitdir, ...)
            add_to_data_changelog(f"{run}/report.rds", changelog, run, cwd=None, effects=eff)

    # lines 272-364
    _message("Starting analysis for the list of the following runs:\n", "\n".join(runs_started))
    for run in runs_started:
        rows = get_run_status(run, effects=eff).rows()
        row: Mapping[str, object] = rows[0] if rows else RunStatus(run, None, {})
        readme.add_run(row, rowname=run if not rows else None, effects=eff)
        flush()
        _collect_errors(model, row, error_list)
        if _r_in(_col(row, "Conv"), CONVERGED_OR_NOT):
            _analyse_converged_run(run, row, grs, comp_scen, error_list, eff)
        elif model != "MAgPIE":
            _message(run, " does not seem to have converged. Skipping!")

    # lines 366-419
    runs_not_started: list[str] | None = None
    testthat_result: str | None = None
    if model == "REMIND":
        runs_to_start = state.run_names(state.read_runs_to_start(f"{mydir}runsToStart.rds", eff))
        if len(runs_started) < len(runs_to_start) + 1:  # BUG-024: a count, not a set comparison
            runs_not_started = readme_mod.runs_not_started(runs_started, runs_to_start)
            readme.add_not_started(runs_not_started)
            flush()
        testthat_result = _evaluate_testthat(eff)
        _archive_old_runs(eff)

    # lines 421-431
    if len(runs_started) < 1:
        error_list.append(ERR_NO_RUNS)
    summary = readme.finish(error_list)
    flush()
    _message("Finished compiling README.md")

    # lines 433-445
    if email:
        _push_to_gitdir(gitdir, readme_path, changelog, eff)

    # lines 447-486
    _message("Composing message and sending it to mattermost channel")
    message = send_notification(
        model,
        mattermost_token,
        today=today,
        summary=summary,
        testthat_result=testthat_result,
        git_info=git_info,
        runs_started=runs_started,
        runs_not_started=runs_not_started,
        error_list=error_list,
        effects=eff,
    )

    # lines 488-491
    state.write_lastcommit(f"{mydir}/lastcommit.rds", commit_tested, eff)
    _message("Function 'evaluateRuns' finished.")
    return EvaluateResult(
        commit_tested=commit_tested,
        runs_started=runs_started,
        error_list=error_list,
        summary=summary,
        testthat_result=testthat_result,
        runs_not_started=runs_not_started,
        readme_path=readme_path,
        message=message,
    )


def _collect_errors(model: str, row: Mapping[str, object], error_list: list[str]) -> None:
    """Lines 279-298: the error texts one run contributes, in R's order and with R's NA semantics."""
    if model == "REMIND":
        if _r_if(
            _r_and(
                _r_not(_r_grepl(_CALIB_OR_TEST_ONE_REGI, _col(row, "RunType"))),
                lambda: not _r_in(_col(row, "Conv"), CONVERGED),
            )
        ):
            error_list.append(ERR_NOT_CONVERGED)
        if _r_if(_r_and(_r_eq(_col(row, "RunType"), "Calib_nash"), lambda: _r_ne(_col(row, "Conv"), "Clb_converged"))):
            error_list.append(ERR_NOT_CONVERGED)
        if _r_if(
            _r_and(
                _r_grepl(_TEST_ONE_REGI, _col(row, "RunType")),
                lambda: _r_ne(_col(row, "modelstat"), "2: Locally Optimal"),
            )
        ):
            error_list.append(ERR_TEST_ONE_REGI)
        if _r_if(_r_eq(_col(row, "Mif"), "sumErr")):
            error_list.append(ERR_SUM_ERR)
    elif model == "MAgPIE":
        if _modelstat_digits(_col(row, "modelstat")) != "2":
            error_list.append(ERR_NOT_CONVERGED)
    if _r_if(_r_ne(_col(row, "runInAppResults"), "yes")):
        error_list.append(ERR_NOT_REPORTED)


def _modelstat_digits(modelstat: object) -> str:
    """Line 294: ``paste0(unique(unlist(strsplit(gsub("[^0-9]", "", x), split = ""))), collapse = "")``.

    An ``NA`` cell goes through ``gsub`` as ``NA`` and prints as ``"NA"``; the string ``"NA"`` loses its letters
    and gives ``""``; both differ from ``"2"``.
    """
    text = _as_text(modelstat)
    if text is None:
        return "NA"
    digits = re.sub("[^0-9]", "", text)
    return "".join(dict.fromkeys(digits))


def _analyse_converged_run(
    run: str,
    row: Mapping[str, object],
    grs: StatusTable,
    comp_scen: bool,
    error_list: list[str],
    eff: Effects,
) -> None:
    """Lines 302-360: inside the run directory, the RSE gdx update, the previous run, runtime and compareScenarios2."""
    rowname = row.rowname if isinstance(row, RunStatus) else run
    with eff.chdir(run):
        _message("Changed to ", normalize_path(".", eff))
        # lines 306-311
        if _r_if(_r_and(_r_grepl(_SSP2_NPI_AMT, rowname), lambda: _r_in(_col(row, "Conv"), CONVERGED))):
            _message(f"Updating the gdx on the RSE server {GDX_ON_RSE_SERVER} with the fulldata.gdx of {rowname}")
            system_run(f"rsync -e ssh -av fulldata.gdx {GDX_ON_RSE_SERVER}", _CALL_RSYNC, eff)
        # lines 312-318
        cfg = _load_config_rdata(eff)
        title = cfg.get("title")
        results_folder = cfg.get("results_folder")
        same_runs = _same_runs(grs, title, results_folder)
        if same_runs:
            # lines 320-324
            current = None if results_folder is None else r_basename(str(results_folder))
            smaller = (
                [] if current is None else [name for name in same_runs if r_collate_key(name) < r_collate_key(current)]
            )
            if smaller:
                last_run = max(smaller, key=r_collate_key)
            else:
                _warn(_CALL_MAX, "no non-missing arguments, returning NA")  # BUG-014
                last_run = "NA"
            if not eff.exists(f"../{last_run}"):
                last_run = f"archive/{last_run}"
            full_path_this = normalize_path(".", eff)
            full_path_last = _normalize_path_warn(f"../{last_run}", _CALL_NORMALIZE_LAST, eff)
            # lines 326-332
            if _r_in(_col(row, "Conv"), CONVERGED):
                current_hours = read_runtime_hours(full_path_this, eff)
                last_hours = read_runtime_hours(full_path_last, eff)
                if current_hours is not None and last_hours is not None:
                    if math.isnan(current_hours) or math.isnan(last_hours):
                        raise RParityError(_ERR_NA_CONDITION)
                    if current_hours > 1.25 * last_hours:
                        error_list.append(ERR_SLOWER)
            # lines 334-357
            title_text = "" if title is None else _as_text(title)
            if (
                comp_scen
                and all(
                    eff.exists(f"{path}/REMIND_generic_{title_text}.mif") for path in (full_path_this, full_path_last)
                )
                and not any(_COMP_WITH_PDF.search(name) is not None for name in eff.listdir_like_r("."))
            ):
                _message("Calling compareScenarios2 with ", full_path_this, " and ", full_path_last)
                out_file_name = f"comp_with_{last_run}"
                cs2com = cs2_command(out_file_name, full_path_this, full_path_last)
                _stdout(f"{cs2com} \n")  # cat(cs2com, "\n")
                remind_folder = cfg.get("remind_folder")
                if remind_folder is None:
                    raise RParityError(_ERR_CHAR_EXPECTED, call=_CALL_SETWD)
                with eff.chdir(str(remind_folder)):
                    system_run(cs2com, _CALL_CS2, eff)
    # line 359-360: withr::local_dir("../")
    _message("Finished analysis for ", run, " and changed back to ", normalize_path(".", eff))


def _load_config_rdata(eff: Effects) -> dict[str, object]:
    """Lines 312-313: ``cfg <- NULL; load("config.Rdata")`` in the run directory."""
    try:
        return load_config("config.Rdata", eff)
    except RParityError as exc:
        if str(exc) == _ERR_CANNOT_OPEN:
            cause = exc.__cause__
            reason = (
                os.strerror(cause.errno)
                if isinstance(cause, OSError) and cause.errno is not None
                else "No such file or directory"
            )
            _warn(_CALL_READCHAR, f"cannot open compressed file 'config.Rdata', probable reason '{reason}'")
            raise RParityError(_ERR_CANNOT_OPEN, call=_CALL_READCHAR) from exc
        raise RParityError(str(exc), call=_CALL_LOAD_CONFIG) from exc


def _same_runs(grs: StatusTable, title: object, results_folder: object) -> list[str]:
    """Lines 314-318: the row names of ``gRS`` that are converged, have a mif, match the title and are not this run."""
    for column in ("Conv", "Mif"):
        if column not in grs.columns:
            raise RParityError(f"Column `{column}` not found in `.data`")
    if title is None:
        raise RParityError(_ERR_PATTERN)  # grepl(NULL, x)
    title_text = _as_text(title)
    assert title_text is not None
    pattern = r_regex(title_text, call=_CALL_GREPL_TITLE)
    current = None if results_folder is None else r_basename(str(results_folder))
    return [
        row.rowname
        for row in grs.rows()
        if _r_in(row["Conv"], CONVERGED)
        and _r_in(row["Mif"], ("yes", "sumErr"))
        and pattern.search(row.rowname) is not None
        and row.rowname != current
    ]


def _evaluate_testthat(eff: Effects) -> str:
    """Lines 380-398: the result text of ``make test-full`` from ``../test-full.log``, renamed with its date."""
    current_name = "../test-full.log"
    try:
        mtime = eff.stat(current_name).mtime
    except OSError:
        return TESTFULL_NOT_FOUND  # file.info()$mtime is NA
    date_tag = dt.datetime.fromtimestamp(mtime).strftime("%Y-%m-%d")
    log_status = _tail([line for line in r_read_lines(eff.read_text(current_name)) if _FAIL_LINE.search(line)])
    new_name = f"tests/test-full-{date_tag}.log"
    target = f"../{new_name}"
    try:
        eff.rename(current_name, target)
    except OSError as exc:
        reason = exc.strerror or (os.strerror(exc.errno) if exc.errno is not None else str(exc))
        _warn(_CALL_FILE_RENAME, f"cannot rename file '{current_name}' to '{target}', reason '{reason}'")
    one_line = log_status[0] if len(log_status) == 1 else None  # isTRUE() needs exactly one element
    if one_line is None or "FAIL" not in one_line:
        return f"`make test-full` did not run properly. Check {_normalize_path_warn(target, _CALL_NORMALIZE_NEW, eff)}"
    if not ("FAIL 0" in one_line and "WARN 0" in one_line):
        return (
            f"Not all tests pass in `make test-full`: {one_line}. Check "
            f"`{_normalize_path_warn(target, _CALL_NORMALIZE_NEW, eff)}`"
        )
    return f"All tests pass in `make test-full`: {one_line}"


def _archive_old_runs(eff: Effects) -> None:
    """Lines 400-418: move the runs whose folder date is older than 90 days to ``archive`` (BUG-033 kept)."""
    today = eff.today()
    old_runs = archive_candidates(today, eff)
    if old_runs:
        cutoff = today - dt.timedelta(days=ARCHIVE_DAYSBACK)
        _message(
            f"Moving {len(old_runs)} runs with timestamp older than {ARCHIVE_DAYSBACK} days ({cutoff.isoformat()})"
            " to 'archive':"
        )
        _stdout(r_print_character(old_runs))
        system_run(f"mv {' '.join(old_runs)} archive", _CALL_MV, eff)


def _push_to_gitdir(gitdir: PathLike | None, readme_path: str, changelog: str | None, eff: Effects) -> None:
    """Lines 434-444: commit the README (and the changelog) in the testing_suite clone."""
    if gitdir is None:
        raise RParityError(_ERR_CHAR_EXPECTED, call=_CALL_SETWD)  # setwd(NULL)
    with eff.chdir(gitdir):
        system_run("git reset --hard origin/master", _CALL_GIT_RESET, eff)
        system_run("git pull", _CALL_GIT_PULL, eff)
        eff.copy(readme_path, ".", overwrite=True)
        if changelog is not None:
            eff.copy(changelog, ".", overwrite=True)
        system_run("git add README.md", _CALL_GIT_ADD_README, eff)
        if eff.exists("data-changelog.csv"):
            system_run("git add data-changelog.csv", _CALL_GIT_ADD_CHANGELOG, eff)
        system_run("git commit -m 'Automated Test Results'", _CALL_GIT_COMMIT, eff)
        system_run("git push", _CALL_GIT_PUSH, eff)
