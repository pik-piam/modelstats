"""``loop_runs``: the coloured status listing of ``rs`` (``R/loopRuns.R``, line by line).

The function prints one line per run directory, built from :func:`modelstats.run_status.get_run_status` through
:func:`modelstats.formatting.print_output`, with the colour legend, the underlined header, the ``Runtime`` display
rewrite and the two colour decision trees of :mod:`modelstats.colors`. Output goes to ``out`` (``sys.stdout`` when
omitted, like R's ``cat``); what R's ``try()`` prints for a failing ``getRunStatus`` call goes to ``sys.stderr``
(R's ``stderr()``) in R's own layout, see :func:`r_try_message` and :func:`r_warnings_text`.

R facts the port relies on (R 4.6.1, verified 2026-10-01, pinned in ``tests/unit/test_loop_runs.py``):

- ``file.info(mydir)`` is taken on the paths as given (not normalised); ``a[a[, "isdir"] == TRUE, ]`` drops files
  and turns a missing path into a row named ``NA`` which the loop then visits as the directory ``NA`` (it prints
  ``NA skipped.``); ``order(mtime, decreasing = TRUE)`` is stable (``order(c(2, 1, 2, 1), decreasing = TRUE)`` is
  ``1 3 2 4``) and puts NA last; ``isTRUE(sortbytime)`` only for exactly ``TRUE``.
- ``normalizePath(x, mustWork = FALSE)`` keeps a missing path as given (after tilde expansion), so the folder width
  uses the basename of the path as given for such a row.
- The skip test ``! file.exists(paste0(i, "/", grep("^config.*|^log.txt$", dir(i), value = TRUE)[1])) &&
  status[["jobInSLURM"]] == "no"`` probes ``<i>/NA`` when nothing matches (``paste0`` renders ``NA``), and on a
  try-error ``status[["jobInSLURM"]]`` fails with *subscript out of bounds* and aborts the whole listing (BUG-005,
  D-03 pending): ``TRUE && logical(0)`` is ``NA`` and ``if (NA)`` fails with *missing value where TRUE/FALSE needed*.
- ``status["Runtime"] <- format(status["Runtime"])`` turns a missing runtime into the string ``NA``;
  ``nchar(status["Runtime"]) < 10`` counts the formatted text (``3.3 hours`` is 9: ``> 3.3 hours``; ``10.5 hours``
  is 10: ``>10.5 hours``); ``gsub(..., status["RunType"])`` keeps a real ``NA`` (``as.character(list(NA_character_))``
  is ``NA``), so it still prints as blanks.
- ``trimws(x, which = "right", whitespace = " ")`` is ``sub(" +$", "", x, perl = TRUE)``: the trailing spaces before
  the final newline go, the newline stays (``"abc    NA   \\n"`` -> ``"abc    NA\\n"``); a coloured row is therefore
  ``ESC[..m<line>\\nESC[39m`` with the close sequence at the start of the next line.
- ``try()`` prints ``Error in <call> : <msg>`` (``Error : <msg>`` without a call; the message moves to its own
  line, indented by two, when ``14 + nchar(call) + nchar(first line)`` exceeds 75) and then the deferred warnings
  as ``In addition: Warning message:`` / ``In <call> : <text>`` (``In <call> :`` + newline + two spaces when
  ``6 + nchar(call) + nchar(first line)`` exceeds 75, ``10 +`` with the ``k: `` numbering of several, ``<text> ``
  without a call, ``There were N warnings (use warnings() to see them)`` above ten and ``There were 50 or more
  warnings (use warnings() to see the first 50)`` at the cap); those warnings are then no longer deferred.
"""

from __future__ import annotations

import math
import os
import re
import sys
import warnings
from collections.abc import Mapping, Sequence
from typing import IO, TYPE_CHECKING

from modelstats.colors import colour_for_magpie, colour_for_remind, is_magpie_row, style
from modelstats.env import default_effects
from modelstats.errors import RParityError, RWarning
from modelstats.formatting import format_runtime, print_output, r_num_str
from modelstats.run_status import StatusTable, _directory_rows, _file_info, _newest_first, get_run_status
from modelstats.runfolders import r_basename
from modelstats.slurm import normalize_path

if TYPE_CHECKING:
    from modelstats.env import Effects

__all__ = ["COL_SEP", "COLTITLES_LOCAL", "COLTITLES_ON_CLUSTER", "loop_runs", "r_try_message", "r_warnings_text"]

type PathLike = str | os.PathLike[str]

#: The column titles of ``R/loopRuns.R:45-52`` after the ``Folder`` title, verbatim (their widths are the column
#: widths; ``AppResults`` prints 3 wide).
COLTITLES_ON_CLUSTER: tuple[str, ...] = (
    "Runtime    ",
    "inSlurm ",
    "RunType    ",
    "RunStatus        ",
    "Warnings ",
    "Iter            ",
    "Conv                 ",
    "modelstat            ",
    "Mif   ",
    "AppResults",
)
COLTITLES_LOCAL: tuple[str, ...] = (
    "Runtime    ",
    "RunType    ",
    "RunStatus        ",
    "Warnings ",
    "Iter            ",
    "Conv                 ",
    "modelstat          ",
    "Mif   ",
)
#: ``colSep`` of ``loopRuns`` (two spaces; ``printOutput``'s own default is three).
COL_SEP = "  "

_CONFIG_OR_LOG = re.compile(r"^config.*|^log.txt$")  # grep("^config.*|^log.txt$", dir(i))
_PENDING = re.compile(r"pending$")
_STARTUP = re.compile(r"startup$")
_SLURM_SUFFIX = re.compile(r" *startup$| *pending$")
_TRAILING_SPACES = re.compile(r" +$")  # trimws(which = "right", whitespace = " "), perl = TRUE
_ERR_NA_CONDITION = "missing value where TRUE/FALSE needed"
_ERR_ZERO_LENGTH = "argument is of length zero"
_ERR_SUBSCRIPT = "subscript out of bounds"
_LONG = 75  # LONG of try() and LONGWARN of PrintWarnings
_R_NWARNINGS = 50  # R's default cap on deferred warnings


# ---------------------------------------------------------------------------
# R's try() and deferred-warning text (stderr)
# ---------------------------------------------------------------------------


def r_try_message(exc: BaseException) -> str:
    """The text R's ``try()`` prints to stderr for a caught error.

    ``Error in <call> : <message>`` when the exception carries the deparsed R call in a ``call`` attribute (as
    :class:`~modelstats.errors.RWarning` does for warnings), else ``Error : <message>``; the message starts on
    its own line, indented by two spaces, when ``14 + nchar(call) + nchar(first message line)`` exceeds 75.
    """
    message = str(exc)
    call = getattr(exc, "call", None)
    if isinstance(call, str) and call:
        prefix = f"Error in {call} : "
        first = message.split("\n", 1)[0]
        width = 14 + len(call) + (len(first) if message else 2)  # nchar(NA) is 2 for an empty message
        if width > _LONG:
            prefix += "\n  "
    else:
        prefix = "Error : "
    return f"{prefix}{message}\n"


def _warning_parts(caught: warnings.WarningMessage) -> tuple[str | None, str]:
    """``(deparsed call, message)`` of one recorded warning: an :class:`RWarning` carries both, anything else only
    its text (R's ``warning(call. = FALSE)`` form)."""
    message = caught.message
    if isinstance(message, RWarning):
        return (message.call or None), message.text
    return None, str(message)


def r_warnings_text(caught: Sequence[warnings.WarningMessage], *, in_addition: bool = False) -> str:
    """R's ``PrintWarnings`` text for deferred warnings, empty when there are none.

    ``Warning message:`` for one, ``Warning messages:`` with ``k: `` numbering for two to ten, ``There were N
    warnings (use warnings() to see them)`` above ten and ``There were 50 or more warnings (use warnings() to see
    the first 50)`` at R's cap; each warning prints as ``In <call> : <text>``, with the text on its own line
    indented by two spaces when ``6 + nchar(call) + nchar(first line)`` (``10 +`` when numbered) exceeds 75, or
    as ``<text> `` without a call. ``in_addition`` prefixes ``In addition: `` as ``try()`` does after an error.
    """
    count = len(caught)
    if count == 0:
        return ""
    prefix = "In addition: " if in_addition else ""
    if count > 10:
        if count < _R_NWARNINGS:
            return f"{prefix}There were {count} warnings (use warnings() to see them)\n"
        return f"{prefix}There were {_R_NWARNINGS} or more warnings (use warnings() to see the first {_R_NWARNINGS})\n"
    lines: list[str] = []
    numbered = count > 1
    lines.append(f"{prefix}Warning messages:\n" if numbered else f"{prefix}Warning message:\n")
    for k, item in enumerate(caught, start=1):
        call, text = _warning_parts(item)
        number = f"{k}: " if numbered else ""
        if call is None:
            lines.append(f"{number}{text} \n")
            continue
        first = text.split("\n", 1)[0]
        margin = 10 if numbered else 6
        break_line = "\n " if margin + len(call) + len(first) > _LONG else ""
        lines.append(f"{number}In {call} :{break_line} {text}\n")
    return "".join(lines)


def _write_stderr(text: str) -> None:
    """``cat(msg, file = stderr())``; a closed or broken stderr never stops the listing."""
    try:
        sys.stderr.write(text)
        sys.stderr.flush()
    except OSError, ValueError, AttributeError:
        pass


# ---------------------------------------------------------------------------
# R cell semantics
# ---------------------------------------------------------------------------


def _is_na(value: object) -> bool:
    """``is.na(x)`` for one cell: ``None`` and a float ``NaN``; the string ``"NA"`` is not missing."""
    return value is None or (isinstance(value, float) and math.isnan(value))


def _as_text(value: object) -> str | None:
    """``as.character(list(x))`` of one cell as ``gsub`` / ``grepl`` see it: ``None`` stays NA, numbers print like R."""
    if _is_na(value):
        return None
    if isinstance(value, str):
        return value
    return r_num_str(value)


def _cell(cells: Mapping[str, object], name: str, absent: str) -> object:
    """``status[[name]]`` / ``status[name]``: an absent column is R's ``NULL`` and fails with ``absent`` downstream."""
    if name not in cells:
        raise RParityError(absent)
    return cells[name]


def _condition(value: bool | None) -> bool:
    """``if (value)``: an ``NA`` condition is an R error."""
    if value is None:
        raise RParityError(_ERR_NA_CONDITION)
    return value


def _job_is_no(table: StatusTable) -> bool:
    """``TRUE && status[["jobInSLURM"]] == "no"`` as the ``if`` of ``R/loopRuns.R:58`` sees it.

    The column of the one-row table compared with ``"no"``: no row or no column is ``logical(0)``, an ``NA`` cell
    is ``NA``, and both make the condition ``NA`` (*missing value where TRUE/FALSE needed*); two or more rows
    cannot happen for one directory but would fail R's ``&&`` length check.
    """
    values = table.column("jobInSLURM")
    if not values:
        raise RParityError(_ERR_NA_CONDITION)
    if len(values) > 1:
        raise RParityError(f"'length = {len(values)}' in coercion to 'logical(1)'")
    return _condition(None if _is_na(values[0]) else _as_text(values[0]) == "no")


# ---------------------------------------------------------------------------
# the listing
# ---------------------------------------------------------------------------


def _as_paths(mydir: PathLike | Sequence[PathLike]) -> list[str]:
    if isinstance(mydir, str | os.PathLike):
        return [os.fspath(mydir)]
    return [os.fspath(path) for path in mydir]


def _legend() -> str:
    """Lines 29-31: ``cat("# Color code: ", yellow("pending"), ..., red("error"), ".\\n\\n", sep = "")``."""
    return (
        "# Color code: "
        + style("yellow", "pending")
        + "/"
        + style("yellow", "startup")
        + ", "
        + style("cyan", "running")
        + ", "
        + style("green", "converged")
        + "/"
        + style("green", "finished")
        + ", "
        + style("orange", "no mif")
        + ", "
        + style("magenta", "conopt stalled?")
        + ", "
        + style("orangered", "error")
        + ".\n\n"
    )


def _columns(width: int, on_cluster: bool) -> tuple[list[str], list[int]]:
    """Lines 44-54: the column titles (``Folder`` padded to ``width``) and ``lenCols``."""
    folder = "Folder" + " " * (width - 6)
    if on_cluster:
        titles = [folder, *COLTITLES_ON_CLUSTER]
        return titles, [len(title) for title in titles[:-1]] + [3]
    titles = [folder, *COLTITLES_LOCAL]
    return titles, [len(title) for title in titles]


def _config_probe(directory: str, effects: Effects) -> str:
    """``paste0(i, "/", grep("^config.*|^log.txt$", dir(i), value = TRUE)[1])`` (``<i>/NA`` without a match)."""
    first = next((name for name in effects.listdir_like_r(directory) if _CONFIG_OR_LOG.search(name)), None)
    return f"{directory}/{first if first is not None else 'NA'}"


def _try_get_run_status(
    directory: str, user: str, effects: Effects
) -> tuple[StatusTable | None, Exception | None, list[warnings.WarningMessage]]:
    """``try(getRunStatus(i, user = user))`` (line 57): the table or the error, plus the warnings raised meanwhile."""
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        try:
            return get_run_status(directory, user=user, effects=effects), None, list(caught)
        except Exception as exc:  # noqa: BLE001 - try() catches every R error condition
            return None, exc, list(caught)


def _display_runtime(cells: dict[str, object]) -> None:
    """Lines 66-79: the ``Runtime`` cell becomes its display text."""
    job = _as_text(_cell(cells, "jobInSLURM", _ERR_ZERO_LENGTH))  # grepl(..., NULL) -> if (logical(0))
    runtime = _cell(cells, "Runtime", _ERR_ZERO_LENGTH)  # is.na(NULL) -> if (! logical(0))
    if job is not None and _PENDING.search(job):
        cells["Runtime"] = "pending"
    elif not _is_na(runtime):
        if isinstance(runtime, str):
            raise RParityError("non-numeric argument to binary operator")  # make_difftime(second = "<text>")
        if not isinstance(runtime, int | float):
            raise TypeError(f"Runtime must be a number or None, got {type(runtime).__name__}")
        text = format_runtime(runtime)
        if _condition(None if job is None else job != "no"):  # if (! status["jobInSLURM"] == "no")
            text = ">" + (" " if len(text) < 10 else "") + text
        cells["Runtime"] = text
    elif job is not None and _STARTUP.search(job):
        cells["Runtime"] = "startup"
    else:
        cells["Runtime"] = "NA"  # format(status["Runtime"]) of a missing value


def _rewrite_cells(cells: dict[str, object]) -> None:
    """Lines 68-82: ``Runtime`` display, the SLURM suffix stripped, ``testOneRegi`` shortened."""
    _display_runtime(cells)
    job = _as_text(cells["jobInSLURM"])
    cells["jobInSLURM"] = None if job is None else _SLURM_SUFFIX.sub("", job)
    run_type = _as_text(_cell(cells, "RunType", "undefined columns selected"))  # status["RunType"] read
    cells["RunType"] = None if run_type is None else run_type.replace("testOneRegi", "1Regi")


def loop_runs(
    mydir: PathLike | Sequence[PathLike],
    user: str | None = None,
    colors: bool = True,
    sortbytime: bool = True,
    effects: Effects | None = None,
    out: IO[str] | None = None,
) -> str | None:
    """``loopRuns(mydir, user, colors, sortbytime)``: print the status listing of the run directories.

    ``mydir`` is a path or a sequence of paths; an empty sequence returns ``"No runs found"`` without printing,
    a first element ``"exit"`` returns ``None`` without printing. ``user`` defaults to the process user, ``colors``
    prints the legend and colours the rows (through :mod:`modelstats.colors`, which only emits escape sequences
    when enabled), ``sortbytime`` lists the newest directory first (else in the given order). Files among the paths
    are dropped, a missing path is listed as ``NA``. The text goes to ``out`` (``sys.stdout`` when omitted); the
    error text of a failing ``get_run_status`` goes to ``sys.stderr`` as R's ``try()`` prints it, and the row is
    reported as ``<basename> skipped because of error`` unless the directory has no ``config*`` / ``log.txt`` file,
    where the listing aborts with ``RParityError('subscript out of bounds')`` (BUG-005). Returns ``None``.
    """
    eff = effects if effects is not None else default_effects()
    stream: IO[str] = sys.stdout if out is None else out
    if user is None:  # line 22
        user = eff.user
    paths = _as_paths(mydir)
    if len(paths) == 0:  # line 23
        return "No runs found"
    if paths[0] == "exit":  # line 24
        return None
    if colors:  # lines 28-32
        stream.write(_legend())
    rows = _directory_rows(paths, [_file_info(path, eff) for path in paths])  # lines 33-34
    dirs = _newest_first(rows) if sortbytime is True else [name for name, _ in rows]  # lines 35-39
    width = min(67, max([15, *(len(r_basename(normalize_path(d, eff))) for d in dirs)]))  # lines 40-41
    titles, len_cols = _columns(width, eff.on_cluster)  # lines 43-54
    stream.write(style("underline", COL_SEP.join(titles)) + " \n")  # line 55
    for i in dirs:
        table, failure, caught = _try_get_run_status(i, user, eff)  # line 57
        if failure is not None:
            _write_stderr(r_try_message(failure) + r_warnings_text(caught, in_addition=True))
        else:
            for item in caught:  # still deferred, as in R: the caller's collector sees them
                warnings.warn(item.message, stacklevel=2)
        if not eff.exists(_config_probe(i, eff)):  # line 58
            if table is None:
                raise RParityError(_ERR_SUBSCRIPT)  # status[["jobInSLURM"]] on the try-error (BUG-005)
            if _job_is_no(table):
                stream.write(f"{i} skipped.\n")  # cat(paste(i, "skipped.\n"))
                continue
        if table is None:  # line 62
            stream.write(f"{r_basename(i)} skipped because of error\n")
            continue
        rows_of_table = table.rows()
        if len(rows_of_table) != 1:
            raise RParityError(_ERR_ZERO_LENGTH)  # grepl("pending$", NULL) in if(): not reachable for one directory
        (row,) = rows_of_table
        cells: dict[str, object] = dict(row)
        _rewrite_cells(cells)  # lines 68-82
        line = print_output(cells, rowname=row.rowname, len_cols=len_cols, col_sep=COL_SEP, effects=eff)  # line 84
        line = _TRAILING_SPACES.sub("", line, count=1)
        magpie = is_magpie_row(cells)  # line 87: evaluated before the colors switch, as in R
        if colors is False:  # isFALSE(colors)
            stream.write(line)
            continue
        name = colour_for_magpie(cells, line) if magpie else colour_for_remind(cells, line)
        stream.write(line if name is None else style(name, line))
    return None
