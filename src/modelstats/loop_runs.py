r"""The run table of ``rs``: ``loopRuns()`` (``R/loopRuns.R``) over the parity records.

Parity port (``migration/03-migration-plan.md`` section 3.3; ``migration/06-user-requests.md`` section 1:
``rs`` is a drop-in). Everything the R function prints is reproduced byte for byte, escape sequences
included, from the records of :func:`modelstats.run_status.get_run_status` (the parity view, one call per
directory as R makes it), the text layer of :mod:`modelstats.formatting` and the crayon bytes and colour
trees of :mod:`modelstats.colors`. What is reproduced, with the R lines:

* the two early returns (``:22-24``): ``"No runs found"`` for no directories, ``None`` for ``"exit"``;
  the user defaults to ``Sys.info()[["user"]]`` (``Effects.user``);
* the colour legend (``:26-32``), printed only with ``colors``;
* ``file.info(mydir)`` and its row names (``:33-39``): files drop out, a missing path survives as an
  all-``NA`` row named ``NA`` (``NA.1``, ``NA.2`` ... for several), a repeated path gets the row name
  ``<path>.1`` from ``make.unique`` and is then looked up under that name; ``sortbytime`` orders the rows
  by ``mtime`` decreasing (ties and the ``NA`` rows in input order, the ``NA`` rows last), otherwise the
  input order stays;
* the folder width ``min(67, max(15, nchar(basename(normalizePath(mydir)))))`` (``:40-41``), the column
  titles with their widths as ``lenCols`` (``AppResults`` 3 on the cluster, ``:43-54``), the two-space
  separator, and the underlined header followed by ``" \n"`` (``cat``'s separator, ``:55``), printed
  whatever ``colors`` says;
* per directory (``:56-133``): ``try(getRunStatus(i, user))``, whose failure prints base R's ``try()``
  diagnostic to stderr (``Error in <call> : <message>`` plus the ``In addition: Warning message:`` block
  of the warnings deferred during the call, :func:`try_error_message`, written to ``err``) before the loop
  goes on; the ``skipped.`` line for a folder without a ``config*`` or ``log.txt`` entry whose
  ``jobInSLURM`` is ``"no"`` (``:58-61``), where a failed ``getRunStatus`` is subscripted before the
  ``try-error`` test and aborts the whole table with ``subscript out of bounds`` (BUG-005, decision D-03
  pending: reproduced as :class:`LoopRunsError`, after the diagnostic); the ``skipped because of error``
  line for any other failed call (``:62-65``); the ``Runtime`` display
  (``:66-79``, :func:`modelstats.formatting.runtime_display`, BUG-042); the ``startup``/``pending`` suffix
  stripped from ``jobInSLURM`` (``:80``); ``testOneRegi`` shortened to ``1Regi`` (``:82``); the row
  through ``printOutput`` and ``trimws(which = "right", whitespace = " ")`` (``:84``), whose
  ``perl = TRUE`` ``$`` strips the blanks before the final newline; the colour trees on the ``unlist``-ed
  row (``:87-132``, :func:`modelstats.colors.colour_for_row`, BUG-006, BUG-043), the style wrapping the
  line and its newline.

Two knobs where R has one: ``colors`` is the function argument (legend and row colours), ``crayon`` is
the package state (``options(crayon.enabled)``, whether any escape sequence is emitted, header underline
included); the goldens were recorded with crayon forced on at 256 colours or off, the CLI passes
``Crayon.from_terminal``.

What is not reproduced here: R defers warnings process-wide, so a warning of one row that raised no error
would surface in the ``In addition:`` block of a later failing row or at the end of the script; ``err``
only receives the warnings of the failing call itself (a GDX without ``o_iterationNumber`` whose
``s80_bool`` is 1 is the one such shape). The ``Calls: ... / Execution halted`` lines after an abort are
``Rscript``'s top-level handler, the CLI's job.
"""

from __future__ import annotations

import os
import re
import sys
from collections.abc import Sequence
from dataclasses import dataclass
from typing import Final, TextIO

from modelstats.colors import Crayon, colour_for_row, colour_legend
from modelstats.env import Effects, PathLike, ProductionEffects
from modelstats.errors import DeferredWarning, ModelstatsError, RunReadError
from modelstats.formatting import CellValue, Row, print_output, runtime_display
from modelstats.run_status import NA, NO_JOB, RunStatus, get_run_status
from modelstats.runfolders import r_basename

__all__ = [
    "COLUMN_SEPARATOR",
    "EXIT",
    "FOLDER_WIDTH_MAX",
    "FOLDER_WIDTH_MIN",
    "LOCAL_TITLES",
    "NO_RUNS_FOUND",
    "ONCLUSTER_TITLES",
    "SKIPPED",
    "SKIPPED_BECAUSE_OF_ERROR",
    "SUBSCRIPT_OUT_OF_BOUNDS",
    "DirectoryRow",
    "LoopRunsError",
    "TableLayout",
    "deferred_warnings_message",
    "directory_rows",
    "display_cells",
    "folder_width",
    "format_row",
    "has_config_or_log",
    "loop_runs",
    "make_unique",
    "ordered_names",
    "table_layout",
    "try_error_message",
    "warnings_block",
]

NO_RUNS_FOUND: Final = "No runs found"
"""What ``loopRuns`` returns for an empty ``mydir`` (``R/loopRuns.R:23``); nothing is printed."""

EXIT: Final = "exit"
"""The first element that makes ``loopRuns`` return ``NULL`` (``R/loopRuns.R:24``; the CLI's prompt)."""

COLUMN_SEPARATOR: Final = "  "
"""``colSep`` (``R/loopRuns.R:43``)."""

FOLDER_WIDTH_MIN: Final = 15
FOLDER_WIDTH_MAX: Final = 67
"""``len <- min(67, max(c(15, nchar(basename(normalizePath(mydir))))))`` (``R/loopRuns.R:40-41``)."""

_FOLDER_TITLE: Final = "Folder"
ONCLUSTER_TITLES: Final[tuple[str, ...]] = (
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
"""The data column titles on the cluster (``R/loopRuns.R:45-47``), padded to their ``lenCols`` width."""

LOCAL_TITLES: Final[tuple[str, ...]] = (
    "Runtime    ",
    "RunType    ",
    "RunStatus        ",
    "Warnings ",
    "Iter            ",
    "Conv                 ",
    "modelstat          ",
    "Mif   ",
)
"""The data column titles elsewhere (``R/loopRuns.R:50-52``)."""

_APP_RESULTS_WIDTH: Final = 3
"""``lenCols <- c(nchar(coltitles)[-length(coltitles)], 3)`` (``R/loopRuns.R:48``): the last width."""

SKIPPED: Final = "skipped."
"""``cat(paste(i, "skipped.\\n"))`` (``R/loopRuns.R:59``)."""

SKIPPED_BECAUSE_OF_ERROR: Final = "skipped because of error"
"""``cat(basename(i), "skipped because of error\\n")`` (``R/loopRuns.R:63``)."""

SUBSCRIPT_OUT_OF_BOUNDS: Final = "subscript out of bounds"
"""R's message for ``status[["jobInSLURM"]]`` on a ``try-error`` (``R/loopRuns.R:58``, BUG-005)."""

_CONFIG_OR_LOG: Final = re.compile(r"^config.*|^log.txt$")
"""``grep("^config.*|^log.txt$", dir(i), value = TRUE)`` (``R/loopRuns.R:58``)."""

_SLURM_SUFFIX: Final = re.compile(r" *startup$| *pending$")
"""``gsub(" *startup$| *pending$", "", jobInSLURM)`` (``R/loopRuns.R:80``)."""

_TEST_ONE_REGI: Final = ("testOneRegi", "1Regi")
"""``gsub("testOneRegi", "1Regi", RunType)`` (``R/loopRuns.R:82``)."""

_TRAILING_BLANKS: Final = re.compile(r" +$")
"""``trimws(x, which = "right", whitespace = " ")``: ``sub(" +$", "", x, perl = TRUE)`` (``R/loopRuns.R:84``).

PCRE's ``$`` also matches before a final newline, as Python's does, so the blanks between the last cell
and the line's newline go.
"""

_MAKE_UNIQUE_SEP: Final = "."

_TRY_ERROR_IN: Final = "Error in "
_TRY_ERROR_NO_CALL: Final = "Error : "
_TRY_SEPARATOR: Final = " : "
_TRY_LONG: Final = 75
_TRY_WIDTH_OFFSET: Final = 14
"""``base::try``: ``prefix <- paste("Error in", dcall, ": ")``, broken after the prefix when
``14 + nchar(dcall) + nchar(first message line) > 75`` (``LONG``); ``"Error : "`` without a call."""

_WARNINGS_HEADER: Final = "In addition: "
_WARNING_ONE: Final = "Warning message:\n"
_WARNING_MANY: Final = "Warning messages:\n"
_WARNINGS_LISTED_MAX: Final = 10
_WARN_LONG: Final = 75
_WARN_WIDTH_OFFSET_ONE: Final = 6
_WARN_WIDTH_OFFSET_MANY: Final = 10
"""``printDeferredWarnings()`` → ``PrintWarnings("In addition: ")`` (``errors.c``): one warning as
``In <call> : <message>`` (broken after the colon when ``6 + nchar(call) + nchar(first line) > 75``), up
to ten numbered ``n: In <call> : <message>`` (offset 10), beyond ten only a count; a call-less warning
prints ``<message> ``. Verified in R 4.6.1."""


class LoopRunsError(ModelstatsError):
    """An R error of ``loopRuns()`` itself; ``str(error)`` is the R condition message."""


# ---------------------------------------------------------------------------------------------
# The rows of file.info(mydir) (R/loopRuns.R:33-39)
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True, slots=True)
class DirectoryRow:
    """One row of ``file.info(mydir)`` that survived the ``isdir`` filter (``R/loopRuns.R:33-34``).

    ``name`` is the row name the loop uses as a path: the input path (made unique by ``file.info``'s
    ``data.frame``), or ``NA`` for a missing path; ``mtime`` is ``NULL``-like ``None`` for the latter.
    """

    name: str
    mtime: float | None


def make_unique(names: Sequence[str], sep: str = _MAKE_UNIQUE_SEP) -> list[str]:
    """``make.unique(names, sep)``: a repeated name gets ``<name><sep><k>`` with the smallest free ``k``.

    The first occurrence stays; every later one tries ``k = 1, 2, ...`` (a counter kept per name) until
    the candidate is neither in the input nor already assigned, as R's ``do_makeunique`` does:
    ``make.unique(c("a", "a", "a.1", "a"))`` is ``a a.2 a.1 a.3`` (checked in R 4.6.1).
    """
    taken = set(names)
    seen: set[str] = set()
    counters: dict[str, int] = {}
    result: list[str] = []
    for name in names:
        if name not in seen:
            seen.add(name)
            result.append(name)
            continue
        k = counters.get(name, 0)
        candidate = name
        while candidate in taken:
            k += 1
            candidate = f"{name}{sep}{k}"
        counters[name] = k
        taken.add(candidate)
        result.append(candidate)
    return result


def directory_rows(paths: Sequence[str], env: Effects) -> list[DirectoryRow]:
    """``a <- file.info(mydir); a <- a[a[, "isdir"] == TRUE, ]`` (``R/loopRuns.R:33-34``).

    ``file.info`` names its rows by the paths, made unique (``/tmp``, ``/tmp.1``); the logical subset
    keeps the directories, drops the files, and turns a missing path (``isdir`` ``NA``) into an all-``NA``
    row that ``[.data.frame`` names ``NA`` and makes unique against the others (``NA``, ``NA.1`` ...).
    Verified in R 4.6.1 on ``c("/tmp", "/tmp", "/nonexist1", "/etc/hostname", "/nonexist2")``.
    """
    names = make_unique(paths)
    kept: list[DirectoryRow] = []
    for name, path in zip(names, paths, strict=True):
        info = env.stat(path)
        if info is None:
            kept.append(DirectoryRow(NA, None))
        elif info.is_dir:
            kept.append(DirectoryRow(name, info.mtime_epoch))
    unique = make_unique([row.name for row in kept])
    return [DirectoryRow(name, row.mtime) for name, row in zip(unique, kept, strict=True)]


def ordered_names(rows: Sequence[DirectoryRow], sortbytime: bool) -> list[str]:
    """``rownames(a[order(a[, "mtime"], decreasing = TRUE), ])`` or ``rownames(a)`` (``R/loopRuns.R:35-39``).

    ``order`` on a short double vector is the stable radix sort: ties keep their input order and the
    ``NA`` rows go last, also with ``decreasing = TRUE`` (checked in R 4.6.1: ``order(c(1, NA, 2, NA, 1),
    decreasing = TRUE)`` is ``3 1 5 2 4``).
    """
    if not sortbytime:
        return [row.name for row in rows]
    ordered = sorted(rows, key=lambda row: (row.mtime is None, -(row.mtime or 0.0)))
    return [row.name for row in ordered]


# ---------------------------------------------------------------------------------------------
# The header (R/loopRuns.R:40-55)
# ---------------------------------------------------------------------------------------------


def folder_width(basenames: Sequence[str]) -> int:
    """``min(67, max(c(15, nchar(basenames))))`` (``R/loopRuns.R:40-41``)."""
    longest = max([FOLDER_WIDTH_MIN, *(len(name) for name in basenames)])
    return min(FOLDER_WIDTH_MAX, longest)


@dataclass(frozen=True, slots=True)
class TableLayout:
    """The column titles and the ``lenCols`` widths of one table (``R/loopRuns.R:43-54``).

    ``titles`` starts with ``Folder`` padded to the folder width; ``widths`` is ``lenCols``: the title
    widths, the last one replaced by 3 on the cluster (``AppResults`` prints ``yes``/``no``).
    """

    titles: tuple[str, ...]
    widths: tuple[int, ...]

    @property
    def header(self) -> str:
        """``paste(coltitles, collapse = colSep)`` (``R/loopRuns.R:55``), before the underline."""
        return COLUMN_SEPARATOR.join(self.titles)


def table_layout(width: int, on_cluster: bool) -> TableLayout:
    """The titles and widths for a folder column of ``width`` characters (``R/loopRuns.R:44-54``)."""
    folder = _FOLDER_TITLE + " " * (width - len(_FOLDER_TITLE))
    if on_cluster:
        titles = (folder, *ONCLUSTER_TITLES)
        widths = (*(len(title) for title in titles[:-1]), _APP_RESULTS_WIDTH)
    else:
        titles = (folder, *LOCAL_TITLES)
        widths = tuple(len(title) for title in titles)
    return TableLayout(titles, widths)


# ---------------------------------------------------------------------------------------------
# One row (R/loopRuns.R:56-133)
# ---------------------------------------------------------------------------------------------


def has_config_or_log(directory: str, env: Effects) -> bool:
    """``file.exists(paste0(i, "/", grep("^config.*|^log.txt$", dir(i), value = TRUE)[1]))`` (``:58``).

    The first match in ``dir()`` order; without a match ``[1]`` is ``NA`` and the probe is ``<i>/NA``,
    which exists only for an entry of that name.
    """
    matches = [name for name in env.listdir_like_r(directory) if _CONFIG_OR_LOG.search(name)]
    first = matches[0] if matches else NA
    return env.exists(f"{directory}/{first}")


def display_cells(record: RunStatus) -> dict[str, CellValue]:
    """The row as ``printOutput`` and the colour trees see it (``R/loopRuns.R:66-82``).

    ``Runtime`` becomes its display text (:func:`runtime_display`), ``jobInSLURM`` loses its
    ``startup``/``pending`` suffix and ``RunType`` its ``testOneRegi``; every other cell stays as the
    record holds it (``unlist`` turns the numbers to text at the comparison, ``r_as_character``).
    """
    cells: dict[str, CellValue] = dict(record.cells)
    cells["Runtime"] = runtime_display(record.Runtime, record.jobInSLURM)
    cells["jobInSLURM"] = _SLURM_SUFFIX.sub("", record.jobInSLURM)
    cells["RunType"] = record.RunType.replace(*_TEST_ONE_REGI)
    return cells


def format_row(run: str, cells: dict[str, CellValue], layout: TableLayout, on_cluster: bool) -> str:
    """``trimws(printOutput(status, lenCols, colSep), which = "right", whitespace = " ")`` (``:84``).

    The columns are ``printOutput``'s defaults for the mode (``file.exists("/p")``), the widths the
    layout's ``lenCols``; the blanks before the final newline are stripped, the newline stays.
    """
    text = print_output(
        Row(run, cells), len_cols=layout.widths, col_sep=COLUMN_SEPARATOR, on_cluster=on_cluster
    )
    return _TRAILING_BLANKS.sub("", text, count=1)


def try_error_message(error: RunReadError) -> str:
    r"""What base R's ``try()`` writes to stderr for ``error`` (``R/loopRuns.R:57``, ``silent = FALSE``).

    ``Error in <call> : <message>\n`` with the call as ``deparse(call, nlines = 1L)`` printed it
    (``RunReadError.call``), the message moved to its own line indented by two blanks when
    ``14 + nchar(call) + nchar(first message line) > 75``; ``Error : <message>\n`` for an error without a
    call; then the warnings deferred during the call as ``printDeferredWarnings()`` prints them
    (:func:`deferred_warnings_message`). Display widths are taken as character counts (the messages are
    ASCII). From ``base::try`` in R 4.6.1; the goldens ``migration/goldens/rs/synthetic-remind.err:5-10``
    and ``synthetic-bug005.err:5-6`` pin both shapes.
    """
    message = str(error)
    if error.call is None:
        prefix = _TRY_ERROR_NO_CALL
    else:
        prefix = f"{_TRY_ERROR_IN}{error.call}{_TRY_SEPARATOR}"
        if _TRY_WIDTH_OFFSET + len(error.call) + len(_first_line(message)) > _TRY_LONG:
            prefix += "\n  "
    return f"{prefix}{message}\n{deferred_warnings_message(error.warnings)}"


def deferred_warnings_message(warnings: Sequence[DeferredWarning]) -> str:
    """``printDeferredWarnings()`` after ``try()``'s error line: the ``In addition:`` block, or nothing."""
    return warnings_block(warnings, in_addition=True)


def warnings_block(warnings: Sequence[DeferredWarning], *, in_addition: bool) -> str:
    """``PrintWarnings(header)`` (``errors.c``): the ``Warning message(s):`` block, or nothing.

    ``in_addition`` selects the header ``In addition: `` of ``printDeferredWarnings()`` (after ``try()``'s
    error line, ``R/loopRuns.R:57``, and after an uncaught error); without it the block is the one R's top
    level prints for the warnings of a script that ends normally (``rs -C`` with a failing ``sacct``).
    """
    header = _WARNINGS_HEADER if in_addition else ""
    if not warnings:
        return ""
    if len(warnings) == 1:
        return f"{header}{_WARNING_ONE}{_warning_line(warnings[0], _WARN_WIDTH_OFFSET_ONE)}"
    if len(warnings) <= _WARNINGS_LISTED_MAX:
        lines = "".join(
            f"{n}: {_warning_line(warning, _WARN_WIDTH_OFFSET_MANY)}" for n, warning in enumerate(warnings, 1)
        )
        return f"{header}{_WARNING_MANY}{lines}"
    return f"{header}There were {len(warnings)} warnings (use warnings() to see them)\n"


def _warning_line(warning: DeferredWarning, offset: int) -> str:
    """One entry of ``PrintWarnings``: ``In <call> : <message>`` (broken when long) or ``<message> ``."""
    if warning.call is None:
        return f"{warning.message} \n"
    head = f"In {warning.call} :"
    if offset + len(warning.call) + len(_first_line(warning.message)) > _WARN_LONG:
        head += "\n "
    return f"{head} {warning.message}\n"


def _first_line(text: str) -> str:
    r"""``strsplit(text, "\n")[[1L]][1L]``: the part before the first newline."""
    return text.split("\n", 1)[0]


def _status_of(name: str, user: str, env: Effects) -> RunStatus | RunReadError:
    """``try(getRunStatus(i, user = user))`` (``R/loopRuns.R:57``): the record, or the caught ``try-error``.

    ``getRunStatus`` on one existing directory yields exactly its row; on a missing path (an ``NA`` row
    name) the ``NA`` row. The directories were listed a moment ago, so an empty result is a race with the
    filesystem that R would turn into an obscure ``if (NA)`` error; it is reported plainly here.
    """
    try:
        records = get_run_status([name], user=user, effects=env)
    except RunReadError as error:
        return error
    if len(records) != 1:
        raise LoopRunsError(f"getRunStatus({name!r}) returned {len(records)} rows instead of one")
    return records[0]


def _write_row(
    name: str,
    user: str,
    layout: TableLayout,
    out: TextIO,
    err: TextIO,
    env: Effects,
    *,
    colors: bool,
    crayon: Crayon,
) -> None:
    """One iteration of ``for (i in mydir)`` (``R/loopRuns.R:56-133``)."""
    record = _status_of(name, user, env)
    if isinstance(record, RunReadError):
        err.write(try_error_message(record))  # try() prints the condition as it catches it (:57)
    if not has_config_or_log(name, env):
        if isinstance(record, RunReadError):  # BUG-005: the try-error is subscripted before it is tested
            raise LoopRunsError(SUBSCRIPT_OUT_OF_BOUNDS)
        if record.jobInSLURM == NO_JOB:
            out.write(f"{name} {SKIPPED}\n")
            return
    if isinstance(record, RunReadError):
        out.write(f"{r_basename(name)} {SKIPPED_BECAUSE_OF_ERROR}\n")
        return
    cells = display_cells(record)
    line = format_row(record.run, cells, layout, env.on_cluster)
    out.write(crayon.paint(colour_for_row(cells, line, colors), line))


# ---------------------------------------------------------------------------------------------
# The table (R/loopRuns.R:20-134)
# ---------------------------------------------------------------------------------------------


def loop_runs(
    dirs: PathLike | Sequence[PathLike],
    user: str | None = None,
    colors: bool = True,
    sortbytime: bool = True,
    effects: Effects | None = None,
    out: TextIO | None = None,
    err: TextIO | None = None,
    *,
    crayon: Crayon | None = None,
) -> str | None:
    """``loopRuns(mydir, user, colors, sortbytime)`` (``R/loopRuns.R:20-134``), written to ``out``.

    Returns ``"No runs found"`` for no directories and ``None`` otherwise, as R does (``:23-24``); for an
    ``"exit"`` first element nothing is printed. ``user`` defaults to ``Effects.user`` (``:22``);
    ``effects`` to the production environment; ``out`` to the current ``sys.stdout`` and ``err``, which
    receives the ``try()`` diagnostics of the failed ``getRunStatus`` calls (:func:`try_error_message`),
    to the current ``sys.stderr``; ``crayon`` to crayon enabled at 256 colours (what the goldens were
    recorded with), ``Crayon.disabled()`` emits no escape sequence at all.

    Off the cluster ``jobInSLURM`` is the string ``"NA"``, never ``"no"``, so the ``skipped.`` line of
    ``:58-61`` is unreachable there and a config-less folder or a missing path prints as a full row (the
    ``errors--local`` golden: ``noconfig`` as a row where the cluster prints ``NA skipped.``).

    Raises :class:`LoopRunsError` where ``loopRuns`` aborts (BUG-005, after the legend and the header were
    written), and lets the errors of ``printOutput`` and of the colour rules
    (:class:`modelstats.formatting.PrintOutputError`, :class:`modelstats.colors.ColourRuleError`)
    propagate as R does outside its ``try()``.
    """
    env = effects if effects is not None else ProductionEffects()
    stream = out if out is not None else sys.stdout
    errors = err if err is not None else sys.stderr
    paint = crayon if crayon is not None else Crayon()
    paths = [os.fspath(dirs)] if isinstance(dirs, str | os.PathLike) else [os.fspath(p) for p in dirs]
    who = env.user if user is None else user
    if not paths:
        return NO_RUNS_FOUND
    if paths[0] == EXIT:
        return None
    if colors:
        stream.write(colour_legend(paint))
    names = ordered_names(directory_rows(paths, env), sortbytime)
    width = folder_width([r_basename(env.normalize_path(name)) for name in names])
    layout = table_layout(width, env.on_cluster)
    stream.write(f"{paint.underline(layout.header)} \n")
    for name in names:
        _write_row(name, who, layout, stream, errors, env, colors=colors, crayon=paint)
    return None
