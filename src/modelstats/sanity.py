"""The sanity-check table of ``R/getSanityChecks.R``: :func:`get_sanity_checks`.

Parity port (``migration/03-migration-plan.md`` section 3.3, "getSanityChecks"; ``06`` section 1 keeps
every ``rs`` table a drop-in). The R function prints, for a list of REMIND runs, the six plausibility
counts that ``getRunStatus()`` computes from the ``convGDX2MIF`` and ``piamInterfaces`` outputs
(``R/getRunStatus.R:316-360``). What is reproduced, with the R lines:

* ``dirs = NULL`` is the default AMT lookup (``:22-26``): the ``Results from`` line, the regular
  expression of the current cycle and ``dir(amtPath, pattern, full.names = TRUE)``. The pattern comes
  from :func:`amt_runcode`: ``amt-state.json`` next to ``runcode.rds`` when the Python AMT has written
  one, ``runcode.rds`` otherwise (``06`` section 3.4, decision D-26; the goldens have no state file).
  A missing ``runcode.rds`` is R's ``readRDS`` error ``cannot open the connection``.
* ``normalizePath(dirs, mustWork = TRUE)`` (``:29``): the first path ``realpath`` cannot resolve stops
  the call with ``path[<i>]="<path>": <strerror>`` before anything is printed.
* the folder width ``min(67, max(15, nchar(basename(...))))`` (``:29-30``), the column titles and
  their widths (``:33-40``), the blank line, the explanation in cyan (the newline inside the colour,
  ``:45-46``), the titles joined with two spaces and underlined, followed by a space and a newline
  (``:47``), and another blank line (``:49``). Cyan and underline are crayon's and depend only on
  whether crayon is enabled (:class:`modelstats.colors.Crayon`), never on ``rs -b``.
* one ``getRunStatus(i)`` per directory (``:52``), the row through ``printOutput`` with ``lenCols``
  and ``cols`` but the default ``colSep`` of three spaces under the two-space header (``:55``, register
  BUG-034, decision D-18 pending), and a run whose record lacks any sanity column silently omitted
  (``:53``, BUG-012, D-04 pending): a MAgPIE run, a REMIND run without its mif, a path that is a file.
  An error of ``getRunStatus()`` propagates (:class:`modelstats.errors.RunReadError`) after the
  header, as R's does.
"""

from __future__ import annotations

import json
import os
import sys
from collections.abc import Sequence
from pathlib import PurePosixPath
from typing import Final, TextIO

from modelstats.colors import Crayon
from modelstats.env import Effects, PathLike, ProductionEffects
from modelstats.errors import ModelstatsError
from modelstats.facts import physical_path
from modelstats.formatting import StatusRow, print_output
from modelstats.rdata_io import read_rds, scalar
from modelstats.run_status import get_run_status

__all__ = [
    "AMT_STATE_FILE",
    "CANNOT_OPEN_CONNECTION",
    "EXPLANATION_URL",
    "HEADER_SEPARATOR",
    "INVALID_PATTERN",
    "SANITY_COLUMNS",
    "SANITY_TITLES",
    "SanityError",
    "amt_runcode",
    "column_titles",
    "default_amt_dirs",
    "folder_width",
    "get_sanity_checks",
    "header",
    "normalized_or_error",
    "sanity_row",
]

SANITY_COLUMNS: Final[tuple[str, ...]] = (
    "summationErrors",
    "rangeErrors",
    "fixErrors",
    "missingProjVars",
    "projSummationErrors",
    "projSummationErrorsRegional",
)
"""``cols`` of ``R/getSanityChecks.R:40-43``: the record columns printed, in order."""

SANITY_TITLES: Final[tuple[str, ...]] = (
    "SumErr",
    "RangeErr",
    "FixingErr",
    "MissingVar",
    "ProjSumErr",
    "ProjSumErrReg",
)
"""The column titles after ``Folder`` (``R/getSanityChecks.R:33-37``); their widths are ``nchar()``."""

EXPLANATION_URL: Final = (
    "https://github.com/remindmodel/remind/blob/develop/tutorials/05_AnalysingModelOutputs.md"
    "#7-visualizing-run-status-and-summation-checks-for-runs"
)
"""The tutorial section the header points to (``R/getSanityChecks.R:46``)."""

HEADER_SEPARATOR: Final = "  "
"""``colSep`` of ``R/getSanityChecks.R:28``: used for the header only (BUG-034)."""

AMT_STATE_FILE: Final = "amt-state.json"
"""The state file of the Python AMT, in the checkout directory next to ``runcode.rds`` (``06`` 3.2)."""

CANNOT_OPEN_CONNECTION: Final = "cannot open the connection"
"""R's ``readRDS`` error for a file it cannot open (``R/getSanityChecks.R:24`` off the cluster)."""

INVALID_PATTERN: Final = "invalid 'pattern' argument"
"""R's ``dir()`` error when ``runcode.rds`` holds anything but one character string (``:25``)."""

_FOLDER_TITLE: Final = "Folder"
_MIN_FOLDER_WIDTH: Final = 15
_MAX_FOLDER_WIDTH: Final = 67
_NO_SUCH_FILE: Final = "No such file or directory"
_NOT_A_DIRECTORY: Final = "Not a directory"
_STATE_BLOCKS: Final = ("cycle", "last_completed")


class SanityError(ModelstatsError):
    """An R error of ``getSanityChecks()`` itself (not of ``getRunStatus()``); ``str()`` is R's message."""


# --- the default AMT lookup (R/getSanityChecks.R:22-26) ----------------------------------------------


def amt_runcode(effects: Effects) -> str:
    """The regular expression naming the runs of the current AMT cycle (``06`` section 3.4, D-26).

    ``amt-state.json`` in the AMT checkout wins when it carries a ``runcode``: the running cycle's
    (``cycle.runcode``), else the last completed one's (``last_completed.runcode``, the idle state).
    Without the file, or without a runcode in it, ``runcode.rds`` is read as ``R/getSanityChecks.R:24``
    does: a missing file is :class:`SanityError` ``cannot open the connection`` and a value that is
    not one character string is ``dir()``'s ``invalid 'pattern' argument``.
    """
    from_state = _state_runcode(effects)
    if from_state is not None:
        return from_state
    path = effects.cluster_paths.amt_runcode
    if not effects.exists(path):
        raise SanityError(CANNOT_OPEN_CONNECTION)
    pattern = scalar(read_rds(physical_path(path, effects)))
    if not isinstance(pattern, str):
        raise SanityError(INVALID_PATTERN)
    return pattern


def _state_runcode(effects: Effects) -> str | None:
    """The runcode of ``amt-state.json``, or ``None`` when there is no file or no runcode in it."""
    path = f"{effects.cluster_paths.amt_dir}/{AMT_STATE_FILE}"
    if not effects.exists(path):
        return None
    try:
        state = json.loads(effects.read_text(path))
    except ValueError as error:
        raise SanityError(f"{path}: {error}") from error
    for block in _STATE_BLOCKS:
        runcode = _runcode_of(state, block)
        if runcode is not None:
            return runcode
    return None


def _runcode_of(state: object, block: str) -> str | None:
    """``state[block]["runcode"]`` when it is a string."""
    if not isinstance(state, dict):
        return None
    section = state.get(block)
    if not isinstance(section, dict):
        return None
    runcode = section.get("runcode")
    return runcode if isinstance(runcode, str) else None


def default_amt_dirs(effects: Effects, out: TextIO) -> list[str]:
    r"""``R/getSanityChecks.R:22-26``: announce the AMT output directory and list the cycle's runs.

    ``cat("Results from", amtPath, "\n")`` separates its arguments with spaces, so the line ends in
    a space; ``dir(full.names = TRUE)`` pastes the directory, which ends in a slash, so the paths carry
    a double slash that ``getRunStatus()`` normalises away.
    """
    paths = effects.cluster_paths
    out.write(f"Results from {paths.amt_output} \n")
    pattern = amt_runcode(effects)
    return effects.listdir_like_r(paths.amt_output, pattern=pattern, full_names=True)


# --- the folder column (R/getSanityChecks.R:29-30) ----------------------------------------------------


def normalized_or_error(path: PathLike, index: int, effects: Effects) -> str:
    """One element of ``normalizePath(dirs, mustWork = TRUE)``: the real path, or R's error.

    R calls ``realpath(3)`` and stops at the first element it cannot resolve with
    ``path[<i>]="<path>": <strerror(errno)>`` (``i`` is 1-based, the path as given). The two errors a
    run path can produce are reproduced through the effects: ``No such file or directory`` when a
    component is missing (a dangling symlink included) and ``Not a directory`` when a leading
    component is a file.
    """
    if effects.exists(path):
        return effects.normalize_path(path)
    raise SanityError(f'path[{index}]="{os.fspath(path)}": {_realpath_reason(path, effects)}')


def _realpath_reason(path: PathLike, effects: Effects) -> str:
    """``strerror`` of the first component ``realpath`` fails on: a file in the way, or nothing there."""
    absolute = PurePosixPath(effects.cwd) / os.fspath(path)
    for ancestor in reversed(absolute.parents):
        if ancestor == PurePosixPath("/"):
            continue
        if not effects.exists(ancestor):
            return _NO_SUCH_FILE
        if not effects.is_dir(ancestor):
            return _NOT_A_DIRECTORY
    return _NO_SUCH_FILE


def folder_width(dirs: Sequence[PathLike], effects: Effects) -> int:
    """``min(67, max(c(15, nchar(basename(normalizePath(dirs, mustWork = TRUE))))))`` (``:29-30``).

    The paths are resolved in order, so the error names the first unresolvable one; no path at all
    gives the minimum width.
    """
    longest = max(
        (len(os.path.basename(normalized_or_error(path, i, effects))) for i, path in enumerate(dirs, 1)),
        default=0,
    )
    return min(_MAX_FOLDER_WIDTH, max(_MIN_FOLDER_WIDTH, longest))


# --- the header (R/getSanityChecks.R:33-49) -------------------------------------------------------------


def column_titles(width: int) -> tuple[str, ...]:
    """``coltitles`` of ``R/getSanityChecks.R:33-37``: ``Folder`` padded to ``width``, then the six names.

    ``paste0("Folder", strrep(" ", len - 6))`` is ``Folder`` left-justified in ``len`` characters, since
    ``len`` is never below 15; ``nchar(coltitles)`` are the ``lenCols`` of every row.
    """
    return (_FOLDER_TITLE.ljust(width), *SANITY_TITLES)


def header(titles: Sequence[str], crayon: Crayon) -> str:
    r"""The four header lines of ``R/getSanityChecks.R:45-49``.

    A blank line; ``cat(cyan("For column explanations see: ...\n"))``, the newline inside the colour
    so the closing sequence starts the next line; ``cat(underline(paste(coltitles, collapse = "  ")),
    "\n")``, where ``cat`` puts a space before the newline; a blank line.
    """
    explanation = crayon.cyan(f"For column explanations see: {EXPLANATION_URL}\n")
    titles_line = crayon.underline(HEADER_SEPARATOR.join(titles))
    return f"\n{explanation}{titles_line} \n\n"


# --- the rows (R/getSanityChecks.R:52-57) --------------------------------------------------------------


def sanity_row(record: StatusRow, len_cols: Sequence[int]) -> str | None:
    """``R/getSanityChecks.R:53-55``: the record's row, or ``None`` when a sanity column is absent.

    ``all(cols %in% names(status))`` skips a record R never gave the block to (BUG-012): the
    row is only printed when every one of :data:`SANITY_COLUMNS` was assigned, a real ``NA`` (the
    project counts without ``projectSummations.rds``) printing as blanks. ``printOutput`` runs with
    ``lenCols`` and ``cols`` and its default ``colSep`` of three spaces (BUG-034).
    """
    if any(name not in record.cells for name in SANITY_COLUMNS):
        return None
    return print_output(record, len_cols=len_cols, cols=SANITY_COLUMNS)


def _as_paths(dirs: PathLike | Sequence[PathLike]) -> list[PathLike]:
    """A single path as a one-element vector, as R's character vector would be."""
    return [dirs] if isinstance(dirs, str | os.PathLike) else list(dirs)


def get_sanity_checks(
    dirs: PathLike | Sequence[PathLike] | None = None,
    effects: Effects | None = None,
    out: TextIO | None = None,
    *,
    crayon: Crayon | None = None,
) -> None:
    """``getSanityChecks(dirs)`` of ``R/getSanityChecks.R:19-58``, written to ``out``.

    ``dirs`` is the vector of run folders; ``None`` (R's ``NULL``) is the default AMT lookup of
    :func:`default_amt_dirs`, an empty sequence is a header over nothing. ``out`` defaults to the
    process's standard output at call time, ``effects`` to :class:`modelstats.env.ProductionEffects`
    and ``crayon`` to crayon enabled at 256 colours, the state the goldens were recorded under; the
    ``rs`` command passes :meth:`modelstats.colors.Crayon.from_terminal`. Raises :class:`SanityError`
    with R's message for a path that cannot be resolved or a missing ``runcode.rds``, and whatever
    :func:`modelstats.run_status.get_run_status` raises for a run folder R aborts on; what was printed
    before the error stays printed, as under ``cat``.
    """
    env = effects if effects is not None else ProductionEffects()
    stream = out if out is not None else sys.stdout
    styles = crayon if crayon is not None else Crayon()
    paths = default_amt_dirs(env, stream) if dirs is None else _as_paths(dirs)
    titles = column_titles(folder_width(paths, env))
    len_cols = [len(title) for title in titles]
    stream.write(header(titles, styles))
    for path in paths:
        for record in get_run_status(path, effects=env):
            row = sanity_row(record, len_cols)
            if row is not None:
                stream.write(row)
