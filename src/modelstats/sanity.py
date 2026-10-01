"""``getSanityChecks``: the sanity-check overview of REMIND runs (``R/getSanityChecks.R``, line by line).

The function prints a header and then, per run directory, the six sanity columns of
:func:`modelstats.run_status.get_run_status` through :func:`modelstats.formatting.print_output`.
Everything is byte-identical with R, including its two pending bugs:

- the header joins the column titles with two spaces, the rows are printed with ``printOutput``'s default
  separator of three spaces (BUG-034, D-18 pending), so the cells sit one character further right than their
  titles;
- a run whose status record lacks any of the six columns (MAgPIE runs, REMIND runs without a mif) is silently
  omitted (BUG-012, D-04 pending).

R facts the port relies on (R 4.6.1, verified 2026-10-01, pinned in ``tests/unit/test_sanity.py``):

- ``cat("Results from", amtPath, "\\n")`` separates its arguments with a space, hence ``... output/ \\n``;
- ``dir(path, pattern, full.names = TRUE)`` lists files and directories alike, skips dot-files, keeps the
  names an unanchored regular-expression search matches, sorts them like ``dir()`` (ICU root collation,
  ``Effects.listdir_like_r``) and joins ``path`` and name with an unconditional ``/``: with the AMT path's
  trailing slash the entries read ``.../output//<name>``. An invalid pattern is the error
  ``invalid 'pattern' regular expression``. The AMT pattern is ``.*-AMT_<date>|.*-AMT_<date+1>``
  (``R/modeltests.R:92``), which R's TRE engine and Python's ``re`` read identically;
- ``normalizePath(dirs, mustWork = TRUE)`` expands a leading tilde, resolves an existing path to its real path
  and stops at the first element that does not exist with ``path[k]="<expanded>": No such file or directory``
  (1-based ``k``); ``character(0)`` gives ``character(0)`` and the folder width 15;
- ``cat(cyan(text))`` wraps the newline inside the escape sequences; ``cat(underline(header), "\\n")`` puts a
  space before the newline. Whether escapes are emitted is :mod:`modelstats.colors`' process-wide flag, never
  tty detection here (the harness forces colours on, see ``02-python-libraries.md`` section 4.5).
"""

from __future__ import annotations

import os
import re
import sys
from collections.abc import Sequence
from typing import IO

from modelstats import colors
from modelstats.env import Effects, PathLike, default_effects
from modelstats.errors import RParityError
from modelstats.formatting import print_output
from modelstats.rdata_io import read_rds, scalar
from modelstats.run_status import get_run_status
from modelstats.runfolders import r_basename
from modelstats.slurm import normalize_path

__all__ = [
    "AMT_PATH",
    "AMT_RUNCODE",
    "COLUMN_TITLES",
    "EXPLANATION",
    "SANITY_COLUMNS",
    "get_sanity_checks",
    "r_dir_pattern",
]

#: ``amtPath``: where the automated model tests write their REMIND runs (trailing slash as in R).
AMT_PATH = "/p/projects/remind/modeltests/remind/output/"
#: ``runcode.rds``: the regular expression of the current AMT run names, written by ``modeltests``.
AMT_RUNCODE = "/p/projects/remind/modeltests/remind/runcode.rds"

_COL_SEP = "  "
#: The column titles after ``Folder``; their widths are the ``lenCols`` of the rows.
COLUMN_TITLES: tuple[str, ...] = ("SumErr", "RangeErr", "FixingErr", "MissingVar", "ProjSumErr", "ProjSumErrReg")
#: The status columns printed under them (``cols``), in the same order.
SANITY_COLUMNS: tuple[str, ...] = (
    "summationErrors",
    "rangeErrors",
    "fixErrors",
    "missingProjVars",
    "projSummationErrors",
    "projSummationErrorsRegional",
)
EXPLANATION = (
    "For column explanations see: https://github.com/remindmodel/remind/blob/develop/tutorials/"
    "05_AnalysingModelOutputs.md#7-visualizing-run-status-and-summation-checks-for-runs\n"
)

_FOLDER_MIN = 15
_FOLDER_MAX = 67


def r_dir_pattern(path: str, pattern: str | None, effects: Effects) -> list[str]:
    """``dir(path = path, pattern = pattern, full.names = TRUE)``.

    The entries of ``path`` without dot-files in R's collation order, kept when ``pattern`` (an unanchored
    regular expression; ``None`` keeps everything) matches the name, each prefixed with ``path`` and a ``/``
    (R adds the separator even after a trailing slash). A missing directory or a file lists nothing.
    """
    names = effects.listdir_like_r(path)
    if pattern is not None:
        try:
            regex = re.compile(pattern)
        except re.error:
            raise RParityError("invalid 'pattern' regular expression") from None
        names = [name for name in names if regex.search(name) is not None]
    return [f"{path}/{name}" for name in names]


def _normalize_must_work(paths: Sequence[str], effects: Effects) -> list[str]:
    """``normalizePath(paths, mustWork = TRUE)``: the real paths, or R's error at the first missing element."""
    normalized: list[str] = []
    for k, path in enumerate(paths, start=1):
        candidate = normalize_path(path, effects)
        if candidate == "" or not effects.exists(candidate):
            raise RParityError(f'path[{k}]="{candidate}": No such file or directory')
        normalized.append(candidate)
    return normalized


def _amt_dirs(effects: Effects, stream: IO[str]) -> list[str]:
    """Lines 21-25: the current AMT runs (``readRDS`` of the runcode, then ``dir()`` with that pattern)."""
    stream.write(f"Results from {AMT_PATH} \n")  # cat("Results from", amtPath, "\n")
    runcode = scalar(read_rds(AMT_RUNCODE, effects))
    pattern = None if runcode is None else str(runcode)
    return r_dir_pattern(AMT_PATH, pattern, effects)


def get_sanity_checks(
    dirs: PathLike | Sequence[PathLike] | None = None,
    effects: Effects | None = None,
    out: IO[str] | None = None,
) -> None:
    """``getSanityChecks(dirs)``: print the sanity-check table of the runs to ``out`` (``sys.stdout`` when omitted).

    ``dirs`` is a path or a sequence of paths; ``None`` prints ``Results from <AMT path>`` and takes the runs
    of the current automated model tests (the ``runcode.rds`` pattern applied to the AMT output directory,
    which exists on the cluster only: elsewhere ``readRDS`` fails with ``cannot open the connection`` after
    that first line). A directory that does not exist raises ``RParityError`` with R's ``normalizePath``
    message before anything is printed. Each run is looked up with :func:`get_run_status` and its defaults
    (newest-first sort, the process user, detailed) and printed only when all six sanity columns are present.
    """
    eff = default_effects() if effects is None else effects
    stream = sys.stdout if out is None else out

    if dirs is None:
        paths = _amt_dirs(eff, stream)
    elif isinstance(dirs, str | os.PathLike):
        paths = [os.fspath(dirs)]
    else:
        paths = [os.fspath(path) for path in dirs]

    # len <- min(67, max(c(15, nchar(basename(normalizePath(dirs, mustWork = TRUE))))))
    width = max([_FOLDER_MIN, *(len(r_basename(path)) for path in _normalize_must_work(paths, eff))])
    width = min(_FOLDER_MAX, width)

    coltitles = ["Folder" + " " * (width - len("Folder")), *COLUMN_TITLES]
    len_cols = [len(title) for title in coltitles]
    cols = list(SANITY_COLUMNS)

    stream.write("\n")
    stream.write(colors.style("cyan", EXPLANATION))
    stream.write(colors.style("underline", _COL_SEP.join(coltitles)) + " \n")
    stream.write("\n")

    for path in paths:
        status = get_run_status(path, effects=eff)
        if all(column in status.columns for column in cols):
            # one path gives at most one row; printOutput is written for exactly one
            for row in status.rows():
                stream.write(print_output(row, rowname=row.rowname, len_cols=len_cols, cols=cols, effects=eff))
