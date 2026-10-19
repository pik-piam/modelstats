"""One line of the status or sanity table: fixed-width cells joined by a separator.

R source: R/printOutput.R.

Deviations and fixes (details in PORTING-BUGS.md):

- the line comes back without the trailing newline R appended (BUG-006);
- widths for only some of the columns raise ``ValueError`` where R failed with an obscure error;
  with no widths at all, or only the first column's, R's fallback (13, 14, ... characters) is kept
  (BUG-005);
- a single column works where R stopped; a NaN cell is blank as in R (BUG-035).
"""

from __future__ import annotations

import math
from collections.abc import Mapping, Sequence
from typing import Protocol

from modelstats import _shell
from modelstats._timefmt import r_str

__all__ = ["CLUSTER_COLUMNS", "LOCAL_COLUMNS", "TableRow", "default_columns", "format_row"]

CLUSTER_COLUMNS = (
    "runtime",
    "job_in_slurm",
    "run_type",
    "run_status",
    "warnings",
    "iter",
    "conv",
    "modelstat",
    "mif",
    "run_in_app_results",
)
LOCAL_COLUMNS = ("runtime", "run_type", "run_status", "warnings", "iter", "conv", "modelstat", "mif")


class TableRow(Protocol):
    """What ``format_row`` needs: a name and one attribute per column (a ``RunStatus``)."""

    @property
    def name(self) -> str:
        """The folder name, printed in the first column."""
        ...


def default_columns(on_cluster: bool) -> list[str]:
    """The columns of the status table: with ``inSlurm`` and ``AppResults`` on the cluster only."""
    return list(CLUSTER_COLUMNS if on_cluster else LOCAL_COLUMNS)


def _format_cell(value: object, width: int) -> str:
    """Blank for NA (``None``/NaN), otherwise the text padded or cut to ``width``."""
    if width < 0:
        raise ValueError(f"negative column width {width}")
    if value is None or (isinstance(value, float) and math.isnan(value)):
        return " " * width
    return r_str(value).ljust(width)[:width]


def format_row(
    row: TableRow,
    col_widths: Sequence[int],
    col_sep: str = "   ",
    cols: Sequence[str] | None = None,
    *,
    cells: Mapping[str, object] | None = None,
) -> str:
    """Format one run as a table line.

    Args:
        row: A ``RunStatus`` (any object with a ``name`` and the column attributes).
        col_widths: Width of the first column followed by one width per column; extra widths are
            ignored. With only the first width the columns get ``13, 14, ...`` characters counted
            from the last column; with no widths at all the same, without separators between the
            columns (the one after the folder column stays).
        col_sep: String separating the columns.
        cols: Attributes to print; default: the status columns of ``default_columns``.
        cells: Display values that replace the attribute of a column (``{"runtime": "pending"}``).

    Returns:
        The line without newline; trailing spaces are kept.

    Raises:
        ValueError: Widths for only some of the columns, or a negative width.
    """
    columns = list(cols) if cols is not None else default_columns(_shell.on_cluster())
    if 1 < len(col_widths) < len(columns) + 1:
        raise ValueError(f"{len(col_widths)} widths for the folder and {len(columns)} columns")
    overrides = cells or {}
    first_width = col_widths[0] if col_widths else 67
    count = len(columns)
    formatted = []
    for position, column in enumerate(columns):
        width = col_widths[position + 1] if len(col_widths) > 1 else count - position + 12
        value = overrides[column] if column in overrides else getattr(row, column)
        formatted.append(_format_cell(value, width))
    separator = col_sep if col_widths else ""
    return _format_cell(row.name, first_width) + col_sep + separator.join(formatted)
