"""Status records as a table, and the CSV counts behind the sanity columns of ``getRunStatus()``.

Two things the R function does with tabular data are reproduced here (``migration/03-migration-plan.md``
section 5, ``tables.py``):

* ``getRunStatus()`` returns a ``data.frame`` (``R/getRunStatus.R:28, 365``) whose rows are keyed by the
  run's basename and whose columns appear in the order of their first assignment inside the loop
  (``:38`` to ``:360``). The Python record list of phase 2 (``run_status.RunStatus``) is the primary
  result; :func:`to_dataframe` builds the pandas equivalent of the R frame from any rows exposing
  ``run`` and ``cells`` (``modelstats.formatting.StatusRow``, the one row shape of the presentation
  layer): run names as the index, the R column order (:data:`STATUS_COLUMNS`), the R storage types
  (character, integer, double), the literal ``"NA"`` strings kept as strings and a real ``NA`` as a
  missing value. A column one run has and another lacks (the sanity block of ``:325-361`` for a
  MAgPIE run in the same call) is ``NA`` in the lacking row, as in R; a run name seen twice updates
  the earlier row in place, because ``out[i, col] <- value`` on an existing row name overwrites
  (register BUG-022, decision D-13).
* The sanity counts read three CSV-like files with ``read.csv2`` (``:327-328``, ``:336-337``,
  ``:345-346``) and only use ``length(unique(tmp$variable))`` and ``nrow(tmp)``. :func:`read_error_csv`
  streams the file with the ``csv`` module under the quoting rules of ``read.csv2`` (``quote = '"'``,
  doubled quotes, no comment character, no escape character, empty lines skipped, the literal ``NA``
  as a missing value, the header on the first non-blank line) and keeps only the row count and the
  distinct values of the named columns, so the 36 MB ``log_fixOnRef.csv`` of a fixture run never
  sits in memory as a table. Not reproduced, by reading the ``read.table`` documentation: a header
  one field shorter than the data (row names from column 1), ``check.names`` renaming of header
  fields, ``type.convert`` of the ``variable`` column (which could only merge values that differ as
  text but not as numbers or logicals), and a quote character in the middle of an unquoted field.
  ``$`` partial matching of the column name is reproduced (``tmp$variable`` finds a unique column
  whose name starts with ``variable``). An input without a line is the R error ``no lines
  available in input``, raised as :class:`EmptyCsvError`, uncaught in R as in Python.
"""

from __future__ import annotations

import csv
import os
from collections.abc import Iterable, Iterator, Mapping, Sequence
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Any, Final

import pandas as pd

from modelstats.env import PathLike
from modelstats.errors import ModelstatsError
from modelstats.formatting import CellValue, StatusRow

__all__ = [
    "COLUMN_ORDER",
    "STATUS_COLUMNS",
    "EmptyCsvError",
    "ErrorCsv",
    "RType",
    "StatusColumn",
    "column_order",
    "dollar_column",
    "fix_error_count",
    "range_error_count",
    "read_error_csv",
    "summation_error_count",
    "to_dataframe",
]

INDEX_NAME: Final = "run"
"""The index of the frame holds ``rownames(out)``, the run basenames (``R/getRunStatus.R:36``)."""


class RType(Enum):
    """The R storage mode of a status column, which fixes the pandas dtype."""

    CHARACTER = "character"
    INTEGER = "integer"
    DOUBLE = "double"


@dataclass(frozen=True, slots=True)
class StatusColumn:
    """One column of the R frame: its name, storage mode and the R lines that assign it."""

    name: str
    rtype: RType
    source: str


STATUS_COLUMNS: Final[tuple[StatusColumn, ...]] = (
    StatusColumn("jobInSLURM", RType.CHARACTER, "R/getRunStatus.R:38"),
    StatusColumn("RunType", RType.CHARACTER, "R/getRunStatus.R:70-73"),
    StatusColumn("modelstat", RType.CHARACTER, "R/getRunStatus.R:82-102"),
    StatusColumn("runInAppResults", RType.CHARACTER, "R/getRunStatus.R:106-115 (cluster only)"),
    StatusColumn("Mif", RType.CHARACTER, "R/getRunStatus.R:121-136"),
    StatusColumn("Iter", RType.CHARACTER, "R/getRunStatus.R:151-156, 300"),
    StatusColumn("RunStatus", RType.CHARACTER, "R/getRunStatus.R:152-243"),
    StatusColumn("Warnings", RType.CHARACTER, "R/getRunStatus.R:256-273 (detailed only)"),
    StatusColumn("Conv", RType.CHARACTER, "R/getRunStatus.R:278-304 (detailed only)"),
    StatusColumn("Runtime", RType.DOUBLE, "R/getRunStatus.R:309-313 (detailed only)"),
    StatusColumn("summationErrors", RType.INTEGER, "R/getRunStatus.R:326-331 (detailed, REMIND with mif)"),
    StatusColumn("rangeErrors", RType.INTEGER, "R/getRunStatus.R:334-340"),
    StatusColumn("fixErrors", RType.INTEGER, "R/getRunStatus.R:343-348"),
    StatusColumn("missingProjVars", RType.INTEGER, "R/getRunStatus.R:351-360"),
    StatusColumn("projSummationErrors", RType.INTEGER, "R/getRunStatus.R:351-360"),
    StatusColumn("projSummationErrorsRegional", RType.INTEGER, "R/getRunStatus.R:351-360"),
)
"""The columns of ``getRunStatus()`` in order of first assignment (``migration/03-migration-plan.md`` 3.1)."""

COLUMN_ORDER: Final[tuple[str, ...]] = tuple(spec.name for spec in STATUS_COLUMNS)
_SPEC_BY_NAME: Final[Mapping[str, StatusColumn]] = {spec.name: spec for spec in STATUS_COLUMNS}
_DTYPE_BY_RTYPE: Final[Mapping[RType, str]] = {
    RType.CHARACTER: "str",
    RType.INTEGER: "Int64",
    RType.DOUBLE: "Float64",
}


def column_order(names: Iterable[str]) -> list[str]:
    """The R order of the given column names: :data:`COLUMN_ORDER` first, unknown names after it as seen.

    Every row assigns its columns in the fixed sequence of the loop, so the union over the rows in
    first-assignment order is this sequence restricted to the names present (register BUG-012 note:
    a MAgPIE first row does not change the order).
    """
    seen: dict[str, None] = dict.fromkeys(names)
    known = [name for name in COLUMN_ORDER if name in seen]
    unknown = [name for name in seen if name not in _SPEC_BY_NAME]
    return known + unknown


def _merge_rows(rows: Iterable[StatusRow]) -> dict[str, dict[str, CellValue]]:
    """Rows keyed by run name in first-seen order; a repeated name updates its row in place (D-13)."""
    merged: dict[str, dict[str, CellValue]] = {}
    for row in rows:
        merged.setdefault(row.run, {}).update(row.cells)
    return merged


def _column(name: str, values: Sequence[CellValue], index: pd.Index[Any]) -> pd.Series[Any]:
    """A pandas column with the dtype of the R storage mode; an unknown column keeps its objects."""
    spec = _SPEC_BY_NAME.get(name)
    dtype = "object" if spec is None else _DTYPE_BY_RTYPE[spec.rtype]
    return pd.Series(list(values), index=index, dtype=dtype, name=name)


def to_dataframe(rows: Iterable[StatusRow]) -> pd.DataFrame:
    """The ``data.frame`` ``getRunStatus()`` returns, as pandas (run names as the index, R column order).

    ``rows`` are the parity records (``run_status.RunStatus``) or any ``formatting.StatusRow`` (``run``
    and ``cells``; a column R never assigned is absent from ``cells``). Character columns use the
    ``str`` dtype (``"NA"`` stays the string ``"NA"``, a real ``NA`` is missing), integer columns
    ``Int64`` and ``Runtime`` ``Float64`` (R stores whole seconds as a double), both with ``pd.NA`` for
    ``NA``. No rows give the empty frame ``data.frame()``.
    """
    merged = _merge_rows(rows)
    index = pd.Index(list(merged), name=INDEX_NAME, dtype="str")
    names = column_order(name for cells in merged.values() for name in cells)
    columns = {name: _column(name, [cells.get(name) for cells in merged.values()], index) for name in names}
    return pd.DataFrame(columns, index=index)


# ---------------------------------------------------------------------------------------------
# read.csv2 counting
# ---------------------------------------------------------------------------------------------

NA_STRING: Final = "NA"
"""``na.strings = "NA"``: the one field text ``read.table`` reads as a missing value."""


class EmptyCsvError(ModelstatsError):
    """``read.table``'s ``no lines available in input``: the file has no non-blank line."""


@dataclass(frozen=True, slots=True)
class ErrorCsv:
    """What ``getRunStatus()`` uses of a ``read.csv2()`` result.

    ``header`` is the first non-blank line split by ``sep``; ``nrow`` counts the data rows, blank
    lines excluded; ``distinct`` maps each requested column that exists (after ``$`` matching) to
    ``length(unique(tmp$column))``, where every ``NA`` field counts as one value.
    """

    path: Path
    header: tuple[str, ...]
    nrow: int
    distinct: Mapping[str, int]


def dollar_column(header: Sequence[str], name: str) -> int | None:
    """The index ``tmp$name`` selects: the exact name, else the one column starting with ``name``, else none.

    R's ``$`` on a data frame partially matches a unique prefix (``exact = FALSE``); two candidates
    give ``NULL`` as a missing column does.
    """
    if name in header:
        return header.index(name)
    candidates = [index for index, column in enumerate(header) if column.startswith(name)]
    return candidates[0] if len(candidates) == 1 else None


def _records(path: PathLike, sep: str) -> Iterator[list[str]]:
    """The records of the file under ``read.csv2`` quoting, blank lines skipped (``blank.lines.skip``)."""
    with open(path, encoding="utf-8", errors="surrogateescape", newline="") as handle:
        reader = csv.reader(handle, delimiter=sep, quotechar='"', doublequote=True, strict=False)
        for record in reader:
            if record:
                yield record


def _field_value(text: str) -> str | None:
    """A field as a character column holds it: ``"NA"`` is the missing value, anything else itself."""
    return None if text == NA_STRING else text


def read_error_csv(path: PathLike, *, sep: str = ";", distinct: Sequence[str] = ("variable",)) -> ErrorCsv:
    """``read.csv2(path, sep = sep)`` reduced to ``nrow()`` and ``length(unique(tmp$col))`` for ``distinct``.

    ``sep`` defaults to ``read.csv2``'s semicolon (``R/getRunStatus.R:336``); the summation and
    fix-on-ref files pass ``","`` (``:327, 345``). Raises :class:`EmptyCsvError` for a file without a
    non-blank line.
    """
    records = _records(path, sep)
    header = next(records, None)
    if header is None:
        raise EmptyCsvError(f"no lines available in input: {os.fspath(path)}")
    columns = {name: index for name in distinct if (index := dollar_column(header, name)) is not None}
    seen: dict[str, set[str | None]] = {name: set() for name in columns}
    nrow = 0
    for record in records:
        nrow += 1
        for name, index in columns.items():
            seen[name].add(_field_value(record[index]) if index < len(record) else "")
    return ErrorCsv(Path(path), tuple(header), nrow, {name: len(values) for name, values in seen.items()})


def summation_error_count(path: PathLike) -> int:
    """``length(unique(read.csv2(sumErrFile, sep = ",")$variable))`` (``R/getRunStatus.R:327-328``)."""
    return read_error_csv(path, sep=",").distinct.get("variable", 0)


def range_error_count(path: PathLike) -> int:
    """``nrow(read.csv2(rangeErrFile))`` (``R/getRunStatus.R:336-337``): one row per non-blank line."""
    return read_error_csv(path, sep=";", distinct=()).nrow


def fix_error_count(path: PathLike) -> int:
    """``length(unique(read.csv2(fixErrFile, sep = ",")$variable))`` (``R/getRunStatus.R:345-346``)."""
    return read_error_csv(path, sep=",").distinct.get("variable", 0)
