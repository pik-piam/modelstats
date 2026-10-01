"""Table helpers: the ``read.csv2`` counts of ``R/getRunStatus.R:325-349`` and the pandas view.

``count_unique_variable`` is ``length(unique(read.csv2(file, sep = ",")$variable))`` and
``count_rows`` is ``nrow(read.csv2(file))`` (``sep = ";"``). Both go through a small emulation
of ``read.table`` as ``read.csv2`` configures it (``header = TRUE``, ``quote = "\\""``,
``dec = ","``, ``fill = TRUE``, ``comment.char = ""``, ``na.strings = "NA"``,
``blank.lines.skip = TRUE``, ``strip.white = FALSE``, ``check.names = TRUE``), verified against
R 4.6.1 on the fixture files and on the edge cases listed in ``tests/unit/test_tables.py``:

- a quote character starts a quoted region anywhere in a field; ``""`` inside one is a literal
  quote; separators and newlines inside quotes belong to the field; a quote left open runs to
  the end of the file (R warns ``EOF within quoted string`` and keeps the row), unless it opens
  within the first five lines, where R's header reader drops every data row;
- empty lines are skipped, lines of blanks are rows;
- the record width is the largest field count among the first five lines; a header one name
  short of it makes the first field the row name (duplicate or missing row names are errors);
  rows short of the width are padded with empty fields (NA only after the type conversion of a
  numeric or logical column), surplus fields spill into a new record;
- ``$variable`` partially matches the ``make.names``-mangled header (exact name first, then a
  unique prefix, else no column at all, which counts 0);
- ``unique()`` sees the column after ``type.convert``: ``"NA"`` and missing fields are NA, a
  column of ``T``/``F``/``TRUE``/``FALSE`` is logical, a column of numbers with ``,`` as the
  decimal mark is numeric (so ``"1"``, ``"1,0"`` and ``"01"`` are one value and blanks are NA),
  anything else stays character with ``""`` and blanks as values of their own.
"""

from __future__ import annotations

import math
import os
import re
from collections.abc import Iterable, Mapping, Sequence
from pathlib import Path
from typing import Protocol, runtime_checkable

import pandas as pd

from modelstats.errors import RParityError

__all__ = ["count_rows", "count_unique_variable", "to_dataframe"]

type PathLike = str | os.PathLike[str]

_HEAD_LINES = 5
_NA_STRING = "NA"
_RESERVED = frozenset(
    {
        "if", "else", "repeat", "while", "function", "for", "next", "break", "TRUE", "FALSE", "NULL", "Inf", "NaN",
        "NA", "NA_integer_", "NA_real_", "NA_character_", "NA_complex_", "in",
    }
)  # fmt: skip
_BLANK = " \t\n\r\f\v"
_LOGICAL = {"T": True, "TRUE": True, "F": False, "FALSE": False}
_NAN = object()


# ---------------------------------------------------------------------------
# public API
# ---------------------------------------------------------------------------


def count_unique_variable(csv_path: PathLike) -> int:
    """``length(unique(read.csv2(csv_path, sep = ",")$variable))``."""
    names, rows = _read_table(_load(csv_path), ",")
    index = _match_column(names, "variable")
    if index is None:
        return 0
    return _unique_count_after_type_convert([row[index] for row in rows])


def count_rows(csv_path: PathLike) -> int:
    """``nrow(read.csv2(csv_path))``."""
    _names, rows = _read_table(_load(csv_path), ";")
    return len(rows)


@runtime_checkable
class TableLike(Protocol):
    """What ``to_dataframe`` needs from a status table (``modelstats.run_status.StatusTable``)."""

    @property
    def columns(self) -> Sequence[str]: ...

    def to_json_rows(self) -> list[dict[str, object]]: ...


def to_dataframe(table: TableLike | Iterable[Mapping[str, object]]) -> pd.DataFrame:
    """A pandas view of a status table: one row per run, the run name (``_row``) as the index.

    Cells are kept verbatim (object dtype): ``None`` is a real NA, the string ``"NA"`` stays a
    string, a column absent from a row is ``None``. Columns are in the table's order.
    """
    if isinstance(table, TableLike):
        columns: list[str] = [col for col in table.columns if col != "_row"]
        rows = table.to_json_rows()
    else:
        rows = [dict(row) for row in table]
        columns = []
        for row in rows:
            for key in row:
                if key != "_row" and key not in columns:
                    columns.append(key)
    index = pd.Index([row.get("_row") for row in rows], name="run")
    data = {col: [row.get(col) for row in rows] for col in columns}
    return pd.DataFrame(data, index=index, columns=columns, dtype=object)


# ---------------------------------------------------------------------------
# read.table emulation
# ---------------------------------------------------------------------------


def _load(path: PathLike) -> str:
    # read.csv2 opens a file() connection, which applies path.expand() (a leading ~ only)
    text = Path(os.path.expanduser(os.fspath(path))).read_bytes().decode("utf-8", "surrogateescape")
    return text.replace("\r\n", "\n").replace("\r", "\n")


def _tokenize(text: str, sep: str) -> tuple[list[list[str]], int | None]:
    """Physical lines as lists of fields (quotes resolved) and the index of the line opening an unclosed quote."""
    if '"' not in text:
        return [line.split(sep) for line in text.split("\n")], None
    token = re.compile(f'"((?:[^"]|"")*)("|\\Z)|([^"{re.escape(sep)}\n]+)|([{re.escape(sep)}\n])', re.DOTALL)
    lines: list[list[str]] = []
    fields: list[str] = []
    field: list[str] = []
    unclosed: int | None = None
    for match in token.finditer(text):
        quoted, closing, literal, control = match.groups()
        if control == "\n":
            fields.append("".join(field))
            lines.append(fields)
            fields, field = [], []
        elif control is not None:
            fields.append("".join(field))
            field = []
        elif literal is not None:
            field.append(literal)
        else:
            field.append(quoted.replace('""', '"'))
            if closing == "" and unclosed is None:
                unclosed = len(lines)
    fields.append("".join(field))
    lines.append(fields)
    return lines, unclosed


def _read_table(text: str, sep: str) -> tuple[list[str], list[list[str | None]]]:
    """Header names (after ``make.names``) and data records (``None`` = NA) of ``read.csv2(text, sep = sep)``."""
    lines, unclosed = _tokenize(text, sep)
    # blank.lines.skip: a line is blank when it is empty; the last element is the partial line after
    # the final newline (empty when the text ends with one).
    physical = [fields for fields in lines if fields != [""]]
    if not physical:
        raise RParityError("no lines available in input")
    head = physical[:_HEAD_LINES]
    if all(fields == [""] or not any(f.strip(_BLANK) for f in fields) for fields in head[:1]):
        raise RParityError("first five rows are empty: giving up")
    header = physical[0]
    col1 = len(header)
    cols = max(len(fields) for fields in head)
    rlabp = cols - col1 == 1
    if col1 + rlabp < cols:
        raise RParityError("more columns than column names")
    cols = max(cols, col1)
    names = _make_names(header)
    if unclosed is not None and unclosed < _HEAD_LINES:
        return names, []
    records: list[list[str | None]] = []
    current: list[str | None] = []
    for fields in physical[1:]:
        for value in fields:
            current.append(None if value == _NA_STRING else value)
            if len(current) == cols:
                records.append(current)
                current = []
        if current:
            # scan(fill = TRUE) pads a short row with the empty field ""; type.convert then decides whether
            # it becomes NA (numeric / logical column) or stays a value of its own (character column)
            current.extend([""] * (cols - len(current)))
            records.append(current)
            current = []
    if rlabp:
        row_names = [record[0] for record in records]
        if any(name is None for name in row_names):
            raise RParityError("missing values in 'row.names' are not allowed")
        if len(set(row_names)) != len(row_names):
            raise RParityError("duplicate 'row.names' are not allowed")
        records = [record[1:] for record in records]
    return names, records


def _make_names(names: Sequence[str]) -> list[str]:
    """``make.names(names, unique = TRUE)``."""
    out: list[str] = []
    for name in names:
        s = "".join(ch if ch.isalnum() or ch in "._" else "." for ch in name)
        valid_start = s[:1].isalpha() or (s[:1] == "." and not s[1:2].isdigit())
        if not valid_start:
            s = "X" + s
        if s in _RESERVED:
            s += "."
        out.append(s)
    return _make_unique(out)


def _make_unique(names: list[str]) -> list[str]:
    """``make.unique(names, sep = ".")``."""
    seen: dict[str, int] = {}
    taken = set(names)
    result: list[str] = []
    for name in names:
        if name in seen:
            n = seen[name]
            candidate = f"{name}.{n}"
            while candidate in taken:
                n += 1
                candidate = f"{name}.{n}"
            seen[name] = n + 1
            taken.add(candidate)
            result.append(candidate)
        else:
            seen[name] = 1
            result.append(name)
    return result


def _match_column(names: Sequence[str], wanted: str) -> int | None:
    """``df$wanted``: exact match, else a unique partial (prefix) match, else NULL."""
    if wanted in names:
        return names.index(wanted)
    partial = [i for i, name in enumerate(names) if name.startswith(wanted)]
    return partial[0] if len(partial) == 1 else None


# ---------------------------------------------------------------------------
# type.convert(dec = ",") and unique()
# ---------------------------------------------------------------------------

_NUMBER = re.compile(
    r"""^[+-]?(?:
        inf(?:inity)?|nan
      | 0x[0-9a-f]+(?:p[+-]?[0-9]+)?
      | (?:[0-9]+(?:,[0-9]*)?|,[0-9]+)(?:e[+-]?[0-9]+)?
    )$""",
    re.IGNORECASE | re.VERBOSE,
)


def _r_number(value: str) -> float | object | None:
    """The double R's ``type.convert(dec = ",")`` reads from ``value``, ``_NAN`` for NaN, ``None`` when not a number."""
    stripped = value.strip(_BLANK)
    if _NUMBER.match(stripped) is None:
        return None
    lowered = stripped.lower()
    if lowered.lstrip("+-") in ("nan",):
        return _NAN
    if "0x" in lowered:
        return float.fromhex(lowered if "p" in lowered else lowered + "p0")
    number = float(lowered.replace(",", "."))
    return _NAN if math.isnan(number) else number


def _unique_count_after_type_convert(values: Sequence[str | None]) -> int:
    present = [v for v in values if v is not None and v.strip(_BLANK) != ""]
    if all(v in _LOGICAL for v in present):
        return len({None if v is None or v.strip(_BLANK) == "" else _LOGICAL[v] for v in values})
    numbers = [_r_number(v) for v in present]
    if all(n is not None for n in numbers):
        converted = {None if v is None or v.strip(_BLANK) == "" else _r_number(v) for v in values}
        return len(converted)
    return len(set(values))
