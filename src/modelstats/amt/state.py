"""The AMT state files ``runsToStart.rds``, ``lastcommit.rds``, ``gRS.rds`` and ``runcode.rds`` (plan 03 2.3, D-17).

The four files stay RDS while the R and the Python AMT coexist: the R package remains the
rollback for the whole transition and ``rs -A`` (R or Python) reads ``runcode.rds``, so no
second format is introduced and nothing is read "if present". Everything goes through
:mod:`modelstats.rdata_io` (``rdata``), whose writer was verified to give
``identical(readRDS(original), readRDS(copy))`` in R 4.6.1 for every fixture file.

What each file holds (``str()`` of the fixtures under ``migration/fixtures/p/projects/remind/modeltests/remind/``
and ``landuse/tests/magpie/``, checked 2026-10-01):

- ``lastcommit.rds``, ``runcode.rds``: a length-1 character vector (``R/modeltests.R`` lines 164, 174,
  238, 489);
- ``runsToStart.rds``: the ``selectScenarios()`` data.frame with character row names and character,
  integer and logical columns, ``NA`` in each (lines 136-138, 368-369); it is kept as the pandas frame
  ``rdata`` returns and written back with the same column types;
- ``gRS.rds``: the ``getRunStatus()`` data.frame (lines 226-235) as a :class:`GrsTable`, a
  :class:`~modelstats.run_status.StatusTable` that also remembers the R storage type of every column.
  The fixture has nine character columns holding the literal string ``"NA"`` (never a real NA),
  ``Runtime``, ``summationErrors``, ``rangeErrors`` and ``fixErrors`` as double and the three
  ``projSummation``/``missingProjVars`` columns as integer, each with real NA; a real NA is ``None``
  in the table and the string ``"NA"`` stays a string, in both directions.

R facts reproduced here (R 4.6.1, 2026-10-01; the probes are pinned in ``tests/unit/test_amt_state.py``):

- ``readRDS()`` of a missing file warns ``cannot open compressed file '<path>', probable reason
  '<strerror>'`` from ``gzfile(file, "rb")`` and then fails with ``cannot open the connection``
  (same call); a file that is not an RDS stream fails with ``unknown input format`` in ``readRDS(<expr>)``.
- ``rbind.data.frame`` (line 230) drops zero-length arguments (``NULL`` and a zero-column
  ``data.frame()``), fails with ``numbers of columns of arguments do not match`` when the column
  counts differ, with ``names do not match previous names`` (call ``match.names(clabs, names(xi))``)
  when the names differ, matches columns by name in the order of the first frame, promotes column
  types ``logical < integer < double < character`` (numbers become ``as.character()`` text) and
  makes duplicate row names unique with ``make.unique(sep = "")`` (``r1, r1, r11`` gives ``r1, r12, r11``).
- A column of ``getRunStatus()``'s frame has the type of its first assignment, promoted by later ones:
  ``Runtime`` is ``NA`` then ``as.numeric(round(difftime(...)))`` (double unless every row stayed NA,
  then logical); ``summationErrors``, ``rangeErrors`` and ``fixErrors`` take the literal ``0`` (double)
  for a run without the file and ``length()``/``nrow()`` (integer) otherwise, so one such run makes the
  column double; the ``projectSummations.rds`` values are integer. The Python table cannot tell an R
  integer from an integral double, so :func:`infer_r_type` uses those column defaults for a table
  that was not read from a file (``_GRS_NUMERIC_TYPES``); a column whose type IS known (read from
  ``gRS.rds`` or produced by :func:`rbind_status`) keeps it and is promoted only when a cell needs
  it (:func:`required_r_type`), as ``rbind(gRSold, data.frame())`` keeps an integer column integer
  and ``rbind(int, dbl)`` promotes (R 4.6.1 probes, 2026-10-01); the values are the same either way
  and the R side never depends on the storage type (``rbind`` promotes).
- ``data.frame()`` (what ``getRunStatus(character(0))`` returns) has ``integer(0)`` row names; an empty
  :class:`GrsTable` is written that way so that ``identical()`` holds.

Every write goes through :meth:`modelstats.env.Effects.write_rds`, the one ``saveRDS`` of the package:
``ProductionEffects`` writes a temporary ``.<name>.<random>.tmp`` beside the target (a dot-file, invisible
to R's ``dir()`` and ``list.dirs()``) and renames it over the target, removing it on failure, so a crash
leaves either the old state file or the new one, never a torn one; ``DryRunEffects`` only logs the write.
Reads go through :func:`modelstats.rdata_io.read_rds` with the given Effects, so a recording double sees them.
"""

from __future__ import annotations

import math
import os
import warnings
from collections.abc import Mapping, Sequence
from typing import Any, Literal

import numpy as np
import pandas as pd

from modelstats.env import Effects, PathLike, default_effects
from modelstats.errors import RParityError, RWarning
from modelstats.formatting import r_num_str
from modelstats.rdata_io import read_rds, vector
from modelstats.run_status import StatusTable

__all__ = [
    "GRS_FILE",
    "LASTCOMMIT_FILE",
    "RUNCODE_FILE",
    "RUNS_TO_START_FILE",
    "GrsTable",
    "RType",
    "column_r_types",
    "grs_from_frame",
    "grs_to_frame",
    "infer_r_type",
    "rbind_status",
    "read_grs",
    "read_lastcommit",
    "read_runcode",
    "read_runs_to_start",
    "required_r_type",
    "run_names",
    "save_rds",
    "with_amt_suffix",
    "write_grs",
    "write_lastcommit",
    "write_runcode",
    "write_runs_to_start",
]

#: File names as ``R/modeltests.R`` composes them (``paste0(mydir, "/lastcommit.rds")`` and so on).
RUNS_TO_START_FILE = "runsToStart.rds"
LASTCOMMIT_FILE = "lastcommit.rds"
GRS_FILE = "gRS.rds"
RUNCODE_FILE = "runcode.rds"

#: The deparsed R calls of the four ``readRDS()`` sites, for ``Error in <call> :`` texts.
_CALL_LASTCOMMIT = 'readRDS(paste0(mydir, "/lastcommit.rds"))'  # line 174
_CALL_RUNCODE = 'readRDS(paste0(mydir, "/runcode.rds"))'  # line 238
_CALL_GRS = 'readRDS("gRS.rds")'  # line 228
_CALL_RUNS_TO_START = 'readRDS(paste0(mydir, "runsToStart.rds"))'  # line 368
_CALL_GZFILE = 'gzfile(file, "rb")'
_CALL_RBIND = "rbind(deparse.level, ...)"
_CALL_MATCH_NAMES = "match.names(clabs, names(xi))"

type RType = Literal["logical", "integer", "double", "character"]
_RANK: dict[str, int] = {"logical": 0, "integer": 1, "double": 2, "character": 3}
#: Storage types of ``getRunStatus()``'s numeric columns in R (see the module docstring).
_GRS_NUMERIC_TYPES: dict[str, RType] = {
    "Runtime": "double",
    "summationErrors": "double",
    "rangeErrors": "double",
    "fixErrors": "double",
    "missingProjVars": "integer",
    "projSummationErrors": "integer",
    "projSummationErrorsRegional": "integer",
}


# ---------------------------------------------------------------------------
# Effects
# ---------------------------------------------------------------------------


def _effects(effects: Effects | None) -> Effects:
    return effects if effects is not None else default_effects()


def save_rds(path: PathLike, obj: object, effects: Effects | None = None) -> None:
    """``saveRDS(obj, path)``: :meth:`~modelstats.env.Effects.write_rds` of the given (or default) Effects.

    The object is serialised before anything touches the directory (a value ``rdata`` cannot
    write leaves no trace), and the write is atomic (see the module docstring).
    """
    _effects(effects).write_rds(path, obj)


# ---------------------------------------------------------------------------
# readRDS with R's conditions
# ---------------------------------------------------------------------------


def _read(path: PathLike, effects: Effects | None, r_call: str) -> object:
    """``readRDS(path)``: the object, or R's warning and error for a missing or corrupt file."""
    try:
        return read_rds(path, _effects(effects))
    except RParityError as exc:
        if str(exc) == "cannot open the connection":
            cause = exc.__cause__
            reason = (
                os.strerror(cause.errno)
                if isinstance(cause, OSError) and cause.errno is not None
                else "No such file or directory"
            )
            text = f"cannot open compressed file '{os.fspath(path)}', probable reason '{reason}'"
            warnings.warn(RWarning(_CALL_GZFILE, text), stacklevel=3)
            raise RParityError(str(exc), call=_CALL_GZFILE) from exc
        raise RParityError(str(exc), call=r_call) from exc


def _character(obj: object, what: str) -> str:
    """The one string an R character vector of length 1 holds."""
    if isinstance(obj, pd.DataFrame):
        msg = f"{what} holds a data.frame, not a character string"
        raise TypeError(msg)
    values = vector(obj)
    if len(values) == 1 and isinstance(values[0], str):
        return values[0]
    msg = f"{what} holds {_describe(obj)}, not a character string"
    raise TypeError(msg)


def _frame(obj: object, what: str) -> pd.DataFrame:
    if isinstance(obj, pd.DataFrame):
        return obj
    msg = f"{what} holds {_describe(obj)}, not a data.frame"
    raise TypeError(msg)


def _describe(obj: object) -> str:
    if obj is None:
        return "NULL"
    if isinstance(obj, pd.DataFrame):
        return "a data.frame"
    values = vector(obj)
    return f"a vector of length {len(values)}" if len(values) != 1 else f"{type(values[0]).__name__} {values[0]!r}"


# ---------------------------------------------------------------------------
# lastcommit.rds, runcode.rds
# ---------------------------------------------------------------------------


def read_lastcommit(path: PathLike, effects: Effects | None = None) -> str:
    """``readRDS(paste0(mydir, "/lastcommit.rds"))`` (line 174): the commit hash of the last test."""
    return _character(_read(path, effects, _CALL_LASTCOMMIT), LASTCOMMIT_FILE)


def write_lastcommit(path: PathLike, commit: str, effects: Effects | None = None) -> None:
    """``saveRDS(commitTested, file = paste0(mydir, "/lastcommit.rds"))`` (line 489)."""
    save_rds(path, str(commit), effects)


def read_runcode(path: PathLike, effects: Effects | None = None) -> str:
    """``readRDS(paste0(mydir, "/runcode.rds"))`` (line 238): the regex of the current AMT run names."""
    return _character(_read(path, effects, _CALL_RUNCODE), RUNCODE_FILE)


def write_runcode(path: PathLike, runcode: str, effects: Effects | None = None) -> None:
    """``saveRDS(runcode, file = paste0(mydir, "/runcode.rds"))`` (line 164)."""
    save_rds(path, str(runcode), effects)


# ---------------------------------------------------------------------------
# runsToStart.rds
# ---------------------------------------------------------------------------


def read_runs_to_start(path: PathLike, effects: Effects | None = None) -> pd.DataFrame:
    """``readRDS(paste0(mydir, "runsToStart.rds"))`` (line 368): the ``selectScenarios()`` frame, row names as index."""
    return _frame(_read(path, effects, _CALL_RUNS_TO_START), RUNS_TO_START_FILE)


def write_runs_to_start(path: PathLike, frame: pd.DataFrame, effects: Effects | None = None) -> None:
    """``saveRDS(runsToStart, file = paste0(mydir, "/runsToStart.rds"))`` (line 138), column types as read."""
    save_rds(path, frame, effects)


def run_names(frame: pd.DataFrame) -> list[str]:
    """``rownames(runsToStart)`` (line 369)."""
    return [str(name) for name in frame.index]


def with_amt_suffix(frame: pd.DataFrame) -> pd.DataFrame:
    """``row.names(runsToStart) <- paste0(row.names(runsToStart), "-AMT")`` (line 137), on a copy."""
    out = frame.copy()
    out.index = pd.Index([f"{name}-AMT" for name in run_names(frame)], dtype=object)
    return out


# ---------------------------------------------------------------------------
# gRS.rds: StatusTable <-> data.frame
# ---------------------------------------------------------------------------


class GrsTable(StatusTable):
    """A :class:`~modelstats.run_status.StatusTable` that remembers the R storage type of each column.

    ``r_types`` holds the type of every column read from ``gRS.rds`` or produced by
    :func:`rbind_status`; a column assigned later has no entry and is typed by :func:`infer_r_type`
    at write time. A remembered type is kept as it is and promoted only when a cell needs it
    (:func:`required_r_type`: a string, a non-integral float), never by the column defaults of
    :func:`infer_r_type`, so an integer ``summationErrors`` read from the file is written back as
    integer, as R's ``rbind`` leaves it.
    """

    def __init__(self, r_types: Mapping[str, RType] | None = None) -> None:
        super().__init__()
        self.r_types: dict[str, RType] = dict(r_types or {})

    def __repr__(self) -> str:
        return f"GrsTable({len(self)} rows, columns {self.columns!r}, r_types {self.r_types!r})"


def infer_r_type(column: str, cells: Sequence[object]) -> RType:
    """The R storage type ``getRunStatus()`` gives a column holding ``cells`` (see the module docstring).

    All NA is logical (every assignment was ``NA``); any string makes the column character; a
    non-integral float makes it double; otherwise the known getRunStatus columns take their
    documented type and any other integer-valued column is integer (bools alone are logical).
    """
    present = [c for c in cells if c is not None]
    if not present:
        return "logical"
    if any(isinstance(c, str) for c in present):
        return "character"
    if any(isinstance(c, float) and not c.is_integer() for c in present):
        return "double"
    has_float = any(isinstance(c, float) for c in present)
    has_int = any(isinstance(c, int) and not isinstance(c, bool) for c in present)
    if not has_float and not has_int:
        return "logical"
    return _GRS_NUMERIC_TYPES.get(column, "double" if has_float else "integer")


def required_r_type(cells: Sequence[object]) -> RType:
    """The lowest R storage type that holds ``cells`` (no column defaults, unlike :func:`infer_r_type`).

    No non-NA cell is logical; any string makes it character; a non-integral float double; any other
    number (an int, an integral float) integer; bools alone logical. Used to promote a REMEMBERED column
    type: R's ``rbind`` promotes a stored integer column only when a new value needs it.
    """
    present = [c for c in cells if c is not None]
    if not present:
        return "logical"
    if any(isinstance(c, str) for c in present):
        return "character"
    if any(isinstance(c, float) and not c.is_integer() for c in present):
        return "double"
    if any(isinstance(c, int | float) and not isinstance(c, bool) for c in present):
        return "integer"
    return "logical"


def _promote(a: RType, b: RType) -> RType:
    return a if _RANK[a] >= _RANK[b] else b


def column_r_types(table: StatusTable) -> dict[str, RType]:
    """The R storage type of every column: remembered by a :class:`GrsTable` (promoted only when a cell
    needs it, :func:`required_r_type`), inferred with the getRunStatus defaults otherwise."""
    remembered: Mapping[str, RType] = table.r_types if isinstance(table, GrsTable) else {}
    out: dict[str, RType] = {}
    for column in table.columns:
        cells = table.column(column) or []
        known = remembered.get(column)
        out[column] = infer_r_type(column, cells) if known is None else _promote(known, required_r_type(cells))
    return out


def _as_character(value: object, source: RType) -> str:
    """``as.character()`` of one cell promoted into a character column."""
    if isinstance(value, str):
        return value
    if isinstance(value, bool):
        return "TRUE" if value else "FALSE"
    if isinstance(value, int | float):
        return r_num_str(float(value)) if source == "double" else r_num_str(value)
    msg = f"cannot coerce {type(value).__name__} to character"
    raise TypeError(msg)


def _coerce(value: object, source: RType, target: RType) -> object:
    """One cell of a ``source``-typed column as a ``target``-typed column stores it (``NA`` stays ``NA``)."""
    if value is None or source == target:
        return value
    if target == "character":
        return _as_character(value, source)
    if isinstance(value, bool):
        return int(value)
    return value


def _cell_from_pandas(value: object, rtype: RType) -> object:
    if value is None or value is pd.NA or (isinstance(value, float) and math.isnan(value)):
        return None
    if rtype == "character":
        return str(value)
    if rtype == "double":
        number = float(value)  # type: ignore[arg-type]
        return int(number) if number.is_integer() else number
    if rtype == "integer":
        return int(value)  # type: ignore[call-overload]
    return bool(value)


def _pandas_r_type(series: pd.Series[Any]) -> RType:
    dtype = series.dtype
    if isinstance(dtype, pd.StringDtype):
        return "character"
    if isinstance(dtype, pd.BooleanDtype):
        return "logical"
    if isinstance(dtype, pd.Int8Dtype | pd.Int16Dtype | pd.Int32Dtype | pd.Int64Dtype):
        return "integer"
    if isinstance(dtype, pd.Float32Dtype | pd.Float64Dtype):
        return "double"
    if isinstance(dtype, np.dtype):
        if dtype.kind == "b":
            return "logical"
        if dtype.kind in "iu":
            return "integer"
        if dtype.kind == "f":
            return "double"
        if dtype.kind in "OU":
            return "character"
    msg = f"column {series.name!r} has dtype {dtype}, which is not an R data.frame column type"
    raise TypeError(msg)


def grs_from_frame(frame: pd.DataFrame) -> GrsTable:
    """The ``gRS`` data.frame as a :class:`GrsTable`: rows and columns in frame order, R types remembered.

    A zero-row frame keeps its column types in ``r_types`` only (a StatusTable holds columns through
    cells); ``getRunStatus()`` never produces one.
    """
    types: dict[str, RType] = {str(column): _pandas_r_type(frame[column]) for column in frame.columns}
    table = GrsTable(types)
    rownames = [str(name) for name in frame.index]
    columns = [str(column) for column in frame.columns]
    values = {column: frame[column].tolist() for column in columns}
    for k, rowname in enumerate(rownames):
        for column in columns:
            table[rowname, column] = _cell_from_pandas(values[column][k], types[column])
    return table


def _pandas_column(rtype: RType, cells: Sequence[object], index: pd.Index[Any]) -> pd.Series[Any]:
    if rtype == "character":
        text = [None if c is None else _as_character(c, "character") for c in cells]
        return pd.Series(text, index=index, dtype=object)
    if rtype == "double":
        doubles = [np.nan if c is None else float(c) for c in cells]  # type: ignore[arg-type]
        return pd.Series(np.array(doubles, dtype=np.float64), index=index)
    if rtype == "integer":
        integers = [None if c is None else int(c) for c in cells]  # type: ignore[call-overload]
        return pd.Series(pd.array(integers, dtype="Int32"), index=index)
    return pd.Series(pd.array([None if c is None else bool(c) for c in cells], dtype="boolean"), index=index)


def grs_to_frame(table: StatusTable) -> pd.DataFrame:
    """The table as the R data.frame ``saveRDS(gRS, "gRS.rds")`` stores: typed columns, character row names.

    Column types come from :func:`column_r_types`; an empty table becomes ``data.frame()`` (integer(0)
    row names).
    """
    types = column_r_types(table)
    rownames = table.rownames
    index: pd.Index[Any] = pd.Index(rownames, dtype=object) if rownames else pd.RangeIndex(0)
    frame = pd.DataFrame(index=index)
    for column in table.columns:
        frame[column] = _pandas_column(types[column], table.column(column) or [], index)
    return frame


def read_grs(path: PathLike, effects: Effects | None = None) -> GrsTable:
    """``readRDS("gRS.rds")`` (line 228) as a :class:`GrsTable`."""
    return grs_from_frame(_frame(_read(path, effects, _CALL_GRS), GRS_FILE))


def write_grs(path: PathLike, table: StatusTable, effects: Effects | None = None) -> None:
    """``saveRDS(gRS, "gRS.rds")`` (lines 232, 235): any StatusTable, types remembered or inferred."""
    save_rds(path, grs_to_frame(table), effects)


# ---------------------------------------------------------------------------
# rbind(gRSold, getRunStatus(...)) (line 230)
# ---------------------------------------------------------------------------


def _make_unique(names: Sequence[str], sep: str) -> list[str]:
    """``make.unique(names, sep)``: a duplicate gets the first ``name + sep + k`` (k from 1) not already present."""
    taken = set(names)
    seen: dict[str, int] = {}
    out: list[str] = []
    for name in names:
        if name in seen:
            k = seen[name]
            candidate = f"{name}{sep}{k}"
            while candidate in taken:
                k += 1
                candidate = f"{name}{sep}{k}"
            seen[name] = k + 1
            taken.add(candidate)
            out.append(candidate)
        else:
            seen[name] = 1
            out.append(name)
    return out


def _copy(table: StatusTable, types: Mapping[str, RType]) -> GrsTable:
    out = GrsTable(types)
    for row in table.rows():
        for column in table.columns:
            out[row.rowname, column] = row[column]
        if row.path is not None:
            out.register_path(row.rowname, row.path)
    return out


def rbind_status(old: StatusTable | None, new: StatusTable) -> GrsTable:
    """``rbind(gRSold, getRunStatus(...))`` with ``rbind.data.frame`` semantics (see the module docstring).

    ``old`` is ``None`` when ``gRS.rds`` did not exist (R's ``NULL``). Raises
    :class:`~modelstats.errors.RParityError` with R's text and call for mismatched columns, which
    ``evaluateRuns`` catches in its ``try()``.
    """
    if old is None:
        return _copy(new, column_r_types(new))
    old_types, new_types = column_r_types(old), column_r_types(new)
    if not new.columns:
        return _copy(old, old_types)
    if not old.columns:
        return _copy(new, new_types)
    if len(old.columns) != len(new.columns):
        raise RParityError("numbers of columns of arguments do not match", call=_CALL_RBIND)
    if set(old.columns) != set(new.columns):
        raise RParityError("names do not match previous names", call=_CALL_MATCH_NAMES)
    columns = old.columns
    types: dict[str, RType] = {column: _promote(old_types[column], new_types[column]) for column in columns}
    out = GrsTable(types)
    sources = [(row, old_types) for row in old.rows()] + [(row, new_types) for row in new.rows()]
    names = _make_unique([row.rowname for row, _ in sources], sep="")
    for name, (row, source_types) in zip(names, sources, strict=True):
        for column in columns:
            out[name, column] = _coerce(row[column], source_types[column], types[column])
        if row.path is not None:
            out.register_path(name, row.path)
    return out
