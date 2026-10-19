"""R data files: ``load()``, ``readRDS()`` and ``saveRDS()`` of one string.

R source: the ``load("config.Rdata")``/``load("runstatistics.rda")``/``readRDS()`` calls of
getRunStatus.R, colRunType.R, commandLineInterface.R, promptAndRun.R and modeltests.R.

Conversion rules (over the ``rdata`` package):

- named list -> ``dict`` (with duplicate names the last entry wins, R's ``$`` took the first);
  unnamed list -> ``list``; ``NULL`` -> ``None``; ``NA`` of any type -> ``None`` (NA and NULL are
  no longer distinguishable);
- a length-1 atomic vector -> the scalar (``str``/``int``/``float``/``bool``), longer -> ``list``;
  a named atomic vector of length > 1 -> ``dict`` (names of character vectors are lost);
- ``POSIXct`` -> aware ``datetime`` in UTC (a POSIXct is an instant; its ``tzone`` attribute only
  affects printing). UTC rather than the zone because Python subtracts two datetimes that share one
  ``ZoneInfo`` object *without* consulting it, which would lose an hour across a DST switch;
  ``difftime`` -> ``float`` seconds; ``Date`` -> ``date``; ``factor`` -> its labels;
- ``data.frame`` (and tibbles) -> ``pandas.DataFrame`` of ``object`` columns with the row names as
  index and ``None`` for NA cells;
- objects of other classes (``packageDescription``, ``magpie`` arrays, ...) come back as their
  underlying value without warnings; arrays are flattened in R's column-major order. magclass
  arrays carry ``dimnames`` whose names are empty or repeated (``c("", "t", "")``), which rdata's
  xarray conversion rejects: such arrays are converted without their dimnames (BUG-044).

A file that cannot be parsed raises ``RDataReadError`` whatever the parser's own failure was
(the library-boundary rule of PORTING.md section 4.1).
"""

from __future__ import annotations

import math
import warnings
from collections.abc import Mapping
from datetime import UTC, date, datetime, timedelta
from pathlib import Path
from typing import Any
from unittest.mock import patch

import numpy as np
import pandas as pd
import rdata
from rdata.conversion import DEFAULT_CLASS_MAP
from rdata.conversion import _conversion as _rdata_conversion

from modelstats.errors import RDataReadError

__all__ = ["RDataReadError", "read_rda", "read_rds", "read_rds_frame", "read_rds_string", "write_rds_string"]

_DIFFTIME_SECONDS = {"secs": 1.0, "mins": 60.0, "hours": 3600.0, "days": 86400.0, "weeks": 604800.0}


def _attr_str(attrs: Any, name: str) -> str:
    value = attrs.get(name)
    if value is None or not np.size(value):
        return ""
    return str(np.asarray(value).ravel()[0])


def _posixct(obj: Any, attrs: Any) -> np.ndarray:
    """Seconds since the epoch -> ``datetime64[us]`` (NaT for NA); ``_scalar`` makes them aware."""
    seconds = np.asarray(obj, dtype=float).ravel()
    stamps = [
        np.datetime64(round(s * 1e6), "us") if math.isfinite(s) else np.datetime64("NaT", "us")
        for s in seconds
    ]
    return np.array(stamps)


def _difftime(obj: Any, attrs: Any) -> np.ndarray:
    return np.asarray(obj, dtype=float) * _DIFFTIME_SECONDS.get(_attr_str(attrs, "units"), 1.0)


def _date(obj: Any, attrs: Any) -> np.ndarray:
    """Days since the epoch -> ``datetime64[D]`` (NaT for NA); ``_scalar`` makes them ``date``.

    Inside a data frame pandas widens the column to seconds, so a Date column comes back as
    midnight datetimes (no file the package reads has one).
    """
    days = np.asarray(obj, dtype=float).ravel()
    stamps = [
        np.datetime64(math.floor(d), "D") if math.isfinite(d) else np.datetime64("NaT", "D") for d in days
    ]
    return np.array(stamps)


_CONSTRUCTORS = {**DEFAULT_CLASS_MAP, "POSIXct": _posixct, "difftime": _difftime, "Date": _date}
_convert_array = _rdata_conversion.convert_array


def _convert_array_tolerantly(
    r_array: rdata.parser.RObject, attrs: Mapping[str, Any] | None = None
) -> np.ndarray | Any:
    """Convert an array like rdata, falling back to a plain array when the dimnames cannot be coordinates."""
    try:
        return _convert_array(r_array, attrs)
    except ValueError:
        plain = {key: value for key, value in dict(attrs or {}).items() if key != "dimnames"}
        return _convert_array(r_array, plain)


def _scalar(value: Any) -> Any:
    """One element of an atomic vector as a Python scalar; NA/NaN/NaT -> None."""
    if value is None or value is np.ma.masked or value is pd.NA or value is pd.NaT:
        return None
    if isinstance(value, np.datetime64):
        if np.isnat(value):
            return None
        if np.datetime_data(value)[0] == "D":
            try:
                return date(1970, 1, 1) + timedelta(days=int(value.astype("int64")))
            except ValueError, OverflowError:  # beyond year 9999
                return None
        try:
            value = pd.Timestamp(value)
        except ValueError, OverflowError:  # beyond pandas' range
            return None
    if isinstance(value, pd.Timestamp):
        try:
            return value.to_pydatetime().replace(tzinfo=UTC)
        except ValueError, OverflowError:  # beyond year 9999: no datetime can hold it
            return None
    if isinstance(value, np.generic):
        value = value.item()
    if isinstance(value, float) and math.isnan(value):
        return None
    return value


def _vector(values: list[Any]) -> Any:
    return values[0] if len(values) == 1 else values


def _to_python(obj: Any) -> Any:
    """Recursively normalise an rdata result."""
    if obj is None or isinstance(obj, str | int | float | bool | datetime | date):
        return obj
    if isinstance(obj, dict):
        return {str(key): _to_python(value) for key, value in obj.items()}
    if isinstance(obj, list):
        return [_to_python(value) for value in obj]
    if isinstance(obj, pd.DataFrame):
        columns = {
            str(c): pd.Series([_scalar(v) for v in obj[c]], index=obj.index, dtype=object)
            for c in obj.columns
        }
        return pd.DataFrame(columns, index=obj.index)
    if isinstance(obj, pd.Categorical):
        return _vector([_scalar(v) for v in obj.astype(object)])
    if hasattr(obj, "dims") and hasattr(obj, "coords"):  # xarray.DataArray: named vector or array
        if obj.ndim == 1 and obj.dims[0] in obj.coords and obj.size > 1:
            names = [str(n) for n in obj.coords[obj.dims[0]].values]
            return dict(zip(names, (_scalar(v) for v in obj.values.ravel()), strict=True))
        obj = np.asarray(obj.values)
    if isinstance(obj, np.ma.MaskedArray):
        data = obj.data.ravel(order="F")
        masked = np.ma.getmaskarray(obj).ravel(order="F")
        return _vector([None if m else _scalar(v) for v, m in zip(data, masked, strict=True)])
    if isinstance(obj, np.ndarray):
        return _vector([_scalar(v) for v in obj.ravel(order="F")])
    if isinstance(obj, np.generic):
        return _scalar(obj)
    return obj


def _read(path: Path, reader: Any) -> Any:
    with (
        warnings.catch_warnings(),
        patch.object(_rdata_conversion, "convert_array", _convert_array_tolerantly),  # looked up at call time
    ):
        warnings.filterwarnings("ignore", category=UserWarning, module=r"rdata\.")
        try:
            raw = reader(path, constructor_dict=_CONSTRUCTORS)
        except Exception as error:  # total boundary: the parser sees arbitrary bytes
            raise RDataReadError(f"{path}: {error}") from error
    return _to_python(raw)


def read_rda(path: Path) -> dict[str, Any]:
    """``load(path)``: the saved objects by name, e.g. ``{"cfg": {...}}`` or ``{"stats": {...}}``."""
    return _read(path, rdata.read_rda)


def read_rds(path: Path) -> Any:
    """``readRDS(path)``."""
    return _read(path, rdata.read_rds)


def read_rds_string(path: Path) -> str:
    """``readRDS(path)`` of a file holding one character string (``runcode.rds``, ``lastcommit.rds``)."""
    value = read_rds(path)
    if not isinstance(value, str):
        raise RDataReadError(f"{path}: expected one string, found {_kind(value)}")
    return value


def read_rds_frame(path: Path) -> pd.DataFrame:
    """``readRDS(path)`` of a file holding a data frame (``runsToStart.rds``, ``gRS.rds``)."""
    value = read_rds(path)
    if not isinstance(value, pd.DataFrame):
        raise RDataReadError(f"{path}: expected a data frame, found {_kind(value)}")
    return value


def _kind(value: object) -> str:
    """What a value is, for a message: a short scalar as it is, anything else by its type."""
    if isinstance(value, str | int | float | bool | type(None)) and len(repr(value)) <= 40:
        return repr(value)
    return type(value).__name__


def write_rds_string(path: Path, value: str) -> None:
    """``saveRDS(value, path)`` of one character string (``runcode.rds``, read by R tools)."""
    rdata.write_rds(path, value)
