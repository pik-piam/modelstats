"""R data files: reading ``.Rdata``/``.rda``/``.rds`` and writing ``.rds`` (02 section 4.2).

Everything goes through the ``rdata`` package. Three things are added on top of
its defaults:

* ``POSIXct`` becomes :class:`RTime` (epoch seconds plus the stored ``tzone``) so
  that elapsed times are computed on instants, never on wall-clock datetimes,
  which is what makes them DST-proof; ``difftime`` becomes seconds (its
  ``units`` attribute is applied, not discarded);
* ``magpie`` objects (magclass S4 arrays, what MAgPIE stores as
  ``stats$modelstat``) are read as plain vectors in storage order, which is what
  R's ``as.character(stats$modelstat)`` iterates over; the default converter
  cannot build their 3-D xarray;
* :func:`write_rds` writes atomically (temporary file in the same directory,
  then ``os.replace``) and normalises pandas frames so that R's ``readRDS`` sees
  the same types it wrote (character, integer, double, logical columns, NA in
  each, character row names).

Conventions: a real R ``NA`` (and ``NULL``) is ``None`` on the Python side
(:func:`scalar`, :func:`vector`); when writing, ``None`` and ``NaN`` become R's
``NA``. Error paths that R goldens pin raise :class:`RParityError` with R's
message: a missing file is ``cannot open the connection`` (``load`` and
``readRDS``), a corrupt ``.rds`` is ``unknown input format`` and a corrupt
``.Rdata`` is ``bad restore file magic number (file may be corrupted) -- no data
loaded``.
"""

from __future__ import annotations

import bz2
import dataclasses
import datetime as dt
import gzip
import lzma
import math
import os
import tempfile
import warnings
from collections.abc import Callable, Mapping
from typing import TYPE_CHECKING, Any
from zoneinfo import ZoneInfo

import numpy as np
import pandas as pd
import rdata
import xarray
from rdata.conversion import DEFAULT_CLASS_MAP, SimpleConverter, convert_attrs, convert_python_to_r_data
from rdata.missing import R_FLOAT_NA
from rdata.parser import RData, RObject, RObjectType
from rdata.unparser import unparse_data

from modelstats.errors import RParityError

if TYPE_CHECKING:
    from modelstats.env import Effects

__all__ = [
    "DEFAULT_TZ",
    "RTime",
    "pythonize",
    "rds_bytes",
    "read_rda",
    "read_rds",
    "scalar",
    "vector",
    "write_rds",
]

#: The cluster's ``TZ``: what R uses to display a POSIXct that carries no ``tzone``.
DEFAULT_TZ = "Europe/Berlin"

_BAD_RDA = "bad restore file magic number (file may be corrupted) -- no data loaded"
_ATOMIC_TYPES = frozenset({RObjectType.LGL, RObjectType.INT, RObjectType.REAL, RObjectType.CPLX})
_DIFFTIME_FACTOR = {"secs": 1.0, "mins": 60.0, "hours": 3600.0, "days": 86400.0, "weeks": 604800.0}


# ---------------------------------------------------------------------------
# POSIXct
# ---------------------------------------------------------------------------
@dataclasses.dataclass(frozen=True)
class RTime:
    """A POSIXct as R stores it: seconds since the epoch (a UTC instant) plus the ``tzone`` attribute.

    ``epoch`` is ``None`` for ``NA``; ``tzone`` is ``''`` when R stored none (the
    cluster default :data:`DEFAULT_TZ` applies at display time only).
    """

    epoch: float | None
    tzone: str = ""

    @property
    def is_na(self) -> bool:
        return self.epoch is None

    def elapsed_since(self, other: RTime) -> float | None:
        """Seconds from ``other`` to ``self``, like ``difftime(self, other, units = "secs")``.

        Arithmetic on instants, so a DST change in between counts its real
        duration; ``None`` when either side is ``NA``.
        """
        if self.epoch is None or other.epoch is None:
            return None
        return self.epoch - other.epoch

    def display(self, tz: str = DEFAULT_TZ) -> dt.datetime | None:
        """The instant as an aware datetime in the stored ``tzone`` (or ``tz`` when none was stored).

        For presentation and calendar dates only; never subtract two of these.
        """
        if self.epoch is None:
            return None
        return dt.datetime.fromtimestamp(self.epoch, dt.UTC).astimezone(ZoneInfo(self.tzone or tz))

    @classmethod
    def from_datetime(cls, when: dt.datetime, tzone: str = "") -> RTime:
        """An :class:`RTime` for an aware datetime (for example ``effects.now()``)."""
        if when.tzinfo is None or when.utcoffset() is None:
            msg = "RTime.from_datetime needs an aware datetime"
            raise ValueError(msg)
        return cls(when.timestamp(), tzone)


def _attr_str(attrs: Mapping[str, Any] | None, name: str, default: str) -> str:
    if not attrs:
        return default
    value = attrs.get(name)
    if value is None:
        return default
    flat = np.asarray(value).reshape(-1)
    if flat.size == 0 or flat[0] is None:
        return default
    return str(flat[0])


def _float_values(obj: Any) -> list[float | None]:
    """The values of an R double vector with ``NA`` (and ``NaN``) as ``None``."""
    arr = np.ma.filled(np.ma.asarray(obj, dtype=float), np.nan).reshape(-1)
    return [None if math.isnan(v) else float(v) for v in arr]


def _posixct(obj: Any, attrs: Mapping[str, Any] | None) -> RTime | list[RTime]:
    tz = _attr_str(attrs, "tzone", "")
    times = [RTime(v, tz) for v in _float_values(obj)]
    # A length-1 POSIXct is what the R code uses everywhere; longer ones stay lists.
    return times[0] if len(times) == 1 else times


def _difftime(obj: Any, attrs: Mapping[str, Any] | None) -> float | None | list[float | None]:
    factor = _DIFFTIME_FACTOR[_attr_str(attrs, "units", "secs")]
    secs = [None if v is None else v * factor for v in _float_values(obj)]
    return secs[0] if len(secs) == 1 else secs


RMAP: dict[str | bytes, Callable[[Any, Mapping[str, Any]], Any]] = {
    **DEFAULT_CLASS_MAP,
    "POSIXct": _posixct,
    "difftime": _difftime,
}


# ---------------------------------------------------------------------------
# Reading
# ---------------------------------------------------------------------------
def _has_class(obj: RObject, converter: Any, name: str) -> bool:
    if obj.attributes is None:
        return False
    attrs = convert_attrs(obj, converter)
    classes = attrs.get("class")
    if classes is None:
        return False
    return any(str(c) == name for c in np.asarray(classes).reshape(-1))


class _Converter(SimpleConverter):
    """rdata's converter plus ``as.vector()`` for magclass ``magpie`` arrays.

    A magpie object is a double array with ``dim`` (regions, years, data) and a
    ``dimnames`` list whose names are partly empty; xarray refuses that. R's
    ``as.character()`` on it iterates the storage order, which is exactly the
    parsed vector, so the array attributes are dropped here.
    """

    def _convert_next(self, data: RData | RObject) -> Any:
        obj = data.object if isinstance(data, RData) else data
        if obj.info.type in _ATOMIC_TYPES and _has_class(obj, self._convert_next, "magpie"):
            value = np.ma.asarray(obj.value).reshape(-1)
            self.references[id(obj)] = value
            return value
        return super()._convert_next(data)


def _convert(parsed: RData) -> Any:
    with warnings.catch_warnings():
        # Classes without a constructor (tbl_df, quitte, POSIXt, ...) fall back to their
        # underlying object; R's getRunStatus wraps load() in suppressWarnings() as well.
        warnings.filterwarnings("ignore", message="Missing constructor for R class", category=UserWarning)
        return _Converter(RMAP).convert(parsed)


def _read_bytes(path: str | os.PathLike[str], effects: Effects | None) -> bytes:
    try:
        if effects is None:
            with open(path, "rb") as handle:
                return handle.read()
        return effects.read_bytes(os.fspath(path))
    except OSError as exc:
        # R: load()/readRDS() on a missing or unreadable file (after the "cannot open compressed file" warning)
        raise RParityError("cannot open the connection") from exc


_RDA_MAGICS = (b"RDX2\n", b"RDX3\n", b"RDB2\n", b"RDB3\n", b"RDA2\n", b"RDA3\n")
#: Failures that say nothing about the file format and must not become R's "unknown input format" /
#: "bad restore file magic number": the 4.7 GB ``overview.rds`` exhausts memory before rdata finishes.
_NOT_A_FORMAT_PROBLEM = (MemoryError, RecursionError, KeyboardInterrupt)


def _decompress(raw: bytes) -> bytes:
    """The serialised stream behind R's optional gzip/bzip2/xz compression."""
    if raw.startswith(b"\x1f\x8b"):
        return gzip.decompress(raw)
    if raw.startswith(b"\xfd7zXZ\x00"):
        return lzma.decompress(raw)
    if raw.startswith(b"BZh"):
        return bz2.decompress(raw)
    return raw


def _parse(path: str | os.PathLike[str], effects: Effects | None, kind: str, failure: str) -> RData:
    raw = _read_bytes(path, effects)
    try:
        data = _decompress(raw)
    except _NOT_A_FORMAT_PROBLEM:
        raise
    except Exception as exc:
        raise RParityError(failure) from exc
    is_rda = data.startswith(_RDA_MAGICS)
    if is_rda != (kind == "rda"):
        # readRDS() of an RData stream / load() of an RDS stream: R rejects the magic number
        raise RParityError(failure)
    try:
        return rdata.parser.parse_data(data, extension=f".{kind}")
    except _NOT_A_FORMAT_PROBLEM:
        raise
    except Exception as exc:
        raise RParityError(failure) from exc


def read_rda(path: str | os.PathLike[str], effects: Effects | None = None) -> dict[str, Any]:
    """``load(path)``: the objects of an ``.Rdata``/``.rda`` file by name.

    Values are what ``rdata`` returns (numpy arrays for atomic vectors, dicts
    for named lists, pandas frames for data frames) with :class:`RTime` for
    POSIXct and seconds for difftime. Use :func:`scalar`, :func:`vector` or
    :func:`pythonize` on them. A file that is not an RData file raises
    ``RParityError`` with R's "bad restore file magic number" message.
    """
    parsed = _parse(path, effects, "rda", _BAD_RDA)
    result = _convert(parsed)
    if not isinstance(result, Mapping):
        raise RParityError(_BAD_RDA)
    return {str(key): value for key, value in result.items()}


def read_rds(path: str | os.PathLike[str], effects: Effects | None = None) -> Any:
    """``readRDS(path)``: the single object of an ``.rds`` file (same value conventions as :func:`read_rda`)."""
    parsed = _parse(path, effects, "rds", "unknown input format")
    return _convert(parsed)


# ---------------------------------------------------------------------------
# Unwrapping R vectors
# ---------------------------------------------------------------------------
def _item(value: Any) -> Any:
    """A numpy scalar as a Python scalar; ``NA``/``NaN`` as ``None``."""
    if value is None or value is np.ma.masked or value is pd.NA:
        return None
    if isinstance(value, np.generic):
        value = value.item()
    if isinstance(value, float) and math.isnan(value):
        return None
    return value


def _flatten(x: Any) -> list[Any]:
    """The elements of an R vector (or Python stand-in) with ``NA`` as ``None``."""
    if x is None:
        return []
    if isinstance(x, (RTime, str, bytes, bool, int, float, pd.DataFrame)):
        return [x]
    if isinstance(x, xarray.DataArray):
        x = x.values
    if isinstance(x, Mapping):
        return [_item(v) if not isinstance(v, (list, tuple, np.ndarray, Mapping)) else v for v in x.values()]
    if isinstance(x, (list, tuple)):
        return [_item(v) if not isinstance(v, (list, tuple, np.ndarray, Mapping)) else v for v in x]
    arr = np.ma.ravel(np.ma.asarray(x), order="F")  # R's storage order (column-major) for matrices and arrays
    mask = np.ma.getmaskarray(arr)
    data = np.ma.getdata(arr)
    return [None if mask[i] else _item(data[i]) for i in range(arr.size)]


def scalar(x: Any) -> Any:
    """Unwrap a length-1 R vector to a Python scalar; ``NA``/``NULL`` -> ``None``.

    Only for fields the R code uses as scalars (``cfg$title``, ``cfg$gms$*``,
    ``stats$id``, ``stats$config$model_name``); anything longer is a programming
    error here. R integer -> ``int``, double -> ``float``, character -> ``str``,
    logical -> ``bool``; a named length-1 vector gives its value.
    """
    if x is None:
        return None
    values = _flatten(x)
    if len(values) != 1:
        msg = f"expected a length-1 R vector, got {len(values)} elements"
        raise ValueError(msg)
    return values[0]


def vector(x: Any) -> list[Any]:
    """R vector -> list with ``None`` for ``NA`` elements (e.g. MAgPIE ``stats$modelstat`` over years).

    ``NULL`` gives ``[]``, a scalar gives a one-element list; masks and storage
    order are preserved.
    """
    return _flatten(x)


def pythonize(x: Any) -> Any:
    """Plain Python values for an ``rdata`` structure.

    Named lists become dicts, named vectors dicts of name -> value, length-1
    atomic vectors scalars (R has no scalars, so this is lossless for every
    field the port reads), longer ones lists, ``NA`` and ``NULL`` ``None``;
    :class:`RTime` and pandas frames pass through.
    """
    if x is None or isinstance(x, (RTime, str, bytes, bool, int, float, pd.DataFrame)):
        return x
    if isinstance(x, np.generic):
        return _item(x)
    if isinstance(x, xarray.DataArray):
        values = _flatten(x.values)
        if x.ndim == 1 and x.dims[0] in x.coords:
            names = [str(n) for n in x.coords[x.dims[0]].values]
            return dict(zip(names, values, strict=True))
        return values[0] if len(values) == 1 else values
    if isinstance(x, Mapping):
        return {str(key): pythonize(value) for key, value in x.items()}
    if isinstance(x, (list, tuple)):
        return [pythonize(value) for value in x]
    values = _flatten(x)
    return values[0] if len(values) == 1 else values


# ---------------------------------------------------------------------------
# Writing
# ---------------------------------------------------------------------------
def _na_to_r(values: Any) -> Any:
    """Float arrays with every ``NaN`` as R's ``NA_real_`` bit pattern."""
    arr = np.asarray(values, dtype=np.float64).copy()
    arr[np.isnan(arr)] = R_FLOAT_NA
    return arr


def _is_missing(value: Any) -> bool:
    return value is None or value is pd.NA or (isinstance(value, float) and math.isnan(value))


def _writable_column(series: pd.Series[Any]) -> Any:
    """An R-typed stand-in for a pandas column: integer, double, logical or character with NA."""
    dtype = series.dtype
    if isinstance(dtype, pd.CategoricalDtype):
        return series.array
    if isinstance(dtype, (pd.Int8Dtype, pd.Int16Dtype, pd.Int32Dtype, pd.Int64Dtype, pd.BooleanDtype)):
        return series.array
    if isinstance(dtype, (pd.Float32Dtype, pd.Float64Dtype)):
        return series.array
    if isinstance(dtype, np.dtype):
        if dtype.kind == "f":
            return _na_to_r(series.to_numpy())
        if dtype.kind in "iub":
            return series.to_numpy()
    values = [None if _is_missing(v) else v for v in series.tolist()]
    present = [v for v in values if v is not None]
    if all(isinstance(v, str) for v in present):
        return np.array(values, dtype=object)
    if all(isinstance(v, (bool, np.bool_)) for v in present):
        return pd.array(values, dtype="boolean")
    if all(isinstance(v, (int, np.integer)) for v in present):
        return pd.array(values, dtype="Int32")
    if all(isinstance(v, (int, float, np.integer, np.floating)) for v in present):
        return _na_to_r([np.nan if v is None else float(v) for v in values])
    msg = (
        f"column {series.name!r} mixes types R cannot hold in one column: {sorted({type(v).__name__ for v in present})}"
    )
    raise TypeError(msg)


def _writable_frame(frame: pd.DataFrame) -> pd.DataFrame:
    index: pd.Index[Any]
    if isinstance(frame.index, pd.RangeIndex) and frame.index.start == 0 and frame.index.step == 1:
        index = pd.RangeIndex(1, len(frame) + 1)  # R's automatic row names 1..n
    elif isinstance(frame.index, pd.RangeIndex):
        index = frame.index
    else:
        index = pd.Index([str(v) for v in frame.index], dtype=object)
    columns = {str(name): _writable_column(frame[name]) for name in frame.columns}
    out = pd.DataFrame(index=index)
    for name, values in columns.items():
        out[name] = pd.Series(
            values, index=index, dtype=object if isinstance(values, np.ndarray) and values.dtype.kind == "O" else None
        )
    return out


def _writable(obj: Any) -> Any:
    """``obj`` with the port's conventions mapped onto what ``rdata`` writes.

    ``None``/``NaN`` become ``NA``, pandas frames are normalised column by
    column, dicts become named lists, lists stay R lists (pass a numpy array
    for an atomic vector; a ``str`` is a length-1 character vector).
    """
    if obj is None or isinstance(obj, (str, bool, int)):
        return obj
    if isinstance(obj, float):
        return R_FLOAT_NA if math.isnan(obj) else obj
    if isinstance(obj, pd.DataFrame):
        return _writable_frame(obj)
    if isinstance(obj, Mapping):
        return {str(key): _writable(value) for key, value in obj.items()}
    if isinstance(obj, (list, tuple)):
        return [_writable(value) for value in obj]
    if isinstance(obj, np.ndarray) and obj.dtype.kind == "f":
        return _na_to_r(obj)
    if isinstance(obj, np.ndarray) and obj.dtype.kind == "O":
        return np.array([None if _is_missing(v) else v for v in obj.reshape(-1)], dtype=object)
    return obj


def rds_bytes(obj: Any) -> bytes:
    """The gzip-compressed RDS serialisation of ``obj`` (what ``saveRDS`` writes by default)."""
    r_data = convert_python_to_r_data(_writable(obj), file_type="rds")
    raw = unparse_data(r_data, file_format="xdr", file_type="rds")
    return gzip.compress(raw, compresslevel=6, mtime=0)


def write_rds(path: str | os.PathLike[str], obj: Any) -> None:
    """``saveRDS(obj, path)``, atomically: a temporary file next to ``path`` is renamed over it.

    Round-trips the AMT state shapes (a character string, data frames with
    character row names, NA and character/integer/double/logical columns) so
    that ``identical(readRDS(original), readRDS(copy))`` holds in R.
    """
    data = rds_bytes(obj)
    target = os.fspath(path)
    directory = os.path.dirname(target) or "."
    fd, tmp = tempfile.mkstemp(dir=directory, prefix=f".{os.path.basename(target)}.", suffix=".tmp")
    try:
        with os.fdopen(fd, "wb") as handle:
            handle.write(data)
            handle.flush()
            os.fsync(handle.fileno())
        os.replace(tmp, target)
    except BaseException:
        try:
            os.unlink(tmp)
        except OSError:
            pass
        raise
