"""R data files as the R package reads and writes them: ``load()``, ``readRDS()``, ``saveRDS()``.

The reading layer below ``config``, ``runstats`` and the AMT state (``migration/03-migration-plan.md``
section 5). The ``rdata`` package parses ``.Rdata``/``.rda``/``.rds`` files (gzip and XZ alike); this
module adds the semantics the R code relies on, as recorded in ``migration/02-python-libraries.md``
section 4.2:

* a named list is a ``dict`` with ``str`` keys at every level, ``NULL`` is ``None``, and a length-1
  vector is unwrapped by :func:`scalar`; ``NA`` becomes ``None`` in each of its representations (a
  masked element of a logical or integer vector, a NaN carrying R's NA payload in a double vector,
  a plain ``None`` inside a character vector). A plain ``NaN`` is ``None`` as well: R's ``is.na()``
  is TRUE for both and the R code never tells them apart;
* ``POSIXct`` is an :class:`RTime`: seconds since the epoch plus the ``tzone`` attribute. Elapsed time
  is computed on those instants (:meth:`RTime.elapsed_since`), never on local wall-clock datetimes,
  which is what makes ``difftime(timeGAMSEnd, timeGAMSStart, units = "secs")``
  (``R/getRunStatus.R:311``) DST-safe in Python;
* ``difftime`` is a float in seconds (its ``units`` attribute applied);
* classes R carries along without caring (``packageDescription`` in every REMIND
  ``runstatistics.rda``, the ``magpie`` array of a MAgPIE ``stats$modelstat``) come back as their
  underlying objects, as R's ``load()`` returns them, without warnings;
* :func:`write_rds` writes what ``saveRDS()`` would have written for a single string
  (``runcode.rds``, ``migration/06-user-requests.md`` section 3.4), a character vector, a named list
  or a data frame with row names, so that R's ``readRDS()`` sees an ``identical()`` object.
"""

from __future__ import annotations

import datetime as dt
import math
import warnings
from collections.abc import Iterator, Mapping, Sequence
from contextlib import contextmanager
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Any, Final
from zoneinfo import ZoneInfo

import numpy as np
import pandas as pd
import rdata
import xarray as xr
from rdata.conversion import DEFAULT_CLASS_MAP, SimpleConverter, convert_array
from rdata.conversion.to_r import DEFAULT_CLASS_MAP as DEFAULT_WRITE_CLASS_MAP
from rdata.conversion.to_r import build_r_object
from rdata.parser import RData, RObject, RObjectType

if TYPE_CHECKING:
    from rdata.conversion.to_r import Converter

type RScalar = str | int | float | bool | None
"""One element of an R atomic vector; ``None`` is ``NA`` (and ``NULL`` where a scalar is expected)."""

DEFAULT_TZ: Final = "Europe/Berlin"
"""The cluster's ``TZ``: what R uses to display a ``POSIXct`` that carries no ``tzone`` attribute."""

_DIFFTIME_SECONDS: Final[Mapping[str, float]] = {
    "secs": 1,
    "mins": 60,
    "hours": 3600,
    "days": 86400,
    "weeks": 604800,
}
_ARRAY_TYPES: Final = frozenset({RObjectType.LGL, RObjectType.INT, RObjectType.REAL, RObjectType.CPLX})
_R_DIGITS: Final = 15
"""Significant digits of ``as.character(<double>)``: R's ``formatReal`` with ``digits = 15``."""


class RDataError(Exception):
    """An R data file could not be read or written with the semantics this module promises."""


# ---------------------------------------------------------------------------------------------
# POSIXct and difftime
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True, slots=True)
class RTime:
    """A ``POSIXct`` value as R stores it: seconds since the epoch and the ``tzone`` attribute.

    ``epoch`` is a UTC instant (``None`` for ``NA``); ``tzone`` is ``""`` when R stored none, in
    which case R displays the value in the process time zone (:data:`DEFAULT_TZ` on the cluster).
    Arithmetic happens on the instants: two aware datetimes sharing one ``ZoneInfo`` subtract as
    wall-clock values in Python and lose an hour across a DST change (``02`` section 4.2).
    """

    epoch: float | None
    tzone: str = ""

    @property
    def is_na(self) -> bool:
        """``is.na(x)``."""
        return self.epoch is None

    def elapsed_since(self, start: RTime) -> float | None:
        """``as.numeric(difftime(self, start, units = "secs"))``; ``None`` when either side is NA."""
        if self.epoch is None or start.epoch is None:
            return None
        return self.epoch - start.epoch

    def as_datetime(self) -> dt.datetime | None:
        """The instant as an aware UTC datetime (safe to subtract); ``None`` for NA."""
        if self.epoch is None:
            return None
        return dt.datetime.fromtimestamp(self.epoch, dt.UTC)

    def local(self, default_tz: str = DEFAULT_TZ) -> dt.datetime | None:
        """The instant in its ``tzone`` (``default_tz`` when R stored none): for display only."""
        instant = self.as_datetime()
        if instant is None:
            return None
        return instant.astimezone(ZoneInfo(self.tzone or default_tz))

    @classmethod
    def from_datetime(cls, moment: dt.datetime, tzone: str = "") -> RTime:
        """The ``POSIXct`` of an aware datetime (a naive one is taken as :data:`DEFAULT_TZ` wall time)."""
        if moment.tzinfo is None:
            moment = moment.replace(tzinfo=ZoneInfo(DEFAULT_TZ))
        return cls(moment.timestamp(), tzone)


def _attribute_text(attrs: Mapping[str, Any] | None, name: str, default: str) -> str:
    """The first element of a character attribute (``tzone``, ``units``), or ``default``."""
    value = (attrs or {}).get(name)
    if value is None:
        return default
    elements = vector(value)
    return default if not elements or elements[0] is None else str(elements[0])


def _posixct(obj: Any, attrs: Mapping[str, Any] | None) -> RTime | tuple[RTime, ...]:
    """``rdata`` constructor for class ``POSIXct``: one :class:`RTime`, a tuple for other lengths."""
    tzone = _attribute_text(attrs, "tzone", "")
    epochs = np.ma.filled(np.ma.asarray(obj, dtype=float), np.nan).reshape(-1)
    times = tuple(RTime(None if math.isnan(epoch) else float(epoch), tzone) for epoch in epochs)
    return times[0] if len(times) == 1 else times


def _difftime(obj: Any, attrs: Mapping[str, Any] | None) -> float | tuple[float, ...]:
    """``rdata`` constructor for class ``difftime``: seconds (the ``units`` attribute applied)."""
    factor = _DIFFTIME_SECONDS[_attribute_text(attrs, "units", "secs")]
    values = tuple(float(value) * factor for value in np.ma.filled(np.ma.asarray(obj, dtype=float), np.nan))
    return values[0] if len(values) == 1 else values


def _underlying(obj: Any, attrs: Mapping[str, Any] | None) -> Any:
    """``rdata`` constructor for classes R only carries along: the underlying object, no warning."""
    return obj


READ_CLASS_MAP: Final[Mapping[str | bytes, Any]] = {
    **DEFAULT_CLASS_MAP,
    "POSIXct": _posixct,
    "POSIXt": _underlying,
    "difftime": _difftime,
    "packageDescription": _underlying,  # stats$config$... of every REMIND runstatistics.rda
    "magpie": _underlying,  # MAgPIE stats$modelstat: a magclass array (R/getRunStatus.R:92)
}


class _Converter(SimpleConverter):
    """``SimpleConverter`` that survives arrays whose ``dimnames`` xarray cannot represent.

    A magclass object (``stats$modelstat`` of MAgPIE) is a 3-D array with the dimnames
    ``list("GLO", t = c(...), "main")``: two unnamed entries, which ``rdata`` collapses into one
    coordinate, and ``xarray.DataArray`` refuses the mismatch with a ``ValueError``. R itself only
    ever uses ``as.character(stats$modelstat)`` (``R/getRunStatus.R:92``), i.e. the flat values in
    storage (column-major) order, which is what ``convert_array`` returns without attributes.
    """

    def _convert_next(self, data: RData | RObject) -> Any:
        try:
            return super()._convert_next(data)
        except ValueError:
            obj = data.object if isinstance(data, RData) else data
            if obj.info.type not in _ARRAY_TYPES:
                raise
            return convert_array(obj)


@contextmanager
def _quiet_rdata() -> Iterator[None]:
    """R's ``load()`` knows no "missing constructor" and no "unknown file type"; neither warns here."""
    with warnings.catch_warnings():
        warnings.filterwarnings("ignore", category=UserWarning, module=r"rdata(\.|$)")
        yield


def _str_keys(value: Any) -> Any:
    """``rdata`` names lists with ``numpy.str_`` keys; R code and tests index them with ``str``."""
    if isinstance(value, dict):
        return {str(key): _str_keys(item) for key, item in value.items()}
    if isinstance(value, list):
        return [_str_keys(item) for item in value]
    return value


def _parse(path: Path) -> Any:
    with _quiet_rdata():
        try:
            parsed = rdata.parser.parse_file(path)
        except FileNotFoundError:
            raise
        except Exception as error:  # rdata raises ValueError, NotImplementedError, struct.error, ...
            raise RDataError(f"cannot read R data file {path}: {error}") from error
        return _str_keys(_Converter(constructor_dict=READ_CLASS_MAP).convert(parsed))


def read_rda(path: Path) -> dict[str, Any]:
    """R ``load(path)``: the objects of an ``.Rdata``/``.rda`` file by name.

    ``config.Rdata`` holds ``cfg`` (gzip), ``runstatistics.rda`` holds ``stats`` (XZ). A missing file
    raises ``FileNotFoundError`` as R's ``load()`` errors; unreadable content raises :class:`RDataError`.
    """
    objects = _parse(path)
    if not isinstance(objects, dict):
        raise RDataError(f"{path} is not an R data file holding named objects (an .rds file?)")
    return {str(name): value for name, value in objects.items()}


def read_rds(path: Path) -> Any:
    """R ``readRDS(path)``: the one object of an ``.rds`` file (``runcode.rds``, ``gRS.rds``, ...)."""
    return _parse(path)


# ---------------------------------------------------------------------------------------------
# Vectors and scalars
# ---------------------------------------------------------------------------------------------


def element(value: Any) -> RScalar:
    """One element of an R vector as a Python value; ``NA`` is ``None`` in every representation.

    A masked element (logical, integer), a NaN (double: R's ``NA_real_`` is a NaN with a payload,
    which ``rdata`` keeps as a NaN) and ``None`` (character) are all ``is.na()`` in R.
    """
    if value is None or np.ma.is_masked(value):
        return None
    item: Any = value.item() if isinstance(value, np.generic) else value
    if isinstance(item, float) and math.isnan(item):
        return None
    if isinstance(item, str | bool | int | float):
        return item
    raise RDataError(f"not an element of an R atomic vector: {type(value).__name__}")


def _elements(value: Any) -> list[RScalar]:
    if value is None:
        return []  # length(NULL) == 0
    if isinstance(value, xr.DataArray):
        value = value.values
    if isinstance(value, np.ndarray | np.generic):
        # R's as.vector() order for arrays is column-major.
        return [element(item) for item in np.ma.asarray(value).reshape(-1, order="F")]
    if isinstance(value, list | tuple):
        return [element(item) for item in value]  # YAML sequences (config.yml)
    if isinstance(value, str | bool | int | float):
        return [element(value)]  # YAML scalars
    raise RDataError(f"not an R atomic vector: {type(value).__name__}")


def vector(value: Any) -> tuple[RScalar, ...]:
    """An R vector as a tuple of Python values (``NA`` -> ``None``); ``NULL`` is the empty tuple."""
    return tuple(_elements(value))


def scalar(value: Any) -> RScalar:
    """A length-1 R vector as a Python value; ``NULL`` and ``NA`` are ``None``.

    For the fields the R code uses as scalars (``cfg$title``, ``cfg$gms$*``, ``stats$id``, ...);
    any other length is a programming error here and raises :class:`RDataError`.
    """
    if value is None:
        return None
    elements = _elements(value)
    if len(elements) != 1:
        raise RDataError(f"expected a length-1 R vector, got {len(elements)} elements")
    return elements[0]


def r_round(value: float) -> int:
    """R ``round(x, 0)`` as an ``int``: half to even (``round(0.5)`` is 0, ``round(2.5)`` is 2)."""
    return round(value)


def _format_double(value: float) -> str:
    """``as.character(<double>)``: R's ``formatReal`` with 15 significant digits, ``scipen = 0``.

    The shortest representation with at most 15 significant digits is found by rounding to 15
    and dropping trailing zeros; fixed notation wins over scientific when it is not wider
    (``1e+05`` beats ``100000``, ``10000`` and ``123456`` stay fixed, ``0.1 + 0.2`` is ``0.3``).
    """
    if math.isnan(value):
        return "NaN"
    if math.isinf(value):
        return "Inf" if value > 0 else "-Inf"
    if value == 0:
        return "0"
    mantissa, exponent_text = f"{abs(value):.{_R_DIGITS - 1}e}".split("e")
    exponent = int(exponent_text)
    significant = len(mantissa.replace(".", "").rstrip("0"))
    negative = value < 0
    right_digits = max(0, significant - exponent - 1)
    left_digits = exponent + 1 if exponent >= 0 else 1
    fixed_width = negative + left_digits + (right_digits > 0) + right_digits
    scientific_width = negative + significant + (significant > 1) + (4 if abs(exponent) < 100 else 5)
    if fixed_width <= scientific_width:
        return f"{value:.{right_digits}f}"
    digits = f"{abs(value):.{significant - 1}e}".split("e")[0]
    sign = "-" if negative else ""
    return f"{sign}{digits}e{'-' if exponent < 0 else '+'}{abs(exponent):02d}"


def r_as_character(value: RScalar) -> str | None:
    """``as.character(x)`` of one element: ``NA`` stays ``None``, logicals are ``TRUE``/``FALSE``.

    Doubles follow R's 15-significant-digit rule (``as.character(2)`` is ``"2"``, ``13`` is
    ``"13"``, ``1e5`` is ``"1e+05"``), which is what ``paste0(stats$modelstat, collapse = "")``
    (``R/getRunStatus.R:92-94``) and the GDX value concatenation produce.
    """
    if value is None:
        return None
    if isinstance(value, bool):
        return "TRUE" if value else "FALSE"
    if isinstance(value, int):
        return str(value)
    if isinstance(value, float):
        return _format_double(value)
    return value


# ---------------------------------------------------------------------------------------------
# Writing
# ---------------------------------------------------------------------------------------------


def _row_names(index: pd.Index[Any]) -> Any:
    """The ``row.names`` attribute: R's compact form for default indexes, else character names."""
    count = len(index)
    if isinstance(index, pd.RangeIndex) and index.step == 1 and index.start in (0, 1):
        # R stores automatic row names as c(NA_integer_, -nrow), and integer(0) for no rows.
        if count == 0:
            return np.array([], dtype=np.int32)
        return np.ma.array(np.array([0, -count], dtype=np.int32), mask=[True, False])
    return np.array([str(name) for name in index], dtype=object)


def _character_vector(items: Sequence[object]) -> Any:
    """A character vector with ``NA`` for ``None``/``NaN``/``pd.NA``; non-strings are an error."""
    values: list[str | None] = []
    for item in items:
        if isinstance(item, str):
            values.append(item)
        elif item is None or item is pd.NA or (isinstance(item, float) and math.isnan(item)):
            values.append(None)
        else:
            raise RDataError(f"cannot write a {type(item).__name__} inside a character vector")
    return np.array(values, dtype=object)


def _column_values(series: pd.Series[Any]) -> Any:
    """A data frame column as the numpy array ``rdata`` writes with R's NA semantics."""
    dtype = series.dtype
    if isinstance(dtype, pd.StringDtype) or (isinstance(dtype, np.dtype) and dtype.kind == "O"):
        return _character_vector(series.tolist())  # character, NA_character_
    if isinstance(dtype, pd.BooleanDtype):
        data = series.to_numpy(dtype=np.bool_, na_value=False)
        return np.ma.array(data, mask=series.isna().to_numpy())  # logical with NA
    if isinstance(dtype, pd.Int8Dtype | pd.Int16Dtype | pd.Int32Dtype | pd.Int64Dtype):
        data = series.to_numpy(dtype=np.int32, na_value=0)
        return np.ma.array(data, mask=series.isna().to_numpy())  # integer with NA
    if isinstance(dtype, pd.Float32Dtype | pd.Float64Dtype):
        return series.to_numpy(dtype=np.float64, na_value=rdata.missing.R_FLOAT_NA)
    if isinstance(dtype, np.dtype) and dtype.kind in "biuf":
        return series.to_numpy()  # floats keep their bits: an R NA read from a file stays an NA
    raise RDataError(f"cannot write a data frame column of dtype {dtype} as an R vector")


def _dataframe_to_r(frame: pd.DataFrame, converter: Converter) -> RObject:
    """Python-to-R constructor for ``pd.DataFrame``: a ``data.frame`` with (character) row names.

    Replaces ``rdata``'s own, which rejects the ``str`` index of pandas 3 (``pd.DataFrame pd.Index
    str not implemented``) and ignores nullable integer and string columns.
    """
    names = [str(name) for name in frame.columns]
    columns = [converter.convert_to_r_object(_column_values(frame[name])) for name in frame.columns]
    attributes = converter.convert_to_r_attributes(
        {"names": np.array(names, dtype="U"), "class": "data.frame", "row.names": _row_names(frame.index)}
    )
    return build_r_object(RObjectType.VEC, value=columns, is_object=True, attributes=attributes)


WRITE_CLASS_MAP: Final[Mapping[type[Any], Any]] = {**DEFAULT_WRITE_CLASS_MAP, pd.DataFrame: _dataframe_to_r}


def _atomic_vector(items: Sequence[object]) -> Any:
    """A list or tuple as the R atomic vector ``c(...)`` would be: character, logical, integer or double."""
    present = [item for item in items if item is not None]
    if all(isinstance(item, str) for item in present):
        return _character_vector(items)
    mask = [item is None for item in items]
    if all(isinstance(item, bool) for item in present):
        return np.ma.array([bool(item) for item in items], mask=mask, dtype=np.bool_)
    if all(isinstance(item, int) and not isinstance(item, bool) for item in present):
        return np.ma.array([0 if item is None else item for item in items], mask=mask, dtype=np.int32)
    if all(isinstance(item, int | float) and not isinstance(item, bool) for item in present):
        return np.array(
            [rdata.missing.R_FLOAT_NA if item is None else item for item in items], dtype=np.float64
        )
    raise RDataError(
        "a list is written as an R vector only when its elements are all str, bool, int or float"
    )


def _writable(value: object) -> Any:
    """Normalise the supported Python values to what ``rdata`` serialises as R expects."""
    if value is None or isinstance(value, str | bool | int | float | np.ndarray | pd.DataFrame):
        return value
    if isinstance(value, list | tuple):
        return _atomic_vector(value)
    if isinstance(value, Mapping):
        return {str(key): _writable(item) for key, item in value.items()}
    raise RDataError(f"cannot write a {type(value).__name__} as an R object")


def write_rds(path: Path, value: object) -> None:
    """R ``saveRDS(value, path)`` (gzip, serialization format 3) for the objects the port writes.

    ``str`` becomes a length-1 character vector (``runcode.rds``, ``lastcommit.rds``), ``bool``,
    ``int`` and ``float`` length-1 logical, integer and double vectors, ``None`` is ``NULL``, a list
    or tuple an atomic vector of the same kind with ``NA`` for ``None``, a ``dict`` with string keys
    a named list, and a ``pd.DataFrame`` a ``data.frame`` whose index becomes the row names
    (``runsToStart.rds``, ``gRS.rds``; a default ``RangeIndex`` gives R's automatic row names).
    Anything else raises :class:`RDataError`.
    """
    try:
        rdata.write_rds(path, _writable(value), constructor_dict=WRITE_CLASS_MAP)
    except (NotImplementedError, TypeError, ValueError) as error:
        raise RDataError(f"cannot write {type(value).__name__} to {path}: {error}") from error
