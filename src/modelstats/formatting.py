"""Text of the status table: ``R/printOutput.R`` and the runtime display of ``R/loopRuns.R``.

Parity port (``migration/03-migration-plan.md`` section 3.3; ``migration/06-user-requests.md``
section 6 asks for a column specification instead of the reversed-index arithmetic). Everything here
is pure: the one environment probe of the R code, ``file.exists("/p")`` for the default column set
(``R/printOutput.R:28``), is the ``on_cluster`` argument that the caller takes from ``Effects``.

What is reproduced, with the R lines:

* ``printOutput(string, len1stcol, lenCols, colSep, cols)`` (``R/printOutput.R:14-50``), both
  regimes: with ``lenCols`` every cell is padded or cut to its width and the cells are joined with
  ``colSep``; without ``lenCols`` the widths are ``i + 12`` counting from the last column and no
  separator is inserted. A real missing value (``is.na``) is blanks of the column width, the string
  ``"NA"`` prints as ``NA``, a column that the record lacks is the R error ``undefined columns
  selected``, a width the R code cannot ``rep`` is ``invalid 'times' argument``, and the line ends
  with a newline.
* the ``Runtime`` display of ``R/loopRuns.R:66-79``: ``format(round(make_difftime(second = s), 1))``
  with lubridate's unit choice, the ``>`` marker for a run that is still in SLURM, and the
  ``pending`` / ``startup`` / ``NA`` precedence.
* ``piamutils::niceround`` for the ``conoptspy >Nh`` status (``R/getRunStatus.R:184``).
* the number-to-text rules of ``paste0`` / ``as.character`` (15 significant digits) and ``format``
  (7 significant digits) for doubles (``migration/02-python-libraries.md`` section 4.8), together
  with R's ``round(x, digits)`` and ``signif(x, digits)``, which differ from Python's ``round`` on
  binary near-ties (``round(0.45, 1)`` is ``0.4`` in R and ``0.5`` in Python).
"""

from __future__ import annotations

import math
import re
from collections.abc import Mapping, Sequence
from dataclasses import dataclass
from decimal import ROUND_HALF_EVEN, Decimal
from typing import Protocol, runtime_checkable

__all__ = [
    "ARGUMENT_OF_LENGTH_ZERO",
    "INVALID_TIMES_ARGUMENT",
    "LOCAL_COLUMNS",
    "ONCLUSTER_COLUMNS",
    "UNDEFINED_COLUMNS",
    "CellValue",
    "ColumnSpec",
    "PrintOutputError",
    "Row",
    "StatusRow",
    "column_specs",
    "default_columns",
    "format_cell",
    "format_runtime",
    "is_na",
    "mark_active",
    "niceround",
    "print_output",
    "r_as_character",
    "r_format_number",
    "r_round",
    "r_signif",
    "runtime_display",
]

#: One cell of the status table as R holds it: a string (possibly the literal ``"NA"``), a number,
#: a logical, or a real missing value (``None``; a float ``nan`` is also ``is.na``).
type CellValue = str | int | float | bool | None

#: ``R/printOutput.R:29-30``: the columns printed on the cluster when ``cols`` is not given.
ONCLUSTER_COLUMNS: tuple[str, ...] = (
    "Runtime",
    "jobInSLURM",
    "RunType",
    "RunStatus",
    "Warnings",
    "Iter",
    "Conv",
    "modelstat",
    "Mif",
    "runInAppResults",
)
#: ``R/printOutput.R:32``: the columns printed elsewhere when ``cols`` is not given.
LOCAL_COLUMNS: tuple[str, ...] = (
    "Runtime",
    "RunType",
    "RunStatus",
    "Warnings",
    "Iter",
    "Conv",
    "modelstat",
    "Mif",
)

#: R's message for ``string[, cols]`` with a name the data frame lacks (``R/printOutput.R:38``).
UNDEFINED_COLUMNS = "undefined columns selected"
#: R's message for ``rep(" ", len)`` with an ``NA`` or negative ``len`` (``R/printOutput.R:17-18``).
INVALID_TIMES_ARGUMENT = "invalid 'times' argument"
#: R's message for ``if (is.na(NULL))``: ``rownames()`` of the vector that ``string[, cols]`` becomes
#: for a single column (``R/printOutput.R:38, 47``; see ``print_output``).
ARGUMENT_OF_LENGTH_ZERO = "argument is of length zero"

#: Width of column ``i`` (counting from the last one) when no width is given (``R/printOutput.R:46``).
_WIDTH_OFFSET = 12
#: ``R/loopRuns.R:73``: the ``>`` marker gets a space when the runtime text is shorter than this.
_MARKER_PAD_BELOW = 10

#: lubridate's ``make_difftime(second = s)`` unit thresholds (``02`` section 4.8, verified with
#: lubridate 1.9.5): the first unit whose limit ``abs(s)`` is below.
_DIFFTIME_UNITS: tuple[tuple[str, float, float], ...] = (
    ("secs", 1.0, 60.0),
    ("mins", 60.0, 3600.0),
    ("hours", 3600.0, 86400.0),
    ("days", 86400.0, math.inf),
)

#: R's ``DBL_DIG``: significant digits beyond which ``round()`` returns its input unchanged.
_DBL_DIG = 15
#: ``as.character()`` of a double uses this many significant digits (``coerceVector``: ``DBL_DIG``).
_AS_CHARACTER_DIGITS = 15
#: ``format()`` of a double uses ``getOption("digits")``, 7 by default.
_FORMAT_DIGITS = 7
#: ``KP_MAX`` of R's ``format.c``: the largest power of ten the ``roundingwidens`` test considers.
_KP_MAX = 22
#: ``MAX_DIGITS`` of ``nmath/fprec.c`` and ``fround.c``.
_SIGNIF_MAX_DIGITS = 22
_ROUND_MAX_DIGITS = 308
#: ``DBL_MAX_10_EXP``, the ``max10e`` of ``fprec.c``.
_MAX_10_EXP = 308
_LOG10_2 = math.log10(2.0)

_PENDING_SUFFIX = re.compile(r"pending$")
_STARTUP_SUFFIX = re.compile(r"startup$")


class PrintOutputError(RuntimeError):
    """An R error of ``printOutput()``; ``str(error)`` is the R condition message."""


@runtime_checkable
class StatusRow(Protocol):
    """One row of the status table as ``printOutput`` sees it: the row name and the named cells.

    ``run`` is ``rownames(string)`` (the display basename of the run) and ``cells`` the columns the
    record carries; a column that R never assigned (a brief record, a MAgPIE record without the
    sanity columns) is absent from ``cells``, not ``None``. The ``RunStatus`` parity record of phase 2
    exposes the same two members; ``Row`` is the plain carrier for callers that have a mapping.
    """

    @property
    def run(self) -> str:
        """``rownames(string)``: the text of the first column."""
        ...

    @property
    def cells(self) -> Mapping[str, CellValue]:
        """The columns of the record, by their R names."""
        ...


@dataclass(frozen=True)
class Row:
    """A ``StatusRow`` made of a row name and a mapping."""

    run: str
    cells: Mapping[str, CellValue]


@dataclass(frozen=True)
class ColumnSpec:
    """One printed column: its name in the record and its width in characters.

    ``width`` is ``None`` where R's ``lenCols[...]`` index runs past the vector (an ``NA`` width),
    which ``format_cell`` turns into the R error ``invalid 'times' argument``.
    """

    name: str
    width: int | None


def default_columns(on_cluster: bool) -> tuple[str, ...]:
    """The columns ``printOutput`` prints without ``cols``: ``R/printOutput.R:27-33``.

    R probes ``file.exists("/p")``; the caller passes ``Effects.on_cluster()``.
    """
    return ONCLUSTER_COLUMNS if on_cluster else LOCAL_COLUMNS


def column_specs(cols: Sequence[str], len_cols: Sequence[int] | None) -> list[ColumnSpec]:
    """The width of every data column, in display order: ``R/printOutput.R:45-46``.

    R walks the reversed columns with ``i`` counting from the last one and takes
    ``ifelse(i >= length(lenCols), i + 12, lenCols[ncol + 2 - i])``: without ``lenCols`` every
    width is ``i + 12`` (the first data column is the widest, the last is 13 wide); with ``lenCols``
    the display column ``j`` (1-based) takes ``lenCols[j + 1]`` as long as ``i < length(lenCols)``,
    falls back to ``i + 12`` once ``i`` reaches the length of ``lenCols``, and is ``NA`` (``None``)
    when ``lenCols`` is too short for the index, which errors when the cell is formatted. A
    ``lenCols`` of length ``ncol + 1`` (the ``loopRuns`` and ``getSanityChecks`` case) therefore
    maps ``lenCols[1]`` to the folder column and ``lenCols[j + 1]`` to data column ``j``.
    """
    n_cols = len(cols)
    n_widths = 0 if len_cols is None else len(len_cols)
    specs: list[ColumnSpec] = []
    for j, name in enumerate(cols, start=1):
        i = n_cols + 1 - j
        if i >= n_widths:
            width: int | None = i + _WIDTH_OFFSET
        elif len_cols is not None and j < n_widths:
            width = len_cols[j]
        else:
            width = None
        specs.append(ColumnSpec(name, width))
    return specs


def is_na(value: CellValue) -> bool:
    """``is.na(x)`` for one cell: a real missing value, including a float ``nan`` (``NaN`` is ``NA``)."""
    return value is None or (isinstance(value, float) and math.isnan(value))


def format_cell(value: CellValue, width: int | None) -> str:
    """``formatstr(x, len)`` of ``R/printOutput.R:17-20``.

    A missing value is ``len`` blanks; anything else is pasted to text (``paste0``, see
    ``r_as_character``), padded with ``len`` blanks and cut to ``len`` characters (``substr`` counts
    characters, not bytes). ``rep(" ", len)`` runs in both branches, so an ``NA`` or negative
    width is the R error ``invalid 'times' argument`` whatever the value.
    """
    if width is None or width < 0:
        raise PrintOutputError(INVALID_TIMES_ARGUMENT)
    if is_na(value):
        return " " * width
    return (r_as_character(value) + " " * width)[:width]


def print_output(
    record: StatusRow,
    len1stcol: int = 67,
    len_cols: Sequence[int] | None = None,
    col_sep: str = "   ",
    cols: Sequence[str] | None = None,
    *,
    on_cluster: bool = False,
) -> str:
    """``printOutput(string, len1stcol, lenCols, colSep, cols)`` of ``R/printOutput.R:14-50``.

    The line is the row name padded or cut to ``len1stcol`` (``lenCols[1]`` when ``lenCols`` is
    given), then ``col_sep``, then the cells of ``cols`` (the default set of ``default_columns``
    when ``None``) in ``column_specs`` widths, joined with ``col_sep`` when ``lenCols`` is given and
    with nothing otherwise (``R/printOutput.R:47``, ``is.null(lenCols)``: an empty ``len_cols`` is
    not ``NULL`` and keeps the separator), and a newline. A record without cells returns ``""``
    (``R/printOutput.R:22``, before any column is looked up).

    The R errors are raised as ``PrintOutputError`` in R's order: ``undefined columns selected``
    for a column the record lacks (``string[, cols]``, line 38), ``invalid 'times' argument`` for a
    width ``column_specs`` could not resolve (line 46) or an unusable ``len1stcol`` (line 48), and
    ``argument is of length zero`` for exactly one column, because ``string[, cols]`` drops a
    one-column data frame to a vector whose ``rownames()`` is ``NULL`` (line 48; a suspected R bug
    reported to the register, reproduced for parity).
    """
    if len_cols is not None and len(len_cols) > 0:
        len1stcol = len_cols[0]
    cells = record.cells
    if len(cells) == 0:
        return ""
    names = list(default_columns(on_cluster) if cols is None else cols)
    if any(name not in cells for name in names):
        raise PrintOutputError(UNDEFINED_COLUMNS)
    body = ("" if len_cols is None else col_sep).join(
        format_cell(cells[spec.name], spec.width) for spec in column_specs(names, len_cols)
    )
    if len(names) == 1:
        raise PrintOutputError(ARGUMENT_OF_LENGTH_ZERO)
    return format_cell(record.run, len1stcol) + col_sep + body + "\n"


# --- the Runtime column of loopRuns ------------------------------------------------------------------


def format_runtime(seconds: float | None) -> str:
    """``format(round(make_difftime(second = s), 1))`` of ``R/loopRuns.R:71``.

    lubridate picks the unit from ``abs(s)`` (``secs`` below 60, ``mins`` below 3600, ``hours``
    below 86400, else ``days``), ``round(x, 1)`` is R's rounding (``r_round``), and
    ``format.difftime`` prints the number with 7 significant digits (``r_format_number``) followed
    by a space and the unit: ``45 secs``, ``35.4 mins``, ``1.5 hours``, ``2.3 days``,
    ``1e+05 days``. A missing value falls through every comparison into ``days`` (``NA days``).
    """
    if seconds is None:
        return "NA days"
    unit, divisor = _difftime_unit(seconds)
    if math.isnan(seconds) or math.isinf(seconds):
        return f"{r_format_number(seconds)} {unit}"
    return f"{r_format_number(r_round(seconds / divisor, 1))} {unit}"


def _difftime_unit(seconds: float) -> tuple[str, float]:
    """The unit and its length in seconds that ``make_difftime`` chooses for ``seconds``."""
    magnitude = abs(seconds)
    for unit, divisor, limit in _DIFFTIME_UNITS:
        if magnitude < limit:
            return unit, divisor
    return _DIFFTIME_UNITS[-1][0], _DIFFTIME_UNITS[-1][1]


def mark_active(runtime_text: str) -> str:
    """``paste0(">", if (nchar(x) < 10) " ", x)`` of ``R/loopRuns.R:73``: the running-job marker."""
    pad = " " if len(runtime_text) < _MARKER_PAD_BELOW else ""
    return f">{pad}{runtime_text}"


def runtime_display(runtime: float | None, job_in_slurm: str) -> str:
    """The ``Runtime`` text of a table row: ``R/loopRuns.R:66-79``.

    In R's order: a ``jobInSLURM`` ending in ``pending`` is ``pending``; a numeric runtime is
    ``format_runtime``, marked with ``>`` when the job is not ``"no"`` (so also for the local
    ``"NA"`` and for ``startup``); a missing runtime with ``jobInSLURM`` ending in ``startup`` is
    ``startup``; otherwise ``format(NA)``, the text ``NA``.
    """
    if _PENDING_SUFFIX.search(job_in_slurm):
        return "pending"
    if not is_na(runtime):
        text = format_runtime(runtime)
        return text if job_in_slurm == "no" else mark_active(text)
    if _STARTUP_SUFFIX.search(job_in_slurm):
        return "startup"
    return "NA"


# --- piamutils::niceround --------------------------------------------------------------------------


def niceround(x: float | None, digits: int = 3) -> str:
    """``piamutils::niceround(x, digits)`` for one number, as ``R/getRunStatus.R:184`` calls it.

    ``digits <- pmax(digits, ceiling(log10(abs(x))))`` keeps every digit before the decimal point,
    then ``format(signif(x, digits), scientific = FALSE)``: ``0.2``, ``2`` (from 1.5), ``12`` (from
    12.3), ``123457`` (from 123456.7). ``NA`` is ``NA``, ``NaN`` is ``NaN``, infinities print as
    ``Inf``.
    """
    if x is None:
        return "NA"
    if math.isnan(x) or math.isinf(x):
        return r_format_number(x)
    kept = digits if x == 0 else max(digits, math.ceil(math.log10(abs(x))))
    return r_format_number(r_signif(x, kept), scientific=False)


# --- R's numbers as text -----------------------------------------------------------------------------


def r_as_character(value: CellValue) -> str:
    """``paste0(x)`` / ``as.character(x)`` of one cell.

    Logicals are ``TRUE`` / ``FALSE``, integers print in full, doubles with 15 significant digits in
    the narrower of fixed and scientific notation (``8304``, ``12345.678``, ``1e+05``, ``1e+15``,
    ``0.333333333333333``), a missing value is ``NA`` and strings are themselves.
    """
    match value:
        case None:
            return "NA"
        case bool():
            return "TRUE" if value else "FALSE"
        case int():
            return str(value)
        case float():
            return r_format_number(value, digits=_AS_CHARACTER_DIGITS)
        case str():
            return value


@dataclass(frozen=True)
class _Scientific:
    """The decomposition R's ``scientific()`` (``format.c``) computes for one double."""

    neg: bool
    kpower: int
    nsig: int
    rounding_widens: bool


def _scientific(x: float, digits: int) -> _Scientific:
    """``scientific()`` of R's ``format.c``: sign, decimal exponent and significant digits needed.

    ``alpha = nearbyint(|x| / 10^kp)`` is the value scaled to ``digits`` integer digits (exact
    decimal arithmetic here, long double in R), ``nsig`` is ``digits`` minus its trailing zeros, and
    a value that rounds up to the next power of ten (``9.99999999999999999``) carries over.
    """
    if x == 0.0:
        return _Scientific(False, 0, 1, False)
    r = Decimal(abs(x))
    kp = r.adjusted() - digits + 1
    alpha = int(r.scaleb(-kp).to_integral_value(ROUND_HALF_EVEN))
    nsig = digits
    for _ in range(digits):
        alpha, remainder = divmod(alpha, 10)
        if remainder:
            break
        nsig -= 1
    if nsig == 0:
        nsig = 1
        kp += 1
    kpower = kp + digits - 1
    # R: "scientific format may do more rounding than fixed format (9996 with 3 digits is 1e+04 in
    # scientific, 9996 in fixed)"; the fuzz and the comparison are double arithmetic, so that
    # 1e23 - 0.5 absorbs the fuzz and the width stays one wider (verified against R 4.6.1).
    rgt = min(max(digits - kpower, 0), _KP_MAX)
    fuzz = 0.5 / 10.0**rgt
    rounding_widens = 0 < kpower <= _KP_MAX and abs(x) < 10.0**kpower - fuzz
    return _Scientific(x < 0, kpower, nsig, rounding_widens)


def r_format_number(x: float, digits: int = _FORMAT_DIGITS, *, scientific: bool | None = None) -> str:
    """``format(x, digits)`` of one double: R's ``formatReal`` plus ``encodeReal``.

    The fixed and the scientific widths are computed from ``_scientific``; fixed notation wins when
    it is not wider (``wF <= wE``), unless ``scientific`` forces one notation the way
    ``format(scientific = FALSE/TRUE)`` does (verified: ``format(1e-300, scientific = FALSE)`` is
    fixed), and the text is C's ``%*.*f`` or ``%*.*e`` with the chosen width and digits, trailing
    zeros dropped as ``as.character`` does. Verified against R 4.6.1 on 24,500 doubles across all
    magnitudes; the one known deviation is a subnormal (``5e-324``) under ``scientific=False``, which
    R still prints in scientific notation and no modelstats value can reach. 7 digits reproduce
    ``format()`` (``format.difftime``, ``niceround``), 15 digits ``as.character()``. ``NaN``, ``Inf``
    and ``-Inf`` print as R does.
    """
    if math.isnan(x):
        return "NaN"
    if math.isinf(x):
        return "Inf" if x > 0 else "-Inf"
    if x == 0.0:
        x = 0.0  # a signed zero prints as 0
    sci = _scientific(x, digits)
    neg = 1 if sci.neg else 0
    left = sci.kpower + 1 - (1 if sci.rounding_widens else 0)
    sleft = neg + (1 if left <= 0 else left)
    rgt = max(sci.nsig - left, 0)
    if left < 0:
        sleft = 1 + neg
    width_fixed = sleft + rgt + (1 if rgt else 0)
    exponent_digits = 2 if (left > 100 or left <= -99) else 1
    mantissa_digits = sci.nsig - 1
    width_sci = neg + (1 if mantissa_digits > 0 else 0) + mantissa_digits + 4 + exponent_digits
    fixed = width_fixed <= width_sci if scientific is None else not scientific
    if fixed:
        return _drop_trailing_zeros(f"{x:{width_fixed}.{rgt}f}")
    return _drop_trailing_zeros(f"{x:{width_sci}.{mantissa_digits}e}")


def _drop_trailing_zeros(text: str) -> str:
    """``EncodeRealDrop0``: remove trailing zeros of a fraction, and the point when nothing is left."""
    mantissa, sep, exponent = text.partition("e")
    if "." in mantissa:
        mantissa = mantissa.rstrip("0").rstrip(".")
    return f"{mantissa}{sep}{exponent}"


def r_round(x: float, digits: int) -> float:
    """``round(x, digits)`` of R 4.x (``nmath/fround.c``), which is not Python's ``round``.

    For ``digits > 0`` R compares the two decimal candidates ``floor(x * 10^d) / 10^d`` and
    ``ceiling(x * 10^d) / 10^d`` by their double distance to ``x`` and takes the nearer one, the
    even last digit on an exact tie; ``round(0.45, 1)`` is therefore ``0.4`` (both distances round
    to the same double) where Python gives ``0.5`` (the binary value is above the tie). Values with
    more than 15 significant digits requested are returned unchanged; ``digits == 0`` is
    ``nearbyint`` (half to even), negative digits scale the same way.
    """
    if math.isnan(x) or math.isinf(x) or x == 0.0:
        return x
    if x < 0.0:
        return -r_round(-x, digits)
    digits = min(digits, _ROUND_MAX_DIGITS)
    if digits == 0:
        return float(round(x))
    if digits < 0:
        pow10 = 10.0 ** (-digits)
        return float(round(x / pow10)) * pow10
    log10_x = _LOG10_2 * (0.5 + (math.frexp(x)[1] - 1))
    if log10_x + digits > _DBL_DIG:
        return x
    pow10 = 10.0**digits
    x10 = pow10 * x
    i10 = math.floor(x10)
    down = i10 / pow10
    up = math.ceil(x10) / pow10
    distance_up = up - x
    distance_down = x - down
    if distance_up < distance_down or (distance_up == distance_down and i10 % 2 == 1):
        return up
    return down


def r_signif(x: float, digits: int) -> float:
    """``signif(x, digits)`` of R (``nmath/fprec.c``): ``nearbyint(x * 10^e) / 10^e`` in doubles.

    The scaling is done in double arithmetic as R does it, so ``signif(0.35, 1)`` is ``0.4``
    (``0.35 * 10`` is exactly ``3.5``, which rounds to the even ``4``) although the binary value of
    ``0.35`` lies below the tie.
    """
    if math.isnan(x) or math.isinf(x) or x == 0.0:
        return x
    if digits > _SIGNIF_MAX_DIGITS:
        return x
    digits = max(digits, 1)
    sign = -1.0 if x < 0.0 else 1.0
    magnitude = abs(x)
    log10_x = math.log10(magnitude)
    e10 = int(digits - 1 - math.floor(log10_x))
    p10 = 1.0
    if e10 > _MAX_10_EXP:
        p10 = 10.0 ** (e10 - _MAX_10_EXP)
        e10 = _MAX_10_EXP
    if e10 > 0:
        pow10 = 10.0**e10
        return sign * (float(round((magnitude * pow10) * p10)) / pow10) / p10
    pow10 = 10.0 ** (-e10)
    return sign * (float(round(magnitude / pow10)) * pow10)
