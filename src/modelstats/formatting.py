"""Text formatting of modelstats: ``printOutput``, the runtime display of ``loopRuns`` and the number helpers.

Ported from ``R/printOutput.R``, the display code of ``R/loopRuns.R`` (``format(round(make_difftime(second = s), 1))``)
and ``piamutils::niceround``. The number-to-text helpers reproduce R's own formatting arithmetic (``formatReal`` in
``src/main/format.c``, ``fround`` in ``src/nmath/fround.c`` and ``fprec`` in ``src/nmath/fprec.c``) so that the text
is byte-identical to what R prints; Python's ``round`` and ``:g`` formatting differ from R on ties (R rounds
``1.05`` to ``1`` and ``signif(0.35, 1)`` to ``0.4``) and on the fixed-versus-scientific choice.
"""

from __future__ import annotations

import math
from collections.abc import Mapping, Sequence
from decimal import Decimal
from typing import TYPE_CHECKING

from modelstats.errors import RParityError

if TYPE_CHECKING:
    from modelstats.env import Effects

__all__ = [
    "COLS_LOCAL",
    "COLS_ON_CLUSTER",
    "format_number",
    "format_runtime",
    "niceround",
    "print_output",
    "r_num_str",
    "r_round",
    "r_signif",
]

#: The default column set of ``printOutput`` when ``/p`` exists (the cluster) and otherwise, in R's order.
COLS_ON_CLUSTER: tuple[str, ...] = (
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
COLS_LOCAL: tuple[str, ...] = ("Runtime", "RunType", "RunStatus", "Warnings", "Iter", "Conv", "modelstat", "Mif")

# R's integer range (NA_integer_ is -2^31); a Python int outside it would be a double in R.
_R_INT_MIN = -(2**31) + 1
_R_INT_MAX = 2**31 - 1

# Constants of R's format.c / fprec.c / fround.c.
_KP_MAX = 27  # long double builds
_DBL_DIG = 15
_MAX10E = 308  # DBL_MAX_10_EXP
_SIGNIF_MAX_DIGITS = 22
_M_LOG10_2 = math.log10(2.0)
# format(x, scientific = FALSE) is not an unbounded preference for fixed notation: measured on R 4.6.1, it behaves
# as a penalty of 310 (1e-314 and 9.9e-315 print fixed, 1e-315 and 5e-324 print scientific; a numeric
# `scientific = 310` reproduces every case, 309 does not).
_SCIPEN_FIXED = 310
_ERR_TIMES = "invalid 'times' argument"


# ---------------------------------------------------------------------------
# R arithmetic: nearbyint, R_pow_di, round(x, d), signif(x, d)
# ---------------------------------------------------------------------------


def _nearbyint(value: float) -> float:
    """C ``nearbyint`` in the default rounding mode: ties to even."""
    if math.isnan(value) or math.isinf(value):
        return value
    return float(round(value))


def _c_round(value: float) -> float:
    """C ``round``: ties away from zero."""
    return math.floor(value + 0.5) if value >= 0 else -math.floor(-value + 0.5)


def _r_pow_di(x: float, n: int) -> float:
    """``R_pow_di``: x**n by repeated squaring, in double arithmetic like R."""
    if math.isnan(x):
        return x
    result = 1.0
    if n != 0:
        if math.isinf(x):
            return math.pow(x, float(n))
        negative = n < 0
        if negative:
            n = -n
        while True:
            if n & 1:
                result *= x
            n >>= 1
            if n:
                x *= x
            else:
                break
        if negative:
            result = 1.0 / result
    return result


def r_round(x: float, digits: float = 0) -> float:
    """R's ``round(x, digits)`` (``fround``, the R >= 4.0.0 algorithm).

    For ``digits > 0`` the two candidates ``floor(x * 10^d) / 10^d`` and ``ceiling(x * 10^d) / 10^d`` are
    measured in double arithmetic and the closer one wins; on a measured tie the even candidate. That is not what
    Python's ``round`` does (``round(1.05, 1)`` is ``1`` in R and ``1.1`` in Python).
    """
    if math.isnan(x) or math.isnan(digits):
        return x + digits
    if math.isinf(x) or digits == math.inf:
        return x
    if digits == -math.inf:
        return 0.0
    if digits > _MAX10E:
        digits = _MAX10E
    dig = int(math.floor(digits + 0.5))
    sign = 1.0
    if x < 0.0:
        sign = -1.0
        x = -x
    if dig == 0:
        return sign * _nearbyint(x)
    if dig > 0:
        # ~= log10(x), cheaper (logb(x) is the binary exponent)
        logb = (math.frexp(x)[1] - 1) if x != 0.0 else -math.inf
        l10x = _M_LOG10_2 * (0.5 + logb)
        if l10x + dig > _DBL_DIG:
            # rounding to so many digits that no rounding is needed
            return sign * x
        pow10 = _r_pow_di(10.0, dig)
        x10 = pow10 * x
        i10 = math.floor(x10)
        xd = i10 / pow10
        xu = math.ceil(x10) / pow10
        du = xu - x
        dd = x - xd
        use_upper = du < dd or (du == dd and math.fmod(i10, 2.0) == 1)
        return sign * (xu if use_upper else xd)
    pow10 = _r_pow_di(10.0, -dig)
    return sign * _nearbyint(x / pow10) * pow10


def r_signif(x: float, digits: float = 6) -> float:
    """R's ``signif(x, digits)`` (``fprec``), in the same double arithmetic as R.

    ``signif(0.35, 1)`` is ``0.4`` in R because ``0.35 * 10`` rounds to ``3.5`` before ``nearbyint`` sees it.
    """
    if math.isnan(x) or math.isnan(digits):
        return x + digits
    if math.isinf(x):
        return x
    if math.isinf(digits):
        if digits > 0.0:
            return x
        digits = 1.0
    if x == 0.0:
        return x
    dig = int(_c_round(digits))
    if dig > _SIGNIF_MAX_DIGITS:
        return x
    if dig < 1:
        dig = 1
    sign = 1.0
    if x < 0.0:
        sign = -1.0
        x = -x
    l10 = math.log10(x)
    e10 = int(dig - 1 - math.floor(l10))
    if abs(l10) < _MAX10E - 2:
        p10 = 1.0
        if e10 > _MAX10E:
            # numbers less than 10^(dig - 1 - 308)
            p10 = _r_pow_di(10.0, e10 - _MAX10E)
            e10 = _MAX10E
        if e10 > 0:
            # try always to have pow >= 1 and so exactly representable
            pow10 = _r_pow_di(10.0, e10)
            return sign * (_nearbyint((x * pow10) * p10) / pow10) / p10
        pow10 = _r_pow_di(10.0, -e10)
        return sign * (_nearbyint(x / pow10) * pow10)
    # large or small
    do_round = _MAX10E - l10 >= _r_pow_di(10.0, -dig)
    e2 = dig + (_SIGNIF_MAX_DIGITS if e10 > 0 else -_SIGNIF_MAX_DIGITS)
    p10 = _r_pow_di(10.0, e2)
    x *= p10
    big10 = _r_pow_di(10.0, e10 - e2)
    x *= big10
    if do_round:
        x += 0.5
    x = math.floor(x) / p10
    return sign * x / big10


# ---------------------------------------------------------------------------
# R number formatting: formatReal + sprintf
# ---------------------------------------------------------------------------


def _scientific(r: float, digits: int) -> tuple[int, int, bool]:
    """``scientific()`` of R's format.c for a finite ``r > 0``: (kpower, nsig, roundingwidens).

    ``|r| = alpha * 10^kpower`` with ``alpha`` rounded to ``digits`` significant digits (ties to even on the exact
    binary value, as R's long double computation does for every realistic magnitude) and ``nsig`` the number of
    those digits that are not trailing zeros.
    """
    text = f"{r:.{digits - 1}e}"
    mantissa, _, exponent = text.partition("e")
    kpower = int(exponent)
    stripped = mantissa.replace(".", "").rstrip("0")
    nsig = len(stripped) if stripped else 1
    # Scientific is allowed to do rounding, fixed is not: check if rounding widens.
    rgt = digits - kpower
    rgt = 0 if rgt < 0 else (_KP_MAX if rgt > _KP_MAX else rgt)
    fuzz = 0.5 / _r_pow_di(10.0, rgt)
    roundingwidens = 0 < kpower <= _KP_MAX and Decimal(r) < Decimal(10) ** kpower - Decimal(fuzz)
    return kpower, nsig, roundingwidens


def format_number(x: float, digits: int = 7, *, scipen: float = 0) -> str:
    """One finite or non-finite double as R's ``format(x, digits = digits)`` prints it (``formatReal``).

    Fixed notation is used when it needs no more width than scientific notation plus ``scipen``
    (``scientific = FALSE`` is ``scipen = 310``, see ``_SCIPEN_FIXED``). ``digits = 15`` with ``scipen = 0`` is
    ``as.character`` / ``paste0``; ``digits = 7`` is ``format`` and ``print``.

    The significant digits are taken from the exact binary value (ties to even). R scales in 80-bit long double
    instead, which agrees with that for every value with ``|x| >= 1e-9`` or ``|x| < 1e-12`` that was checked
    (40 000 near-tie doubles) but can differ by one unit in the fifteenth digit for 15-digit near-ties in between;
    no number modelstats prints lives there.
    """
    if math.isnan(x):
        return "NaN"
    if x == math.inf:
        return "Inf"
    if x == -math.inf:
        return "-Inf"
    if x == 0.0:
        return "0"
    neg = 1 if x < 0.0 else 0
    kpower, nsig, roundingwidens = _scientific(abs(x), digits)
    left = kpower + 1
    if roundingwidens:
        left -= 1
    sleft = neg + (1 if left <= 0 else left)
    rgt = max(0, nsig - left)
    width_fixed = sleft + rgt + (1 if rgt != 0 else 0)
    exp_digits = 2 if (kpower >= 100 or kpower <= -99) else 1
    decimals = nsig - 1
    width_sci = neg + (1 if decimals > 0 else 0) + decimals + 4 + exp_digits
    if width_fixed <= width_sci + scipen:
        return f"{x:.{rgt}f}"
    return f"{x:.{decimals}e}"


def r_num_str(x: object) -> str:
    """A number as R's ``paste0`` / ``as.character`` renders it.

    Python ``int`` within R's integer range prints plainly; every other number is an R double: 15 significant
    digits, integral values without decimals and scientific notation where R's width rule picks it
    (``1e+05``, ``1e+15``, ``123456``, ``12345.678``). ``None`` is ``NA``, ``bool`` is ``TRUE``/``FALSE``.
    """
    if x is None:
        return "NA"
    if isinstance(x, bool):
        return "TRUE" if x else "FALSE"
    if isinstance(x, int):
        if _R_INT_MIN <= x <= _R_INT_MAX:
            return str(x)
        return format_number(float(x), _DBL_DIG)
    if isinstance(x, float):
        return format_number(x, _DBL_DIG)
    raise TypeError(f"r_num_str expects a number or None, got {type(x).__name__}")


# ---------------------------------------------------------------------------
# format(round(make_difftime(second = s), 1)) and piamutils::niceround
# ---------------------------------------------------------------------------


def format_runtime(seconds: float | None) -> str:
    """``format(round(lubridate::make_difftime(second = seconds), 1))`` of ``R/loopRuns.R``.

    The unit follows the magnitude (``secs`` below 60, ``mins`` below 3600, ``hours`` below 86400, else ``days``),
    the value is divided, rounded to one decimal with R's ``round`` and printed with seven significant digits:
    ``45 secs``, ``2.1 mins``, ``1.5 hours``, ``24 hours``, ``30 days``. ``None`` (R's ``NA``) gives ``NA days``.
    """
    if seconds is None:
        return "NA days"
    value = float(seconds)
    magnitude = abs(value)
    if math.isnan(magnitude):
        unit, divisor = "days", 86400.0
    elif magnitude < 60:
        unit, divisor = "secs", 1.0
    elif magnitude < 3600:
        unit, divisor = "mins", 60.0
    elif magnitude < 86400:
        unit, divisor = "hours", 3600.0
    else:
        unit, divisor = "days", 86400.0
    rounded = r_round(value / divisor, 1)
    return f"{format_number(rounded, 7)} {unit}"


def niceround(x: float | None, digits: float = 3) -> str:
    """``piamutils::niceround(x, digits)`` for one value.

    ``digits`` is raised to ``ceiling(log10(abs(x)))`` so that integral digits are never rounded away, then
    ``format(signif(x, digits), scientific = FALSE)``: ``niceround(9.99, 1)`` is ``10``, ``niceround(0.35, 1)``
    is ``0.4`` and ``niceround(120.4, 1)`` is ``120``.
    """
    if x is None:
        return "NA"
    value = float(x)
    if math.isnan(value) or math.isinf(value):
        return format_number(value, 7)
    if value == 0.0:
        effective = float(digits)
    else:
        effective = max(float(digits), math.ceil(math.log10(abs(value))))
    return format_number(r_signif(value, effective), 7, scipen=_SCIPEN_FIXED)


# ---------------------------------------------------------------------------
# printOutput
# ---------------------------------------------------------------------------


def _is_na(value: object) -> bool:
    """R's ``is.na`` for one cell: ``None`` and a float ``NaN`` are missing; the string ``"NA"`` is not."""
    return value is None or (isinstance(value, float) and math.isnan(value))


def _as_character(value: object) -> str:
    """``paste0(x)`` of one non-missing cell."""
    if isinstance(value, str):
        return value
    if isinstance(value, bool | int | float):
        return r_num_str(value)
    raise TypeError(f"print_output cannot format a cell of type {type(value).__name__}")


def _format_cell(value: object, width: object) -> str:
    """``formatstr(x, len)`` of ``R/printOutput.R``: blanks for a missing value, else padded and cut to ``len``.

    ``substr`` and ``rep`` count characters, so the width is in code points, not bytes. A missing or negative
    width is R's ``rep(" ", NA)`` error (``invalid 'times' argument``).
    """
    if width is None or not isinstance(width, int | float) or isinstance(width, bool) or math.isnan(width):
        raise RParityError(_ERR_TIMES)
    length = int(width)
    if length < 0:
        raise RParityError(_ERR_TIMES)
    if _is_na(value):
        return " " * length
    return (_as_character(value) + " " * length)[:length]


def print_output(
    row: Mapping[str, object],
    *,
    rowname: str,
    len1stcol: float = 67,
    len_cols: Sequence[float] | None = None,
    col_sep: str = "   ",
    cols: Sequence[str] | None = None,
    effects: Effects | None = None,
) -> str:
    """One status row as ``printOutput`` of ``R/printOutput.R`` returns it, newline included.

    ``row`` is the status record (column -> value): ``None`` is a real ``NA`` and prints as blanks of the column
    width, the string ``"NA"`` prints as ``NA``, numbers print like R's ``paste0`` (``r_num_str``), ``bool`` as
    ``TRUE``/``FALSE``. ``rowname`` is the data.frame row name (the run's basename). Without ``cols`` the column
    set depends on ``effects.on_cluster`` exactly as R probes ``/p``; a column missing from ``row`` raises
    ``RParityError('undefined columns selected')``.

    Two regimes, built from the last column backwards as R does: with ``len_cols`` the cell ``i`` counted from
    the last column is padded or cut to ``len_cols[ncol + 1 - i]`` (0-based) and the cells are joined by
    ``col_sep``, the folder column is ``len_cols[0]`` wide; without ``len_cols`` the widths are ``i + 12`` and no
    separator is inserted between the cells (``col_sep`` still follows the folder column). ``len_cols`` shorter
    than the columns raises ``RParityError("invalid 'times' argument")`` where R's ``rep(" ", NA)`` fails, and a
    single selected column raises ``RParityError('argument is of length zero')`` because R's ``[, cols]`` drops
    the data.frame (and its row names) to a vector there. An empty ``row`` returns ``""``.
    """
    if len_cols is not None and len(len_cols) > 0:
        len1stcol = len_cols[0]
    if len(row) == 0:
        return ""
    if cols is None:
        if effects is None:
            from modelstats.env import default_effects

            effects = default_effects()
        cols = COLS_ON_CLUSTER if effects.on_cluster else COLS_LOCAL
    selected = list(reversed(list(cols)))
    missing = [name for name in selected if name not in row]
    if missing:
        raise RParityError("undefined columns selected")
    ncol = len(selected)
    n_widths = 0 if len_cols is None else len(len_cols)
    out = ""
    for i, name in enumerate(selected, start=1):
        if i >= n_widths:
            width: object = i + 12
        else:
            index = ncol + 2 - i  # 1-based into lenCols
            width = len_cols[index - 1] if len_cols is not None and index <= n_widths else None
        cell = _format_cell(row[name], width)
        separator = "" if (len_cols is None or i == 1) else col_sep
        out = cell + separator + out
    if ncol == 1:
        # string[, cols] dropped to a vector: rownames() is NULL and is.na(NULL) has length zero
        raise RParityError("argument is of length zero")
    return _format_cell(rowname, len1stcol) + col_sep + out + "\n"
