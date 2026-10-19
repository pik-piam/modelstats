"""Clock, R rounding and R number formatting.

R source: ``lubridate::make_difftime`` + ``format(round(., 1))``, ``piamutils::niceround``,
``as.character()``/``paste0()`` of numbers, ``Sys.time()``/``Sys.Date()``.

``now()`` and ``today()`` are the only wall-clock reads of the package so that tests can replace
them (call them through the module, ``_timefmt.now()``). ``r_round``/``r_signif`` reproduce R's
``round()`` (R >= 4.0.0 algorithm) and ``signif()``, because ``round(x, 1)`` differs from
Python's ``round`` on decimal ties (R: ``round(1.05, 1) == 1``, Python: ``1.1``); they are ports of
R's ``fround``/``fprec`` (R 4.6), including the branches for the extremes of the double range.
``r_num_str`` prints R's 15 significant digits and can differ from R's ``format`` on rare 15-digit
near-ties.
"""

from __future__ import annotations

import math
import sys
from datetime import date, datetime

__all__ = [
    "format_runtime",
    "niceround",
    "now",
    "r_num_str",
    "r_paste0",
    "r_round",
    "r_signif",
    "r_str",
    "today",
]

_DBL_DIG = 15
_MAX10E = 308
_MAX_SIGNIF_DIGITS = 22


def _r_pow_di(x: float, n: int) -> float:
    """``x ** n`` by repeated squaring, as R's ``R_pow_di`` computes its powers of ten."""
    if math.isnan(x):
        return x
    if n == 0:
        return 1.0
    if not math.isfinite(x):
        return x**n
    negative = n < 0
    if negative:
        n = -n
    result = 1.0
    while True:
        if n & 1:
            result *= x
        n >>= 1
        if n:
            x *= x
        else:
            break
    return 1.0 / result if negative else result


def now() -> datetime:
    """``Sys.time()``: the current time as an aware datetime in the local zone."""
    return datetime.now().astimezone()


def today() -> date:
    """``Sys.Date()``."""
    return date.today()


def r_round(x: float, digits: int = 0) -> float:
    """``round(x, digits)`` as R computes it.

    R picks the closer of the two decimal candidates and, when both are equally close in double
    arithmetic, the one whose last digit is even.
    """
    if not math.isfinite(x) or x == 0 or digits > _MAX10E + _DBL_DIG:
        return x
    if digits < -_MAX10E:
        return 0.0
    if digits == 0:
        return float(round(x))
    sign = -1.0 if x < 0 else 1.0
    x = abs(x)
    # R: l10x = M_LOG10_2 * (0.5 + logb(x)); logb(x) is the binary exponent of x
    l10x = math.log10(2) * (0.5 + math.frexp(x)[1] - 1)
    if l10x + digits > _DBL_DIG:
        return sign * x
    if digits <= _MAX10E:
        pow10 = _r_pow_di(10.0, digits)
        x10 = x * pow10
        i10 = math.floor(x10)
        lower = i10 / pow10
        upper = math.ceil(x10) / pow10
    else:  # very small x: scale in two steps so that neither overflows
        p10 = _r_pow_di(10.0, digits - _MAX10E)
        pow10 = _r_pow_di(10.0, _MAX10E)
        x10 = (x * pow10) * p10
        i10 = math.floor(x10)
        lower = i10 / pow10 / p10
        upper = math.ceil(x10) / pow10 / p10
    to_upper = upper - x
    to_lower = x - lower
    if to_upper < to_lower or (i10 % 2 == 1 and to_upper == to_lower):
        return sign * upper
    return sign * lower


def r_signif(x: float, digits: int = 6) -> float:
    """``signif(x, digits)`` as R computes it (``fprec``)."""
    if not math.isfinite(x) or x == 0:
        return x
    dig = max(1, digits)
    if dig > _MAX_SIGNIF_DIGITS:
        return x
    sign = -1.0 if x < 0 else 1.0
    x = abs(x)
    l10 = math.log10(x)
    e10 = dig - 1 - math.floor(l10)
    if abs(l10) < _MAX10E - 2:
        p10 = 1.0
        if e10 > _MAX10E:
            p10 = _r_pow_di(10.0, e10 - _MAX10E)
            e10 = _MAX10E
        if e10 > 0:
            pow10 = _r_pow_di(10.0, e10)
            return sign * (float(round((x * pow10) * p10)) / pow10) / p10
        pow10 = _r_pow_di(10.0, -e10)
        return sign * (float(round(x / pow10)) * pow10)
    # very large or very small values: scale in two steps so that neither overflows
    do_round = math.log10(sys.float_info.max) - l10 >= _r_pow_di(10.0, -dig)
    e2 = dig + (_MAX_SIGNIF_DIGITS if e10 > 0 else -_MAX_SIGNIF_DIGITS)
    p10 = _r_pow_di(10.0, e2)
    big10 = _r_pow_di(10.0, e10 - e2)
    x *= p10
    x *= big10
    if do_round:
        x += 0.5
    x = math.floor(x) / p10
    return sign * x / big10


def _positional(value: float, sig: int) -> str:
    """``format(value, digits = sig, scientific = FALSE)``: fixed notation, trailing zeros dropped."""
    if value == 0:
        return "0"
    if math.isnan(value):
        return "NaN"
    if math.isinf(value):
        return "Inf" if value > 0 else "-Inf"
    exponent = math.floor(math.log10(abs(value)))
    decimals = max(0, sig - 1 - exponent)
    text = f"{value:.{decimals}f}"
    if "." in text:
        text = text.rstrip("0").rstrip(".")
    return text


def _scientific(value: float, sig: int) -> str:
    """R's scientific notation with up to ``sig`` significant digits: ``1e+05``, ``1.5e-07``."""
    mantissa, exponent = f"{value:.{sig - 1}e}".split("e")
    if "." in mantissa:
        mantissa = mantissa.rstrip("0").rstrip(".")
    digits = exponent[1:].lstrip("0").zfill(2)
    return f"{mantissa}e{exponent[0]}{digits}"


def r_num_str(x: float | int) -> str:
    """``as.character(x)`` / ``paste0(x)`` of a number: ``2`` not ``2.0``, ``1e+05``, ``0.333333333333333``.

    R prints up to 15 significant digits and uses scientific notation when it is shorter than the
    fixed one (R occasionally drops to 14 digits on a near-tie; the port always keeps 15).
    """
    if isinstance(x, bool):
        return "TRUE" if x else "FALSE"
    if isinstance(x, int):
        return str(x)
    if not math.isfinite(x) or x == 0:
        return _positional(x, _DBL_DIG)
    fixed = _positional(x, _DBL_DIG)
    scientific = _scientific(x, _DBL_DIG)
    return fixed if len(fixed) <= len(scientific) else scientific


def r_str(value: object) -> str:
    """``as.character()`` of a scalar: ``TRUE``/``FALSE`` for booleans, ``2`` for ``2.0``, else ``str``."""
    if isinstance(value, bool | int | float):
        return r_num_str(value)
    return str(value)


def r_paste0(values: object) -> str:
    """``paste0(as.character(x), collapse = "")`` of a scalar, a vector or a named vector.

    ``None`` inside a vector is ``NA`` and prints as ``NA``; a bare ``None`` is ``NULL`` and prints
    nothing (BUG-038: both arrive as ``None``).
    """
    if values is None:
        return ""
    if isinstance(values, dict):
        items: list[object] = list(values.values())
    elif isinstance(values, list):
        items = values
    else:
        items = [values]
    return "".join("NA" if item is None else r_str(item) for item in items)


def format_runtime(seconds: float) -> str:
    """``format(round(lubridate::make_difftime(second = seconds), 1))``: ``3.4 hours``, ``60 secs``.

    The unit follows the magnitude (secs < 60 <= mins < 3600 <= hours < 86400 <= days); the value
    is rounded to one decimal and printed with up to seven significant digits without trailing
    zeros (R would switch to ``1e+05 days`` beyond 100000 days).
    """
    magnitude = abs(seconds)
    if magnitude < 60:
        unit, divisor = "secs", 1.0
    elif magnitude < 3600:
        unit, divisor = "mins", 60.0
    elif magnitude < 86400:
        unit, divisor = "hours", 3600.0
    else:
        unit, divisor = "days", 86400.0
    return f"{_positional(r_round(seconds / divisor, 1), 7)} {unit}"


def niceround(x: float, digits: int = 3) -> str:
    """``piamutils::niceround(x, digits)``: ``signif`` keeping the integer digits, never scientific.

    ``0.35 -> 0.4``, ``1.25 -> 1``, ``9.99 -> 10``, ``12.3 -> 12``, ``120.4 -> 120``.
    """
    if x != 0 and math.isfinite(x):
        digits = max(digits, math.ceil(math.log10(abs(x))))
    return _positional(r_signif(x, digits), 7)
