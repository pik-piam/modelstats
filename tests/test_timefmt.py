"""Tests of _timefmt against values computed by R 4.6.1 (see the tables in PORTING.md section 3.9)."""

from __future__ import annotations

import math
from datetime import date, datetime

import pytest

from modelstats import _timefmt
from modelstats._timefmt import format_runtime, niceround, r_num_str, r_paste0, r_round, r_signif, r_str

# fmt: off
R_ROUND_1 = [
    (0.05, 0.0), (0.15, 0.1), (0.25, 0.2), (0.35, 0.3), (0.45, 0.4), (1.05, 1.0), (1.15, 1.1),
    (2.675, 2.7), (1.005, 1.0), (59.95, 60.0), (59.99, 60.0), (2.09, 2.1), (17.425, 17.4),
    (3.45, 3.5), (12.35, 12.3), (0.95, 0.9), (100.05, 100.0), (1e-7, 0.0), (123456.789, 123456.8),
    (-1.05, -1.0), (-0.25, -0.2), (5.0, 5.0), (0.0, 0.0), (7.5, 7.5), (1234567.85, 1234567.9),
]
R_ROUND_2 = [
    (0.05, 0.05), (0.15, 0.15), (0.25, 0.25), (2.675, 2.67), (1.005, 1.0), (17.425, 17.42),
    (123456.789, 123456.79), (-1.05, -1.05), (1e-7, 0.0),
]
R_ROUND_0 = [(0.5, 0.0), (1.5, 2.0), (2.5, 2.0), (-0.5, 0.0), (3.7, 4.0), (1e10 + 0.5, 1e10)]
R_SIGNIF_1 = [
    (0.35, 0.4), (0.45, 0.4), (0.25, 0.2), (0.15, 0.2), (2.5, 2.0), (3.5, 4.0), (0.95, 1.0),
    (1.95, 2.0), (99.5, 100.0), (0.005, 0.005), (0.015, 0.02), (0.0349, 0.03), (1.25, 1.0),
    (9.99, 10.0), (12.3, 10.0), (120.4, 100.0), (0.151, 0.2), (2.45, 2.0), (0.051, 0.05),
    (1.05, 1.0), (10.5, 10.0), (0.049, 0.05), (123.456, 100.0), (0.000123456, 1e-4), (-2.5, -2.0),
]
R_SIGNIF_3 = [
    (0.35, 0.35), (99.5, 99.5), (0.0349, 0.0349), (120.4, 120.0), (123.456, 123.0),
    (0.000123456, 0.000123), (-2.5, -2.5),
]
R_NICEROUND_1 = [
    (0.35, "0.4"), (0.45, "0.4"), (0.25, "0.2"), (0.15, "0.2"), (2.5, "2"), (3.5, "4"), (0.95, "1"),
    (1.95, "2"), (99.5, "100"), (0.005, "0.005"), (0.015, "0.02"), (0.0349, "0.03"), (1.25, "1"),
    (9.99, "10"), (12.3, "12"), (120.4, "120"), (0.151, "0.2"), (2.45, "2"), (0.051, "0.05"),
    (1.05, "1"), (10.5, "10"), (0.049, "0.05"), (123.456, "123"), (0.000123456, "0.0001"),
    (-2.5, "-2"), (0.0, "0"),
]
R_NICEROUND_3 = [(0.35, "0.35"), (120.4, "120"), (123.456, "123"), (0.000123456, "0.000123"), (0.0, "0")]
R_RUNTIME = [
    (0, "0 secs"), (3, "3 secs"), (45.6, "45.6 secs"), (59.99, "60 secs"), (60, "1 mins"),
    (63, "1 mins"), (125.4, "2.1 mins"), (3599, "60 mins"), (3600, "1 hours"), (3661, "1 hours"),
    (5400, "1.5 hours"), (12345, "3.4 hours"), (62730.5, "17.4 hours"), (86399, "24 hours"),
    (86400, "1 days"), (2592000, "30 days"), (-5, "-5 secs"), (90000.4, "1 days"), (0.05, "0 secs"),
    (59.95, "60 secs"), (59.949, "59.9 secs"), (3780, "1 hours"), (95040, "1.1 days"),
    (604800, "7 days"), (8640000, "100 days"), (35.95, "36 secs"), (2.25, "2.2 secs"),
    (36000, "10 hours"),
]
R_AS_CHARACTER = [
    (2.0, "2"), (2.5, "2.5"), (100000.0, "1e+05"), (123456.0, "123456"), (100001.0, "100001"),
    (0.0001, "1e-04"), (0.00012, "0.00012"), (1 / 3, "0.333333333333333"), (1e15, "1e+15"),
    (1e16, "1e+16"), (123456789012.0, "123456789012"), (0.1 + 0.2, "0.3"), (50.0, "50"),
    (1e-5, "1e-05"), (12345678.9, "12345678.9"), (1234567890123456.0, "1234567890123456"),
    (0.0, "0"), (-3.0, "-3"), (-0.5, "-0.5"), (100000.5, "100000.5"), (2 / 3, "0.666666666666667"),
    (0.00015, "0.00015"), (1234.5678901234567, "1234.56789012346"), (99999.0, "99999"),
    (999999.0, "999999"), (1e300, "1e+300"), (1.5e-300, "1.5e-300"), (0.1, "0.1"), (7e-5, "7e-05"),
]
# fmt: on


@pytest.mark.parametrize(("value", "expected"), R_ROUND_1)
def test_r_round_one_decimal_matches_r(value: float, expected: float) -> None:
    assert r_round(value, 1) == expected


@pytest.mark.parametrize(("value", "expected"), R_ROUND_2)
def test_r_round_two_decimals_matches_r(value: float, expected: float) -> None:
    assert r_round(value, 2) == expected


@pytest.mark.parametrize(("value", "expected"), R_ROUND_0)
def test_r_round_integer_matches_r(value: float, expected: float) -> None:
    assert r_round(value) == expected


def test_r_round_passes_non_finite_through() -> None:
    assert math.isnan(r_round(math.nan, 1))
    assert r_round(math.inf, 1) == math.inf


def test_r_round_differs_from_python_on_decimal_ties() -> None:
    assert round(1.05, 1) == 1.1
    assert r_round(1.05, 1) == 1.0


@pytest.mark.parametrize(("value", "expected"), R_SIGNIF_1)
def test_r_signif_one_digit_matches_r(value: float, expected: float) -> None:
    assert r_signif(value, 1) == expected


@pytest.mark.parametrize(("value", "expected"), R_SIGNIF_3)
def test_r_signif_three_digits_matches_r(value: float, expected: float) -> None:
    assert r_signif(value, 3) == expected


@pytest.mark.parametrize(("value", "expected"), R_NICEROUND_1)
def test_niceround_one_digit_matches_r(value: float, expected: str) -> None:
    assert niceround(value, 1) == expected


@pytest.mark.parametrize(("value", "expected"), R_NICEROUND_3)
def test_niceround_three_digits_matches_r(value: float, expected: str) -> None:
    assert niceround(value, 3) == expected


@pytest.mark.parametrize(("seconds", "expected"), R_RUNTIME)
def test_format_runtime_matches_r(seconds: float, expected: str) -> None:
    assert format_runtime(seconds) == expected


@pytest.mark.parametrize(("value", "expected"), R_AS_CHARACTER)
def test_r_num_str_matches_as_character(value: float, expected: str) -> None:
    assert r_num_str(value) == expected


def test_r_signif_and_r_round_at_the_extremes() -> None:
    # expected values from R 4.6.1: sprintf("%.17g", ...)
    assert r_signif(1e-22, 2) == 1e-22
    assert r_signif(9.5e306, 1) == 9.000000000000005e306
    assert r_signif(1.7e308, 1) == 1.0000000000000006e308
    assert r_signif(1e308, 1) == 1.0000000000000006e308
    assert r_signif(1e308, 5) == 1.0000000000000002e308
    assert r_signif(1.2612042507803999e308, 2) == 1.3000000000000005e308
    assert r_signif(1.09e308, 2) == 1.1000000000000004e308
    assert r_round(1e-300, 310) == 9.9999999999999953e-301
    assert r_round(4.045e-311, 324) == 4.045e-311  # digits > 323: unchanged
    assert r_round(1.5e-310, 310) == 1.9999999999999939e-310
    assert r_round(2.2250738585072014e-308, 309) == 2.1999999999999987e-308
    assert r_round(1e-320, 320) == 1e-320
    assert r_round(2.5e-309, 309) == 3.0000000000000007e-309
    assert r_round(5.0, -400) == 0.0
    assert r_round(123456.0, -3) == 123000.0
    assert r_round(25.0, -1) == 20.0


def test_niceround_default_is_three_digits_like_piamutils() -> None:
    assert niceround(0.123456) == "0.123"
    assert niceround(math.nan) == "NaN"
    assert niceround(math.inf) == "Inf"


def test_format_runtime_non_finite() -> None:
    assert format_runtime(math.nan) == "NaN days"
    assert format_runtime(math.inf) == "Inf days"
    assert format_runtime(-math.inf) == "-Inf days"


def test_r_str_scalars() -> None:
    assert r_str(True) == "TRUE"
    assert r_str(2.0) == "2"
    assert r_str(7) == "7"
    assert r_str("x") == "x"


def test_r_num_str_integers_and_booleans() -> None:
    assert r_num_str(7) == "7"
    assert r_num_str(True) == "TRUE"
    assert r_num_str(math.nan) == "NaN"
    assert r_num_str(-math.inf) == "-Inf"


def test_now_is_aware_and_today_is_a_date() -> None:
    moment = _timefmt.now()
    assert isinstance(moment, datetime) and moment.tzinfo is not None
    assert isinstance(_timefmt.today(), date)


def test_r_paste0() -> None:
    assert r_paste0([2.0, 0.0, 7.0]) == "207"
    assert r_paste0(2.0) == "2"
    assert r_paste0({"y1995": 2.0, "y2000": 7.0}) == "27"
    assert r_paste0([2.0, None, 7.0]) == "2NA7"  # as.character(NA) is "NA"
    assert r_paste0(None) == ""  # NULL
    assert r_paste0([]) == ""
