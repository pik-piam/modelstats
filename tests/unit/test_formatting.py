"""modelstats.formatting against R.

The expected strings were produced with Rscript (R 4.6.1, lubridate 1.9.5, piamutils 0.2.1) on 2026-10-01:
``format(round(make_difftime(second = s), 1))``, ``as.character(x)``, ``format(x)``, ``piamutils::niceround``,
``round``/``signif`` and ``printOutput`` itself; they are embedded here so the suite is hermetic. Two tests re-run
the oracle live when Rscript with lubridate and piamutils is on the machine and skip otherwise. The printoutput
golden cases are replayed directly when ``migration/`` is present.
"""

from __future__ import annotations

import json
import math
import shutil
import subprocess
from pathlib import Path

import pytest
from _fake_effects import FakeEffects

from modelstats.errors import RParityError
from modelstats.formatting import (
    COLS_LOCAL,
    COLS_ON_CLUSTER,
    format_number,
    format_runtime,
    niceround,
    print_output,
    r_num_str,
    r_round,
    r_signif,
)

REPO = Path(__file__).resolve().parents[2]
PRINTOUTPUT_CASES = REPO / "migration" / "cases" / "printoutput"
PRINTOUTPUT_GOLDENS = REPO / "migration" / "goldens" / "printoutput"

# ---------------------------------------------------------------------------
# format_runtime: format(round(make_difftime(second = s), 1))
# ---------------------------------------------------------------------------

# the workflow specification's list, the table of 02-python-libraries.md section 4.8 and edge cases; R output
RUNTIME_TABLE: list[tuple[float, str]] = [
    (0, "0 secs"),
    (1, "1 secs"),
    (59, "59 secs"),
    (59.99, "60 secs"),
    (60, "1 mins"),
    (61, "1 mins"),
    (125.4, "2.1 mins"),
    (3599, "60 mins"),
    (3600, "1 hours"),
    (3661, "1 hours"),
    (5400, "1.5 hours"),
    (12345, "3.4 hours"),
    (86399, "24 hours"),
    (86400, "1 days"),
    (90000, "1 days"),
    (2592000, "30 days"),
    (8.64e6, "100 days"),
    (3, "3 secs"),
    (45.6, "45.6 secs"),
    (62730.5, "17.4 hours"),
    (-1, "-1 secs"),
    (-90, "-1.5 mins"),
    (-5400, "-1.5 hours"),
    (-86400, "-1 days"),
    (1e8, "1157.4 days"),
    (1e9, "11574.1 days"),
    (1e10, "115740.7 days"),
    (1e12, "11574074 days"),
    (1e13, "115740741 days"),
    (1e15, "11574074074 days"),
    (0.5, "0.5 secs"),
    (0.05, "0 secs"),
    (0.049, "0 secs"),
    (59.95, "60 secs"),
    (59.949, "59.9 secs"),
    (3599.5, "60 mins"),
    (86399.5, "24 hours"),
    (2.5, "2.5 secs"),
    (7.25, "7.2 secs"),
    (7.35, "7.3 secs"),
    (7.45, "7.4 secs"),
    (7.55, "7.6 secs"),
    (63, "1 mins"),  # 1.05 mins: R measures a tie and rounds to even, Python's round() would say 1.1
]


@pytest.mark.parametrize(("seconds", "expected"), RUNTIME_TABLE, ids=[str(s) for s, _ in RUNTIME_TABLE])
def test_format_runtime_matches_r(seconds: float, expected: str) -> None:
    assert format_runtime(seconds) == expected


def test_format_runtime_accepts_int_and_missing() -> None:
    assert format_runtime(8304) == "2.3 hours"
    assert format_runtime(None) == "NA days"  # make_difftime(second = NA): na.omit() leaves "days"
    assert format_runtime(math.nan) == "NaN days"
    assert format_runtime(math.inf) == "Inf days"


# ---------------------------------------------------------------------------
# round and signif: R's arithmetic, not Python's
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("x", "digits", "expected"),
    [
        (1.05, 1, 1.0),
        (63 / 60, 1, 1.0),
        (0.15, 1, 0.1),
        (0.25, 1, 0.2),
        (0.35, 1, 0.3),
        (0.45, 1, 0.4),
        (2.5, 0, 2.0),
        (0.5, 0, 0.0),
        (1.5, 0, 2.0),
        (-1.5, 0, -2.0),
        (1.25, 1, 1.2),
        (2.675, 2, 2.67),
        (5.015, 2, 5.02),
        (12345 / 3600, 1, 3.4),
        (86399 / 3600, 1, 24.0),
        (1234.5678, -2, 1200.0),
    ],
)
def test_r_round_like_r(x: float, digits: int, expected: float) -> None:
    assert r_round(x, digits) == expected


def test_r_round_specials() -> None:
    assert math.isnan(r_round(math.nan, 1))
    assert r_round(math.inf, 1) == math.inf
    assert r_round(1.23456, math.inf) == 1.23456
    assert r_round(1.23456, -math.inf) == 0.0
    assert r_round(0.0, 1) == 0.0


@pytest.mark.parametrize(
    ("x", "digits", "expected"),
    [
        (0.35, 1, 0.4),  # 0.35 * 10 is the double 3.5, nearbyint gives 4
        (1.25, 1, 1.0),
        (2.5, 1, 2.0),
        (9.99, 1, 10.0),
        (0.05, 1, 0.05),
        (12.3, 2, 12.0),
        (120.4, 3, 120.0),
        (-2.5, 1, -2.0),
        (123456.789, 4, 123500.0),
        (0.0, 3, 0.0),
    ],
)
def test_r_signif_like_r(x: float, digits: int, expected: float) -> None:
    assert r_signif(x, digits) == expected


def test_r_signif_specials() -> None:
    assert math.isnan(r_signif(math.nan, 1))
    assert r_signif(math.inf, 1) == math.inf
    assert r_signif(1.23456789, math.inf) == 1.23456789
    assert r_signif(1.23456789, -math.inf) == 1.0  # digits -Inf is treated as 1
    assert r_signif(1.23456789, 0) == 1.0  # digits below 1 count as 1
    assert r_signif(1.23456789, 23) == 1.23456789  # above 22 the value is returned


# ---------------------------------------------------------------------------
# niceround
# ---------------------------------------------------------------------------

NICEROUND_TABLE: list[tuple[float, int, str]] = [
    (0.35, 1, "0.4"),
    (1.2, 1, "1"),
    (1.25, 1, "1"),
    (2.5, 1, "2"),
    (9.99, 1, "10"),
    (12.3, 1, "12"),
    (120.4, 1, "120"),
    (0.05, 1, "0.05"),
    (100, 1, "100"),
    (0, 1, "0"),
    (0.0049, 1, "0.005"),
    (1234.5678, 1, "1235"),
    (1234.5678, 3, "1235"),
    (0.001234, 3, "0.00123"),
    (99.99, 1, "100"),
    (999.95, 3, "1000"),
    (-2.5, 1, "-2"),
    (-0.35, 1, "-0.4"),
    (123456789.1, 1, "123456789"),
    (1e-7, 3, "0.0000001"),
    (0.000123456, 2, "0.00012"),
    (1e15, 3, "1000000000000000"),
    (1e20, 3, "100000000000000000000"),
    (123.456, 0, "123"),
    (5, 1, "5"),
    (0.5, 1, "0.5"),
    (0.95, 1, "1"),
    (1.05, 1, "1"),
]


@pytest.mark.parametrize(("x", "digits", "expected"), NICEROUND_TABLE, ids=[f"{x}-{d}" for x, d, _ in NICEROUND_TABLE])
def test_niceround_matches_r(x: float, digits: int, expected: str) -> None:
    assert niceround(x, digits) == expected


def test_niceround_default_digits_and_missing() -> None:
    assert niceround(3.14159) == "3.14"
    assert niceround(None) == "NA"
    assert niceround(math.nan) == "NaN"
    assert niceround(math.inf, 1) == "Inf"
    assert niceround(-math.inf, 1) == "-Inf"


# ---------------------------------------------------------------------------
# r_num_str (paste0 / as.character) and format_number (format)
# ---------------------------------------------------------------------------

AS_CHARACTER_TABLE: list[tuple[float, str]] = [
    (100000.0, "1e+05"),
    (123456.0, "123456"),
    (1e15, "1e+15"),
    (0.1, "0.1"),
    (0.0001, "1e-04"),
    (0.00012, "0.00012"),
    (100000.5, "100000.5"),
    (1 / 3, "0.333333333333333"),
    (2 / 3, "0.666666666666667"),
    (100000.1, "100000.1"),
    (123456789012345.0, "123456789012345"),
    (1234567890123456.0, "1234567890123456"),
    (0.1 + 0.2, "0.3"),
    (1e-15, "1e-15"),
    (123456.7, "123456.7"),
    (-100000.0, "-1e+05"),
    (-123456.0, "-123456"),
    (1e100, "1e+100"),
    (1e-100, "1e-100"),
    (1e22, "1e+22"),
    (99999.99999999999, "1e+05"),
    (1234567890123456789.0, "1234567890123456768"),
    (8304.0, "8304"),
    (12345.678, "12345.678"),
    (2.5, "2.5"),
    (0.0, "0"),
    (-0.0, "0"),
    (3e5, "3e+05"),
    (1e6, "1e+06"),
    (123400.0, "123400"),
    (1e-5, "1e-05"),
    (0.001, "0.001"),
    (123456789.12345679, "123456789.123457"),
    (10.0, "10"),
    (2.0**53, "9007199254740992"),
    (2.0**63, "9223372036854775808"),
    (5e-324, "4.94065645841247e-324"),
    (1.7976931348623157e308, "1.79769313486232e+308"),
    (999999999999999.0, "999999999999999"),
    (1000000000000001.0, "1e+15"),
    (100000000000000.5, "1e+14"),
    (0.00012345678901234567, "0.000123456789012346"),
    (12345678901234.5, "12345678901234.5"),
    (0.45, "0.45"),
]


@pytest.mark.parametrize(("x", "expected"), AS_CHARACTER_TABLE, ids=[repr(x) for x, _ in AS_CHARACTER_TABLE])
def test_r_num_str_double_matches_as_character(x: float, expected: str) -> None:
    assert r_num_str(x) == expected


def test_r_num_str_integers_and_specials() -> None:
    # R integers print plainly; a Python int beyond R's integer range is a double in R
    assert r_num_str(100000) == "100000"
    assert r_num_str(123456789) == "123456789"
    assert r_num_str(-5) == "-5"
    assert r_num_str(2**31 - 1) == "2147483647"
    assert r_num_str(2**31) == "2147483648"
    assert r_num_str(10**15) == "1e+15"
    assert r_num_str(True) == "TRUE"
    assert r_num_str(False) == "FALSE"
    assert r_num_str(None) == "NA"
    assert r_num_str(math.nan) == "NaN"
    assert r_num_str(math.inf) == "Inf"
    assert r_num_str(-math.inf) == "-Inf"
    with pytest.raises(TypeError):
        r_num_str("12")


FORMAT_TABLE: list[tuple[float, str]] = [
    (1 / 3, "0.3333333"),
    (2 / 3, "0.6666667"),
    (123456789012345.0, "1.234568e+14"),
    (12345.678, "12345.68"),
    (123456789.12345679, "123456789"),
    (2.0**53, "9.007199e+15"),
    (999999999999999.0, "1e+15"),
    (0.00012345678901234567, "0.0001234568"),
    (12345678901234.5, "1.234568e+13"),
    (1234567890123.45, "1.234568e+12"),
    (99999.9999, "1e+05"),
    (9.9999999, "10"),
    (999999.99, "1e+06"),
    (1234567.8, "1234568"),
    (0.1234567891, "0.1234568"),
    (3.4, "3.4"),
    (24.0, "24"),
    (115740.7, "115740.7"),
    (100000.0, "1e+05"),
    (123456.0, "123456"),
]


@pytest.mark.parametrize(("x", "expected"), FORMAT_TABLE, ids=[repr(x) for x, _ in FORMAT_TABLE])
def test_format_number_seven_digits_matches_format(x: float, expected: str) -> None:
    assert format_number(x) == expected


def test_format_number_scientific_false_penalty() -> None:
    # format(x, scientific = FALSE) measured on R 4.6.1: fixed up to a penalty of 310; the fixed text is the
    # exact binary expansion sprintf("%.0f") produces, as R prints it
    assert format_number(1e120, scipen=310) == (
        "999999999999999980003468347394201181668805192897008518188648311830772414627428725464789434929992439754776"
        "075181077037056"
    )
    assert format_number(1e-314, scipen=310).startswith("0.000")
    assert format_number(1e-315, scipen=310) == "1e-315"
    assert format_number(1e-314, scipen=309) == "1e-314"


# ---------------------------------------------------------------------------
# print_output
# ---------------------------------------------------------------------------


def test_print_output_len_cols_regime_pads_truncates_and_joins() -> None:
    # R: printOutput(data.frame(a = "x", b = "yy", row.names = "run"), lenCols = c(5, 3, 4), colSep = "|",
    #                 cols = c("a", "b"))
    assert print_output({"a": "x", "b": "yy"}, rowname="run", len_cols=[5, 3, 4], col_sep="|", cols=["a", "b"]) == (
        "run  |x  |yy  \n"
    )


def test_print_output_without_len_cols_uses_i_plus_12_and_no_separator() -> None:
    # R: printOutput(df, cols = c("a", "b")): rowname to 67, colSep, then a (14 wide) and b (13 wide)
    expected = "run" + " " * 64 + "   " + "x" + " " * 13 + "yy" + " " * 11 + "\n"
    assert print_output({"a": "x", "b": "yy"}, rowname="run", cols=["a", "b"]) == expected
    assert print_output({"a": "x", "b": "yy"}, rowname="run", len1stcol=4, cols=["a", "b"]) == (
        "run    x             yy           \n"
    )


def test_print_output_len_cols_first_entry_overrides_len1stcol() -> None:
    line = print_output(
        {"a": "x", "b": "yy"}, rowname="run", len1stcol=20, len_cols=[5, 3, 4], col_sep="|", cols=["a", "b"]
    )
    assert line == "run  |x  |yy  \n"


def test_print_output_single_width_falls_back_to_i_plus_12_with_separator() -> None:
    # lenCols = 5: every cell has i >= length(lenCols) and takes i + 12, colSep is still inserted
    assert print_output({"a": "x", "b": "yy"}, rowname="run", len_cols=[5], col_sep="|", cols=["a", "b"]) == (
        "run  |x             |yy           \n"
    )


def test_print_output_zero_and_fractional_widths() -> None:
    assert print_output({"a": "x", "b": "yy"}, rowname="run", len_cols=[5, 0, 0], col_sep="|", cols=["a", "b"]) == (
        "run  ||\n"
    )
    # rep() and substr() truncate a fractional width
    assert print_output(
        {"a": "x", "b": "yy"}, rowname="run", len_cols=[5.9, 2.7, 3.2], col_sep="|", cols=["a", "b"]
    ) == ("run  |x |yy \n")


def test_print_output_truncates_rowname_and_cells_by_characters() -> None:
    line = print_output(
        {"a": "Terminätéd ✓ ok"}, rowname="rün-äbcdefghij", len_cols=[6, 8, 8], col_sep="|", cols=["a", "a"]
    )
    assert line == "rün-äb|Terminät|Terminät\n"


def test_print_output_real_na_versus_string_na_and_numbers() -> None:
    row = {"a": None, "b": "NA", "c": math.nan, "d": 1e5, "e": 100000, "f": True, "g": 8304.0, "h": 12345.678}
    line = print_output(row, rowname="r", len_cols=[3, 4, 4, 4, 6, 7, 5, 6, 10], col_sep="|", cols=list("abcdefgh"))
    assert line == "r  |    |NA  |    |1e+05 |100000 |TRUE |8304  |12345.678 \n"


def test_print_output_default_columns_follow_on_cluster() -> None:
    local_row = {name: "v" for name in COLS_LOCAL}
    cluster_row = {name: "v" for name in COLS_ON_CLUSTER}
    local = print_output(local_row, rowname="r", effects=FakeEffects(on_cluster=False))
    cluster = print_output(cluster_row, rowname="r", effects=FakeEffects(on_cluster=True))
    assert local.count("v") == len(COLS_LOCAL) == 8
    assert cluster.count("v") == len(COLS_ON_CLUSTER) == 10
    # the cluster set needs jobInSLURM and runInAppResults: a local record fails on the cluster
    with pytest.raises(RParityError, match=r"^undefined columns selected$"):
        print_output(local_row, rowname="r", effects=FakeEffects(on_cluster=True))
    # and the probe is the only Effects member used
    effects = FakeEffects(on_cluster=False)
    print_output(local_row, rowname="r", effects=effects)
    assert [call.method for call in effects.calls] == ["on_cluster"]


def test_print_output_explicit_cols_need_no_effects() -> None:
    assert print_output({"RunStatus": "ok", "Conv": "converged"}, rowname="r", cols=["RunStatus", "Conv"]).startswith(
        "r "
    )


def test_print_output_errors_like_r() -> None:
    row = {"a": "x", "b": "yy"}
    with pytest.raises(RParityError, match=r"^undefined columns selected$"):
        print_output(row, rowname="run", cols=["a", "zz"])
    # [, cols] with one column drops to a vector: rownames() is NULL and is.na(NULL) has length zero
    with pytest.raises(RParityError, match=r"^argument is of length zero$"):
        print_output(row, rowname="run", cols=["a"])
    with pytest.raises(RParityError, match=r"^argument is of length zero$"):
        print_output(row, rowname="run", len_cols=[5, 3], cols=["a"])
    # fewer widths than columns: lenCols[ncol + 2 - i] is NA and rep(" ", NA) fails
    with pytest.raises(RParityError, match=r"^invalid 'times' argument$"):
        print_output(row, rowname="run", len_cols=[5, 3], cols=["a", "b"])
    with pytest.raises(RParityError, match=r"^invalid 'times' argument$"):
        print_output(row, rowname="run", len_cols=[5, -1, 4], cols=["a", "b"])
    with pytest.raises(TypeError):
        print_output({"a": object()}, rowname="run", len_cols=[3, 3, 3], cols=["a"] * 2)


def test_print_output_empty_row_and_empty_cols() -> None:
    assert print_output({}, rowname="run") == ""
    assert print_output({"a": "x"}, rowname="run", cols=[]) == "run" + " " * 64 + "   \n"


def test_print_output_duplicate_cols_and_long_rowname() -> None:
    # R: printOutput(data.frame(a = "x", row.names = "abcdefghij"), lenCols = c(4, 2, 2), cols = c("a", "a"))
    assert print_output({"a": "x"}, rowname="abcdefghij", len_cols=[4, 2, 2], cols=["a", "a"]) == "abcd   x    x \n"


# ---------------------------------------------------------------------------
# the printoutput golden cases, replayed directly (needs migration/)
# ---------------------------------------------------------------------------


def _printoutput_case_ids() -> list[str]:
    if not PRINTOUTPUT_CASES.is_dir():
        return []
    return sorted(path.stem for path in PRINTOUTPUT_CASES.glob("*.json"))


@pytest.mark.skipif(not PRINTOUTPUT_CASES.is_dir(), reason=f"{PRINTOUTPUT_CASES} is absent (migration/ not linked)")
@pytest.mark.parametrize("case_id", _printoutput_case_ids())
def test_printoutput_golden_case(case_id: str) -> None:
    spec = json.loads((PRINTOUTPUT_CASES / f"{case_id}.json").read_text(encoding="utf-8"))
    kwargs: dict[str, object] = {}
    for key, name in (("len1stcol", "len1stcol"), ("lenCols", "len_cols"), ("colSep", "col_sep"), ("cols", "cols")):
        if spec.get(key) is not None:
            kwargs[name] = spec[key]
    effects = FakeEffects(on_cluster=spec.get("mode", "oncluster") == "oncluster")
    error_file = PRINTOUTPUT_GOLDENS / f"{case_id}.error.json"
    out_file = PRINTOUTPUT_GOLDENS / f"{case_id}.out"
    if error_file.exists():
        expected_error = json.loads(error_file.read_text(encoding="utf-8"))["error"]
        with pytest.raises(RParityError) as info:
            print_output(spec["string"]["columns"], rowname=spec["string"]["rowname"], effects=effects, **kwargs)  # type: ignore[arg-type]
        assert str(info.value) == expected_error
    else:
        assert out_file.exists(), f"golden {out_file} missing"
        got = print_output(spec["string"]["columns"], rowname=spec["string"]["rowname"], effects=effects, **kwargs)  # type: ignore[arg-type]
        assert got.encode("utf-8") == out_file.read_bytes()


# ---------------------------------------------------------------------------
# live Rscript oracle (skipped without R, lubridate and piamutils)
# ---------------------------------------------------------------------------


def _rscript(code: str) -> list[str] | None:
    """Lines printed by ``Rscript -e code``, or ``None`` when R or a package is missing."""
    if shutil.which("Rscript") is None:
        return None
    try:
        proc = subprocess.run(["Rscript", "-e", code], capture_output=True, text=True, timeout=120, check=False)
    except OSError, subprocess.TimeoutExpired:
        return None
    if proc.returncode != 0:
        return None
    return proc.stdout.splitlines()


def test_format_runtime_against_live_rscript() -> None:
    values = [s for s, _ in RUNTIME_TABLE]
    code = (
        "suppressMessages(library(lubridate)); "
        f"for (s in c({', '.join(repr(float(v)) for v in values)})) "
        "cat(format(round(make_difftime(second = s), 1)), sep = '\\n')"
    )
    lines = _rscript(code)
    if lines is None:
        pytest.skip("Rscript with lubridate is not available")
    assert lines == [format_runtime(v) for v in values]


def test_niceround_against_live_rscript() -> None:
    code = "; ".join(f"cat(piamutils::niceround({x!r}, {d}), sep = '\\n')" for x, d, _ in NICEROUND_TABLE)
    lines = _rscript(code)
    if lines is None:
        pytest.skip("Rscript with piamutils is not available")
    assert lines == [niceround(x, d) for x, d, _ in NICEROUND_TABLE]
