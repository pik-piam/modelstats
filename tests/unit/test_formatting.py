"""``modelstats.formatting`` reproduces ``R/printOutput.R`` and the runtime display of ``R/loopRuns.R``.

Every expected value below was produced by R 4.6.1 (lubridate 1.9.5, piamutils) on 2026-10-01 with
``printOutput()`` sourced from ``R/printOutput.R``, ``format(round(make_difftime(second = s), 1))``,
``piamutils::niceround``, ``as.character`` / ``format`` on doubles, and ``round`` / ``signif``; the
goldens under ``migration/goldens/printoutput`` are the byte-level contract
(``tests/golden/test_printoutput.py``), these tests pin the edge cases the goldens do not reach.
"""

from __future__ import annotations

import math
from collections.abc import Mapping
from dataclasses import dataclass

import pytest

from modelstats.formatting import (
    ARGUMENT_OF_LENGTH_ZERO,
    INVALID_TIMES_ARGUMENT,
    LOCAL_COLUMNS,
    ONCLUSTER_COLUMNS,
    UNDEFINED_COLUMNS,
    CellValue,
    ColumnSpec,
    PrintOutputError,
    Row,
    StatusRow,
    column_specs,
    default_columns,
    format_cell,
    format_runtime,
    is_na,
    mark_active,
    niceround,
    print_output,
    r_as_character,
    r_format_number,
    r_round,
    r_signif,
    runtime_display,
)

NAN = float("nan")
INF = float("inf")

# The one-row data frame of the R probe: RunStatus, Conv, Runtime.
PROBE = Row("run", {"RunStatus": "Normal completion", "Conv": "converged", "Runtime": 8304})
FOLDER = "run" + " " * 64  # len1stcol 67


def line(*parts: str) -> str:
    return "".join(parts) + "\n"


# --- column_specs: R/printOutput.R:45-46 -------------------------------------------------------------


def widths(cols: list[str], len_cols: list[int] | None) -> list[int | None]:
    return [spec.width for spec in column_specs(cols, len_cols)]


def test_without_lencols_widths_count_down_from_ncol_plus_12() -> None:
    assert widths(list(ONCLUSTER_COLUMNS), None) == [22, 21, 20, 19, 18, 17, 16, 15, 14, 13]
    assert widths(list(LOCAL_COLUMNS), None) == [20, 19, 18, 17, 16, 15, 14, 13]
    assert widths(["RunStatus", "Conv"], None) == [14, 13]


def test_lencols_of_ncol_plus_one_maps_j_plus_one_to_column_j() -> None:
    len_cols = [67, 11, 8, 11, 17, 9, 16, 21, 21, 6, 3]
    assert widths(list(ONCLUSTER_COLUMNS), len_cols) == len_cols[1:]
    assert column_specs(["Runtime"], [67, 11])[0] == ColumnSpec("Runtime", 11)


def test_short_lencols_fall_back_to_i_plus_12_or_na() -> None:
    # i counts from the last column: i >= length(lenCols) -> i + 12, else lenCols[ncol + 2 - i].
    assert widths(["RunStatus", "Conv"], [5]) == [14, 13]
    assert widths(["RunStatus", "Conv"], [5, 4]) == [14, None]
    assert widths(["RunStatus", "Conv"], [5, 4, 3]) == [4, 3]
    assert widths(["RunStatus", "Conv"], [5, 4, 3, 2]) == [4, 3]
    assert widths(["RunStatus", "Conv", "Runtime"], [5, 4, 3]) == [15, 3, None]
    assert widths(["RunStatus", "Conv"], []) == [14, 13]


def test_default_columns_follow_the_cluster_probe() -> None:
    assert default_columns(True) == ONCLUSTER_COLUMNS
    assert default_columns(False) == LOCAL_COLUMNS
    assert "jobInSLURM" not in LOCAL_COLUMNS
    assert "runInAppResults" not in LOCAL_COLUMNS


# --- format_cell: formatstr of R/printOutput.R:17-20 -------------------------------------------------


@pytest.mark.parametrize(
    ("value", "width", "expected"),
    [
        ("converged", 13, "converged    "),
        ("Normal completion", 14, "Normal complet"),
        ("NA", 5, "NA   "),
        (None, 5, "     "),
        (NAN, 4, "    "),
        (INF, 5, "Inf  "),
        (-INF, 5, "-Inf "),
        (True, 6, "TRUE  "),
        (False, 6, "FALSE "),
        (8304, 6, "8304  "),
        (100000, 7, "100000 "),
        (100000.0, 7, "1e+05  "),
        (123456.0, 7, "123456 "),
        (1e15, 6, "1e+15 "),
        (12345.678, 10, "12345.678 "),
        ("Terminätéd ✓ ok", 17, "Terminätéd ✓ ok  "),
        ("rün-ä", 3, "rün"),
        ("x", 0, ""),
    ],
)
def test_format_cell_pads_cuts_and_pastes_like_r(value: CellValue, width: int, expected: str) -> None:
    assert format_cell(value, width) == expected


@pytest.mark.parametrize("width", [None, -1])
@pytest.mark.parametrize("value", ["x", None])
def test_na_or_negative_width_is_the_rep_error_whatever_the_value(
    value: CellValue, width: int | None
) -> None:
    with pytest.raises(PrintOutputError, match=INVALID_TIMES_ARGUMENT):
        format_cell(value, width)


def test_is_na_is_none_or_nan_only() -> None:
    assert is_na(None) and is_na(NAN)
    assert not is_na("NA") and not is_na(0) and not is_na(INF) and not is_na("")


# --- print_output: R/printOutput.R:14-50 (R probe of 2026-10-01) ------------------------------------


def test_two_columns_without_lencols_have_no_separator_between_cells() -> None:
    assert print_output(PROBE, cols=["RunStatus", "Conv"]) == line(
        FOLDER, "   ", "Normal complet", "converged    "
    )


def test_single_column_is_the_rownames_null_error() -> None:
    # string[, "Conv"] drops to a vector; if (is.na(rownames(vector))) is if (logical(0)).
    with pytest.raises(PrintOutputError, match=ARGUMENT_OF_LENGTH_ZERO):
        print_output(PROBE, cols=["Conv"])


def test_empty_cols_prints_the_folder_and_the_separator_only() -> None:
    assert print_output(PROBE, cols=[]) == line(FOLDER, "   ")


def test_empty_lencols_is_not_null_and_keeps_the_separator() -> None:
    expected = line(FOLDER, "   ", "Normal complet", "   ", "converged    ")
    assert print_output(PROBE, cols=["RunStatus", "Conv"], len_cols=[]) == expected
    assert print_output(PROBE, cols=["RunStatus", "Conv"], len_cols=None) != expected


def test_lencols_first_entry_is_the_folder_width_and_the_rest_the_cell_widths() -> None:
    assert print_output(PROBE, cols=["RunStatus", "Conv"], len_cols=[5]) == line(
        "run  ", "   ", "Normal complet", "   ", "converged    "
    )
    assert print_output(PROBE, cols=["RunStatus", "Conv"], len_cols=[5, 4, 3]) == line(
        "run  ", "   ", "Norm", "   ", "con"
    )
    assert print_output(PROBE, cols=["RunStatus", "Conv"], len_cols=[5, 4, 3, 2]) == line(
        "run  ", "   ", "Norm", "   ", "con"
    )
    assert print_output(PROBE, cols=["RunStatus", "Conv"], len_cols=[5, 0, 4]) == line(
        "run  ", "   ", "", "   ", "conv"
    )


@pytest.mark.parametrize(
    ("cols", "len_cols"),
    [
        (["RunStatus", "Conv"], [5, 4]),
        (["RunStatus", "Conv", "Runtime"], [5, 4, 3]),
        (["RunStatus", "Conv", "Runtime"], [5, 4]),
        (["RunStatus", "Conv"], [5, -1, 4]),
    ],
)
def test_lencols_too_short_or_negative_is_the_rep_error(cols: list[str], len_cols: list[int]) -> None:
    with pytest.raises(PrintOutputError, match=INVALID_TIMES_ARGUMENT):
        print_output(PROBE, cols=cols, len_cols=len_cols)


def test_len1stcol_is_overridden_by_lencols_and_may_be_zero() -> None:
    assert print_output(PROBE, cols=["RunStatus", "Conv"], len1stcol=0) == line(
        "", "   ", "Normal complet", "converged    "
    )
    assert print_output(PROBE, cols=["RunStatus", "Conv"], len1stcol=2, len_cols=[5, 4, 3]).startswith(
        "run  "
    )
    assert print_output(PROBE, cols=["RunStatus", "Conv"], len1stcol=2).startswith("ru   ")


def test_absent_column_is_undefined_columns_before_any_width_is_checked() -> None:
    with pytest.raises(PrintOutputError, match=UNDEFINED_COLUMNS):
        print_output(PROBE, cols=["RunStatus", "Nope"], len_cols=[5, 4])
    with pytest.raises(PrintOutputError, match=UNDEFINED_COLUMNS):
        print_output(PROBE)  # the local default set needs eight columns
    with pytest.raises(PrintOutputError, match=UNDEFINED_COLUMNS):
        print_output(PROBE, on_cluster=True)


def test_record_without_cells_is_the_empty_string_before_columns_are_looked_up() -> None:
    assert print_output(Row("run", {})) == ""
    assert print_output(Row("run", {}), cols=["Nope"], len_cols=[-1]) == ""


def test_duplicate_columns_print_twice() -> None:
    assert print_output(PROBE, cols=["Conv", "Conv"]) == line(
        FOLDER, "   ", "converged     ", "converged    "
    )


def test_real_na_is_blank_and_the_string_na_prints() -> None:
    row = Row("NA", {"a": "NA", "b": None, "c": NAN})
    assert print_output(row, cols=["a", "b", "c"]) == line(
        "NA" + " " * 65, "   ", "NA" + " " * 13, " " * 14, " " * 13
    )


def test_numbers_and_logicals_paste_like_r() -> None:
    row = Row("r", {"a": 100000, "b": 100000.0, "c": 123456.0, "d": 1e15, "e": True, "f": INF})
    assert print_output(row, cols=["a", "b", "c", "d", "e", "f"], len_cols=[1, 7, 7, 7, 6, 5, 5]) == line(
        "r",
        "   ",
        "100000 ",
        "   ",
        "1e+05  ",
        "   ",
        "123456 ",
        "   ",
        "1e+15 ",
        "   ",
        "TRUE ",
        "   ",
        "Inf  ",
    )


def test_default_columns_on_cluster_and_locally() -> None:
    cells: dict[str, CellValue] = dict.fromkeys(ONCLUSTER_COLUMNS, "x")
    on_cluster = print_output(Row("r", cells), on_cluster=True)
    local = print_output(Row("r", cells), on_cluster=False)
    assert len(on_cluster) == 67 + 3 + sum(range(13, 23)) + 1
    assert len(local) == 67 + 3 + sum(range(13, 21)) + 1


@dataclass(frozen=True)
class RecordLike:
    """A phase-2 style record: ``run`` and ``cells`` as properties."""

    name: str
    values: Mapping[str, CellValue]

    @property
    def run(self) -> str:
        return self.name

    @property
    def cells(self) -> Mapping[str, CellValue]:
        return self.values


def test_any_object_with_run_and_cells_is_a_status_row() -> None:
    record = RecordLike("r", {"RunStatus": "Normal completion", "Conv": "converged"})
    assert isinstance(record, StatusRow)
    assert isinstance(PROBE, StatusRow)
    assert print_output(record, cols=["RunStatus", "Conv"]) == print_output(
        Row("r", record.values), cols=["RunStatus", "Conv"]
    )


# --- the Runtime column: R/loopRuns.R:66-79 --------------------------------------------------------


@pytest.mark.parametrize(
    ("seconds", "expected"),
    [
        (0, "0 secs"),
        (1, "1 secs"),
        (59, "59 secs"),
        (60, "1 mins"),
        (61, "1 mins"),
        (3599, "60 mins"),
        (3600, "1 hours"),
        (5400, "1.5 hours"),
        (86399, "24 hours"),
        (86400, "1 days"),
        (90000, "1 days"),
        (8.64e6, "100 days"),
        (3, "3 secs"),
        (45.6, "45.6 secs"),
        (59.99, "60 secs"),
        (125.4, "2.1 mins"),
        (3661, "1 hours"),
        (12345, "3.4 hours"),
        (62730.5, "17.4 hours"),
        (604800, "7 days"),
        (2592000, "30 days"),
        (2013, "33.5 mins"),
        (8304, "2.3 hours"),
        (75, "1.2 mins"),  # 1.25: an exact tie, rounded to even
        (105, "1.8 mins"),
        (4500, "1.2 hours"),
        (108000, "1.2 days"),
        (0.45, "0.4 secs"),  # R's round(0.45, 1); Python's round gives 0.5
        (0.05, "0 secs"),
        (0.06, "0.1 secs"),
        (59.05, "59 secs"),
        (59.95, "60 secs"),
        (-75, "-1.2 mins"),
        (1e9, "11574.1 days"),
        (8.64e9, "1e+05 days"),
        (1e13, "115740741 days"),
    ],
)
def test_format_runtime_matches_lubridate_and_format_difftime(seconds: float, expected: str) -> None:
    assert format_runtime(seconds) == expected


def test_format_runtime_of_missing_values_falls_into_days() -> None:
    assert format_runtime(None) == "NA days"
    assert format_runtime(NAN) == "NaN days"
    assert format_runtime(INF) == "Inf days"
    assert format_runtime(-INF) == "-Inf days"


def test_mark_active_pads_short_texts() -> None:
    assert mark_active("1.5 hours") == "> 1.5 hours"
    assert mark_active("12.3 hours") == ">12.3 hours"
    assert mark_active("1234 hours") == ">1234 hours"


@pytest.mark.parametrize(
    ("runtime", "job", "expected"),
    [
        (None, "pascalfu priority pending", "pending"),
        (8304, "pascalfu standby pending", "pending"),
        (8304, "no", "2.3 hours"),
        (8304, "NA", "> 2.3 hours"),  # the local "NA" is not "no": marked
        (8304, "pascalfu priority", "> 2.3 hours"),
        (77760, "pascalfu priority", ">21.6 hours"),
        (120, "pascalfu priority startup", "> 2 mins"),  # numeric runtime wins over startup
        (None, "pascalfu priority startup", "startup"),
        (NAN, "pascalfu priority startup", "startup"),
        (None, "no", "NA"),
        (NAN, "NA", "NA"),
        (None, "otheruser", "NA"),
    ],
)
def test_runtime_display_precedence(runtime: float | None, job: str, expected: str) -> None:
    assert runtime_display(runtime, job) == expected


# --- niceround: R/getRunStatus.R:184 ----------------------------------------------------------------


@pytest.mark.parametrize(
    ("x", "digits", "expected"),
    [
        (0.2, 1, "0.2"),
        (0.3, 1, "0.3"),
        (0.5, 1, "0.5"),
        (1.1, 1, "1"),
        (1.5, 1, "2"),
        (2.5, 1, "2"),
        (3.5, 1, "4"),
        (9.5, 1, "10"),
        (12.3, 1, "12"),
        (12.5, 1, "12"),
        (13.5, 1, "14"),
        (99.5, 1, "100"),
        (123.4, 1, "123"),
        (1234.5, 1, "1234"),
        (0.15, 1, "0.2"),  # signif's double scaling: 0.15 * 10 is exactly 1.5
        (0.35, 1, "0.4"),
        (0.05, 1, "0.05"),
        (1e-5, 1, "0.00001"),
        (123456.7, 1, "123457"),
        (1e7, 1, "10000000"),
        (12345678.9, 1, "12345679"),
        (0, 1, "0"),
        (-0.2, 1, "-0.2"),
        (-1.5, 1, "-2"),
        (1.1, 3, "1.1"),
        (2.675, 3, "2.68"),
        (0.12345, 3, "0.123"),
        (4.35, 2, "4.4"),
        (1.25, 2, "1.2"),
        (10.5, 2, "10"),
    ],
)
def test_niceround_matches_piamutils(x: float, digits: int, expected: str) -> None:
    assert niceround(x, digits) == expected


def test_niceround_of_missing_values() -> None:
    assert niceround(None, 1) == "NA"
    assert niceround(NAN, 1) == "NaN"
    assert niceround(INF, 1) == "Inf"


# --- R's numbers as text ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("value", "expected"),
    [
        (8304.0, "8304"),
        (12345.678, "12345.678"),
        (1e15, "1e+15"),
        (100000.0, "1e+05"),
        (123456.0, "123456"),
        (110000.0, "110000"),
        (300000.0, "3e+05"),
        (0.1, "0.1"),
        (0.0001, "1e-04"),
        (0.00011, "0.00011"),
        (1 / 3, "0.333333333333333"),
        (2 / 3, "0.666666666666667"),
        (0.1 + 0.2, "0.3"),
        (1234567890123456.0, "1234567890123456"),
        (1e15 + 1, "1e+15"),
        (float(2**60), "1152921504606846976"),
        (6.02214076e23, "6.02214076e+23"),
        (1.602176634e-19, "1.602176634e-19"),
        (1e100, "1e+100"),
        (-8304.0, "-8304"),
        (-0.5, "-0.5"),
        (999999.9999999999, "1e+06"),
        (100000.5, "100000.5"),
        (1234567.8, "1234567.8"),
        (3.14159265358979, "3.14159265358979"),
        (0.0, "0"),
        (-0.0, "0"),
        (NAN, "NaN"),
        (INF, "Inf"),
        (-INF, "-Inf"),
        (100000, "100000"),
        (True, "TRUE"),
        (False, "FALSE"),
        (None, "NA"),
        ("NA", "NA"),
        ("text", "text"),
    ],
)
def test_r_as_character(value: CellValue, expected: str) -> None:
    assert r_as_character(value) == expected


@pytest.mark.parametrize(
    ("value", "expected"),
    [
        (12345.678, "12345.68"),
        (1234567890123456.0, "1.234568e+15"),
        (1 / 3, "0.3333333"),
        (1234.5678, "1234.568"),
        (99999.99999, "1e+05"),
        (float(2**53), "9.007199e+15"),
        (123456.7, "123456.7"),
        (1234567.8, "1234568"),
        (1e23, "1e+23"),
        (35.4, "35.4"),
        (24.0, "24"),
        (1.5, "1.5"),
    ],
)
def test_r_format_number_with_seven_digits(value: float, expected: str) -> None:
    assert r_format_number(value) == expected


def test_r_format_number_forced_notation() -> None:
    assert r_format_number(1e15, scientific=False) == "1000000000000000"
    assert r_format_number(1e-5, scientific=False) == "0.00001"
    assert r_format_number(1e-300, scientific=False) == "0." + "0" * 299 + "1"
    # R's width bookkeeping leaves one blank in front of 1e23 (the fuzz is absorbed in double arithmetic).
    assert r_format_number(1e23, scientific=False) == " 99999999999999991611392"
    assert r_format_number(123.0, scientific=True) == "1.23e+02"
    assert r_format_number(0.5, scientific=True) == "5e-01"


@pytest.mark.parametrize(
    ("x", "digits", "expected"),
    [
        (0.45, 1, 0.4),
        (1.45, 1, 1.4),
        (0.15, 1, 0.1),
        (0.35, 1, 0.3),
        (2.675, 2, 2.67),
        (1.25, 1, 1.2),
        (1.35, 1, 1.4),
        (59.95, 1, 60.0),
        (33.55, 1, 33.5),
        (2.5, 0, 2.0),
        (3.5, 0, 4.0),
        (-1.25, 1, -1.2),
        (-0.45, 1, -0.4),
        (1234.5, -2, 1200.0),
        (0.1234567890123456, 20, 0.1234567890123456),
    ],
)
def test_r_round_is_the_r_4_algorithm(x: float, digits: int, expected: float) -> None:
    assert r_round(x, digits) == expected


def test_r_round_differs_from_python_round_on_binary_near_ties() -> None:
    assert round(0.45, 1) == 0.5
    assert r_round(0.45, 1) == 0.4


@pytest.mark.parametrize(
    ("x", "digits", "expected"),
    [
        (0.35, 1, 0.4),
        (0.15, 1, 0.2),
        (0.25, 1, 0.2),
        (2.5, 1, 2.0),
        (12.5, 2, 12.0),
        (1.25, 2, 1.2),
        (2.675, 3, 2.68),
        (4.35, 2, 4.4),
        (0.12345, 3, 0.123),
        (123456.7, 6, 123457.0),
        (-1.5, 1, -2.0),
        (0.0, 1, 0.0),
    ],
)
def test_r_signif_is_fprec(x: float, digits: int, expected: float) -> None:
    assert r_signif(x, digits) == expected


def test_non_finite_values_pass_through_round_and_signif() -> None:
    assert math.isnan(r_round(NAN, 1)) and math.isnan(r_signif(NAN, 1))
    assert r_round(INF, 1) == INF and r_signif(-INF, 1) == -INF
