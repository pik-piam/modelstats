from __future__ import annotations

from dataclasses import dataclass

import pytest

from modelstats import _shell
from modelstats.print_output import CLUSTER_COLUMNS, LOCAL_COLUMNS, default_columns, format_row


@dataclass
class Row:
    name: str = "SSP2-NPi-AMT_2026-10-18_10.00.00"
    runtime: object = "3.4 hours"
    job_in_slurm: object = "no"
    run_type: object = "nash"
    run_status: object = "Normal completion"
    warnings: object = "0"
    iter: object = "12/50"
    conv: object = "converged"
    modelstat: object = "2: Locally Optimal"
    mif: object = "yes"
    run_in_app_results: object = "yes"
    summation_errors: object = 0
    range_errors: object = 2
    fix_errors: object = None


WIDTHS = [15, 11, 8, 11, 17, 9, 16, 21, 21, 6, 3]


def test_cluster_row_layout() -> None:
    line = format_row(Row(), WIDTHS, col_sep="  ", cols=CLUSTER_COLUMNS)
    assert line == (
        "SSP2-NPi-AMT_20"
        "  3.4 hours  "
        "  no      "
        "  nash       "
        "  Normal completion"
        "  0        "
        "  12/50           "
        "  converged            "
        "  2: Locally Optimal   "
        "  yes   "
        "  yes"
    )


def test_cells_are_padded_or_cut_and_na_is_blank() -> None:
    row = Row(name="r", runtime=None, run_type="a very long run type", run_status="NA")
    line = format_row(row, [3, 5, 4, 6], col_sep="|", cols=["runtime", "run_type", "run_status"])
    assert line == "r  |     |a ve|NA    "


def test_numbers_print_like_r() -> None:
    row = Row(name="r", summation_errors=3, range_errors=0, fix_errors=None, runtime=12345.0)
    cols = ["summation_errors", "range_errors", "fix_errors", "runtime"]
    assert format_row(row, [1, 2, 2, 2, 6], col_sep=" ", cols=cols) == "r 3  0     12345 "


def test_default_columns_depend_on_the_cluster(monkeypatch: pytest.MonkeyPatch) -> None:
    assert default_columns(True) == list(CLUSTER_COLUMNS)
    assert default_columns(False) == list(LOCAL_COLUMNS)
    monkeypatch.setattr(_shell, "on_cluster", lambda: False)
    line = format_row(Row(name="r"), [1, 2, 2, 2, 2, 2, 2, 2, 2], col_sep="|")
    assert line == "r|3.|na|No|0 |12|co|2:|ye"
    monkeypatch.setattr(_shell, "on_cluster", lambda: True)
    assert format_row(Row(name="r"), [1] + [2] * 10, col_sep="|") == "r|3.|no|na|No|0 |12|co|2:|ye|ye"


def test_partial_or_negative_widths_raise() -> None:
    row = Row(name="r")
    with pytest.raises(ValueError, match="2 widths"):
        format_row(row, [1, 5], col_sep="|", cols=["runtime", "run_type", "run_status"])
    with pytest.raises(ValueError, match="negative"):
        format_row(row, [1, -1], col_sep="|", cols=["runtime"])


def test_extra_widths_are_ignored() -> None:
    assert (
        format_row(Row(name="r"), [1, 2, 2, 99, 99], col_sep="|", cols=["runtime", "run_type"]) == "r|3.|na"
    )


def test_cells_override_the_attributes() -> None:
    row = Row(name="r", runtime=12345.0, job_in_slurm="standby pending")
    line = format_row(
        row,
        [1, 9, 7],
        col_sep="|",
        cols=["runtime", "job_in_slurm"],
        cells={"runtime": "pending", "job_in_slurm": "standby"},
    )
    assert line == "r|pending  |standby"


def test_single_column_zero_width_and_no_columns() -> None:
    assert format_row(Row(name="r"), [1, 4], col_sep="|", cols=["conv"]) == "r|conv"
    assert format_row(Row(name="r"), [1, 0, 2], col_sep="|", cols=["job_in_slurm", "mif"]) == "r||ye"
    assert format_row(Row(name="r"), [1], col_sep="|", cols=[]) == "r|"


def test_nan_bool_and_number_cells() -> None:
    row = Row(name="r", runtime=float("nan"), warnings=True, summation_errors=100000, range_errors=1e5)
    line = format_row(
        row, [1, 3, 5, 6, 5], col_sep="|", cols=["runtime", "warnings", "summation_errors", "range_errors"]
    )
    assert line == "r|   |TRUE |100000|1e+05"


def test_default_separator_is_three_spaces() -> None:
    assert format_row(Row(name="r"), [1, 2], cols=["mif"]) == "r   ye"


def test_non_ascii_values_are_cut_by_character() -> None:
    assert (
        format_row(Row(name="Ärger", run_status="Ärger"), [3, 2], col_sep="|", cols=["run_status"])
        == "Ärg|Är"
    )


def test_loop_runs_title_widths() -> None:
    titles = [
        "Folder" + " " * 9,
        "Runtime    ",
        "inSlurm ",
        "RunType    ",
        "RunStatus        ",
        "Warnings ",
        "Iter            ",
        "Conv                 ",
        "modelstat            ",
        "Mif   ",
        "AppResults",
    ]
    widths = [len(t) for t in titles[:-1]] + [3]
    line = format_row(Row(), widths, col_sep="  ", cols=CLUSTER_COLUMNS)
    assert len(line) == sum(widths) + 2 * (len(widths) - 1)
    assert line.endswith("  yes   " + "  yes")


def test_single_width_keeps_fallback_widths_with_separators() -> None:
    row = Row(name="r", runtime="a" * 20, run_type="b" * 20)
    line = format_row(row, [3], col_sep=" | ", cols=["runtime", "run_type"])
    assert line == "r   | " + "a" * 14 + " | " + "b" * 13


def test_no_widths_means_no_separators() -> None:
    row = Row(name="n" * 70, runtime="a", run_type="b")
    line = format_row(row, [], col_sep="|", cols=["runtime", "run_type"])
    assert line == "n" * 67 + "|" + "a" + " " * 13 + "b" + " " * 12
