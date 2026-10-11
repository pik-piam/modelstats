"""Unit tests of the pure README builder ``modelstats.amt.readme``.

``R/modeltests.R:202-222, 249-256, 276-278, 367-378, 421-430``.

Synthetic inputs only; the R goldens are compared in ``tests/golden/test_amt_readme.py``.
"""

from __future__ import annotations

from dataclasses import replace
from datetime import date
from typing import Any

import pytest

from modelstats.amt.readme import (
    APP_RESULTS_WIDTH,
    BLANK_LINE,
    CODE_FENCE,
    COLUMN_SEPARATOR,
    COLUMN_TITLES,
    COLUMN_WIDTHS,
    LOOK_GOOD,
    NOT_STARTED_TITLE,
    README_COLUMNS,
    ReadmeReport,
    git_info_lines,
    header_line,
    intro_lines,
    not_started_lines,
    readme_lines,
    render_readme,
    status_line,
    summary_line,
)
from modelstats.formatting import ONCLUSTER_COLUMNS, UNDEFINED_COLUMNS, PrintOutputError, Row
from modelstats.run_status import RunStatus, SanityCounts

TODAY = date(2026, 9, 30)
TESTED = "3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f"
MERGES = [
    "3f5e2a1 Merge pull request #999 from example/feature",
    "1a2b3c4 Merge pull request #998 from example/bugfix",
]
HEADER = (
    "Run                                             Runtime        RunType      RunStatus           "
    "Warnings   Iter              Conv                   modelstat            Mif     AppResults"
)
INTRO = [
    CODE_FENCE,
    "This is the result of the automated model tests for REMIND on 2026-09-30.",
    "Path to runs: /p/projects/remind/modeltests/remind/output/",
    "Direct and interactive access to plots: open shinyResults::appResults, "
    "then use 'AMT' as keyword in the title search",
    "Each run folder below should contain a compareScenarios PDF comparing the output of the current and"
    " the last successful tests (comp_with_RUN-DATE.pdf)",
    "Note: 'Mif' = 'no' indicates a possible error in output generation, please check!",
    "If you are currently viewing the email: Overview of the last test is in red, "
    "and of the current test in green",
]

#: A completed, converged REMIND run as ``getRunStatus(i)`` reports it on the cluster.
FINISHED_RUN = RunStatus(
    run="SSP2-NPi-AMT_2026-09-28_10.30.27",
    path="/p/projects/remind/modeltests/remind/output/SSP2-NPi-AMT_2026-09-28_10.30.27",
    jobInSLURM="no",
    RunType="nash",
    modelstat="2: Locally Optimal",
    runInAppResults="yes",
    Mif="yes",
    Iter="26/100",
    RunStatus="Normal completion",
    detailed=True,
    Warnings="27",
    Conv="converged",
    Runtime=8304,
    sanity=SanityCounts(0, 0, 0, 2, 32, 0),
)

REPORT = ReadmeReport(
    today=TODAY,
    runs_path="/p/projects/remind/modeltests/remind/output/",
    compare_scenarios=True,
    git_info=git_info_lines(TESTED, TODAY, MERGES),
    runs=[FINISHED_RUN],
    runs_not_started=["SSP2-NDC-AMT"],
    summary=LOOK_GOOD,
)


def finished_run(**overrides: Any) -> RunStatus:
    """:data:`FINISHED_RUN` with some fields replaced."""
    return replace(FINISHED_RUN, **overrides)


def report(**overrides: Any) -> ReadmeReport:
    """:data:`REPORT` with some fields replaced."""
    return replace(REPORT, **overrides)


def cell(line: str, index: int) -> str:
    """The text of column ``index`` of a row (0 is the run name): widths and separators of the title line."""
    start = sum(COLUMN_WIDTHS[:index]) + len(COLUMN_SEPARATOR) * index
    return line[start : start + COLUMN_WIDTHS[index]]


# --- titles and widths ----------------------------------------------------------------------------------


def test_the_title_line_is_the_r_paste_of_the_titles() -> None:
    assert header_line() == HEADER
    assert header_line() == COLUMN_SEPARATOR.join(COLUMN_TITLES)


def test_the_widths_are_the_title_lengths_but_three_for_the_last_column() -> None:
    assert COLUMN_WIDTHS == (46, 11, 0, 11, 18, 9, 16, 21, 19, 6, 3)
    assert COLUMN_WIDTHS[-1] == APP_RESULTS_WIDTH
    assert len(COLUMN_WIDTHS) == len(COLUMN_TITLES) == len(README_COLUMNS) + 1


def test_the_row_columns_are_print_outputs_cluster_default() -> None:
    assert README_COLUMNS == ONCLUSTER_COLUMNS
    assert README_COLUMNS[1] == "jobInSLURM" and COLUMN_WIDTHS[2] == 0


# --- one row -------------------------------------------------------------------------------------------


def test_a_finished_run_prints_its_runtime_in_hours_and_cuts_app_results() -> None:
    line = status_line(finished_run())
    assert line == (
        "SSP2-NPi-AMT_2026-09-28_10.30.27                2.3 hours      nash         Normal completion   "
        "27         26/100            converged              2: Locally Optimal   yes     yes"
    )
    assert "\n" not in line
    # The last title, AppResults, is ten characters; the last cell is three (R/modeltests.R:256).
    assert len(line) == len(HEADER) - (len(COLUMN_TITLES[-1]) - APP_RESULTS_WIDTH)
    assert cell(line, 0) == "SSP2-NPi-AMT_2026-09-28_10.30.27" + " " * 14
    assert cell(line, 1) == "2.3 hours  "
    assert cell(line, 10) == "yes"


def test_the_job_column_is_empty_and_app_results_no_keeps_a_trailing_blank() -> None:
    line = status_line(finished_run(jobInSLURM="12345 standby", runInAppResults="no"))
    assert "12345" not in line
    assert line.endswith("no ")
    assert cell(line, 2) == ""
    assert cell(line, 10) == "no "


@pytest.mark.parametrize(
    ("seconds", "text"),
    [(45, "45 secs"), (1223, "20.4 mins"), (12213, "3.4 hours"), (77679, "21.6 hours"), (200000, "2.3 days")],
)
def test_the_runtime_follows_make_difftime(seconds: int, text: str) -> None:
    assert cell(status_line(finished_run(Runtime=seconds)), 1) == f"{text:<11}"


def test_a_double_runtime_is_numeric_too() -> None:
    row = Row("x", {**FINISHED_RUN.cells, "Runtime": 8304.0})
    assert cell(status_line(row), 1) == "2.3 hours  "


def test_a_missing_runtime_is_blanks_not_na_days() -> None:
    assert cell(status_line(finished_run(Runtime=None)), 1) == " " * 11


def test_a_logical_runtime_is_not_numeric() -> None:
    row = Row("x", {**FINISHED_RUN.cells, "Runtime": True})
    assert cell(status_line(row), 1) == "TRUE       "


def test_the_record_is_left_untouched() -> None:
    cells = dict(FINISHED_RUN.cells)
    row = Row("x", cells)
    assert cell(status_line(row), 1) == "2.3 hours  "
    assert cells["Runtime"] == 8304


def test_a_long_run_name_is_cut_to_the_run_column() -> None:
    line = status_line(finished_run(run="x" * 60))
    assert cell(line, 0) == "x" * 46
    assert line[46:48] == COLUMN_SEPARATOR


def test_a_brief_record_raises_undefined_columns_as_print_output_does() -> None:
    with pytest.raises(PrintOutputError, match=UNDEFINED_COLUMNS):
        status_line(finished_run(detailed=False))


def test_a_record_without_cells_is_an_empty_line() -> None:
    assert status_line(Row("x", {})) == ""


# --- git info, summary, the not-started block ---------------------------------------------------------


def test_git_info_lines() -> None:
    assert git_info_lines(TESTED, TODAY, MERGES) == [
        f"Tested commit: {TESTED}",
        "The test of 2026-09-30 contains these merges:",
        *MERGES,
    ]
    assert git_info_lines(TESTED, TODAY, []) == [
        f"Tested commit: {TESTED}",
        "The test of 2026-09-30 contains these merges:",
    ]


def test_summary_line_of_no_errors() -> None:
    assert summary_line([]) == LOOK_GOOD == "Summary: AMT runs look good."


def test_summary_line_joins_the_unique_errors_in_first_appearance_order() -> None:
    errors = [
        "Some run(s) did not converge",
        "Summation checks for some run(s) revealed some gaps",
        "Some run(s) did not converge",
        "Some run(s) did not report correctly",
    ]
    assert summary_line(errors) == (
        "Summary: Some run(s) did not converge. Summation checks for some run(s) revealed some gaps. "
        "Some run(s) did not report correctly"
    )


def test_not_started_lines_wrap_the_names_in_blank_lines() -> None:
    assert not_started_lines(["a", "b"]) == [BLANK_LINE, NOT_STARTED_TITLE, "a", "b", BLANK_LINE]
    assert not_started_lines([]) == [" ", "These scenarios did not start at all:", " "]


# --- the whole block --------------------------------------------------------------------------------


def test_intro_lines_are_the_fence_and_the_six_header_sentences() -> None:
    assert intro_lines(TODAY, "/p/projects/remind/modeltests/remind/output/", compare_scenarios=True) == INTRO


def test_intro_lines_paste_the_runs_path_as_given() -> None:
    lines = intro_lines(date(2027, 1, 2), "/somewhere/remind/output/", compare_scenarios=False)
    assert lines[1] == "This is the result of the automated model tests for REMIND on 2027-01-02."
    assert lines[2] == "Path to runs: /somewhere/remind/output/"


def test_readme_lines_in_r_order() -> None:
    assert readme_lines(REPORT) == [
        *INTRO,
        f"Tested commit: {TESTED}",
        "The test of 2026-09-30 contains these merges:",
        *MERGES,
        HEADER,
        status_line(FINISHED_RUN),
        " ",
        "These scenarios did not start at all:",
        "SSP2-NDC-AMT",
        " ",
        LOOK_GOOD,
        CODE_FENCE,
    ]


def test_without_compare_scenarios_the_pdf_line_is_absent() -> None:
    lines = readme_lines(report(compare_scenarios=False))
    assert not any("compareScenarios" in line for line in lines)
    assert lines[:4] == INTRO[:4] and lines[4:6] == INTRO[5:7]
    assert len(lines) == len(readme_lines(REPORT)) - 1


def test_without_the_not_started_block() -> None:
    lines = readme_lines(report(runs_not_started=None))
    assert NOT_STARTED_TITLE not in lines
    assert lines[-3:] == [status_line(finished_run()), LOOK_GOOD, CODE_FENCE]


def test_an_empty_not_started_list_still_writes_the_block() -> None:
    lines = readme_lines(report(runs_not_started=[]))
    assert lines[-5:] == [" ", NOT_STARTED_TITLE, " ", LOOK_GOOD, CODE_FENCE]


def test_no_runs_prints_the_title_line_alone() -> None:
    lines = readme_lines(report(runs=[], runs_not_started=None, summary="Summary: No runs started"))
    assert lines[-3:] == [HEADER, "Summary: No runs started", CODE_FENCE]


def test_render_terminates_every_line_with_a_newline() -> None:
    text = render_readme(report())
    assert text.startswith("```\n")
    assert text.endswith("Summary: AMT runs look good.\n```\n")
    assert text.count("\n") == len(readme_lines(report()))
    assert text.encode("utf-8").decode("utf-8") == text


def test_the_runs_are_printed_in_the_given_order() -> None:
    second = finished_run(run="default-AMT_2026-09-28_13.23.51", Runtime=12213)
    lines = readme_lines(report(runs=[finished_run(), second]))
    index = lines.index(HEADER)
    assert lines[index + 1].startswith("SSP2-NPi-AMT")
    assert lines[index + 2].startswith("default-AMT")
