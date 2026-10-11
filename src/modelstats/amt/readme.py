"""The README block of ``evaluateRuns``, byte for byte.

``R/modeltests.R:202-222, 249-256, 276-278, 367-378, 421-430``.

The REMIND AMT publishes one Markdown file to the ``testing_suite`` repository and the same text is
read by the e-mail subscribers, so ``migration/06-user-requests.md`` 4.1 keeps the block "exactly as R
writes it": the header lines, the git information, the column titles with their widths, one
``printOutput`` line per run with the runtime through ``format_runtime``, the "did not start" block,
the summary and the code fences. This module is pure: it takes plain values and the parity records of
``modelstats.run_status`` and returns text; no ``Effects``, no clock, no file. The evaluation of the
runs (which runs started, the error list, the merges since the last test) is ``amt.evaluate``'s job.

What is reproduced, with the R lines:

* the opening fence and the header sentences (``:203-217``, :func:`intro_lines`); the compareScenarios
  sentence only when the comparison is on (``:210-213``); the keyword is ``AMT`` because the AMT is
  REMIND-only (``06`` section 2, D-27);
* the git information (``:219-222``): ``Tested commit: <sha>``, ``The test of <today> contains these
  merges:`` and one line per merge (none when there is none);
* the title line (``:249-255``): the eleven titles, blanks included, joined with two spaces; the
  widths ``lenCols`` are the title lengths except the last column, which is three characters wide
  (``:256``), so ``jobInSLURM`` prints as an empty cell and ``runInAppResults`` is cut to ``yes`` or
  ``no `` (a zero-width column keeps its separators, which is why the title line has eight blanks
  between ``Runtime`` and ``RunType``);
* one row per started run (``:276-278``): ``format(round(make_difftime(second = x), 1))`` replaces
  a numeric ``Runtime`` (``R/loopRuns.R:71``, :func:`modelstats.formatting.format_runtime`), then
  ``printOutput(grsi, lenCols, colSep)`` with its default column set, which on the cluster (the
  only place the AMT runs) is :data:`modelstats.formatting.ONCLUSTER_COLUMNS`
  (``R/printOutput.R:28-30``); ``sub`` strips the newline ``printOutput`` adds and ``write`` puts
  it back, so the row ends with exactly one;
* the block of scenarios that did not start (``:374-377``): a line holding one blank, the title, one
  name per line, one blank again;
* the summary (``:421-426``): ``Summary: AMT runs look good.`` or the unique error texts in first
  appearance order joined with ``. ``, and the closing fence (``:429-430``).

Every line is terminated by a newline as R's ``write()`` does; the bytes are UTF-8, the sandbox locale
of the goldens (``migration/goldens/amt/remind-*/README.md``, pinned by
``tests/golden/test_amt_readme.py``).
"""

from __future__ import annotations

from collections.abc import Iterable, Sequence
from dataclasses import dataclass
from datetime import date
from typing import Final, TypeGuard

from modelstats.formatting import ONCLUSTER_COLUMNS, CellValue, Row, StatusRow, format_runtime, print_output

__all__ = [
    "APP_RESULTS_WIDTH",
    "BLANK_LINE",
    "CODE_FENCE",
    "COLUMN_SEPARATOR",
    "COLUMN_TITLES",
    "COLUMN_WIDTHS",
    "KEYWORD",
    "LOOK_GOOD",
    "MODEL",
    "NOT_STARTED_TITLE",
    "README_COLUMNS",
    "SUMMARY_PREFIX",
    "ReadmeReport",
    "git_info_lines",
    "header_line",
    "intro_lines",
    "not_started_lines",
    "readme_lines",
    "render_readme",
    "status_line",
    "summary_line",
]

MODEL: Final = "REMIND"
"""The only model of the rewrite (``06`` section 2): ``model`` of ``R/modeltests.R:204``."""

KEYWORD: Final = "AMT"
"""``ifelse(model == "MAgPIE", "weeklyTests", "AMT")`` for REMIND (``R/modeltests.R:208``)."""

CODE_FENCE: Final = "```"
"""The first and the last line (``R/modeltests.R:203, 430``)."""

BLANK_LINE: Final = " "
"""``write(" ", readme)``: the blank lines around the "did not start" block hold one space (``:374, 377``)."""

NOT_STARTED_TITLE: Final = "These scenarios did not start at all:"
"""``R/modeltests.R:375``; the Mattermost text reuses it (``:465``)."""

SUMMARY_PREFIX: Final = "Summary: "
"""``R/modeltests.R:424, 426``."""

LOOK_GOOD: Final = "Summary: AMT runs look good."
"""The summary of an empty error list (``R/modeltests.R:424``)."""

COLUMN_SEPARATOR: Final = "  "
"""``colSep`` of ``R/modeltests.R:250``: two blanks between the title cells and between the row cells."""

COLUMN_TITLES: Final[tuple[str, ...]] = (
    "Run                                           ",
    "Runtime    ",
    "",
    "RunType    ",
    "RunStatus         ",
    "Warnings ",
    "Iter            ",
    "Conv                 ",
    "modelstat          ",
    "Mif   ",
    "AppResults",
)
"""``coltitles`` of ``R/modeltests.R:251-254``, blanks included: their lengths are the column widths."""

APP_RESULTS_WIDTH: Final = 3
"""The width of the last column, ``runInAppResults`` (``R/modeltests.R:256``): ``yes`` or ``no ``."""

COLUMN_WIDTHS: Final[tuple[int, ...]] = (*(len(title) for title in COLUMN_TITLES[:-1]), APP_RESULTS_WIDTH)
"""``lenCols <- c(nchar(coltitles)[-length(coltitles)], 3)`` (``R/modeltests.R:256``).

``lenCols[1]`` is the run column, ``lenCols[j + 1]`` the width of data column ``j`` of
:data:`README_COLUMNS` (``modelstats.formatting.column_specs``): 46, 11, 0, 11, 18, 9, 16, 21, 19, 6, 3.
"""

README_COLUMNS: Final[tuple[str, ...]] = ONCLUSTER_COLUMNS
"""The columns of a row: ``printOutput``'s default where ``/p`` exists (``R/printOutput.R:27-30``).

``evaluateRuns`` passes no ``cols`` (``R/modeltests.R:278``) and runs on the cluster, so the set is
fixed here instead of probing the file system; the title line of ``COLUMN_TITLES`` matches this set.
"""

_RUNTIME: Final = "Runtime"
_DATE_FORMAT: Final = "%Y-%m-%d"
"""``format(Sys.time(), "%Y-%m-%d")`` (``R/modeltests.R:174``)."""


@dataclass(frozen=True, slots=True)
class ReadmeReport:
    """The inputs of one README: what ``evaluateRuns`` knows when it writes the block.

    ``today`` is the day of the evaluation (``:174``); ``runs_path`` the text after ``Path to runs:``,
    which R pastes as ``mydir`` and ``output/`` (``:206``), so the caller passes the output directory
    with its trailing slash; ``compare_scenarios`` is the ``compScen`` flag (``:210``); ``git_info``
    the lines of :func:`git_info_lines`; ``runs`` the detailed parity records of the started runs in
    the order they are reported (``:277``); ``runs_not_started`` the scenario names of the "did not
    start" block, or ``None`` when the block is not written (``:367-378``: R writes it, even empty,
    whenever fewer runs started than scenarios were to start, the default run included); ``summary``
    the line of :func:`summary_line`.
    """

    today: date
    runs_path: str
    compare_scenarios: bool
    git_info: Sequence[str]
    runs: Sequence[StatusRow]
    runs_not_started: Sequence[str] | None
    summary: str


def git_info_lines(tested_commit: str, today: date, merges: Iterable[str]) -> list[str]:
    """``gitInfo`` of ``R/modeltests.R:219-221``: the tested commit, the day, one line per merge.

    ``merges`` are the ``Merge pull request`` lines of ``git log --merges --pretty=oneline
    <last>..<tested> --abbrev-commit`` (``:198-200``), already filtered; none is allowed.
    """
    return [
        f"Tested commit: {tested_commit}",
        f"The test of {today.strftime(_DATE_FORMAT)} contains these merges:",
        *merges,
    ]


def summary_line(errors: Iterable[str]) -> str:
    """``summary`` of ``R/modeltests.R:421-426``: the unique error texts in first-appearance order.

    An empty list is ``Summary: AMT runs look good.`` (R's ``is.null(errorList)``); otherwise
    ``paste0(unique(errorList), collapse = ". ")`` after ``Summary: ``, which puts no final full stop.
    """
    unique = list(dict.fromkeys(errors))
    if not unique:
        return LOOK_GOOD
    return SUMMARY_PREFIX + ". ".join(unique)


def header_line() -> str:
    """``paste(coltitles, collapse = colSep)`` (``R/modeltests.R:255``)."""
    return COLUMN_SEPARATOR.join(COLUMN_TITLES)


def status_line(record: StatusRow) -> str:
    """One run's row (``R/modeltests.R:276-278``), without the trailing newline.

    ``grsi["Runtime"] <- format(round(make_difftime(second = ...), 1))`` applies when the record has
    a ``Runtime`` cell that ``is.numeric``: a number (seconds, live or final) becomes ``3.4 hours``
    through :func:`modelstats.formatting.format_runtime`; the ``NA`` of a run without a GAMS time is
    logical in a single-directory frame (``R/getRunStatus.R:309``), stays ``NA`` and prints as
    blanks. The record must carry the detailed columns (``getRunStatus(i)`` defaults to
    ``detailed = TRUE``); a brief one raises ``printOutput``'s ``undefined columns selected`` as R
    would. ``printOutput`` returns ``""`` for a record without cells (``R/printOutput.R:22``) and
    R writes an empty line for it.
    """
    cells: dict[str, CellValue] = dict(record.cells)
    runtime = cells.get(_RUNTIME)
    if _is_numeric(runtime):
        cells[_RUNTIME] = format_runtime(runtime)
    line = print_output(
        Row(record.run, cells), len_cols=COLUMN_WIDTHS, col_sep=COLUMN_SEPARATOR, cols=README_COLUMNS
    )
    return line.removesuffix("\n")


def _is_numeric(value: CellValue) -> TypeGuard[int | float]:
    """``is.numeric(x)`` of one cell: an integer or a double, not a logical (``NA`` included)."""
    return isinstance(value, int | float) and not isinstance(value, bool)


def not_started_lines(names: Iterable[str]) -> list[str]:
    """The "did not start" block (``R/modeltests.R:374-377``): blank, title, the names, blank."""
    return [BLANK_LINE, NOT_STARTED_TITLE, *names, BLANK_LINE]


def intro_lines(today: date, runs_path: str, compare_scenarios: bool) -> list[str]:
    """The opening fence and the header sentences (``R/modeltests.R:203-217``).

    The compareScenarios sentence (``:210-213``) is written only when the comparison is on; the model
    and the keyword are REMIND's (``:204, 208``).
    """
    lines = [
        CODE_FENCE,
        f"This is the result of the automated model tests for {MODEL} on {today.strftime(_DATE_FORMAT)}.",
        f"Path to runs: {runs_path}",
        "Direct and interactive access to plots: open shinyResults::appResults, "
        f"then use '{KEYWORD}' as keyword in the title search",
    ]
    if compare_scenarios:
        lines.append(
            "Each run folder below should contain a compareScenarios PDF comparing the output of the current"
            " and the last successful tests (comp_with_RUN-DATE.pdf)"
        )
    lines.append("Note: 'Mif' = 'no' indicates a possible error in output generation, please check!")
    lines.append(
        "If you are currently viewing the email: Overview of the last test is in red, "
        "and of the current test in green"
    )
    return lines


def readme_lines(report: ReadmeReport) -> list[str]:
    """Every line of the README in R's order (``R/modeltests.R:202-222, 255, 278, 374-377, 429-430``)."""
    lines = intro_lines(report.today, report.runs_path, report.compare_scenarios)
    lines.extend(report.git_info)
    lines.append(header_line())
    lines.extend(status_line(record) for record in report.runs)
    if report.runs_not_started is not None:
        lines.extend(not_started_lines(report.runs_not_started))
    lines.append(report.summary)
    lines.append(CODE_FENCE)
    return lines


def render_readme(report: ReadmeReport) -> str:
    """The README text: :func:`readme_lines`, each line terminated by a newline as ``write()`` does."""
    return "".join(f"{line}\n" for line in readme_lines(report))
