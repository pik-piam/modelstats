"""The ``README.md`` of ``evaluateRuns`` (``R/modeltests.R``), built line by line exactly as R writes it.

``evaluateRuns`` assembles the README in R's ``tempdir()`` with ``write(x, readme, append = TRUE)`` calls spread
over the function: lines 203-217 the header, 219-222 the git information, 250-256 the column titles, 275-278 one
line per started run, 374-377 the scenarios that did not start, 423-430 the summary and the closing fence. This
module is the pure part of that: it knows the texts, R's ``write()`` semantics and the ``printOutput`` regime of the
run lines, and :class:`Readme` keeps the accumulated text in the order R writes it (so that a failure in between
leaves the same partial README that R leaves). Reading the run status, the git log and the state files, deciding
whether the "did not start" block applies (``R/modeltests.R:370``, BUG-024) and writing the file through ``Effects``
belong to ``amt/evaluate.py``.

R facts the builder relies on (R 4.6.1, verified 2026-10-01, pinned in ``tests/unit/test_amt_readme.py``):

- ``write(x, file, append = TRUE)`` is ``cat(x, file = file, sep = "\\n", append = TRUE)`` for a character vector:
  every element is followed by a newline, and because the separator contains a newline ``cat`` ends the output
  with one even when there is no element, so ``write(character(0))`` (and ``write(NULL)``) writes a single empty
  line; an ``NA`` element prints as ``NA``;
- ``paste0`` drops a zero-length argument: with ``model = NULL`` the first line reads ``... tests for  on <date>.``
  and the keyword line ``use '' as keyword``; the test ``model == "REMIND" && compScen == TRUE`` then stops with
  ``missing value where TRUE/FALSE needed`` when ``compScen`` is ``TRUE`` (``logical(0) && TRUE`` is ``NA``) and
  continues when it is ``FALSE`` (``logical(0) && FALSE`` is ``FALSE``);
- ``sub("\\n$", "", x)`` removes exactly one trailing newline; ``printOutput`` returns one;
- ``setdiff(x, y)`` keeps the order of ``x`` and drops duplicates; ``unique(errorList)`` keeps first occurrences;
  ``paste0(..., collapse = ". ")`` joins them with a dot and a space, without a final dot;
- ``is.numeric(NA)`` is ``FALSE``: the ``Runtime`` of a run without a finished ``runstatistics.rda`` is the logical
  ``NA`` of ``getRunStatus.R:309`` and prints as blanks of the column width; a numeric ``Runtime`` is replaced by
  ``format(round(make_difftime(second = x), 1))`` (:func:`modelstats.formatting.format_runtime`) before printing.
"""

from __future__ import annotations

import re
from collections.abc import Iterator, Mapping, Sequence
from typing import TYPE_CHECKING

from modelstats.errors import RParityError
from modelstats.formatting import format_runtime, print_output

if TYPE_CHECKING:
    from modelstats.env import Effects

__all__ = [
    "COLTITLES",
    "COLUMN_TITLE_LINE",
    "COL_SEP",
    "DATETIME_PATTERN",
    "DEFAULT_SCENARIO",
    "FENCE",
    "LEN_COLS",
    "NOT_STARTED_TITLE",
    "SUMMARY_OK",
    "Readme",
    "build_readme",
    "git_info",
    "header_lines",
    "not_started_text",
    "r_write",
    "run_line",
    "runs_not_started",
    "runtime_formatted",
    "scenarios_started",
    "summary_line",
]

#: The code fence that opens and closes the README (lines 203 and 430).
FENCE = "```"
#: ``colSep`` of line 250: the run lines and the title line use two spaces, not ``printOutput``'s default three.
COL_SEP = "  "
#: ``coltitles`` of lines 251-254, byte for byte (the empty title is the ``jobInSLURM`` column).
COLTITLES: tuple[str, ...] = (
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
#: ``paste(coltitles, collapse = colSep)`` (line 255).
COLUMN_TITLE_LINE = COL_SEP.join(COLTITLES)
#: ``lenCols <- c(nchar(coltitles)[-length(coltitles)], 3)`` (line 256): the folder column and then one width per
#: status column counted from the last one, the ``AppResults`` cell cut to three characters.
LEN_COLS: tuple[int, ...] = tuple(len(title) for title in COLTITLES[:-1]) + (3,)
#: ``datetimepattern`` of line 371: the ``_YYYY-MM-DD_HH.MM.SS`` suffix of a run folder name.
DATETIME_PATTERN = re.compile(r"_[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}\.[0-9]{2}\.[0-9]{2}")
#: The scenario line 373 expects in addition to the row names of ``runsToStart.rds``.
DEFAULT_SCENARIO = "default-AMT"
#: Line 375.
NOT_STARTED_TITLE = "These scenarios did not start at all:"
#: Line 424.
SUMMARY_OK = "Summary: AMT runs look good."

_ERR_NA_CONDITION = "missing value where TRUE/FALSE needed"
_NOTE_LINE = "Note: 'Mif' = 'no' indicates a possible error in output generation, please check!"
_EMAIL_LINE = (
    "If you are currently viewing the email: Overview of the last test is in red, and of the current test in green"
)
_COMP_SCEN_LINE = (
    "Each run folder below should contain a compareScenarios PDF comparing the output of the current and"
    " the last successful tests (comp_with_RUN-DATE.pdf)"
)


# ---------------------------------------------------------------------------
# R's write()
# ---------------------------------------------------------------------------


def r_write(x: str | Sequence[str | None] | None) -> str:
    """The bytes R's ``write(x, file)`` appends for a character vector ``x`` (``cat(x, sep = "\\n")``).

    Every element is followed by a newline; a zero-length vector (``character(0)``, ``NULL``, an empty sequence)
    writes a single newline because ``cat`` always terminates when the separator contains one; an ``NA`` element
    (``None``) prints as ``NA``.
    """
    if x is None:
        elements: list[str | None] = []
    elif isinstance(x, str):
        elements = [x]
    else:
        elements = list(x)
    if not elements:
        return "\n"
    return "".join(("NA" if element is None else element) + "\n" for element in elements)


# ---------------------------------------------------------------------------
# The pieces, in the order evaluateRuns writes them
# ---------------------------------------------------------------------------


def _header_writes(model: str | None, today: str, mydir: str, comp_scen: bool) -> Iterator[str]:
    """The arguments of the ``write()`` calls of lines 203-217, one at a time, failing where R fails."""
    model_text = "" if model is None else model  # paste0 drops a NULL
    yield FENCE
    yield f"This is the result of the automated model tests for {model_text} on {today}."
    yield f"Path to runs: {mydir}output/"
    keyword = "" if model is None else ("weeklyTests" if model == "MAgPIE" else "AMT")  # ifelse(logical(0), ..)
    yield (
        "Direct and interactive access to plots: open shinyResults::appResults, then use '"
        f"{keyword}' as keyword in the title search"
    )
    if model is None:
        # NULL == "REMIND" is logical(0); logical(0) && TRUE is NA, which `if` rejects; logical(0) && FALSE is FALSE.
        if comp_scen:
            raise RParityError(_ERR_NA_CONDITION)
    elif model == "REMIND" and comp_scen:
        yield _COMP_SCEN_LINE
    yield _NOTE_LINE
    yield _EMAIL_LINE


def header_lines(model: str | None, today: str, mydir: str, comp_scen: bool) -> list[str]:
    """The header lines (``R/modeltests.R`` 203-217): the opening fence, the model and date, the path to the runs,
    the shinyResults keyword (``weeklyTests`` for MAgPIE, ``AMT`` otherwise), the compareScenarios note for REMIND
    with ``compScen``, the Mif note and the e-mail colour note.

    ``mydir`` is pasted as given (``paste0(mydir, "output/")`` expects its trailing slash). ``model = None`` is R's
    ``NULL``: the first and the keyword line get an empty model and ``comp_scen = True`` raises
    ``RParityError('missing value where TRUE/FALSE needed')`` after the keyword line; :class:`Readme` keeps those
    lines, this function loses them with the exception.
    """
    return list(_header_writes(model, today, mydir, comp_scen))


def git_info(commit_tested: str, today: str, merges: Sequence[str]) -> list[str]:
    """The ``gitInfo`` vector of lines 219-221, written to the README (line 222) and repeated in the Mattermost
    message (line 459): the tested commit, the ``contains these merges`` line and the merge lines themselves
    (zero or more ``git log --merges --pretty=oneline ... | grep 'Merge pull request'`` lines)."""
    return [f"Tested commit: {commit_tested}", f"The test of {today} contains these merges:", *merges]


def runtime_formatted(row: Mapping[str, object]) -> dict[str, object]:
    """A copy of the status record with ``Runtime`` as ``format(round(make_difftime(second = x), 1))``
    (lines 275-277): only a numeric ``Runtime`` (R's ``is.numeric``: an ``int`` or ``float``, not the logical
    ``bool``) is formatted, a missing one stays ``None`` and prints as blanks, a record without the column is
    left alone."""
    cells = dict(row)
    seconds = cells.get("Runtime")
    if isinstance(seconds, int | float) and not isinstance(seconds, bool):
        cells["Runtime"] = format_runtime(seconds)
    return cells


def run_line(
    row: Mapping[str, object],
    *,
    rowname: str | None = None,
    effects: Effects | None = None,
) -> str:
    """One run's README line (line 278): ``printOutput(grsi, lenCols = lenCols, colSep = colSep)`` with the
    ``Runtime`` formatted first and the trailing newline removed (``sub("\\n$", "", ...)``).

    ``row`` is the single record of ``get_run_status(i)`` (a :class:`modelstats.run_status.RunStatus` or any
    mapping); ``rowname`` is the data.frame row name and defaults to the record's ``rowname`` attribute. The
    column set is ``printOutput``'s default, which depends on ``effects.on_cluster`` exactly as R probes ``/p``.
    """
    name = rowname if rowname is not None else getattr(row, "rowname", None)
    if not isinstance(name, str):
        raise TypeError("run_line needs rowname (the data.frame row name) for a mapping without a rowname attribute")
    text = print_output(runtime_formatted(row), rowname=name, len_cols=LEN_COLS, col_sep=COL_SEP, effects=effects)
    return text.removesuffix("\n")


def scenarios_started(runs_started: Sequence[str]) -> list[str]:
    """``gsub(datetimepattern, "", runsStarted)`` (line 372): the run folder names without their time stamps."""
    return [DATETIME_PATTERN.sub("", run) for run in runs_started]


def runs_not_started(runs_started: Sequence[str], runs_to_start: Sequence[str]) -> list[str]:
    """``setdiff(c("default-AMT", runsToStart), scenariosStarted)`` (line 373): the scenarios, in the order of
    ``runsToStart.rds`` with ``default-AMT`` first and without duplicates, that no started run folder names.

    Whether the block is written at all is the count-based test of line 370 (``length(runsStarted) <
    length(runsToStart) + 1``, BUG-024), which the caller performs.
    """
    started = set(scenarios_started(runs_started))
    result: list[str] = []
    for scenario in (DEFAULT_SCENARIO, *runs_to_start):
        if scenario not in started and scenario not in result:
            result.append(scenario)
    return result


def not_started_text(names: Sequence[str]) -> str:
    """The four ``write()`` calls of lines 374-377: a line holding a single space, the title, one line per
    scenario (a single empty line when there is none, see :func:`r_write`) and the space line again."""
    return r_write(" ") + r_write(NOT_STARTED_TITLE) + r_write(names) + r_write(" ")


def summary_line(error_list: Sequence[str] | None) -> str:
    """``summary`` of lines 423-427: ``Summary: AMT runs look good.`` for an empty (``NULL``) error list, else
    ``Summary: `` and the distinct messages in order of first occurrence joined by ``. ``."""
    if not error_list:
        return SUMMARY_OK
    unique: list[str] = []
    for message in error_list:
        if message not in unique:
            unique.append(message)
    return "Summary: " + ". ".join(unique)


# ---------------------------------------------------------------------------
# The accumulating README
# ---------------------------------------------------------------------------


class Readme:
    """The README text as ``evaluateRuns`` accumulates it; every method is one or more R ``write()`` calls.

    The caller writes :attr:`text` to the README file after each step it wants to be visible on failure (R appends
    to the file in ``tempdir()`` as it goes, so an error in the run loop leaves the lines written so far). The
    steps, in R's order: :meth:`begin`, :meth:`add_git_info`, :meth:`add_column_titles`, :meth:`add_run` per
    started run, :meth:`add_not_started` (REMIND, when line 370 says so) and :meth:`finish`.
    """

    def __init__(self) -> None:
        self._text = ""

    @property
    def text(self) -> str:
        """Everything written so far, newline-terminated lines (empty before :meth:`begin`)."""
        return self._text

    @property
    def lines(self) -> list[str]:
        """The lines written so far, without their newlines."""
        if not self._text:
            return []
        body = self._text.removesuffix("\n")
        return body.split("\n")

    def write(self, x: str | Sequence[str | None] | None) -> None:
        """``write(x, readme, append = TRUE)``: see :func:`r_write`."""
        self._text += r_write(x)

    def begin(self, model: str | None, today: str, mydir: str, comp_scen: bool) -> None:
        """Lines 203-217 (the first ``write`` without ``append`` creates the file: the text starts over)."""
        self._text = ""
        for line in _header_writes(model, today, mydir, comp_scen):
            self.write(line)

    def add_git_info(self, git_info_lines: Sequence[str]) -> None:
        """``write(gitInfo, readme, append = TRUE)`` (line 222) with the vector of :func:`git_info`."""
        self.write(git_info_lines)

    def add_column_titles(self) -> None:
        """Line 255."""
        self.write(COLUMN_TITLE_LINE)

    def add_run(
        self,
        row: Mapping[str, object],
        *,
        rowname: str | None = None,
        effects: Effects | None = None,
    ) -> str:
        """Line 278 for one started run; returns the line written (see :func:`run_line`)."""
        line = run_line(row, rowname=rowname, effects=effects)
        self.write(line)
        return line

    def add_not_started(self, names: Sequence[str]) -> None:
        """Lines 374-377 (see :func:`not_started_text`)."""
        self._text += not_started_text(names)

    def finish(self, error_list: Sequence[str] | None) -> str:
        """Lines 429-430: the summary line and the closing fence; returns the summary (reused in the message)."""
        summary = summary_line(error_list)
        self.write(summary)
        self.write(FENCE)
        return summary


def build_readme(
    *,
    model: str | None,
    today: str,
    mydir: str,
    comp_scen: bool,
    git_info_lines: Sequence[str],
    runs: Sequence[tuple[str, Mapping[str, object]]],
    runs_not_started_names: Sequence[str] | None = None,
    error_list: Sequence[str] | None,
    effects: Effects | None = None,
) -> str:
    """The complete README of a run of ``evaluateRuns`` that reached the closing fence.

    ``runs`` are ``(rowname, record)`` pairs in ``runsStarted`` order; ``runs_not_started_names`` is ``None`` when
    the block of lines 374-377 is not written (MAgPIE, or enough runs started) and the (possibly empty) scenario
    list otherwise.
    """
    readme = Readme()
    readme.begin(model, today, mydir, comp_scen)
    readme.add_git_info(git_info_lines)
    readme.add_column_titles()
    for rowname, row in runs:
        readme.add_run(row, rowname=rowname, effects=effects)
    if runs_not_started_names is not None:
        readme.add_not_started(runs_not_started_names)
    readme.finish(error_list)
    return readme.text
