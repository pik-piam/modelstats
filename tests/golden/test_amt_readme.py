"""``render_readme`` reproduces every R README golden of the REMIND AMT byte for byte (``pytest -m golden``).

``migration/harness/make_goldens.sh amt`` ran ``modeltests()`` inside the sandbox for every case of
``migration/cases/amt/`` and kept, under ``migration/goldens/amt/<case>/``, the README from R's
``tempdir()`` (``README.md``), the state files as values (``state/``), every fake-binary call
(``trace.jsonl``) and the outcome (``result.json``). The builder of ``modelstats.amt.readme`` is pure, so
this test rebuilds ``evaluateRuns``'s inputs for each ``remind-*`` case whose ``result.json`` says
``ok`` and compares the rendered bytes with the golden (``migration/06-user-requests.md`` 4.1: the README
block is kept exactly; this is the informative comparison of 4.4 item 3 for the README, with no allowed
difference). The ``magpie-*`` cases are out of scope (D-27); the error cases wrote no README, or a partial
one that R never finished.

How each input is reconstructed (``R/modeltests.R`` lines in brackets):

* ``today`` [174]: the sandbox clock of ``result.json`` (``frozen``), the day in the sandbox zone;
* ``runs_path`` [206]: ``mydir`` of ``result.json`` followed by ``output/``;
* ``compare_scenarios`` [210]: ``compScen`` of the case definition ``migration/cases/amt/<case>/case.json``;
* ``tested_commit`` [196]: the right end of the ``<last>..<tested>`` range that the trace records for
  ``git log --merges`` (what ``sub("commit ", "", git log -1)`` produced);
* the merges [197-200]: the canned output of the fake ``git`` (``migration/harness/fakebin/git``) for that
  call, run here with the case directory as ``RS_AMT_CASE_DIR`` exactly as the sandbox did, filtered to the
  ``Merge pull request`` lines as R's ``| grep`` did;
* the rows [236-246, 276-278]: ``gRS.rds`` (``state/output_gRS_rds.json``) is the frame
  ``getRunStatus()`` returned for every folder of ``output/`` in the same R process, so its rows are the
  parity records of the started runs (``printOutput`` reads none of the columns that differ between a
  one-folder and a many-folder frame); the started runs are the row names matching ``runcode.rds``
  (``state/runcode_rds.json``), in ``list.dirs()`` order (``modelstats.env.sort_like_r``); each row becomes
  a ``RunStatus`` with the JSON types as the status goldens use them (a string stays a string, ``Runtime``
  is whole seconds, ``null`` is a real ``NA``);
* ``runs_not_started`` [367-373]: R's rule over ``runsToStart.rds`` (``state/runsToStart_rds.json`` row
  names): the block exists when fewer runs started than scenarios plus the default run, and lists
  ``setdiff(c("default-AMT", runsToStart), <started names without their date and time>)``;
* ``summary`` [421-426]: the error list is the outcome of the evaluation rules (``amt.evaluate``'s job,
  not the builder's), so the README comparison takes the line from the golden README itself, the last
  line before the closing fence, and ``test_the_recorded_summary_is_the_rule_outcome_of_the_rows`` pins
  that line against R's per-run rules [280-298] and the "no runs" rule [421], applied to the
  reconstructed rows and joined by ``summary_line``. The one rule it cannot rebuild is the runtime
  comparison [326-331]: it reads ``runstatistics.rda`` of the previous run of the same title, which the
  case's ``prepare.sh`` writes inside the sandbox (``remind-evaluate-slow``), so that error text is
  allowed exactly in the cases whose ``case.json`` note announces it.
"""

from __future__ import annotations

import json
import os
import re
import subprocess
from collections.abc import Sequence
from datetime import date
from pathlib import Path
from typing import Any

import pytest

from modelstats.amt.evaluate import (
    CHECK_RUNTIME,
    NO_RUNS_STARTED,
    NOT_CONVERGED,
    NOT_OPTIMAL,
    NOT_REPORTED,
    SUMMATION_GAPS,
)
from modelstats.amt.readme import (
    LOOK_GOOD,
    SUMMARY_PREFIX,
    ReadmeReport,
    git_info_lines,
    render_readme,
    summary_line,
)
from modelstats.env import sort_like_r
from modelstats.run_status import RunStatus, SanityCounts
from tests.conftest import CASES, GOLDENS, MIGRATION, resolve_frozen

pytestmark = pytest.mark.golden

AMT_GOLDENS = GOLDENS / "amt"
AMT_CASES = CASES / "amt"
FAKE_GIT = MIGRATION / "harness" / "fakebin" / "git"
STATE = "state"
GRS_FILE = "output_gRS_rds.json"
RUNCODE_FILE = "runcode_rds.json"
RUNS_TO_START_FILE = "runsToStart_rds.json"
DEFAULT_RUN = "default-AMT"
MERGE_MARK = "Merge pull request"
OK = "ok"
#: ``datetimepattern`` of ``R/modeltests.R:371``.
DATETIME_PATTERN = re.compile(r"_[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}\.[0-9]{2}\.[0-9]{2}")

# The error texts of ``evaluateRuns`` (``R/modeltests.R:282-298, 330, 421``) are imported from
# ``modelstats.amt.evaluate``; the rules below stay this module's own reading of the R lines.
#: What a case note says when its ``prepare.sh`` plants a slower run (the runtime rule of ``:326-331``).
RUNTIME_MARK = "Check runtime!"
CONVERGED = ("converged", "converged (had INFES)")
OPTIMAL = "2: Locally Optimal"
CALIBRATION_CONVERGED = "Clb_converged"
SUMMATION_ERRORS = "sumErr"
REPORTED = "yes"
ERROR_SEPARATOR = ". "

if not AMT_GOLDENS.is_dir():
    pytest.skip(f"AMT goldens absent: {AMT_GOLDENS}", allow_module_level=True)


def read_json(path: Path) -> dict[str, Any]:
    data: dict[str, Any] = json.loads(path.read_text(encoding="utf-8"))
    return data


def remind_cases() -> list[str]:
    """Every ``remind-*`` case directory under the AMT goldens, in name order."""
    return sorted(path.name for path in AMT_GOLDENS.glob("remind-*") if path.is_dir())


def is_ok(case_id: str) -> bool:
    return bool(read_json(AMT_GOLDENS / case_id / "result.json")["status"] == OK)


def has_readme(case_id: str) -> bool:
    return (AMT_GOLDENS / case_id / "README.md").is_file()


#: The evaluate cases that completed: ``start`` and "nothing to do" cases are ``ok`` without a README.
CASE_IDS = [case_id for case_id in remind_cases() if is_ok(case_id) and has_readme(case_id)]


# --- the inputs, rebuilt from the golden -------------------------------------------------------------


def state_value(case_id: str, name: str) -> Any:
    """The ``value`` of one recorded state file (``migration/goldens/README.md``: RDS files as values)."""
    recorded = read_json(AMT_GOLDENS / case_id / STATE / name)
    assert recorded["exists"], f"{case_id}: {name} was not recorded as existing"
    return recorded["value"]


def frame_rows(case_id: str, name: str) -> list[dict[str, Any]]:
    """The rows of a recorded data frame, ``_row`` first."""
    rows: list[dict[str, Any]] = state_value(case_id, name)["rows"]
    return rows


def git_calls(case_id: str) -> list[list[str]]:
    """The argv of every fake ``git`` call in the trace, in call order."""
    trace = (AMT_GOLDENS / case_id / "trace.jsonl").read_text(encoding="utf-8")
    calls = [json.loads(line) for line in trace.splitlines() if line.strip()]
    return [list(call["argv"]) for call in calls if call["tool"] == "git"]


def merge_range(case_id: str) -> str:
    """``<last>..<tested>`` of the ``git log --merges`` call (``R/modeltests.R:197-199``)."""
    for argv in git_calls(case_id):
        if argv[:2] == ["log", "--merges"]:
            (span,) = [arg for arg in argv if ".." in arg]
            return span
    raise AssertionError(f"{case_id}: no git log --merges call in the trace")


def commit_tested(case_id: str) -> str:
    """``commitTested`` (``:196``): the right end of the range R built from it."""
    return merge_range(case_id).split("..", 1)[1]


def merges(case_id: str) -> list[str]:
    """What ``git log --merges ... | grep 'Merge pull request'`` printed for the case (``:197-200``).

    The fake ``git`` prints the case's ``git-log-merges.txt`` or its built-in default; it is run here
    the way the sandbox ran it (``RS_AMT_CASE_DIR`` set, no trace file), and the ``grep`` is applied.
    """
    environ = {"PATH": os.environ["PATH"], "RS_AMT_CASE_DIR": str(AMT_CASES / case_id)}
    argv = [
        "sh",
        str(FAKE_GIT),
        "log",
        "--merges",
        "--pretty=oneline",
        merge_range(case_id),
        "--abbrev-commit",
    ]
    completed = subprocess.run(argv, env=environ, capture_output=True, text=True, check=True)
    return [line for line in completed.stdout.splitlines() if MERGE_MARK in line]


def optional_int(value: Any) -> int | None:
    if value is None:
        return None
    assert isinstance(value, int | float) and value == int(value), repr(value)
    return int(value)


def record_from_row(row: dict[str, Any], output_dir: str) -> RunStatus:
    """A ``gRS.rds`` row as the detailed parity record ``getRunStatus(i)`` returns for that folder.

    The six sanity columns are one block: present with the three counts when R reached it (a REMIND
    run with a mif), all ``null`` otherwise.
    """
    sanity = None
    if row.get("summationErrors") is not None:
        sanity = SanityCounts(
            summationErrors=int(row["summationErrors"]),
            rangeErrors=int(row["rangeErrors"]),
            fixErrors=int(row["fixErrors"]),
            missingProjVars=optional_int(row.get("missingProjVars")),
            projSummationErrors=optional_int(row.get("projSummationErrors")),
            projSummationErrorsRegional=optional_int(row.get("projSummationErrorsRegional")),
        )
    name: str = row["_row"]
    return RunStatus(
        run=name,
        path=f"{output_dir}{name}",
        jobInSLURM=row["jobInSLURM"],
        RunType=row["RunType"],
        modelstat=row["modelstat"],
        runInAppResults=row.get("runInAppResults"),
        Mif=row["Mif"],
        Iter=row["Iter"],
        RunStatus=row["RunStatus"],
        detailed=True,
        Warnings=row.get("Warnings"),
        Conv=row.get("Conv"),
        Runtime=optional_int(row.get("Runtime")),
        sanity=sanity,
    )


def started_runs(case_id: str, output_dir: str) -> list[RunStatus]:
    """``grep(runcode, list.dirs(...))`` (``:246``) over the ``gRS.rds`` rows, in ``list.dirs()`` order."""
    (runcode,) = state_value(case_id, RUNCODE_FILE)
    rows = {row["_row"]: row for row in frame_rows(case_id, GRS_FILE)}
    names = sort_like_r(name for name in rows if re.search(runcode, name))
    return [record_from_row(rows[name], output_dir) for name in names]


def runs_not_started(case_id: str, started: list[str]) -> list[str] | None:
    """The block of ``R/modeltests.R:367-373``, or ``None`` when R does not write it."""
    to_start = [row["_row"] for row in frame_rows(case_id, RUNS_TO_START_FILE)]
    if not len(started) < len(to_start) + 1:
        return None
    scenarios_started = {DATETIME_PATTERN.sub("", name) for name in started}
    return [name for name in dict.fromkeys([DEFAULT_RUN, *to_start]) if name not in scenarios_started]


def recorded_summary(case_id: str) -> str:
    """The summary line of the golden README: the last line before the closing fence (``:429-430``)."""
    lines = (AMT_GOLDENS / case_id / "README.md").read_text(encoding="utf-8").splitlines()
    assert lines[-1] == "```" and lines[-2].startswith(SUMMARY_PREFIX), case_id
    return lines[-2]


def summary_parts(summary: str) -> list[str]:
    """The error texts a summary line was joined from (``:426``); none for ``AMT runs look good.``."""
    if summary == LOOK_GOOD:
        return []
    return summary.removeprefix(SUMMARY_PREFIX).split(ERROR_SEPARATOR)


def record_errors(record: RunStatus) -> list[str]:
    """The texts one run adds to ``errorList`` (``R/modeltests.R:280-298``), in R's order.

    The rules read the parity record's strings as R reads ``grsi[, "RunType"]`` and friends; the
    runtime rule (``:326-331``) is not among them, see the module docstring.
    """
    errors: list[str] = []
    if not re.search("Calib_nash|testOneRegi", record.RunType) and record.Conv not in CONVERGED:
        errors.append(NOT_CONVERGED)
    if record.RunType == "Calib_nash" and record.Conv != CALIBRATION_CONVERGED:
        errors.append(NOT_CONVERGED)
    if "testOneRegi" in record.RunType and record.modelstat != OPTIMAL:
        errors.append(NOT_OPTIMAL)
    if record.Mif == SUMMATION_ERRORS:
        errors.append(SUMMATION_GAPS)
    if record.runInAppResults != REPORTED:
        errors.append(NOT_REPORTED)
    return errors


def rule_errors(runs: Sequence[RunStatus]) -> list[str]:
    """``errorList`` after the evaluation loop (``:273-298``) and the "no runs" rule (``:421``)."""
    errors = [error for run in runs for error in record_errors(run)]
    if not runs:
        errors.append(NO_RUNS_STARTED)
    return errors


def expects_runtime_error(case_id: str) -> bool:
    """Whether the case definition plants a slower run (its note says so)."""
    note = str(read_json(AMT_CASES / case_id / "case.json").get("note", ""))
    return RUNTIME_MARK in note


def report_of(case_id: str) -> ReadmeReport:
    result = read_json(AMT_GOLDENS / case_id / "result.json")
    case = read_json(AMT_CASES / case_id / "case.json")
    today: date = resolve_frozen(result["frozen"]).date()
    output_dir = f"{result['mydir']}output/"
    runs = started_runs(case_id, output_dir)
    return ReadmeReport(
        today=today,
        runs_path=output_dir,
        compare_scenarios=bool(case["compScen"]),
        git_info=git_info_lines(commit_tested(case_id), today, merges(case_id)),
        runs=runs,
        runs_not_started=runs_not_started(case_id, [run.run for run in runs]),
        summary=recorded_summary(case_id),
    )


# --- the tests ---------------------------------------------------------------------------------------


def test_every_ok_evaluate_case_is_tested_and_no_start_case_wrote_a_readme() -> None:
    assert CASE_IDS, "no REMIND AMT README golden with status ok"
    for case_id in remind_cases():
        if is_ok(case_id) and not has_readme(case_id):
            assert "evaluate" not in case_id, f"{case_id}: an ok evaluate case without a README"
    assert sorted(CASE_IDS) == CASE_IDS


def test_the_fake_git_is_the_one_the_goldens_were_made_with() -> None:
    assert FAKE_GIT.is_file(), FAKE_GIT
    assert "git-log-merges" in FAKE_GIT.read_text(encoding="utf-8")


@pytest.mark.parametrize("case_id", CASE_IDS)
def test_readme_equals_the_golden(case_id: str) -> None:
    expected = (AMT_GOLDENS / case_id / "README.md").read_bytes()
    assert render_readme(report_of(case_id)).encode("utf-8") == expected


@pytest.mark.parametrize("case_id", CASE_IDS)
def test_the_recorded_summary_is_the_rule_outcome_of_the_rows(case_id: str) -> None:
    """The summary taken from the golden is what ``summary_line`` makes of R's rules on the rows."""
    result = read_json(AMT_GOLDENS / case_id / "result.json")
    runs = started_runs(case_id, f"{result['mydir']}output/")
    parts = summary_parts(recorded_summary(case_id))
    assert (CHECK_RUNTIME in parts) == expects_runtime_error(case_id), case_id
    assert summary_line(part for part in parts if part != CHECK_RUNTIME) == summary_line(rule_errors(runs))
