"""``found_in_slurm`` and ``current_runs`` against the R goldens (``pytest -m golden``).

``migration/harness/make_goldens.R`` (``fam_slurm``) ran ``foundInSlurm(dir, user)`` inside the sandbox for
each row of ``migration/cases/slurm.tsv`` under that row's squeue case and wrote ``{"value": ...}`` or
``{"error": <R message>}`` to ``migration/goldens/slurm/<id>.json``. The Python side runs on the
``FakeEffects`` of the same row (overlay, canned scheduler, clock, user) and must reproduce the value or
the exact error message; the register's bugs (BUG-009, BUG-019, BUG-023, BUG-031, BUG-035) are ordinary
passing rows. Rows below ``projects/synthetic`` need the synthetic layer and skip only when it is not built.

``rs -C`` has no golden family of its own: its R output is the ``rs`` family. Every ``rs`` row whose
arguments are ``-C`` with at most ``-b``, ``-A``, ``-d N`` and ``-u USER`` (no filter, no coupling flags)
pins ``current_runs``: the number of runs is the ``Runs found: N`` line of ``<id>.err`` (or zero for the
two "no runs" warnings), the run basenames are the ``Folder`` column of ``<id>.out``, and the R error of
``current--regex-bracket`` (BUG-008) is the message under ``Error in grepl(...)`` of its ``.err``.
"""

from __future__ import annotations

import json
import re
from collections.abc import Callable
from pathlib import Path

import pytest

from modelstats.env import FakeEffects
from modelstats.runfolders import r_basename
from modelstats.slurm import SlurmError, current_runs, found_in_slurm, lookup_slurm
from tests.conftest import GoldenCase, golden_cases

pytestmark = pytest.mark.golden

SYNTHETIC_PREFIX = "projects/synthetic/"
CASES = golden_cases("slurm")

_ANSI = re.compile(r"\x1b\[[0-9;]*m")
_RUNS_FOUND = re.compile(r"Runs found: (\d+)")
_NO_RUNS = re.compile(r"No currently running runs found|No runs found in the past \d+ days")
_R_ERROR = re.compile(r"^Error in .*\n\s+(.*)$", re.MULTILINE)


def test_every_slurm_golden_has_a_case(goldens_dir: Path) -> None:
    on_disk = {path.stem for path in (goldens_dir / "slurm").glob("*.json")}
    assert on_disk == {case.id for case in CASES}


@pytest.mark.parametrize("case", CASES, ids=lambda case: case.id)
def test_slurm_golden(
    case: GoldenCase,
    case_effects: Callable[[GoldenCase], FakeEffects],
    goldens_dir: Path,
    request: pytest.FixtureRequest,
) -> None:
    if case.row["dir"].startswith(SYNTHETIC_PREFIX):
        request.getfixturevalue("synthetic_dir")
    effects = case_effects(case)
    expected = json.loads((goldens_dir / "slurm" / f"{case.id}.json").read_text(encoding="utf-8"))
    directory = case.logical_path(case.row["dir"])
    # make_goldens.R passes user = col(row, "user", Sys.getenv("RS_USER")); an empty cell is the sandbox
    # user, which is what the R default (Sys.info()) and FakeEffects.user give here.
    user = case.row["user"] or None
    if "error" in expected:
        with pytest.raises(SlurmError) as excinfo:
            found_in_slurm(directory, effects, user)
        assert str(excinfo.value) == expected["error"]
    else:
        assert found_in_slurm(directory, effects, user) == expected["value"]
        lookup = lookup_slurm(directory, effects, user)
        assert lookup.text == expected["value"]
        assert lookup.call.argv == ("squeue", "-h", "-o", "%u %Z %j %M %T %q")
        assert len(lookup.jobs) == len(lookup.matched)


# ---------------------------------------------------------------------------------------------
# rs -C through the rs goldens
# ---------------------------------------------------------------------------------------------


def _current_args(argv: str) -> tuple[str | None, int] | None:
    """``(user, daysback)`` for an ``rs`` argv made of ``-C``, ``-b``, ``-A``, ``-d N`` and ``-u USER``.

    Any other argument gives ``None`` (the row is not a plain ``-C`` case). ``None`` as user means the
    ``-u`` default ("you": ``Sys.info()``); ``-A`` sets ``opt$user <- NULL``
    (``R/commandLineInterface.R:127``), which ``paste0`` renders as the empty string.
    """
    tokens = argv.split()
    user: str | None = None
    daysback = 0
    seen_c = False
    index = 0
    while index < len(tokens):
        token = tokens[index]
        if token in {"-C", "-Cb"}:
            seen_c = True
        elif token == "-A":
            user = ""
        elif token == "-d" and index + 1 < len(tokens) and tokens[index + 1].isdigit():
            daysback = int(tokens[index + 1])
            index += 1
        elif token == "-u" and index + 1 < len(tokens):
            user = tokens[index + 1]
            index += 1
        elif token != "-b":
            return None
        index += 1
    return (user, daysback) if seen_c else None


RS_CURRENT_CASES = [case for case in golden_cases("rs") if _current_args(case.row["argv"]) is not None]


def _folder_column(out_text: str) -> list[str]:
    """The first word of every table row below the ``Folder`` header, ANSI stripped."""
    lines = _ANSI.sub("", out_text).splitlines()
    try:
        header = next(i for i, line in enumerate(lines) if line.startswith("Folder"))
    except StopIteration:
        return []
    return [line.split()[0] for line in lines[header + 1 :] if line.strip()]


def test_rs_current_cases_are_selected() -> None:
    ids = {case.id for case in RS_CURRENT_CASES}
    assert {
        "current",
        "current--daysback",
        "current--regex-bracket",
        "current--users-comma",
        "amt--current",
    } <= ids
    assert "current--filter" not in ids


@pytest.mark.parametrize("case", RS_CURRENT_CASES, ids=lambda case: case.id)
def test_current_runs_against_rs_golden(
    case: GoldenCase,
    case_effects: Callable[[GoldenCase], FakeEffects],
    goldens_dir: Path,
) -> None:
    effects = case_effects(case)
    args = _current_args(case.row["argv"])
    assert args is not None
    user, daysback = args
    if user is None:
        user = case.user  # opt$user == "you" -> Sys.info()[["user"]] (R/commandLineInterface.R:109)
    err_text = (goldens_dir / "rs" / f"{case.id}.err").read_text(encoding="utf-8")
    out_text = (goldens_dir / "rs" / f"{case.id}.out").read_text(encoding="utf-8")
    r_error = _R_ERROR.search(err_text)
    if r_error is not None:
        with pytest.raises(SlurmError) as excinfo:
            current_runs(user, daysback, effects)
        assert str(excinfo.value) == r_error.group(1)
        return
    found = current_runs(user, daysback, effects)
    runs_found = _RUNS_FOUND.search(err_text)
    expected_count = int(runs_found.group(1)) if runs_found else 0
    if runs_found is None:
        assert _NO_RUNS.search(err_text) is not None
    assert len(found.runs) == expected_count
    assert sorted(r_basename(run) for run in found.runs) == sorted(_folder_column(out_text))
    assert len(found.calls) == (4 if daysback > 0 else 2)
    assert found.calls[0].argv[:2] == ("squeue", "-u")
