"""``get_run_status`` against every R golden under ``migration/goldens/status`` (``pytest -m golden``).

``migration/harness/make_goldens.R`` (``fam_status``) ran ``getRunStatus(dirs, sort, user, detailed)`` inside
the sandbox for each row of ``migration/cases/status.tsv`` in both modes (``oncluster``: ``/p`` exists,
``local``: the fixtures under ``/fixtures/p``) and both detail levels, and wrote
``status/<mode>/<detailed|brief>/<id>.json``: ``{"rows": [{"_row": <row name>, <columns>}]}`` as
``jsonlite`` renders the data frame (a real ``NA`` as ``null``, a column some row never assigned as absent
from the frame, hence ``null`` in a row that lacks it only when another row has it), or ``{"error": <R
message>}`` when the call aborted. The Python side runs on the ``FakeEffects`` of the same row (overlay,
canned scheduler, clock, user, mode) and must reproduce the rows by key order, type and value, or the exact
R error message. Every register bug the status family exercises (BUG-001, -002, -003, -005, -010, -021,
-022, -029, -030, -036, -038) is an ordinary passing row; the one row whose R golden records no R error at
all - ``synthetic__broken__output__gdx-broken``, where the GDX library killed the R process (BUG-037,
decision D-20, "the port raises") - asserts the raised ``RunReadError`` and the GDX file it names, as that
is all the golden pins. Rows below ``projects/synthetic`` need the synthetic layer and skip only when it is
not built.
"""

from __future__ import annotations

import json
from collections.abc import Callable
from pathlib import Path
from typing import Any

import pytest

from modelstats.env import FakeEffects, Mode
from modelstats.facts import RunReadError
from modelstats.run_status import RunStatus, get_run_status, to_dataframe
from modelstats.tables import column_order
from tests.conftest import GoldenCase, golden_cases

pytestmark = pytest.mark.golden

SYNTHETIC_PREFIX = "projects/synthetic/"
R_ABORT_PREFIX = "R process aborted"
SETS = [(mode, detailed) for mode in (Mode.ONCLUSTER, Mode.LOCAL) for detailed in (True, False)]
CASES = {mode: golden_cases("status", mode) for mode in (Mode.ONCLUSTER, Mode.LOCAL)}
PARAMS = [
    pytest.param(mode, detailed, case, id=f"{mode.value}/{'detailed' if detailed else 'brief'}/{case.id}")
    for mode, detailed in SETS
    for case in CASES[mode]
]
FOCUSED_IDS = (
    "synthetic__remind__output__multistatus",
    "focused__dup-basenames",
    "synthetic__remind__output__noconfig",
    "synthetic__remind__output__noconfig-fulllst",
    "synthetic__remind__output__gdx-tie",
    "focused__frozen-live-runtime-local",
    "synthetic__remind__output__running-live",
    "synthetic__remind__output__nostats",
    "synthetic__remind__output__gdx-noiter",
    "synthetic__broken__output__gdx-broken",
)


def golden_path(goldens_dir: Path, mode: Mode, detailed: bool, case_id: str) -> Path:
    return goldens_dir / "status" / mode.value / ("detailed" if detailed else "brief") / f"{case_id}.json"


def rows_as_r_frame(records: list[RunStatus]) -> list[dict[str, Any]]:
    """The records as ``jsonlite`` wrote the data frame: ``_row`` first, the union of columns in R order.

    A column some record lacks is ``None`` in that row (the frame has it, the cell is ``NA``).
    """
    rows = [record.to_row() for record in records]
    columns = column_order(name for row in rows for name in row)
    return [
        {"_row": record.run, **{c: row.get(c) for c in columns}}
        for record, row in zip(records, rows, strict=True)
    ]


def assert_same_rows(actual: list[dict[str, Any]], expected: list[dict[str, Any]]) -> None:
    """Equal by key order, type and value, row by row (``03`` 4.6: JSON by key and type)."""
    assert [row["_row"] for row in actual] == [row["_row"] for row in expected]
    for got, want in zip(actual, expected, strict=True):
        assert list(got) == list(want), f"columns of {want['_row']}"
        for key, value in want.items():
            assert type(got[key]) is type(value), f"{want['_row']}.{key}: {got[key]!r} vs {value!r}"
            assert got[key] == value, f"{want['_row']}.{key}"


def test_every_status_golden_has_a_case(goldens_dir: Path) -> None:
    for mode, detailed in SETS:
        on_disk = {p.stem for p in golden_path(goldens_dir, mode, detailed, "x").parent.glob("*.json")}
        assert on_disk == {case.id for case in CASES[mode]}, f"{mode.value}/{detailed}"


def test_focused_cases_are_in_the_case_list() -> None:
    ids = {case.id for case in CASES[Mode.ONCLUSTER]}
    assert set(FOCUSED_IDS) <= ids


@pytest.mark.parametrize(("mode", "detailed", "case"), PARAMS)
def test_status_golden(
    mode: Mode,
    detailed: bool,
    case: GoldenCase,
    case_effects: Callable[[GoldenCase], FakeEffects],
    goldens_dir: Path,
    request: pytest.FixtureRequest,
) -> None:
    if any(d.startswith(SYNTHETIC_PREFIX) for d in case.row["dirs"].split(",")):
        request.getfixturevalue("synthetic_dir")
    effects = case_effects(case)
    expected = json.loads(golden_path(goldens_dir, mode, detailed, case.id).read_text(encoding="utf-8"))
    dirs = case.logical_paths("dirs")
    sort = case.row.get("sort") or "nf"
    # make_goldens.R passes user = col(row, "user", Sys.getenv("RS_USER")); an empty cell is the sandbox
    # user, which is what the R default (Sys.info()) and FakeEffects.user give here.
    user = case.row["user"] or None
    if "error" in expected:
        with pytest.raises(RunReadError) as excinfo:
            get_run_status(dirs, sort=sort, user=user, detailed=detailed, effects=effects)
        if expected["error"].startswith(R_ABORT_PREFIX):
            # BUG-037 / D-20: R recorded no error message (the process died); the port names the file.
            assert ".gdx" in str(excinfo.value)
        else:
            assert str(excinfo.value) == expected["error"]
        return
    records = get_run_status(dirs, sort=sort, user=user, detailed=detailed, effects=effects)
    assert_same_rows(rows_as_r_frame(records), expected["rows"])
    assert all(record.detailed is detailed for record in records)
    frame = to_dataframe(records)
    assert list(frame.index) == [row["_row"] for row in expected["rows"]]
    if expected["rows"]:
        assert list(frame.columns) == [key for key in expected["rows"][0] if key != "_row"]
