"""``Run.to_status_record()`` against every status golden under ``migration/goldens`` (``pytest -m golden``).

``migration/06-user-requests.md`` 5.3: the clean view is proven to sit on the same facts as the parity view
by reproducing the status goldens through ``Run`` objects. For each row of ``migration/cases/status.tsv``
(both modes, both detail levels, the same ``FakeEffects`` as ``tests/golden/test_status.py``) the call-level
rules of ``get_run_status`` (R's loop order under ``sort``, the ``"NA"`` row of a missing path, the merge of
repeated names, BUG-022 / D-13) are applied here, and one ``Run`` per directory supplies the record; the
rows must equal the golden by key order, type and value, or the call must raise the exact R error (the
``gdx-broken`` row asserts the raise and the file name, as in ``test_status.py``, D-20). On every successful
row the clean view is read as well: it never raises where the parity view did not, ``status_detail`` equals
the parity ``RunStatus`` wherever ``full.log`` has at most one status line (BUG-036 is the one documented
difference), the snapshot is JSON-able, and the two views share one cached facts layer: from the first
access on, no file is streamed or handed to a library twice and ``squeue`` runs once per run
(:class:`ReadLog`), and a second pass over both views reads nothing.
"""

from __future__ import annotations

import json
from collections import Counter
from collections.abc import Callable
from pathlib import Path

import pytest

from modelstats.env import FakeEffects, Mode
from modelstats.errors import ModelstatsError, RunReadError
from modelstats.run import Run
from modelstats.run_status import RunStatus, merge_records, ordered_directories
from tests.conftest import GoldenCase, golden_cases
from tests.golden.test_status import (
    R_ABORT_PREFIX,
    SYNTHETIC_PREFIX,
    assert_same_rows,
    golden_path,
    rows_as_r_frame,
)
from tests.unit.test_facts import library_reads

pytestmark = pytest.mark.golden

SETS = [(mode, detailed) for mode in (Mode.ONCLUSTER, Mode.LOCAL) for detailed in (True, False)]
CASES = {mode: golden_cases("status", mode) for mode in (Mode.ONCLUSTER, Mode.LOCAL)}
PARAMS = [
    pytest.param(mode, detailed, case, id=f"{mode.value}/{'detailed' if detailed else 'brief'}/{case.id}")
    for mode, detailed in SETS
    for case in CASES[mode]
]
MIF_VALUES = {"NA": "unknown", "no": "missing", "yes": "present", "sumErr": "summation_errors"}


class ReadLog:
    """Every read of the facts layer.

    The streams opened and programs run through ``FakeEffects``, and the files handed to the GDX, R-data
    and configuration readers (patched by :func:`library_reads` to report here).
    """

    def __init__(self, effects: FakeEffects, monkeypatch: pytest.MonkeyPatch) -> None:
        self.effects = effects
        self.library = library_reads(monkeypatch)

    def mark(self) -> tuple[int, int]:
        """Where the log stands now."""
        return len(self.effects.calls), len(self.library)

    def since(self, mark: tuple[int, int]) -> Counter[str]:
        """How often each file was streamed or handed to a library, and each program run, since ``mark``."""
        calls, library = mark
        recent = self.effects.calls[calls:]
        reads = [call.args[0] for call in recent if call.name == "open_binary"]
        reads += [call.tool for call in recent if call.name == "run"]
        reads += self.library[library:]
        return Counter(reads)


def assert_each_read_once(counts: Counter[str]) -> None:
    """``06`` 5.2: from the first access on, both views read every file once and run ``squeue`` once."""
    repeated = {name: n for name, n in counts.items() if n > 1}
    assert not repeated, repeated


def records_via_runs(
    dirs: list[str], sort: str, user: str | None, detailed: bool, effects: FakeEffects, log: ReadLog
) -> tuple[list[RunStatus], list[Run]]:
    """``get_run_status`` with one ``Run`` per directory supplying the record (``R/getRunStatus.R:22-37``).

    Both views are read right after construction, so that ``log`` sees every read of a run from its
    first access: the parity record, then the clean view's snapshot (which never raises where the
    record did not, as :func:`assert_clean_view_agrees` requires of every successful row).
    """
    normalized = [effects.normalize_path(path) for path in dirs]
    records: list[RunStatus] = []
    runs: list[Run] = []
    position: dict[str, int] = {}
    for directory in ordered_directories(normalized, sort, effects):
        run = Run(directory, effects, user=user)
        start = log.mark()
        record = run.to_status_record(detailed)
        run.to_dict()
        assert_each_read_once(log.since(start))
        runs.append(run)
        index = position.get(record.run)
        if index is None:
            position[record.run] = len(records)
            records.append(record)
        else:
            records[index] = merge_records(records[index], record)
    return records, runs


def assert_clean_view_agrees(run: Run, record: RunStatus) -> None:
    """The clean view on the same facts: readable, consistent with the record, JSON-able."""
    snapshot = run.to_dict()
    json.dumps(snapshot)
    assert snapshot["name"] == record.run
    if len(run.facts.status_lines) <= 1:
        assert run.status_detail == record.RunStatus
    assert run.mif.value == MIF_VALUES[record.Mif]
    assert (run.run_type == record.RunType) or (record.RunType == "NA" and run.run_type is None)
    if record.detailed:
        assert run.warnings == (None if record.Warnings == "NA" else int(record.Warnings or "0"))
        if record.sanity is not None:
            assert (run.summation_errors, run.range_errors, run.fix_errors) == (
                record.sanity.summationErrors,
                record.sanity.rangeErrors,
                record.sanity.fixErrors,
            )


def assert_no_rereads(log: ReadLog, runs: list[Run], detailed: bool) -> None:
    """Both views over one facts layer: once read, neither view streams a file or calls ``squeue`` again."""
    start = log.mark()
    for run in runs:
        run.to_status_record(detailed)
        run.to_dict()
    assert not log.since(start)


@pytest.mark.parametrize(("mode", "detailed", "case"), PARAMS)
def test_run_status_golden(
    mode: Mode,
    detailed: bool,
    case: GoldenCase,
    case_effects: Callable[[GoldenCase], FakeEffects],
    goldens_dir: Path,
    request: pytest.FixtureRequest,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    if any(d.startswith(SYNTHETIC_PREFIX) for d in case.row["dirs"].split(",")):
        request.getfixturevalue("synthetic_dir")
    effects = case_effects(case)
    log = ReadLog(effects, monkeypatch)
    expected = json.loads(golden_path(goldens_dir, mode, detailed, case.id).read_text(encoding="utf-8"))
    dirs = case.logical_paths("dirs")
    sort = case.row.get("sort") or "nf"
    user = case.row["user"] or None
    if "error" in expected:
        with pytest.raises(RunReadError) as excinfo:
            records_via_runs(dirs, sort, user, detailed, effects, log)
        assert isinstance(excinfo.value, ModelstatsError)
        if expected["error"].startswith(R_ABORT_PREFIX):
            assert ".gdx" in str(excinfo.value)
        else:
            assert str(excinfo.value) == expected["error"]
        return
    records, runs = records_via_runs(dirs, sort, user, detailed, effects, log)
    assert_same_rows(rows_as_r_frame(records), expected["rows"])
    assert all(record.detailed is detailed for record in records)
    for run in runs:
        assert_clean_view_agrees(run, run.to_status_record(detailed))
    assert_no_rereads(log, runs, detailed)
