from __future__ import annotations

import math
from pathlib import Path
from typing import cast

import pytest

from modelstats._gdx import GdxReadError, read_first_found, read_records, read_scalar
from tests.conftest import write_gdx


@pytest.fixture(scope="module")
def fulldata(tmp_path_factory: pytest.TempPathFactory) -> Path:
    import gams.transfer as gt

    return write_gdx(
        tmp_path_factory.mktemp("gdx") / "fulldata.gdx",
        scalars={"o_iterationNumber": 12, "s80_bool": 1, "o_modelstat": 2, "cm_abortOnConsecFail": 4},
        sets={
            "all_regi": ["CAZ", "CHA", "EUR"],
            "solveinfo80": ["modelstat", "solvestat", "resusd", "objval"],
            "t": ["y1995", "y2000", "y2005"],
        },
        parameters={
            "p80_repy": (
                ["all_regi", "solveinfo80"],
                [
                    ("CAZ", "modelstat", 2),
                    ("EUR", "modelstat", 2),
                    ("EUR", "objval", gt.SpecialValues.EPS),
                    ("CHA", "resusd", gt.SpecialValues.POSINF),
                    ("CHA", "objval", gt.SpecialValues.NA),
                    ("CAZ", "solvestat", gt.SpecialValues.UNDEF),
                ],
            ),
            "p80_trackConsecFail": (["all_regi"], [("EUR", 4), ("CAZ", 4)]),
            "p80_modelstat": (["t"], [("y1995", 2), ("y2000", 2), ("y2005", 7)]),
            "universe": (["*"], [("a", 1)]),
        },
    )


def test_read_scalar(fulldata: Path) -> None:
    assert read_scalar(fulldata, "o_iterationNumber") == 12.0
    assert read_scalar(fulldata, "s80_bool") == 1.0
    assert read_scalar(fulldata, "absent") is None


def test_symbol_names_are_case_insensitive(fulldata: Path) -> None:
    assert read_scalar(fulldata, "O_ITERATIONNUMBER") == 12.0
    assert read_first_found(fulldata, ["P80_MODELSTAT"]) == [2.0, 2.0, 7.0]
    assert read_records(fulldata, "P80_TRACKCONSECFAIL") is not None


def test_read_first_found_returns_all_values_of_the_first_existing_symbol(fulldata: Path) -> None:
    assert read_first_found(fulldata, ["o_modelstat", "p80_modelstat"]) == [2.0]
    assert read_first_found(fulldata, ["missing", "p80_modelstat"]) == [2.0, 2.0, 7.0]
    assert read_first_found(fulldata, ["missing", "also_missing"]) is None


def test_read_first_found_is_dense_over_the_domain_set(tmp_path: Path) -> None:
    sparse = write_gdx(
        tmp_path / "sparse.gdx",
        sets={"all_regi": ["CAZ", "CHA", "EUR"]},
        parameters={"p80_modelstat": (["all_regi"], [("EUR", 2)])},
    )
    assert read_first_found(sparse, ["o_modelstat", "p80_modelstat"]) == [0.0, 0.0, 2.0]


def test_read_records_is_dense_over_the_domain_sets(fulldata: Path) -> None:
    records = read_records(fulldata, "p80_repy")
    assert records is not None
    assert list(records.columns) == ["all_regi", "solveinfo80", "value"]
    assert len(records) == 12
    modelstat = records[records["solveinfo80"] == "modelstat"]
    assert list(modelstat["all_regi"]) == ["CAZ", "CHA", "EUR"]
    assert list(modelstat["value"]) == [2.0, 0.0, 2.0]
    by_key = {
        (str(region), str(info)): float(value)
        for region, info, value in zip(
            records["all_regi"], records["solveinfo80"], records["value"], strict=True
        )
    }
    assert by_key[("EUR", "objval")] == 0.0 and math.copysign(1, by_key[("EUR", "objval")]) == 1  # EPS
    assert by_key[("CHA", "resusd")] == math.inf
    assert math.isnan(by_key[("CHA", "objval")])  # NA
    assert math.isnan(by_key[("CAZ", "solvestat")])  # UNDEF
    assert by_key[("CAZ", "resusd")] == 0.0


def test_read_records_region_filter_as_in_abort_check(fulldata: Path) -> None:
    records = read_records(fulldata, "p80_trackConsecFail")
    assert records is not None
    assert list(records["all_regi"]) == ["CAZ", "CHA", "EUR"]
    assert [r for r, v in zip(records["all_regi"], records["value"], strict=True) if v == 4] == ["CAZ", "EUR"]


def test_read_records_universe_domain_and_absent(fulldata: Path) -> None:
    records = read_records(fulldata, "universe")
    assert records is not None
    assert list(records.columns) == ["*", "value"]
    assert list(records["value"]) == [1.0]
    assert read_records(fulldata, "absent") is None
    scalar = read_records(fulldata, "o_modelstat")
    assert scalar is not None and list(scalar.columns) == ["value"] and list(scalar["value"]) == [2.0]


def test_corrupt_truncated_and_missing_files_raise(tmp_path: Path, fulldata: Path) -> None:
    # the GAMS library allocates without bound on some truncated files (BUG-042); the reads run in a
    # memory-capped worker process, so these cases end as GdxReadError instead of taking the machine down
    corrupt = tmp_path / "corrupt.gdx"
    corrupt.write_bytes(b"not a gdx file at all" * 10)
    with pytest.raises(GdxReadError, match="not a GDX file"):
        read_scalar(corrupt, "o_iterationNumber")
    data = fulldata.read_bytes()
    for fraction in (0.5, 0.9, 0.98, 0.995):
        truncated = tmp_path / f"truncated_{fraction}.gdx"
        truncated.write_bytes(data[: int(len(data) * fraction)])
        with pytest.raises(GdxReadError):
            read_first_found(truncated, ["o_modelstat"])
    with pytest.raises(GdxReadError):
        read_records(tmp_path / "missing.gdx", "p80_repy")
    assert read_scalar(fulldata, "o_iterationNumber") == 12.0  # the worker is back after a crash


def test_worker_is_replaced_after_a_kill_and_failures_are_cached(tmp_path: Path, fulldata: Path) -> None:
    import os
    import signal
    import time

    from modelstats import _gdx

    assert read_scalar(fulldata, "o_iterationNumber") == 12.0
    process = _gdx._worker._process
    assert process is not None and process.pid != os.getpid()
    os.kill(process.pid, signal.SIGKILL)
    process.wait()
    assert read_scalar(fulldata, "o_iterationNumber") == 12.0  # a fresh worker
    assert _gdx._worker._process is not None and _gdx._worker._process.pid != process.pid
    # a hanging worker is killed after WORKER_TIMEOUT and the file is remembered as failed
    copy = tmp_path / "copy.gdx"
    copy.write_bytes(fulldata.read_bytes())
    hung = _gdx._worker._process
    os.kill(hung.pid, signal.SIGSTOP)
    original_timeout = _gdx.WORKER_TIMEOUT
    _gdx.WORKER_TIMEOUT = 0.5
    try:
        with pytest.raises(GdxReadError, match="hung"):
            read_scalar(copy, "o_iterationNumber")
    finally:
        _gdx.WORKER_TIMEOUT = original_timeout
    assert hung.poll() is not None
    with pytest.raises(GdxReadError, match="hung"):  # cached, no worker needed
        read_scalar(copy, "o_iterationNumber")
    time.sleep(0.01)
    copy.write_bytes(fulldata.read_bytes())  # the file changed: read again
    os.utime(copy, None)
    assert read_scalar(copy, "o_iterationNumber") == 12.0


def test_worker_bootstrap_caps_memory_and_ignores_the_working_directory(
    tmp_path: Path, fulldata: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    from modelstats import _gdx

    (tmp_path / "struct.py").write_text("raise SystemExit('planted module imported')\n")
    monkeypatch.chdir(tmp_path)
    _gdx._worker.shutdown()
    assert read_scalar(fulldata, "o_iterationNumber") == 12.0
    process = _gdx._worker._process
    assert process is not None
    argv = [str(arg) for arg in cast("list[str]", process.args)]
    assert argv[1] == "-P" and argv[-1] == str(_gdx.WORKER_MEMORY)
