"""Route B: the low-level ``gdxcc`` API against the MIT ``libgdxcclib64.so`` (documented fallback).

Converted from ``migration/harness/smoke/gdx_lowlevel.py``. Route B is not the chosen route
(``migration/02-python-libraries.md`` section 4.1, decision: route A), so the MIT library is not part
of the locked environment; set ``MODELSTATS_GDXLIB`` to a directory holding ``libgdxcclib64.so``
(GAMS-dev/gdx release) to run this check, otherwise it is skipped.
"""

from __future__ import annotations

import os
from pathlib import Path
from typing import Any

import gams.transfer as gt
import gamspy_base
import pytest
from gams.core.gdx import gdxcc

GDXLIB = os.environ.get("MODELSTATS_GDXLIB")

pytestmark = pytest.mark.skipif(
    not GDXLIB or not (Path(GDXLIB) / "libgdxcclib64.so").is_file(),
    reason="MODELSTATS_GDXLIB does not point at a directory with libgdxcclib64.so (route B fallback)",
)


@pytest.fixture(scope="module")
def gdx_file(tmp_path_factory: pytest.TempPathFactory) -> Path:
    path = tmp_path_factory.mktemp("gdx") / "test.gdx"
    m = gt.Container(system_directory=gamspy_base.directory)
    regions = gt.Set(m, "all_regi", records=["CAZ", "EUR"])
    solveinfo = gt.Set(m, "solveinfo80", records=["modelstat", "solvestat"])
    gt.Parameter(m, "o_modelstat", records=2)
    records = [["CAZ", "modelstat", 2.0], ["EUR", "modelstat", 7.0]]
    gt.Parameter(m, "p80_repy", [regions, solveinfo], records=records)
    m.write(str(path))
    return path


def read_parameter(handle: Any, name: str) -> list[tuple[tuple[str, ...], float]] | None:
    ok, symbol_number = gdxcc.gdxFindSymbol(handle, name)
    if not ok:
        return None
    _, _, dimension, _ = gdxcc.gdxSymbolInfo(handle, symbol_number)
    _, record_count = gdxcc.gdxDataReadStrStart(handle, symbol_number)
    rows = []
    for _ in range(record_count):
        _, keys, values, _ = gdxcc.gdxDataReadStr(handle)
        rows.append((tuple(keys[:dimension]), float(values[gdxcc.GMS_VAL_LEVEL])))
    gdxcc.gdxDataReadDone(handle)
    return rows


def test_low_level_reads_scalars_and_parameters(gdx_file: Path) -> None:
    handle = gdxcc.new_gdxHandle_tp()
    rc, message = gdxcc.gdxCreateD(handle, GDXLIB, gdxcc.GMS_SSSIZE)
    assert rc == 1, message
    assert gdxcc.gdxOpenRead(handle, str(gdx_file))[0]
    try:
        assert read_parameter(handle, "o_modelstat") == [((), 2.0)]
        expected = [(("CAZ", "modelstat"), 2.0), (("EUR", "modelstat"), 7.0)]
        assert read_parameter(handle, "p80_repy") == expected
        assert read_parameter(handle, "nope") is None
    finally:
        gdxcc.gdxClose(handle)
        gdxcc.gdxFree(handle)
