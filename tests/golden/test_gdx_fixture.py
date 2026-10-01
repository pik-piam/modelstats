"""Route A reads a real REMIND ``fulldata.gdx`` and agrees with the R oracle.

Converted from ``migration/harness/smoke/gdx_compare.py`` (its route B and C timings are not
reproducible from the locked environment; route A is the decision). The selective read of the
162 MB ``SSP2-EU21-PkBudg650-AMT`` fixture is timed and cross-checked against the R golden of
``getRunStatus()``: the GDX chosen by ``R/getRunStatus.R:53-63`` (``which.max`` of
``o_iterationNumber``, first maximum wins) must carry the ``o_modelstat`` that R printed as
``modelstat`` (``R/getRunStatus.R:81-101``).
"""

from __future__ import annotations

import json
import time
from pathlib import Path

import gams.transfer as gt
import gamspy_base
import pytest

pytestmark = pytest.mark.golden

RUN = "SSP2-EU21-PkBudg650-AMT_2026-09-28_13.54.59"
RUN_DIR = Path("projects/remind/modeltests/remind/output") / RUN
GOLDEN = Path("status/oncluster/detailed") / f"remind__modeltests__remind__output__{RUN}.json"
SYMBOLS = ["o_iterationNumber", "s80_bool", "p80_repy", "o_modelstat"]


def read_symbols(path: Path, symbols: list[str]) -> gt.Container:
    m = gt.Container(system_directory=gamspy_base.directory)
    m.read(str(path), symbols=symbols)
    return m


def iteration_number(path: Path) -> float:
    value: float = read_symbols(path, ["o_iterationNumber"])["o_iterationNumber"].toValue()
    return value


def select_latest_gdx(run_dir: Path) -> Path:
    """The R selection rule: among the existing candidates pick the highest iteration, first wins on ties."""
    candidates = [run_dir / "fulldata.gdx", run_dir / "non_optimal.gdx"]
    existing = [candidate for candidate in candidates if candidate.exists()]
    assert existing, f"no GDX in {run_dir}"
    iterations = [iteration_number(path) for path in existing]
    return existing[iterations.index(max(iterations))]


def test_selective_read_of_the_fixture_matches_the_r_golden(fixtures_dir: Path, goldens_dir: Path) -> None:
    run_dir = fixtures_dir / "p" / RUN_DIR
    golden_row = json.loads((goldens_dir / GOLDEN).read_text(encoding="utf-8"))["rows"][0]
    assert golden_row["_row"] == RUN

    started = time.perf_counter()
    latest = select_latest_gdx(run_dir)
    m = read_symbols(latest, SYMBOLS)
    elapsed = time.perf_counter() - started

    # R: gsub("0", ".", paste0(o_modelstat, collapse = ""))
    modelstat = str(int(m["o_modelstat"].toValue())).replace("0", ".")
    assert golden_row["modelstat"].startswith(modelstat + ":")
    assert m["o_iterationNumber"].toValue() >= 1
    assert m["s80_bool"].toValue() in (0.0, 1.0)
    records = m["p80_repy"].records
    assert records.columns[-1] == "value"
    assert len(records[records.iloc[:, -2].astype(str) == "modelstat"]) >= 1
    assert elapsed < 30, f"selective read of {latest.name} took {elapsed:.1f} s"
