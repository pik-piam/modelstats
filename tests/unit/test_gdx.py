"""modelstats.gdx against gdx2::readGDX (R/getRunStatus.R lines 57, 84, 212-213, 280-288).

Three layers:

* the synthetic matrix of ``gdx_matrix/`` (02 section 4.1) compared with the committed R oracle
  output ``gdx_matrix/r_expected.json`` (runs everywhere, no R needed);
* with R and the oracle packages installed, ``oracle.R`` is re-run to prove the JSON is current and
  that R still aborts on the corrupt files (BUG-037);
* with the fixture tree and R present, the seven status symbols of every real and synthetic
  fixture GDX are read by both sides and compared.

Number comparison uses R's ``as.character()`` text and is exact: R distinguishes ``NaN`` (GAMS
UNDEF, a plain nan here) from ``NA`` (GAMS NA, a nan with the gams.transfer payload that
``modelstats.gdx.is_na`` recognises), also inside the ``p80_repy`` digit string.
"""

from __future__ import annotations

import importlib.util
import json
import math
import shutil
import subprocess
from functools import cache
from pathlib import Path
from types import ModuleType
from typing import Any

import pandas as pd
import pytest

from modelstats.gdx import (
    GdxError,
    GdxFile,
    SymbolInfo,
    is_na,
    open_gdx,
    read_first_found,
    read_param,
    read_scalar,
    read_scalar_strict,
)

MATRIX_DIR = Path(__file__).resolve().parent / "gdx_matrix"
STATUS_SCALARS = ("o_iterationNumber", "s80_bool", "cm_abortOnConsecFail")
SPECIAL_SCALARS = ("sv_eps", "sv_inf", "sv_neginf", "sv_undef", "sv_na")
FIRST_FOUND = ["o_modelstat", "p80_modelstat"]


def _load_matrix_module() -> ModuleType:
    spec = importlib.util.spec_from_file_location("gdx_matrix", MATRIX_DIR / "matrix.py")
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


matrix = _load_matrix_module()
EXPECTED: dict[str, Any] = json.loads((MATRIX_DIR / "r_expected.json").read_text(encoding="utf-8"))
MATRIX_FILES = sorted(p.name for p in MATRIX_DIR.glob("*.gdx"))
VALID_FILES = [name for name in MATRIX_FILES if name not in matrix.CORRUPT]


# --------------------------------------------------------------------------- helpers
def r_chr(value: float) -> str:
    """R ``as.character(<double>)`` for the values that occur (15 significant digits; GAMS NA is ``NA``)."""
    if is_na(value):
        return "NA"
    if math.isnan(value):
        return "NaN"
    if math.isinf(value):
        return "Inf" if value > 0 else "-Inf"
    if value == int(value) and abs(value) < 1e15:
        return str(int(value))
    return f"{value:.15g}"


def r_list(expected: Any) -> list[str] | None:
    """Normalise the oracle's auto-unboxed JSON (a scalar or a list; null = absent) to a list."""
    if expected is None:
        return None
    if isinstance(expected, list):
        return [str(v) for v in expected]
    return [str(expected)]


def same_numbers(python: list[float] | None, expected: Any) -> bool:
    """Python floats against R text, exactly (R "NA" only equals a GAMS NA, "NaN" only a plain nan)."""
    r_values = r_list(expected)
    if python is None or r_values is None:
        return python is None and r_values is None
    if len(python) != len(r_values):
        return False
    return all(r_chr(p) == r for p, r in zip(python, r_values, strict=True))


def modelstat_string(frame: pd.DataFrame) -> str:
    """``paste(p80_repy[, , "modelstat"], collapse = "")`` from the dense frame (nan as "NaN", GAMS NA as "NA")."""
    rows = frame[frame.iloc[:, -2] == "modelstat"]
    return "".join(r_chr(v) for v in rows["value"])


def first_found_string(values: list[float] | None) -> str:
    """``gsub("0", ".", paste0(o_modelstat, collapse = ""))`` (getRunStatus.R:86)."""
    return "".join(r_chr(v) for v in values or []).replace("0", ".")


@cache
def rscript_with_oracle_packages() -> str | None:
    """Path of Rscript when gdx2, quitte and jsonlite load, else None."""
    rscript = shutil.which("Rscript")
    if rscript is None:
        return None
    probe = subprocess.run(
        [rscript, "-e", "suppressMessages({library(gdx2); library(quitte); library(jsonlite)})"],
        capture_output=True,
        text=True,
    )
    return rscript if probe.returncode == 0 else None


def require_r() -> str:
    rscript = rscript_with_oracle_packages()
    if rscript is None:
        pytest.skip("Rscript with gdx2, quitte and jsonlite is not available: R oracle comparison skipped")
    return rscript


def assert_matches_oracle(gdx: GdxFile, expected: dict[str, Any]) -> None:
    """Every status read of one file equals the R oracle entry."""
    for name in STATUS_SCALARS + SPECIAL_SCALARS:
        python = read_scalar(gdx, name)
        assert same_numbers(None if python is None else [python], expected.get(name)), (gdx.path, name, python)
    first_found = read_first_found(gdx, FIRST_FOUND)
    assert not isinstance(expected.get("first_found"), dict), (gdx.path, expected["first_found"])
    assert same_numbers(first_found, expected.get("first_found")), (gdx.path, first_found)
    if first_found is not None:
        assert first_found_string(first_found) == expected["modelstat_string"], gdx.path
    for name in ("p80_repy", "p80_trackConsecFail", "p80_modelstat", "p2"):
        r_param = expected.get(name)
        frame = read_param(gdx, name)
        if r_param is None:
            assert frame is None, (gdx.path, name)
            continue
        assert frame is not None, (gdx.path, name)
        assert len(frame) == math.prod(r_param["dim"]), (gdx.path, name)
        assert same_numbers(gdx.values(name), r_param["values"]), (gdx.path, name, gdx.values(name))
        if name == "p80_repy":
            assert not isinstance(r_param["modelstat_string"], dict), (gdx.path, r_param["modelstat_string"])
            assert modelstat_string(frame) == r_param["modelstat_string"], gdx.path
        if name == "p80_trackConsecFail":
            assert frame.iloc[:, 0].tolist() == r_param["quitte"]["region"], gdx.path
            assert same_numbers(frame["value"].tolist(), r_param["quitte"]["value"]), gdx.path


# --------------------------------------------------------------------------- the matrix against R
def test_matrix_is_complete() -> None:
    assert set(MATRIX_FILES) == set(EXPECTED), "r_expected.json and the GDX files of gdx_matrix/ disagree"
    assert {"full.gdx", "universe.gdx", "special.gdx", "empty_records.gdx", *matrix.CORRUPT} <= set(MATRIX_FILES)


@pytest.mark.parametrize("name", VALID_FILES)
def test_matrix_file_matches_r(name: str) -> None:
    assert_matches_oracle(GdxFile(MATRIX_DIR / name), EXPECTED[name])


@pytest.mark.parametrize("name", list(matrix.CORRUPT))
def test_corrupt_file_raises_where_r_aborts(name: str) -> None:
    path = MATRIX_DIR / name
    with pytest.raises(GdxError):
        GdxFile(path)
    for reader in (read_scalar, read_scalar_strict):
        with pytest.raises(GdxError):
            reader(path, "o_iterationNumber")
    with pytest.raises(GdxError):
        read_first_found(path, FIRST_FOUND)
    with pytest.raises(GdxError):
        read_param(path, "p80_repy")
    # BUG-037: the R process dies with a signal (SIGABRT or SIGSEGV) on the same file
    assert EXPECTED[name]["r_process"]["returncode"] < 0, EXPECTED[name]


def test_missing_file_raises() -> None:
    with pytest.raises(GdxError, match="No such file"):
        read_scalar(MATRIX_DIR / "does-not-exist.gdx", "o_iterationNumber")


def test_r_oracle_is_current() -> None:
    rscript = require_r()
    fresh = matrix.run_oracle([MATRIX_DIR / name for name in MATRIX_FILES], rscript)
    for name in VALID_FILES:
        assert fresh[name] == EXPECTED[name], f"{name}: rerun python tests/unit/gdx_matrix/matrix.py"
    for name in matrix.CORRUPT:
        assert fresh[name]["r_process"]["returncode"] < 0, (name, fresh[name])


# --------------------------------------------------------------------------- semantics pinned without R
def test_absent_symbol_is_none_and_strict_raises() -> None:
    gdx = GdxFile(MATRIX_DIR / "noiter.gdx")
    assert read_scalar(gdx, "o_iterationNumber") is None
    with pytest.raises(GdxError, match="no symbol o_iterationNumber"):
        read_scalar_strict(gdx, "o_iterationNumber")
    assert read_scalar_strict(gdx, "s80_bool") == 0.0
    assert read_param(gdx, "p80_repy") is None
    assert read_first_found(gdx, ["p80_modelstat"]) is None


def test_first_found_takes_the_first_existing_name() -> None:
    assert read_first_found(MATRIX_DIR / "full.gdx", FIRST_FOUND) == [2.0]
    assert read_first_found(MATRIX_DIR / "first_found_second.gdx", FIRST_FOUND) == [2.0, 2.0, 0.0, 2.0]
    assert read_first_found(MATRIX_DIR / "first_found_second.gdx", ["o_modelstat", "o_modelstat"]) is None
    with pytest.raises(ValueError):
        read_first_found(MATRIX_DIR / "full.gdx", [])


def test_scalar_readers_reject_non_scalars() -> None:
    gdx = GdxFile(MATRIX_DIR / "full.gdx")
    with pytest.raises(GdxError, match="not a scalar parameter"):
        read_scalar(gdx, "p80_repy")
    with pytest.raises(GdxError, match="not a scalar parameter"):
        read_scalar(gdx, "all_regi")
    with pytest.raises(GdxError, match="not a Parameter"):
        read_first_found(gdx, ["all_regi"])


def test_dense_frame_layout() -> None:
    frame = read_param(MATRIX_DIR / "sparse_zeros.gdx", "p80_repy")
    assert frame is not None
    assert list(frame.columns) == ["all_regi", "solveinfo80", "value"]
    assert len(frame) == 12 * 4
    assert frame.iloc[:, 0].drop_duplicates().tolist() == matrix.REGIONS
    assert frame.iloc[:, 1].drop_duplicates().tolist() == matrix.SOLVEINFO
    assert frame["value"].dtype == "float64"
    assert modelstat_string(frame) == "000200004000"
    track = read_param(MATRIX_DIR / "full.gdx", "p80_trackConsecFail")
    assert track is not None
    assert track["value"].tolist() == [0.0, 0.0, 0.0, 2.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0]


def test_universe_domain_keeps_record_order_with_na_holes() -> None:
    frame = read_param(MATRIX_DIR / "universe.gdx", "p80_repy")
    assert frame is not None
    assert list(frame.columns) == ["uni_0", "uni_1", "value"]
    assert frame.iloc[:, 0].tolist() == ["EUR", "EUR", "CHA", "CHA"]
    # R: the missing combinations are NA (not NaN), as the oracle's "2NA" modelstat string shows
    assert [r_chr(v) for v in frame["value"]] == ["2", "NA", "NA", "7"]
    assert [is_na(v) for v in frame["value"]] == [False, True, True, False]


def test_special_values_map_like_gamstransfer() -> None:
    gdx = GdxFile(MATRIX_DIR / "special.gdx")
    eps = read_scalar(gdx, "sv_eps")
    assert eps == 0.0 and math.copysign(1.0, eps) == 1.0, "EPS must be a positive zero like R's 0"
    assert read_scalar(gdx, "sv_inf") == math.inf
    assert read_scalar(gdx, "sv_neginf") == -math.inf
    undef, na = read_scalar(gdx, "sv_undef"), read_scalar(gdx, "sv_na")
    assert undef is not None and na is not None and math.isnan(undef) and math.isnan(na)
    assert not is_na(undef) and is_na(na)  # R: NaN versus NA


def test_zero_record_symbols_read_as_zeros() -> None:
    gdx = GdxFile(MATRIX_DIR / "empty_records.gdx")
    assert read_scalar(gdx, "o_iterationNumber") == 0.0
    assert read_first_found(gdx, FIRST_FOUND) == [0.0]
    repy = read_param(gdx, "p80_repy")
    assert repy is not None and len(repy) == 48 and (repy["value"] == 0.0).all()


def test_values_follow_the_file_order_not_sorted() -> None:
    gdx = GdxFile(MATRIX_DIR / "years.gdx")
    assert gdx.values("p80_modelstat") == [3.0, 2.0, 7.0]
    gdx = GdxFile(MATRIX_DIR / "twod.gdx")
    # R c(<magpie>): first domain fastest
    assert gdx.values("p2") == [11.0, 21.0, 0.0, 12.0, 0.0, 32.0]
    frame = gdx.frame("p2")
    assert frame is not None
    # the frame: first domain slowest
    assert frame["value"].tolist() == [11.0, 12.0, 21.0, 0.0, 0.0, 32.0]


def test_symbol_table_and_case_insensitive_lookup() -> None:
    gdx = GdxFile(MATRIX_DIR / "alias.gdx")
    assert gdx.has("P80_REPY") and gdx.has("regi")
    info = gdx.info("REGI")
    assert info == SymbolInfo(name="regi", kind="Alias", dimension=1, domain_names=("*",), alias_with="all_regi")
    assert gdx.info("nothing") is None
    assert read_scalar(MATRIX_DIR / "full.gdx", "O_ITERATIONNUMBER") == 5.0


def test_open_gdx_caches_per_file_object_only() -> None:
    gdx = GdxFile(MATRIX_DIR / "full.gdx")
    assert open_gdx(gdx) is gdx
    assert open_gdx(MATRIX_DIR / "full.gdx") is not gdx
    first = read_param(gdx, "p80_repy")
    assert "p80_repy" in gdx._container and "all_regi" in gdx._container
    assert gdx._dense("p80_repy") is gdx._dense("p80_repy")
    second = read_param(gdx, "p80_repy")
    assert first is not None and second is not None and first.equals(second)
    assert repr(gdx).startswith("GdxFile(")


# --------------------------------------------------------------------------- the real fixture files
def fixture_gdx_files(fixtures: Path, synthetic: Path) -> list[Path]:
    """`find migration/fixtures/p migration/synthetic/p -name '*.gdx' -size +0`."""
    files: list[Path] = []
    for root in (fixtures / "p", synthetic / "p"):
        if root.is_dir():
            files.extend(p for p in root.rglob("*.gdx") if p.is_file() and p.stat().st_size > 0)
    return sorted(files)


def test_every_fixture_gdx_matches_r(paths: Any, fixtures_available: bool) -> None:
    if not fixtures_available:
        pytest.skip("fixture tree absent: real GDX comparison skipped")
    rscript = require_r()
    files = fixture_gdx_files(paths.fixtures, paths.synthetic)
    assert files, "no GDX file found under the fixture tree"
    readable: dict[Path, GdxFile] = {}
    corrupt: list[Path] = []
    for file in files:
        try:
            readable[file] = GdxFile(file)
        except GdxError:
            corrupt.append(file)
    expected = matrix.oracle_process(list(readable), rscript)
    for file, gdx in readable.items():
        assert "r_process" not in expected[str(file)], expected[str(file)]
        assert_matches_oracle(gdx, expected[str(file)])
    # the synthetic gdx-broken case: a text file named fulldata.gdx; R aborts (BUG-037), Python raised above
    assert [p.parent.name for p in corrupt] == ["gdx-broken"], corrupt
    for file in corrupt:
        result = matrix.oracle_process([file], rscript)[str(file)]
        assert result["r_process"]["returncode"] < 0, result
