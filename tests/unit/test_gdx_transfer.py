"""``gams.transfer`` with ``gamspy_base`` reads and writes GDX without a GAMS installation.

Converted from ``migration/harness/smoke/gdx_test.py`` (route A of
``migration/02-python-libraries.md`` section 4.1). Pins the API the port's ``gdx`` module (phase 1)
is built on: selective ``read(symbols=[...])``, scalar ``toValue()``, the records frame with domain
columns followed by ``value``, ``ValueError`` for an absent symbol and for an absent file.
"""

from __future__ import annotations

from pathlib import Path

import gams.transfer as gt
import gamspy_base
import pandas as pd
import pytest

REGIONS = ["CAZ", "CHA", "EUR", "IND", "JPN"]


def container() -> gt.Container:
    return gt.Container(system_directory=gamspy_base.directory)


@pytest.fixture(scope="module")
def gdx_file(tmp_path_factory: pytest.TempPathFactory) -> Path:
    path = tmp_path_factory.mktemp("gdx") / "test.gdx"
    m = container()
    regions = gt.Set(m, "all_regi", records=REGIONS)
    iterations = gt.Set(m, "iteration", records=[str(i) for i in range(1, 4)])
    solveinfo = gt.Set(m, "solveinfo80", records=["modelstat", "solvestat", "resusd", "objval"])
    gt.Parameter(m, "o_iterationNumber", records=3)
    gt.Parameter(m, "s80_bool", records=1)
    gt.Parameter(m, "cm_abortOnConsecFail", records=6)
    gt.Parameter(m, "o_modelstat", records=2)
    repy = pd.DataFrame(
        [[region, "modelstat", 2.0] for region in REGIONS], columns=["all_regi", "solveinfo80", "value"]
    )
    gt.Parameter(m, "p80_repy", [regions, solveinfo], records=repy)
    track = pd.DataFrame([["CAZ", "3", 6.0], ["EUR", "3", 2.0]], columns=["all_regi", "iteration", "value"])
    gt.Parameter(m, "p80_trackConsecFail", [regions, iterations], records=track)
    m.write(str(path))
    assert path.stat().st_size > 0
    return path


def test_scalars_read_back_as_floats(gdx_file: Path) -> None:
    m = gt.Container(str(gdx_file), system_directory=gamspy_base.directory)
    assert m["o_iterationNumber"].toValue() == 3.0
    assert m["s80_bool"].toValue() == 1.0
    assert m["o_modelstat"].toValue() == 2.0
    assert m["cm_abortOnConsecFail"].toValue() == 6.0


def test_records_frame_has_domain_columns_then_value(gdx_file: Path) -> None:
    m = gt.Container(str(gdx_file), system_directory=gamspy_base.directory)
    records = m["p80_repy"].records
    assert records.columns.tolist() == ["all_regi", "solveinfo80", "value"]
    modelstat = records[records.iloc[:, -2].astype(str) == "modelstat"]["value"].tolist()
    assert modelstat == [2.0] * 5
    # R: paste(p80_repy[, , "modelstat"], collapse = "")
    assert "".join(str(int(v)) for v in modelstat) == "22222"


def test_sparse_parameter_keeps_only_stored_records(gdx_file: Path) -> None:
    m = gt.Container(str(gdx_file), system_directory=gamspy_base.directory)
    track = m["p80_trackConsecFail"].records
    assert len(track) == 2  # dense reconstruction is the port's job (02 section 4.1)
    assert track["value"].tolist() == [6.0, 2.0]


def test_selective_read_loads_only_the_named_symbols(gdx_file: Path) -> None:
    m = container()
    m.read(str(gdx_file), symbols=["s80_bool", "o_iterationNumber"])
    assert sorted(m.listSymbols()) == ["o_iterationNumber", "s80_bool"]
    assert m["s80_bool"].toValue() == 1.0


def test_missing_symbol_raises_value_error(gdx_file: Path) -> None:
    m = container()
    with pytest.raises(ValueError, match="does_not_exist"):
        m.read(str(gdx_file), symbols=["does_not_exist"])


def test_missing_file_raises_value_error(tmp_path: Path) -> None:
    with pytest.raises(ValueError):
        gt.Container(str(tmp_path / "nope.gdx"), system_directory=gamspy_base.directory)
