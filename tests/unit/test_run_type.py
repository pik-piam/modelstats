"""``modelstats.run_type.col_run_type`` against the behaviour of ``R/colRunType.R``.

The config discovery runs through the real ``modelstats.config.config_matches`` over a
temporary directory; the loader is stubbed with JSON files so that no R data file is needed
(the golden runner exercises the real ``load_config`` path). One test writes a real
``config.yml`` and goes through the real YAML loader.
"""

from __future__ import annotations

import json
import os
from pathlib import Path
from typing import Any

import pytest

from modelstats import run_type
from modelstats.env import Effects, FileStat
from modelstats.errors import RParityError
from modelstats.run_type import col_run_type

REPO = Path(__file__).resolve().parents[2]
RUNTYPE_GOLDENS = REPO / "migration" / "goldens" / "runtype"


class FsEffects(Effects):
    """A read-only Effects over the real filesystem (what the tests need of it)."""

    def listdir_like_r(self, directory: str | os.PathLike[str]) -> list[str]:
        try:
            names = os.listdir(directory)
        except FileNotFoundError, NotADirectoryError:
            return []
        return sorted(name for name in names if not name.startswith("."))

    def exists(self, path: str | os.PathLike[str]) -> bool:
        return os.path.exists(path)

    def is_dir(self, path: str | os.PathLike[str]) -> bool:
        return os.path.isdir(path)

    def stat(self, path: str | os.PathLike[str]) -> FileStat:
        st = os.stat(path)
        return FileStat(mtime=st.st_mtime, ctime=st.st_ctime, size=st.st_size)

    def read_text(self, path: str | os.PathLike[str]) -> str:
        return Path(path).read_text(encoding="utf-8")

    def read_bytes(self, path: str | os.PathLike[str]) -> bytes:
        return Path(path).read_bytes()

    def glob(self, pattern: str | os.PathLike[str]) -> list[str]:
        raise NotImplementedError

    def run(self, argv: Any, cwd: Any = None, env: Any = None, input: Any = None) -> Any:  # noqa: A002
        raise NotImplementedError

    def now(self) -> Any:
        raise NotImplementedError

    def today(self) -> Any:
        raise NotImplementedError

    @property
    def user(self) -> str:
        return "tester"

    @property
    def on_cluster(self) -> bool:
        return False

    def getenv(self, name: str, default: str = "") -> str:
        return default


@pytest.fixture
def effects() -> FsEffects:
    return FsEffects()


@pytest.fixture
def json_loader(monkeypatch: pytest.MonkeyPatch) -> list[str]:
    """Stub ``load_config`` with JSON files; returns the list of paths it was asked to load."""
    loaded: list[str] = []

    def _load(path: str, effects: Effects) -> dict[str, Any]:
        loaded.append(path)
        with open(path, encoding="utf-8") as handle:
            data: dict[str, Any] = json.load(handle)
        return data

    monkeypatch.setattr(run_type, "_load_config", _load)
    return loaded


def remind(tmp_path: Path, name: str = "config.Rdata", **gms: Any) -> str:
    """A REMIND-style run folder whose config (JSON for the stub) holds ``gms`` settings."""
    cfg = {"model_name": "REMIND", "gms": {"optimization": "nash", **gms}}
    (tmp_path / name).write_text(json.dumps(cfg), encoding="utf-8")
    return str(tmp_path)


# --- MAgPIE and REMIND compositions -------------------------------------------------------------


def test_magpie_returns_gms_optimization(tmp_path: Path, effects: FsEffects, json_loader: list[str]) -> None:
    cfg = {"model_name": "MAgPIE", "gms": {"optimization": "nlp_apr17", "cm_nash_mode": "debug"}}
    (tmp_path / "config.yml").write_text(json.dumps(cfg), encoding="utf-8")
    assert col_run_type(str(tmp_path), effects) == "nlp_apr17"
    assert json_loader == [f"{tmp_path}/config.yml"]


def test_magpie_na_optimization_is_a_real_na(tmp_path: Path, effects: FsEffects, json_loader: list[str]) -> None:
    # R: out <- cfg$gms$optimization is NA_character_, which the harness writes as {"value": null}
    cfg = {"model_name": "MAgPIE", "gms": {"optimization": None}}
    (tmp_path / "config.Rdata").write_text(json.dumps(cfg), encoding="utf-8")
    assert col_run_type(str(tmp_path), effects) is None


def test_magpie_through_the_real_yaml_loader(tmp_path: Path, effects: FsEffects) -> None:
    (tmp_path / "config.yml").write_text(
        "model_name: MAgPIE\ngms:\n  optimization: nlp_apr17\n  c_timesteps: coup2110\n", encoding="utf-8"
    )
    assert col_run_type(str(tmp_path), effects) == "nlp_apr17"


@pytest.mark.parametrize(
    ("gms", "expected"),
    [
        ({}, "nash"),
        ({"optimization": "negishi"}, "negishi"),
        ({"cm_nash_mode": "debug"}, "nash debug"),
        ({"cm_nash_mode": 1}, "nash debug"),
        ({"cm_nash_mode": 1.0}, "nash debug"),
        ({"cm_nash_mode": "1"}, "nash debug"),
        ({"cm_nash_mode": 2}, "nash"),
        ({"cm_nash_mode": "parallel"}, "nash"),
        ({"cm_nash_mode": None}, "nash"),
        ({"CES_parameters": "calibrate"}, "Calib_nash"),
        ({"CES_parameters": "load"}, "nash"),
        ({"CES_parameters": "calibrate", "cm_nash_mode": "debug"}, "Calib_nash debug"),
        ({"cm_MAgPIE_coupling": "on"}, "nash + mag"),
        ({"cm_MAgPIE_coupling": "off"}, "nash"),
        ({"cm_MAgPIE_Nash": 1}, "nash + mag"),
        ({"cm_MAgPIE_Nash": 0}, "nash"),
        ({"cm_MAgPIE_Nash": "1"}, "nash + mag"),
        ({"cm_MAgPIE_coupling": "on", "CES_parameters": "calibrate"}, "Calib_nash + mag"),
        ({"cm_MAgPIE_coupling": "on", "CES_parameters": "calibrate", "cm_nash_mode": 1}, "Calib_nash debug + mag"),
        ({"optimization": "testOneRegi", "c_testOneRegi_region": "EUR"}, "testOneRegi EUR"),
        ({"optimization": "testOneRegi", "c_testOneRegi_region": "EUR", "cm_nash_mode": "debug"}, "debug EUR"),
        ({"optimization": "testOneRegi", "c_testOneRegi_region": "EUR", "cm_nash_mode": 1}, "debug EUR"),
        ({"optimization": "testOneRegi", "c_testOneRegi_region": "EUR", "cm_quick_mode": "on"}, "quick EUR"),
        (
            {"optimization": "testOneRegi", "c_testOneRegi_region": "EUR", "cm_quick_mode": "on", "cm_nash_mode": 1},
            "debug EUR",
        ),
        (
            {"optimization": "testOneRegi", "c_testOneRegi_region": "USA", "CES_parameters": "calibrate"},
            "testOneRegi USA",
        ),
        ({"optimization": "testOneRegi", "c_testOneRegi_region": "EUR", "cm_MAgPIE_coupling": "on"}, "testOneRegi EUR"),
        ({"optimization": "testOneRegi"}, "testOneRegi "),  # paste(mode, NULL) keeps the separator
        ({"optimization": "testOneRegi", "c_testOneRegi_region": []}, "testOneRegi "),
        ({"optimization": "testOneRegiX", "c_testOneRegi_region": "EUR"}, "testOneRegi EUR"),
        ({"c_empty_model": "on"}, "empty model"),
        ({"c_empty_model": "on", "CES_parameters": "calibrate", "cm_MAgPIE_coupling": "on"}, "empty model"),
        ({"optimization": "testOneRegi", "c_testOneRegi_region": "EUR", "c_empty_model": "on"}, "empty model"),
        ({"c_empty_model": "off"}, "nash"),
        # an NA optimization: paste() turns it into text, otherwise it stays a real NA (None)
        ({"optimization": None}, None),
        ({"optimization": None, "cm_nash_mode": "debug"}, "NA debug"),
        ({"optimization": None, "CES_parameters": "calibrate"}, "Calib_NA"),
        ({"optimization": None, "cm_MAgPIE_coupling": "on"}, "NA + mag"),
        ({"optimization": None, "c_empty_model": "on"}, "empty model"),
        ({"optimization": "NA"}, "NA"),  # the string, not NA
    ],
)
def test_remind_composition(
    tmp_path: Path, effects: FsEffects, json_loader: list[str], gms: dict[str, Any], expected: str | None
) -> None:
    assert col_run_type(remind(tmp_path, **gms), effects) == expected


def test_vectors_of_length_one_behave_like_scalars(tmp_path: Path, effects: FsEffects, json_loader: list[str]) -> None:
    cfg = {
        "model_name": ["REMIND"],
        "gms": {"optimization": ["nash"], "cm_nash_mode": [1.0], "CES_parameters": ["calibrate"]},
    }
    (tmp_path / "config.Rdata").write_text(json.dumps(cfg), encoding="utf-8")
    assert col_run_type(str(tmp_path), effects) == "Calib_nash debug"


def test_longer_vectors_are_never_true(tmp_path: Path, effects: FsEffects, json_loader: list[str]) -> None:
    # isTRUE(c(TRUE, TRUE)) is FALSE: a length-2 cm_MAgPIE_coupling does not add " + mag"
    assert col_run_type(remind(tmp_path, cm_MAgPIE_coupling=["on", "on"]), effects) == "nash"


def test_numpy_scalars_are_unwrapped(tmp_path: Path, effects: FsEffects, monkeypatch: pytest.MonkeyPatch) -> None:
    np = pytest.importorskip("numpy")
    (tmp_path / "config.Rdata").write_text("", encoding="utf-8")

    def _load(path: str, eff: Effects) -> dict[str, Any]:
        gms = {"optimization": np.array(["nash"]), "cm_nash_mode": np.int64(1), "CES_parameters": np.array("calibrate")}
        return {"model_name": np.array(["REMIND"]), "gms": gms}

    monkeypatch.setattr(run_type, "_load_config", _load)
    assert col_run_type(str(tmp_path), effects) == "Calib_nash debug"


# --- error paths (BUG-003, BUG-004, BUG-017) ----------------------------------------------------


def golden_error(case: str) -> str | None:
    path = RUNTYPE_GOLDENS / f"{case}.json"
    if not path.is_file():
        return None
    with open(path, encoding="utf-8") as handle:
        error: str = json.load(handle)["error"]
    return error


def test_no_config_in_an_existing_directory_errors_like_r(tmp_path: Path, effects: FsEffects) -> None:
    (tmp_path / "full.gms").write_text("", encoding="utf-8")
    with pytest.raises(RParityError) as excinfo:
        col_run_type(str(tmp_path), effects)
    assert str(excinfo.value) == "argument is of length zero"
    golden = golden_error("synthetic__remind__output__noconfig")
    if golden is not None:
        assert str(excinfo.value) == golden


def test_full_lst_fallback_is_unreachable(tmp_path: Path, effects: FsEffects) -> None:
    # BUG-017: "<dir>/" exists, so the full.lst branch is never reached
    (tmp_path / "full.lst").write_text(" setGlobal optimization  nash         !! def = nash\n", encoding="utf-8")
    with pytest.raises(RParityError, match="^argument is of length zero$"):
        col_run_type(str(tmp_path), effects)
    golden = golden_error("synthetic__remind__output__noconfig-fulllst")
    if golden is not None:
        assert golden == "argument is of length zero"


def test_nonexistent_path_is_na(tmp_path: Path, effects: FsEffects) -> None:
    assert col_run_type(str(tmp_path / "nowhere"), effects) == "NA"


def test_regular_file_is_na(tmp_path: Path, effects: FsEffects) -> None:
    (tmp_path / "afile").write_text("", encoding="utf-8")
    assert col_run_type(str(tmp_path / "afile"), effects) == "NA"


def test_dot_files_are_not_configs(tmp_path: Path, effects: FsEffects, json_loader: list[str]) -> None:
    (tmp_path / ".config.Rdata").write_text("{}", encoding="utf-8")
    with pytest.raises(RParityError, match="^argument is of length zero$"):
        col_run_type(str(tmp_path), effects)


def test_two_matches_error_like_r(tmp_path: Path, effects: FsEffects, json_loader: list[str]) -> None:
    remind(tmp_path)
    (tmp_path / "config.Rdata.bak").write_text("{}", encoding="utf-8")
    with pytest.raises(RParityError) as excinfo:
        col_run_type(str(tmp_path), effects)
    assert str(excinfo.value) == "the condition has length > 1"
    golden = golden_error("synthetic__remind__output__config-bak")
    if golden is not None:
        assert str(excinfo.value) == golden
    assert json_loader == []


def test_two_yml_matches_error_the_same_way(tmp_path: Path, effects: FsEffects, json_loader: list[str]) -> None:
    (tmp_path / "config.yml").write_text("{}", encoding="utf-8")
    (tmp_path / "old_config.yml").write_text("{}", encoding="utf-8")
    with pytest.raises(RParityError, match="^the condition has length > 1$"):
        col_run_type(str(tmp_path), effects)


def test_unanchored_match_loads_literal_config_yml(tmp_path: Path, effects: FsEffects, json_loader: list[str]) -> None:
    # BUG-003: "old_config.yml" matches, but R loads file.path(mydir, "config.yml"), which is absent
    (tmp_path / "old_config.yml").write_text(json.dumps({"model_name": "MAgPIE", "gms": {"optimization": "x"}}))
    with pytest.raises(FileNotFoundError):
        col_run_type(str(tmp_path), effects)
    assert json_loader == [f"{tmp_path}/config.yml"]


def test_unanchored_rdata_match_loads_that_name(tmp_path: Path, effects: FsEffects, json_loader: list[str]) -> None:
    remind(tmp_path, name="config.Rdata.bak")
    assert col_run_type(str(tmp_path), effects) == "nash"
    assert json_loader == [f"{tmp_path}/config.Rdata.bak"]


def test_config_without_model_name(tmp_path: Path, effects: FsEffects, json_loader: list[str]) -> None:
    (tmp_path / "config.Rdata").write_text(json.dumps({"gms": {"optimization": "nash"}}), encoding="utf-8")
    with pytest.raises(RParityError, match="^argument is of length zero$"):
        col_run_type(str(tmp_path), effects)


def test_empty_rdata_cfg(tmp_path: Path, effects: FsEffects, json_loader: list[str]) -> None:
    # load() of a file without a `cfg` object leaves cfg NULL (load_config returns {})
    (tmp_path / "config.Rdata").write_text("{}", encoding="utf-8")
    with pytest.raises(RParityError, match="^argument is of length zero$"):
        col_run_type(str(tmp_path), effects)


def test_model_name_na(tmp_path: Path, effects: FsEffects, json_loader: list[str]) -> None:
    (tmp_path / "config.Rdata").write_text(json.dumps({"model_name": None, "gms": {"optimization": "nash"}}))
    with pytest.raises(RParityError, match="^missing value where TRUE/FALSE needed$"):
        col_run_type(str(tmp_path), effects)


def test_model_name_vector_of_two(tmp_path: Path, effects: FsEffects, json_loader: list[str]) -> None:
    (tmp_path / "config.Rdata").write_text(json.dumps({"model_name": ["REMIND", "MAgPIE"], "gms": {}}))
    with pytest.raises(RParityError, match="^the condition has length > 1$"):
        col_run_type(str(tmp_path), effects)


def test_remind_without_optimization(tmp_path: Path, effects: FsEffects, json_loader: list[str]) -> None:
    # grepl("^testOneRegi", NULL) is logical(0): the `if` fails
    (tmp_path / "config.Rdata").write_text(json.dumps({"model_name": "REMIND", "gms": {}}), encoding="utf-8")
    with pytest.raises(RParityError, match="^argument is of length zero$"):
        col_run_type(str(tmp_path), effects)


def test_default_effects_are_used_when_none_are_given(tmp_path: Path, json_loader: list[str]) -> None:
    assert col_run_type(remind(tmp_path, cm_nash_mode="debug")) == "nash debug"


@pytest.mark.skipif(not RUNTYPE_GOLDENS.is_dir(), reason="migration/goldens is absent")
def test_golden_value_set_is_covered() -> None:
    """Every distinct value and error of the 110 runtype goldens is one the port can produce."""
    seen: set[str] = set()
    for path in RUNTYPE_GOLDENS.glob("*.json"):
        with open(path, encoding="utf-8") as handle:
            data = json.load(handle)
        seen.add(f"error:{data['error']}" if "error" in data else f"value:{data['value']}")
    assert seen <= {
        "value:nash",
        "value:nlp_apr17",
        "value:nash + mag",
        "value:Calib_nash",
        "value:testOneRegi EUR",
        "value:debug EUR",
        "value:NA",
        "error:argument is of length zero",
        "error:the condition has length > 1",
    }
