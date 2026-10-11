"""``col_run_type`` reproduces ``R/colRunType.R`` on synthetic configurations and directories.

The configuration loader is injected (``FakeConfig`` carries the ``cfg`` fields like the R list), so
these tests exercise the composition rules of ``R/colRunType.R:20-35`` and the discovery rules of lines
14-19 on temporary directories through ``ProductionEffects``, without reading any fixture. Two tests
read the R-written files under ``tests/data/r`` with ``modelstats.config.load_config``; R gives
``"nash"`` for that ``config.Rdata`` and ``"nlp_apr17"`` for that ``config.yml`` (verified with
``colRunType()`` on copies of the files).
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import pytest

from modelstats.config import load_config
from modelstats.env import Effects, ProductionEffects
from modelstats.rdata_io import RScalar
from modelstats.run_type import (
    ARGUMENT_OF_LENGTH_ZERO,
    CONDITION_LENGTH_ABOVE_ONE,
    RunTypeError,
    col_run_type,
    compose_run_type,
    config_path_for,
)


@dataclass(frozen=True)
class FakeConfig:
    model_name: RScalar = None
    optimization: RScalar = None
    cm_nash_mode: RScalar = None
    cm_quick_mode: RScalar = None
    c_testOneRegi_region: RScalar = None  # noqa: N815 - the R spelling
    CES_parameters: RScalar = None  # noqa: N815 - the R spelling
    cm_MAgPIE_coupling: RScalar = None  # noqa: N815 - the R spelling
    cm_MAgPIE_Nash: RScalar = None  # noqa: N815 - the R spelling
    c_empty_model: RScalar = None


class RecordingLoader:
    """A loader that returns one configuration and records the paths it was asked for."""

    def __init__(self, cfg: FakeConfig) -> None:
        self.cfg = cfg
        self.paths: list[Path] = []

    def __call__(self, path: Path) -> FakeConfig:
        self.paths.append(path)
        return self.cfg


def remind(**switches: RScalar) -> FakeConfig:
    return FakeConfig("REMIND", **{"optimization": "nash", **switches})


NASH = remind()
MAGPIE = FakeConfig("MAgPIE", "nlp_apr17")


@pytest.fixture(scope="module")
def env() -> Effects:
    return ProductionEffects()


# --- discovery: R/colRunType.R:13-19 --------------------------------------------------------------


def test_directory_without_config_raises_the_r_message(tmp_path: Path, env: Effects) -> None:
    (tmp_path / "full.gms").write_text("", encoding="utf-8")
    loader = RecordingLoader(NASH)
    with pytest.raises(RunTypeError) as excinfo:
        col_run_type(tmp_path, env, loader)
    assert str(excinfo.value) == ARGUMENT_OF_LENGTH_ZERO  # BUG-004
    assert loader.paths == []


def test_two_matching_names_raise_the_r_message(tmp_path: Path, env: Effects) -> None:
    (tmp_path / "config.Rdata").write_bytes(b"")
    (tmp_path / "config.Rdata.bak").write_bytes(b"")
    loader = RecordingLoader(NASH)
    with pytest.raises(RunTypeError) as excinfo:
        col_run_type(tmp_path, env, loader)
    assert str(excinfo.value) == CONDITION_LENGTH_ABOVE_ONE  # BUG-003
    assert loader.paths == []


def test_dot_files_are_invisible_like_in_r_dir(tmp_path: Path, env: Effects) -> None:
    (tmp_path / ".config.yml").write_text("", encoding="utf-8")
    with pytest.raises(RunTypeError, match=ARGUMENT_OF_LENGTH_ZERO):
        col_run_type(tmp_path, env, RecordingLoader(MAGPIE))


def test_unanchored_pattern_matches_old_config_yml_but_loads_the_literal_config_yml(
    tmp_path: Path, env: Effects
) -> None:
    (tmp_path / "old_config.yml").write_text("", encoding="utf-8")
    loader = RecordingLoader(MAGPIE)
    assert col_run_type(tmp_path, env, loader) == "nlp_apr17"
    assert loader.paths == [tmp_path / "config.yml"]  # BUG-003: the literal name, not the match


def test_rdata_match_is_loaded_under_its_matched_name(tmp_path: Path, env: Effects) -> None:
    (tmp_path / "config.Rdata.bak").write_bytes(b"")
    loader = RecordingLoader(NASH)
    assert col_run_type(tmp_path, env, loader) == "nash"
    assert loader.paths == [tmp_path / "config.Rdata.bak"]


def test_pattern_dot_matches_any_character(tmp_path: Path, env: Effects) -> None:
    (tmp_path / "configXRdata").write_bytes(b"")
    loader = RecordingLoader(NASH)
    assert col_run_type(tmp_path, env, loader) == "nash"
    assert loader.paths == [tmp_path / "configXRdata"]


def test_a_sub_directory_named_config_yml_is_listed_too(tmp_path: Path, env: Effects) -> None:
    (tmp_path / "config.yml").mkdir()
    loader = RecordingLoader(MAGPIE)
    assert col_run_type(tmp_path, env, loader) == "nlp_apr17"
    assert loader.paths == [tmp_path / "config.yml"]


def test_missing_directory_is_na(tmp_path: Path, env: Effects) -> None:
    loader = RecordingLoader(NASH)
    assert col_run_type(tmp_path / "nope", env, loader) == "NA"
    assert loader.paths == []


def test_a_file_path_is_na(tmp_path: Path, env: Effects) -> None:
    target = tmp_path / "config.Rdata"
    target.write_bytes(b"")
    assert col_run_type(target, env, RecordingLoader(NASH)) == "NA"


def test_relative_paths_resolve_against_the_working_directory(
    tmp_path: Path, env: Effects, monkeypatch: pytest.MonkeyPatch
) -> None:
    (tmp_path / "run").mkdir()
    (tmp_path / "run" / "config.Rdata").write_bytes(b"")
    monkeypatch.chdir(tmp_path)
    loader = RecordingLoader(NASH)
    assert col_run_type("run", env, loader) == "nash"
    assert loader.paths == [Path("run/config.Rdata")]


def test_config_path_for() -> None:
    assert config_path_for("/run", "config.yml") == Path("/run/config.yml")
    assert config_path_for("/run", "my_config.yml.bak") == Path("/run/config.yml")
    assert config_path_for("/run", "config.Rdata") == Path("/run/config.Rdata")
    assert config_path_for("/run", "config.Rdata~") == Path("/run/config.Rdata~")


# --- composition: R/colRunType.R:20-35 ------------------------------------------------------------


@pytest.mark.parametrize(
    ("switches", "expected"),
    [
        ({}, "nash"),
        ({"optimization": "negishi"}, "negishi"),
        ({"cm_nash_mode": "debug"}, "nash debug"),
        ({"cm_nash_mode": 1}, "nash debug"),
        ({"cm_nash_mode": "1"}, "nash debug"),
        ({"cm_nash_mode": 1.0}, "nash debug"),
        ({"cm_nash_mode": True}, "nash debug"),
        ({"cm_nash_mode": "2"}, "nash"),
        ({"cm_nash_mode": 2}, "nash"),
        ({"cm_nash_mode": "parallel"}, "nash"),
        ({"cm_nash_mode": "1.0"}, "nash"),
        ({"cm_nash_mode": None}, "nash"),
        ({"CES_parameters": "calibrate"}, "Calib_nash"),
        ({"CES_parameters": "load"}, "nash"),
        ({"CES_parameters": "calibrate", "cm_nash_mode": "debug"}, "Calib_nash debug"),
        ({"cm_MAgPIE_coupling": "on"}, "nash + mag"),
        ({"cm_MAgPIE_coupling": "off"}, "nash"),
        ({"cm_MAgPIE_Nash": 1}, "nash + mag"),
        ({"cm_MAgPIE_Nash": "1"}, "nash + mag"),
        ({"cm_MAgPIE_Nash": "0"}, "nash"),
        ({"cm_MAgPIE_Nash": 0}, "nash"),
        ({"cm_MAgPIE_coupling": "on", "cm_MAgPIE_Nash": 1}, "nash + mag"),
        ({"CES_parameters": "calibrate", "cm_nash_mode": 1, "cm_MAgPIE_Nash": 1}, "Calib_nash debug + mag"),
        ({"c_empty_model": "on"}, "empty model"),
        ({"c_empty_model": "on", "CES_parameters": "calibrate", "cm_MAgPIE_Nash": 1}, "empty model"),
        ({"c_empty_model": "off"}, "nash"),
        ({"optimization": "testOneRegi", "c_testOneRegi_region": "EUR"}, "testOneRegi EUR"),
        (
            {"optimization": "testOneRegi", "c_testOneRegi_region": "EUR", "cm_nash_mode": "debug"},
            "debug EUR",
        ),
        ({"optimization": "testOneRegi", "c_testOneRegi_region": "EUR", "cm_nash_mode": 1}, "debug EUR"),
        ({"optimization": "testOneRegi", "c_testOneRegi_region": "EUR", "cm_quick_mode": "on"}, "quick EUR"),
        (
            {
                "optimization": "testOneRegi",
                "c_testOneRegi_region": "EUR",
                "cm_quick_mode": "on",
                "cm_nash_mode": 1,
            },
            "debug EUR",
        ),
        (
            {"optimization": "testOneRegi", "c_testOneRegi_region": "EUR", "CES_parameters": "calibrate"},
            "testOneRegi EUR",
        ),
        (
            {"optimization": "testOneRegi", "c_testOneRegi_region": "EUR", "cm_MAgPIE_Nash": 1},
            "testOneRegi EUR",
        ),
        ({"optimization": "testOneRegiX", "c_testOneRegi_region": "USA"}, "testOneRegi USA"),
        ({"optimization": "testOneRegi"}, "testOneRegi "),
        (
            {"optimization": "testOneRegi", "c_testOneRegi_region": "EUR", "c_empty_model": "on"},
            "empty model",
        ),
        ({"optimization": 5}, "5"),
    ],
)
def test_remind_composition(switches: dict[str, RScalar], expected: str) -> None:
    assert compose_run_type(remind(**switches)) == expected


def test_magpie_returns_the_optimization_without_composition() -> None:
    cfg = FakeConfig("MAgPIE", "nlp_apr17", cm_nash_mode="debug", c_empty_model="on")
    assert compose_run_type(cfg) == "nlp_apr17"


def test_a_model_name_other_than_magpie_takes_the_remind_branch() -> None:
    assert compose_run_type(FakeConfig("anything", "nash", CES_parameters="calibrate")) == "Calib_nash"


def test_missing_model_name_raises_argument_of_length_zero() -> None:
    with pytest.raises(RunTypeError, match=ARGUMENT_OF_LENGTH_ZERO):
        compose_run_type(FakeConfig(None, "nash"))


def test_remind_without_optimization_raises_argument_of_length_zero() -> None:
    with pytest.raises(RunTypeError, match=ARGUMENT_OF_LENGTH_ZERO):
        compose_run_type(FakeConfig("REMIND"))


def test_magpie_without_optimization_returns_none_like_r_null() -> None:
    """``R/colRunType.R:21`` returns ``cfg$gms$optimization`` as is: R 4.6.1 gives ``NULL`` (BUG-046, D-28).

    The caller's ``out[i, "RunType"] <- NULL`` then fails with ``replacement has length zero``
    (``R/getRunStatus.R:73``); that error belongs to the ``get_run_status`` port, not here.
    """
    assert compose_run_type(FakeConfig("MAgPIE")) is None
    assert compose_run_type(FakeConfig("MAgPIE", cm_nash_mode="debug", c_empty_model="on")) is None


# --- the R-written test files through modelstats.config.load_config ---------------------------------


def test_r_written_config_rdata_is_nash(r_testdata_dir: Path, tmp_path: Path, env: Effects) -> None:
    (tmp_path / "config.Rdata").write_bytes((r_testdata_dir / "config.Rdata").read_bytes())
    assert col_run_type(tmp_path, env) == "nash"
    assert col_run_type(tmp_path, env, load_config) == "nash"


def test_r_written_config_yml_is_nlp_apr17(r_testdata_dir: Path, tmp_path: Path, env: Effects) -> None:
    (tmp_path / "config.yml").write_bytes((r_testdata_dir / "config.yml").read_bytes())
    assert col_run_type(tmp_path, env) == "nlp_apr17"
