"""``modelstats.config``: loading ``config.Rdata`` / ``config.yml`` as the R code does.

The loaders read the R-written files of ``tests/data/r``; finding the file (the unanchored regular
expression over ``dir()`` in R's collation order, BUG-003) is ``RunFiles.config_matches`` on
``Effects.listdir_like_r`` and is pinned in ``test_facts.py`` and ``test_env.py``.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pytest
import rdata

from modelstats.config import ConfigError, ConfigKind, RunConfig, config_kind, load_config
from modelstats.rdata_io import vector


@pytest.mark.parametrize(
    ("name", "kind"),
    [
        ("config.yml", ConfigKind.YAML),
        ("old_config.yml", ConfigKind.YAML),
        ("config.Rdata", ConfigKind.RDATA),
        ("config.Rdata.bak", ConfigKind.RDATA),
        ("config.yml~", ConfigKind.RDATA),  # grepl("yml$") is false, R calls load()
    ],
)
def test_config_kind_follows_grepl_yml_dollar(name: str, kind: ConfigKind) -> None:
    assert config_kind(name) is kind


# ---------------------------------------------------------------------------------------------
# load_config: config.Rdata
# ---------------------------------------------------------------------------------------------


@pytest.fixture(scope="module")
def remind(r_testdata_dir: Path) -> RunConfig:
    return load_config(r_testdata_dir / "config.Rdata")


@pytest.fixture(scope="module")
def magpie(r_testdata_dir: Path) -> RunConfig:
    return load_config(r_testdata_dir / "config.yml")


def test_rdata_typed_fields(remind: RunConfig, r_testdata_dir: Path) -> None:
    assert remind.kind is ConfigKind.RDATA and remind.source == r_testdata_dir / "config.Rdata"
    assert remind.model_name == "REMIND"
    assert remind.title == "SSP2-NPi-AMT"
    assert remind.results_folder == "output/SSP2-NPi-AMT_2026-09-25_10.11.12"
    assert remind.remind_folder == "/p/projects/remind/modeltests/remind"
    assert remind.path_magpie is None and remind.cfg_mag_results_folder is None  # NULL
    assert remind.optimization == "nash"
    assert remind.cm_iteration_max == 50 and type(remind.cm_iteration_max) is int
    assert remind.cm_nash_autoconverge == 1.0 and type(remind.cm_nash_autoconverge) is float
    assert remind.cm_nash_mode == "parallel"
    assert remind.cm_quick_mode == "off"
    assert remind.c_testOneRegi_region == "EUR"
    assert remind.CES_parameters == "load"
    assert remind.cm_MAgPIE_coupling == "off"
    assert remind.cm_MAgPIE_Nash is None  # absent from the gms list
    assert remind.c_empty_model == "off"


def test_rdata_raw_and_gms_keep_the_whole_list(remind: RunConfig) -> None:
    assert "files2export" in remind.raw and "slurmConfig" in remind.raw
    assert remind.raw["cfg_mag"] is None
    assert vector(remind.gms["vec"]) == (1.0, 2.0)
    assert vector(remind.gms["charvec"]) == ("x", "y")
    assert remind.gms_scalar("some_flag") is True
    assert remind.gms_scalar("some_na") is None
    assert remind.gms_scalar("cm_startyear") == 2005.0
    assert remind.gms_scalar("absent") is None
    with pytest.raises(ConfigError, match=r"cfg\$gms\$charvec is a vector of length 2"):
        remind.gms_scalar("charvec")


def test_rdata_without_a_cfg_object(tmp_path: Path) -> None:
    path = tmp_path / "config.Rdata"
    rdata.write_rda(path, {"other": np.array([1.0])})
    with pytest.raises(ConfigError, match="does not define a 'cfg' list"):
        load_config(path)


def test_rdata_whose_cfg_is_not_a_list(tmp_path: Path) -> None:
    path = tmp_path / "config.Rdata"
    rdata.write_rda(path, {"cfg": np.array(["x"], dtype=object)})
    with pytest.raises(ConfigError, match="does not define a 'cfg' list"):
        load_config(path)


def test_rdata_with_a_non_list_gms(tmp_path: Path) -> None:
    path = tmp_path / "config.Rdata"
    rdata.write_rda(path, {"cfg": {"title": "t", "gms": np.array(["nash"], dtype=object)}})
    with pytest.raises(ConfigError, match=r"cfg\$gms is not a list"):
        load_config(path)


def test_rdata_with_a_vector_in_a_scalar_field(tmp_path: Path) -> None:
    path = tmp_path / "config.Rdata"
    rdata.write_rda(path, {"cfg": {"title": np.array(["a", "b"], dtype=object), "gms": {}}})
    with pytest.raises(ConfigError, match=r"cfg\$title is a vector of length 2"):
        load_config(path)


def test_rdata_numeric_title_is_pasted_as_character(tmp_path: Path) -> None:
    path = tmp_path / "config.Rdata"
    rdata.write_rda(
        path, {"cfg": {"title": np.array([2.0]), "model_name": np.array(["REMIND"], dtype=object)}}
    )
    cfg = load_config(path)
    assert cfg.title == "2" and cfg.gms == {} and cfg.optimization is None


def test_missing_and_unreadable_files(tmp_path: Path) -> None:
    with pytest.raises(ConfigError, match="cannot load"):
        load_config(tmp_path / "config.Rdata")
    garbage = tmp_path / "config.Rdata"
    garbage.write_bytes(b"garbage")
    with pytest.raises(ConfigError, match="cannot load"):
        load_config(garbage)
    (tmp_path / "config.yml").mkdir()
    with pytest.raises(ConfigError, match="cannot load"):
        load_config(tmp_path / "config.yml")


# ---------------------------------------------------------------------------------------------
# load_config: config.yml
# ---------------------------------------------------------------------------------------------


def test_yaml_typed_fields(magpie: RunConfig, r_testdata_dir: Path) -> None:
    assert magpie.kind is ConfigKind.YAML and magpie.source == r_testdata_dir / "config.yml"
    assert magpie.model_name == "MAgPIE"
    assert magpie.title == "default"
    assert magpie.results_folder == "output/default_2026-09-25_10.11.12"
    assert magpie.optimization == "nlp_apr17"
    assert magpie.remind_folder is None and magpie.cm_nash_mode is None and magpie.c_empty_model is None


def test_yaml_1_1_scalars_and_gms_tags(magpie: RunConfig) -> None:
    assert magpie.gms_scalar("some_flag") is True  # 'yes'
    assert magpie.gms_scalar("nullval") is None  # '~'
    assert (
        magpie.gms_scalar("s15_elastic_demand") == 0 and type(magpie.gms_scalar("s15_elastic_demand")) is int
    )
    assert magpie.gms_scalar("big") == 1_000_000.0
    assert magpie.gms_scalar("date") == "2026-09-25"
    assert magpie.gms["vec"] == {"a": 1.0, "b": 2.0}  # !<namedVector>
    assert magpie.gms["empty"] == []  # !<character> []
    assert magpie.gms_scalar("empty") is None  # character(0): never isTRUE, like NULL
    assert vector(magpie.gms["chars"]) == ("x", "y")
    with pytest.raises(ConfigError, match="not an atomic vector"):
        magpie.gms_scalar("vec")


def test_yaml_errors(tmp_path: Path) -> None:
    path = tmp_path / "config.yml"
    path.write_text("- a\n- b\n", encoding="utf-8")
    with pytest.raises(ConfigError, match="does not hold a mapping"):
        load_config(path)
    path.write_text("title: [unclosed\n", encoding="utf-8")
    with pytest.raises(ConfigError, match="cannot load"):
        load_config(path)
    path.write_text("gms:\n  vec: !<namedVector> [1, 2]\n", encoding="utf-8")
    with pytest.raises(ConfigError, match="namedVector"):
        load_config(path)
    path.write_text("gms:\n  empty: !<character> {a: 1}\n", encoding="utf-8")
    with pytest.raises(ConfigError, match="character"):
        load_config(path)


def test_yaml_without_gms(tmp_path: Path) -> None:
    path = tmp_path / "config.yml"
    path.write_text("title: t\nmodel_name: MAgPIE\ngms: ~\n", encoding="utf-8")
    cfg = load_config(path)
    assert cfg.gms == {} and cfg.optimization is None and cfg.model_name == "MAgPIE"
