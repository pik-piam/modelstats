"""``modelstats.config``: finding and loading ``config.Rdata`` / ``config.yml`` as the R code does.

``dir()`` order is pinned against R 4.6.1 under the sandbox locale (``LC_ALL=C.utf8``, ICU root
collation); the loaders read the R-written files of ``tests/data/r``.
"""

from __future__ import annotations

from pathlib import Path

import numpy as np
import pytest
import rdata

from modelstats.config import (
    CONFIG_FILE_REGEX,
    ConfigError,
    ConfigKind,
    ConfigMatches,
    RunConfig,
    config_kind,
    find_config_file,
    load_config,
    r_collation_key,
    r_dir,
)
from modelstats.rdata_io import vector

# R 4.6.1, LC_ALL=C.utf8 (icuGetCollate() == "root"): dir() of a directory holding these names
# (plus ".hidden", which dir() omits) returned exactly this order.
R_DIR_ORDER = [
    "_x",
    "-a",
    "1",
    "10",
    "2",
    "a",
    "A",
    "ä",
    "a-b",
    "ab",
    "b",
    "B",
    "config.Rdata",
    "config.Rdata.bak",
    "config.yml",
    "Config.yml",
    "config.yml~",
    "old_config.yml",
    "sub.dir",
]
# R 4.6.1, same locale: sort(c("a b", "a_b", "a-b", "a1b", "aab", "aAb", "a", "A", "ab", "aB", "Ab", "AB",
#   "äb", "ab1", "ab10", "ab2", "abc", "ab-c", "ab.c"))
R_SORT_SAMPLE = [
    "a",
    "A",
    "a b",
    "a_b",
    "a-b",
    "a1b",
    "aab",
    "aAb",
    "ab",
    "aB",
    "Ab",
    "AB",
    "äb",
    "ab-c",
    "ab.c",
    "ab1",
    "ab10",
    "ab2",
    "abc",
]


def test_r_dir_reproduces_r_collation_and_skips_dot_files(tmp_path: Path) -> None:
    for name in R_DIR_ORDER + [".hidden"]:
        if name == "sub.dir":
            (tmp_path / name).mkdir()
        else:
            (tmp_path / name).write_text("", encoding="utf-8")
    assert r_dir(tmp_path) == R_DIR_ORDER
    assert r_dir(str(tmp_path)) == R_DIR_ORDER


def test_collation_key_reproduces_r_sort_sample() -> None:
    assert sorted(reversed(R_SORT_SAMPLE), key=r_collation_key) == R_SORT_SAMPLE


def test_collation_punctuation_table_matches_r() -> None:
    """R 4.6.1: ``sort(paste0("a", p, "b"))`` for the ASCII punctuation is this order."""
    punctuation = " _-,;:!?.'\"()[]{}@*/\\&#%`^+<=>|~$"
    names = [f"a{char}b" for char in punctuation]
    assert sorted(reversed(names), key=r_collation_key) == names


def test_r_dir_of_a_missing_path_or_a_file_is_empty(tmp_path: Path) -> None:
    assert r_dir(tmp_path / "absent") == []
    (tmp_path / "file").write_text("", encoding="utf-8")
    assert r_dir(tmp_path / "file") == []


# ---------------------------------------------------------------------------------------------
# find_config_file
# ---------------------------------------------------------------------------------------------


def touch(directory: Path, *names: str) -> Path:
    directory.mkdir(parents=True, exist_ok=True)
    for name in names:
        (directory / name).write_text("", encoding="utf-8")
    return directory


def test_no_config_file(tmp_path: Path) -> None:
    run = touch(tmp_path / "run", "full.gms", "log.txt", "cfg.txt", ".config.yml")
    matches = find_config_file(run)
    assert matches == ConfigMatches(run, ())
    assert matches.path is None and matches.paths == ()


def test_one_config_file(tmp_path: Path) -> None:
    run = touch(tmp_path / "run", "config.Rdata", "full.gms")
    assert find_config_file(run).path == run / "config.Rdata"
    run = touch(tmp_path / "mag", "config.yml", "full.gms")
    assert find_config_file(str(run)).path == run / "config.yml"


def test_unanchored_regex_matches_more_than_the_two_names(tmp_path: Path) -> None:
    """BUG-003 (D-01, parity): ``config.Rdata.bak`` and ``old_config.yml`` match, ``.`` is any character."""
    run = touch(
        tmp_path / "run", "config.Rdata", "config.Rdata.bak", "old_config.yml", "configXyml", "cfg.yml"
    )
    matches = find_config_file(run)
    assert matches.names == (
        "config.Rdata",
        "config.Rdata.bak",
        "configXyml",
        "old_config.yml",
    )
    assert matches.path == run / "config.Rdata"  # the first in dir() order
    assert matches.paths == tuple(run / name for name in matches.names)
    assert CONFIG_FILE_REGEX.search("my_config.Rdata~") and not CONFIG_FILE_REGEX.search("config.Rdat")


def test_dir_order_decides_the_first_match(tmp_path: Path) -> None:
    run = touch(tmp_path / "run", "config.yml", "Config.yml", "config.Rdata", "config.yml~")
    assert find_config_file(run).names == (
        "config.Rdata",
        "config.yml",
        "config.yml~",
    )  # grep is case-sensitive


def test_not_a_directory_has_no_matches(tmp_path: Path) -> None:
    assert find_config_file(tmp_path / "absent").names == ()


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
