"""config: discovery by R's unanchored regex and the two loaders (config.Rdata, config.yml).

The YAML typing table (``yaml_scalars.json``) was produced by R's yaml package
through ``gms::loadConfig`` (``make_data.R``); the test pins every scalar in it.
"""

from __future__ import annotations

import json
import math
import os
import warnings
from pathlib import Path
from typing import Any

import pytest

from modelstats.config import (
    CONFIG_PATTERN,
    config_matches,
    find_config_file,
    load_config,
    load_yaml,
    r_as_character,
    r_double_str,
    r_unlist,
)
from modelstats.errors import RParityError

DATA = Path(__file__).parent / "data"
YAML_EXPECT: dict[str, Any] = json.loads((DATA / "yaml_scalars.json").read_text(encoding="utf-8"))
# The two YAML inputs are stored with a .txt suffix: the repository's pre-commit check-yaml hook (a plain
# YAML 1.1 safe loader) rejects the gms tags. config.yml must carry its real name when it is loaded.
CONFIG_YML_STORED = DATA / "config.yml.txt"
YAML_SCALARS_STORED = DATA / "yaml_scalars.yml.txt"


class DirEffects:
    """``listdir_like_r`` over a real directory (sorted, dot-files skipped) plus the read-only subset."""

    def listdir_like_r(self, path: str) -> list[str]:
        return sorted(name for name in os.listdir(path) if not name.startswith("."))

    def exists(self, path: str) -> bool:
        return os.path.exists(path)

    def read_bytes(self, path: str) -> bytes:
        return Path(path).read_bytes()


class ListingEffects:
    """``listdir_like_r`` answering a canned listing, in the order given."""

    def __init__(self, names: list[str]) -> None:
        self.names = names
        self.asked: list[str] = []

    def listdir_like_r(self, path: str) -> list[str]:
        self.asked.append(path)
        return list(self.names)


def touch(directory: Path, *names: str) -> None:
    for name in names:
        (directory / name).write_bytes(b"")


@pytest.fixture
def config_yml(tmp_path: Path) -> Path:
    """The gms::saveConfig output of make_data.R under its real name."""
    target = tmp_path / "config.yml"
    target.write_bytes(CONFIG_YML_STORED.read_bytes())
    return target


# ---------------------------------------------------------------------------
# Discovery
# ---------------------------------------------------------------------------
def test_pattern_is_r_grep_unanchored_with_any_char_dot() -> None:
    names = [
        "config.Rdata",
        "config.Rdata.bak",
        "old_config.yml",
        "configXRdata",
        "config.yml",
        "aconfig.ymlb",
        "config_Rdata",
    ]
    assert config_matches("/run", ListingEffects([*names, "cfg.txt", "config.yaml", "configRdata"])) == names
    assert CONFIG_PATTERN.pattern == "config.Rdata|config.yml"


def test_find_config_file_none_and_one(tmp_path: Path) -> None:
    assert find_config_file(tmp_path, DirEffects()) is None
    touch(tmp_path, "config.Rdata", "cfg.txt", ".config.yml")
    assert find_config_file(tmp_path, DirEffects()) == "config.Rdata"
    assert config_matches(tmp_path, DirEffects()) == ["config.Rdata"]


def test_find_config_file_returns_the_first_name_in_dir_order() -> None:
    effects = ListingEffects(["cfg.txt", "old_config.yml"])
    assert find_config_file("/run", effects) == "old_config.yml"
    assert effects.asked == ["/run"]


def test_two_matches_fail_like_getrunstatus_line_72(tmp_path: Path) -> None:
    """Golden config-bak: ``load(c(path, path.bak))`` -> gzfile() -> invalid 'description' argument."""
    touch(tmp_path, "config.Rdata", "config.Rdata.bak")
    with pytest.raises(RParityError, match="^invalid 'description' argument$"):
        find_config_file(tmp_path, DirEffects())
    assert config_matches(tmp_path, DirEffects()) == ["config.Rdata", "config.Rdata.bak"]


def test_two_matches_one_yml_still_runs_the_no_branch() -> None:
    with pytest.raises(RParityError, match="^invalid 'description' argument$"):
        find_config_file("/run", ListingEffects(["config.Rdata", "config.yml"]))


def test_two_yml_matches_fail_in_colruntype_line_18() -> None:
    with pytest.raises(RParityError, match="^the condition has length > 1$"):
        find_config_file("/run", ListingEffects(["config.yml", "old_config.yml"]))


# ---------------------------------------------------------------------------
# config.Rdata
# ---------------------------------------------------------------------------
def test_load_config_rdata_gives_plain_python_with_r_types() -> None:
    cfg = load_config(DATA / "config.Rdata")
    assert cfg["title"] == "testOneRegi" and cfg["model_name"] == "REMIND"
    gms = cfg["gms"]
    assert gms["optimization"] == "nash"
    assert gms["cm_nash_mode"] == 1 and type(gms["cm_nash_mode"]) is int
    assert gms["cm_MAgPIE_Nash"] == 0.0 and type(gms["cm_MAgPIE_Nash"]) is float
    assert gms["cm_nash_autoconverge"] == "1"  # a character "1" stays a string: R compares it as text
    assert gms["cm_iteration_max"] == 100
    assert gms["a_true"] is True
    assert gms["a_null"] is None and gms["a_na"] is None and gms["a_na_real"] is None
    assert gms["a_named"] == {"a": 1.0, "b": 2.0}
    assert gms["a_chars"] == ["x", "y"] and gms["a_chr_na"] == ["a", None] and gms["a_empty"] == []
    assert gms["a_utf8"] == "Ärger"
    assert cfg["cfg_mag"]["results_folder"] == "output/:title::date:"


def test_load_config_rdata_without_cfg_object_is_empty() -> None:
    assert load_config(DATA / "nocfg.Rdata") == {}


def test_load_config_decides_the_format_by_the_yml_suffix(tmp_path: Path) -> None:
    as_rdata = tmp_path / "config.Rdata"
    as_rdata.write_text("title: x\n")  # a YAML text under an Rdata name goes through load()
    with pytest.raises(RParityError, match="^bad restore file magic number"):
        load_config(as_rdata)
    as_yml = tmp_path / "old_config.yml"
    as_yml.write_text("title: x\nmodel_name: MAgPIE\n")
    assert load_config(as_yml) == {"title": "x", "model_name": "MAgPIE"}
    with pytest.raises(RParityError, match="^cannot open the connection$"):
        load_config(tmp_path / "missing" / "config.Rdata")


def test_load_config_reads_through_effects() -> None:
    class Canned:
        def __init__(self) -> None:
            self.read: list[str] = []

        def read_bytes(self, path: str) -> bytes:
            self.read.append(path)
            stored = CONFIG_YML_STORED if path.endswith("config.yml") else DATA / Path(path).name
            return stored.read_bytes()

    effects = Canned()
    assert load_config("/elsewhere/config.yml", effects)["model_name"] == "MAgPIE"
    assert load_config("/elsewhere/config.Rdata", effects)["model_name"] == "REMIND"
    assert effects.read == ["/elsewhere/config.yml", "/elsewhere/config.Rdata"]


# ---------------------------------------------------------------------------
# config.yml (gms::saveConfig output)
# ---------------------------------------------------------------------------
def test_load_config_yml_with_gms_tags(config_yml: Path) -> None:
    cfg = load_config(config_yml)
    assert cfg["title"] == "default" and cfg["model_name"] == "MAgPIE"
    assert cfg["input"] == {"regional": "rev4.135_h12_magpie.tgz", "cellular": "rev4.135_cellularmagpie.tgz"}
    assert cfg["repositories"] == {"https://example.org/public": None, "/p/projects/landuse/data/input/archive": None}
    assert cfg["force_download"] is True and cfg["recalibrate"] is False
    assert cfg["calib_accuracy"] == 0.05 and cfg["calib_maxiter"] == 20.0 and type(cfg["calib_maxiter"]) is float
    gms = cfg["gms"]
    assert gms["optimization"] == "nlp_apr17"
    assert gms["s15_elastic_demand"] == 0 and type(gms["s15_elastic_demand"]) is int
    assert gms["nothing"] == []
    assert gms["nv"] == {"a": 1.0, "b": 2.0}
    assert gms["mixed"] == [1, "a"]
    assert gms["big"] == 1000000.0
    assert gms["policy_countries"] == ["DEU", "FRA"]
    assert cfg["results_folder"] == "output/:title::date:"


def test_load_yaml_empty_document_and_duplicate_keys(config_yml: Path) -> None:
    assert load_yaml("") is None
    assert load_config(config_yml) != {}
    with pytest.raises(ValueError, match="Duplicate map key: 'a'"):
        load_yaml("a: 1\na: 2\n")


R_CLASS_OF = {bool: "logical", int: "integer", float: "numeric", str: "character"}


def _values_match(py_values: list[Any], r_class: str, r_values: list[Any]) -> bool:
    if len(py_values) != len(r_values):
        return False
    for py, r in zip(py_values, r_values, strict=True):
        if r is None:
            if py is not None:
                return False
            continue
        if py is None or R_CLASS_OF.get(type(py)) != r_class:
            return False
        if r_class == "numeric":
            if isinstance(r, str):  # Inf, -Inf, NaN
                ok = {"Inf": py == math.inf, "-Inf": py == -math.inf, "NaN": math.isnan(py)}[r]
                if not ok:
                    return False
            elif py != r:
                return False
        elif py != r:
            return False
    return True


def _matches_r(py: Any, r: dict[str, Any]) -> bool:
    """Whether a value from GmsLoader is what R's describe() recorded for it."""
    r_class = r["class"]
    if r_class == "NULL":
        return py is None
    if r_class == "list":
        names = r.get("names")
        items = r["value"]
        if names is None:
            if not isinstance(py, list) or len(py) != len(items):
                return False
            return all(_matches_r(p, d) for p, d in zip(py, items, strict=True))
        if not isinstance(py, dict) or list(py) != names:
            return False
        return all(_matches_r(py[n], d) for n, d in zip(names, items, strict=True))
    # atomic vector: scalar, list or (named) dict on the Python side
    r_values = list(r["value"].values()) if isinstance(r["value"], dict) else r["value"]
    if isinstance(py, dict):
        return list(py) == r.get("names") and _values_match(list(py.values()), r_class, r_values)
    if r.get("names") is not None:
        return False
    values = py if isinstance(py, list) else [py]
    return _values_match(values, r_class, r_values)


def yaml_cases() -> list[str]:
    return [k for k in YAML_EXPECT if k != "__scalars__"]


@pytest.fixture(scope="module")
def loaded_scalars() -> tuple[dict[str, Any], list[str]]:
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        loaded = load_yaml(YAML_SCALARS_STORED.read_text(encoding="utf-8"))
    return loaded, [str(w.message) for w in caught]


@pytest.mark.parametrize("key", yaml_cases(), ids=lambda k: f"{k}={YAML_EXPECT['__scalars__'].get(k, k)!r}")
def test_yaml_typing_equals_r_yaml_package(loaded_scalars: tuple[dict[str, Any], list[str]], key: str) -> None:
    loaded, _ = loaded_scalars
    assert key in loaded
    assert _matches_r(loaded[key], YAML_EXPECT[key]), (loaded[key], YAML_EXPECT[key])


def test_yaml_na_coercions_warn_like_r(loaded_scalars: tuple[dict[str, Any], list[str]]) -> None:
    _, messages = loaded_scalars
    scalars = YAML_EXPECT["__scalars__"]
    na_scalars = [
        scalars[k]
        for k, v in YAML_EXPECT.items()
        if k != "__scalars__" and k in scalars and v["class"] in {"integer", "numeric"} and v["value"] == [None]
    ]
    assert na_scalars, "the probe list holds integer/numeric NA cases"
    for text in na_scalars:
        assert any(m.startswith("NAs introduced by coercion") and text in m for m in messages), text


def test_yaml_na_family_and_tagged_scalars() -> None:
    assert load_yaml("a: .na\nb: .na.real\nc: .na.integer\nd: .na.character\ne: .NA\nf: [1, .na]\n") == {
        "a": None,
        "b": None,
        "c": None,
        "d": None,
        "e": ".NA",
        "f": [1, None],
    }
    assert load_yaml("a: !<character> yes") == {"a": "yes"}
    assert load_yaml("a: !<character> [1.0e+5, 017, yes, ~, .na]") == {"a": ["1e+05", "15", "TRUE", "NULL", "NA"]}
    assert load_yaml("a: !<namedVector> 5") == {"a": "5"}
    assert load_yaml("a: !<namedVector> [1, 2]") == {"a": [1, 2]}
    assert load_yaml("a: !<namedVector> {x: ~}") == {"a": None}
    assert load_yaml("a: !<namedVector> {x: .na, z: 1}") == {"a": {"x": None, "z": 1}}
    assert load_yaml("a: !<namedVector> {x: 1.0e+5, z: a}") == {"a": {"x": "1e+05", "z": "a"}}
    assert load_yaml('a: !!int "5"\nb: !!float 5\nc: !!bool yes\nd: !!str 5\n') == {
        "a": 5,
        "b": 5.0,
        "c": True,
        "d": "5",
    }
    with pytest.warns(RuntimeWarning, match="NAs introduced by coercion"):
        assert load_yaml("a: 0.1e+1000\nb: 1.0e+400\n") == {"a": None, "b": None}


def test_yaml_keys_are_typed_then_named_like_r() -> None:
    assert load_yaml("y: 1\n1: 2\n1.5: 3\n~: 4\n.na: 5\n017: 6\n1.0e+5: 7\n'y': 8\n") == {
        "TRUE": 1,
        "1": 2,
        "1.5": 3,
        "": 4,
        "NA": 5,
        "15": 6,
        "1e+05": 7,
        "y": 8,
    }
    with pytest.raises(ValueError, match="Duplicate map key: 'TRUE'"):
        load_yaml("y: 1\nyes: 2\n")


def test_yaml_merges_keep_document_order_first_wins() -> None:
    assert load_yaml("base: &b {x: 1, w: 2}\nd: {<<: *b, w: 3, z: 4}\n")["d"] == {"x": 1, "w": 2, "z": 4}
    assert load_yaml("base: &b {x: 1, w: 2}\nd: {w: 3, <<: *b, z: 4}\n")["d"] == {"w": 3, "x": 1, "z": 4}
    assert load_yaml("a: &a {x: 1}\nb: &b {x: 2, w: 2}\nd: {<<: [*a, *b], z: 4}\n")["d"] == {"x": 1, "w": 2, "z": 4}


# ---------------------------------------------------------------------------
# unlist / as.character helpers
# ---------------------------------------------------------------------------
def test_r_unlist_coerces_to_the_highest_type_and_drops_null() -> None:
    assert r_unlist({"x": 1, "y": "a"}) == {"x": "1", "y": "a"}
    assert r_unlist({"x": True, "y": 2}) == {"x": 1, "y": 2}
    assert r_unlist({"x": None, "y": 2}) == {"y": 2}
    assert r_unlist({"x": None}) is None
    assert r_unlist([1, 2.5]) == [1.0, 2.5]
    assert r_unlist({"x": 1, "y": 2.5}) == {"x": 1.0, "y": 2.5}
    assert r_unlist({"x": True, "y": False}) == {"x": True, "y": False}
    assert r_unlist({"a": [1, 2], "b": {"c": 1}, "d": [5]}) == {"a1": 1, "a2": 2, "b.c": 1, "d": 5}
    assert r_unlist({"x": 100000.0, "y": "s"}) == {"x": "1e+05", "y": "s"}


@pytest.mark.parametrize(
    ("value", "expected"),
    [
        (True, "TRUE"),
        (False, "FALSE"),
        (1, "1"),
        (2.5, "2.5"),
        (100000.0, "1e+05"),
        (123456.0, "123456"),
        (0.0001, "1e-04"),
        (1 / 3, "0.333333333333333"),
        (1e15, "1e+15"),
        (1234567.0, "1234567"),
        (-5.0, "-5"),
        (100000.5, "100000.5"),
        (123456789012345678.0, "123456789012345680"),
        (0.0, "0"),
        (math.inf, "Inf"),
        (-math.inf, "-Inf"),
        (math.nan, "NaN"),
        (None, "NULL"),
        ("x", "x"),
    ],
)
def test_r_as_character(value: Any, expected: str) -> None:
    assert r_as_character(value) == expected
    if isinstance(value, float):
        assert r_double_str(value) == expected
