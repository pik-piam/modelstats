from __future__ import annotations

import shutil
from pathlib import Path

import pytest

from modelstats._config import (
    ConfigReadError,
    cfg_get,
    find_config,
    load_config,
    r_equals,
    r_equals_number,
    r_greater_than,
)


def test_find_config_exact_names_and_precedence(tmp_path: Path, data_dir: Path) -> None:
    assert find_config(tmp_path) is None
    (tmp_path / "old-config.Rdata").write_text("")
    (tmp_path / "config.yml.bak").write_text("")
    assert find_config(tmp_path) is None
    shutil.copy(data_dir / "config_magpie.yml", tmp_path / "config.yml")
    assert find_config(tmp_path) == tmp_path / "config.yml"
    shutil.copy(data_dir / "config_remind.Rdata", tmp_path / "config.Rdata")
    assert find_config(tmp_path) == tmp_path / "config.Rdata"


def test_load_config_rdata(data_dir: Path) -> None:
    cfg = load_config(data_dir / "config_remind.Rdata")
    assert cfg["model_name"] == "REMIND"
    assert cfg_get(cfg, "gms", "optimization") == "nash"


def test_load_config_yaml_with_gms_tags(data_dir: Path) -> None:
    cfg = load_config(data_dir / "config_magpie.yml")
    assert cfg["model_name"] == "MAgPIE"
    assert cfg_get(cfg, "gms", "optimization") == "nlp_apr17"
    assert cfg_get(cfg, "gms", "policy_countries") == ["DEU", "FRA"]
    assert cfg_get(cfg, "gms", "som_init") == {"A": 0.5, "B": 0.7}
    assert cfg_get(cfg, "gms", "one_region") == "GLO"
    assert cfg_get(cfg, "gms", "switch_on") is True  # YAML 1.1, as R's yaml package reads it
    assert cfg_get(cfg, "gms", "switch_yes") is True
    assert cfg_get(cfg, "files2export", "start") == []


def test_yaml_booleans_like_r(tmp_path: Path) -> None:
    path = tmp_path / "config.yml"
    path.write_text("a: on\nb: yes\nc: y\nd: On\ne: off\nf: no\ng: 'on'\nh: 1\ni: n\nj: N\nk: true\nl: 'y'\n")
    assert load_config(path) == {
        "a": True,
        "b": True,
        "c": True,
        "d": True,
        "e": False,
        "f": False,
        "g": "on",
        "h": 1,
        "i": False,
        "j": False,
        "k": True,
        "l": "y",
    }


def test_load_config_yaml_unknown_tag_and_non_mapping(tmp_path: Path) -> None:
    path = tmp_path / "config.yml"
    path.write_text("gms:\n  x: !expr 1 + 1\n  y: !other [a, b]\n")
    assert load_config(path) == {
        "gms": {"x": "1 + 1", "TRUE": ["a", "b"]}
    }  # the key y is a logical, named TRUE as in R
    path.write_text("- just\n- a list\n")
    assert load_config(path) == {}


def test_cfg_get_missing_levels() -> None:
    cfg = {"gms": {"cm_nash_mode": 1}}
    assert cfg_get(cfg, "gms", "cm_nash_mode") == 1
    assert cfg_get(cfg, "gms", "absent") is None
    assert cfg_get(cfg, "absent", "deeper") is None
    assert cfg_get(cfg, "gms", "cm_nash_mode", "deeper") is None
    assert cfg_get(None, "gms") is None


def test_r_equals_number_compares_numerically() -> None:
    assert r_equals_number(1.0, 1)
    assert r_equals_number(1, 1)
    assert r_equals_number(True, 1)
    assert r_equals_number("1", 1)
    assert not r_equals_number("1.0", 1)
    assert not r_equals_number(1 + 2**-52, 1)
    assert not r_equals_number(None, 1)
    assert not r_equals_number([1], 1)


def test_r_equals_coerces_like_r() -> None:
    assert r_equals("debug", "debug")
    assert r_equals(1.0, "1")
    assert r_equals(1, "1")
    assert r_equals(True, "TRUE")
    assert r_equals(2.5, "2.5")
    assert not r_equals(None, "1")
    assert not r_equals(["on", "on"], "on")
    assert not r_equals({"a": 1}, "a")
    assert not r_equals("on ", "on")


def test_yaml_keys_become_text_like_r_names(tmp_path: Path) -> None:
    path = tmp_path / "config.yml"
    path.write_text("on: 1\nn: 3\n1.0: 4\n~: 5\n0x1F: 6\nplain: 7\n")
    assert load_config(path) == {"TRUE": 1, "FALSE": 3, "1": 4, "": 5, "31": 6, "plain": 7}


def test_yaml_sequences_of_scalars(tmp_path: Path) -> None:
    path = tmp_path / "config.yml"
    path.write_text("one: [EUR]\ntwo: [EUR, USA]\nnum: [1]\nnested: [[a]]\nmaps: [{a: 1}]\n")
    assert load_config(path) == {
        "one": "EUR",
        "two": ["EUR", "USA"],
        "num": 1,
        "nested": "a",
        "maps": [{"a": 1}],
    }


def test_yaml_numbers_r_leaves_as_text(tmp_path: Path) -> None:
    path = tmp_path / "config.yml"
    path.write_text(
        "a: 1_000\nb: 1:30\nc: 0b101\nd: 2020-01-01\ne: 2020-01-01 10:00:00\nf: 1.5_0\ng: 12\nh: 1.5\ni: 0x1F\n"
    )
    assert load_config(path) == {
        "a": "1_000",
        "b": "1:30",
        "c": "0b101",
        "d": "2020-01-01",
        "e": "2020-01-01 10:00:00",
        "f": "1.5_0",
        "g": 12,
        "h": 1.5,
        "i": 31,
    }


def test_yaml_verbatim_tags_as_save_config_writes_them(tmp_path: Path) -> None:
    path = tmp_path / "config.yml"
    path.write_text(
        "gms:\n  input: !<namedVector>\n    x: 1\n    yy: a\n  regions: !<character>\n  - DEU\n  - FRA\n"
        "  one: !<character>\n  - GLO\n  none: !<character> []\n  mixed: !character [1, 2.50, on, 1.0, ~]\n  eq: =\n"
    )
    cfg = load_config(path)
    assert cfg_get(cfg, "gms", "input") == {"x": 1, "yy": "a"}
    assert cfg_get(cfg, "gms", "regions") == ["DEU", "FRA"]
    assert cfg_get(cfg, "gms", "one") == "GLO"
    assert cfg_get(cfg, "gms", "none") == []
    assert cfg_get(cfg, "gms", "mixed") == ["1", "2.5", "TRUE", "1", "NULL"]
    assert cfg_get(cfg, "gms", "eq") == "="


def test_corrupt_config_files_raise_config_read_error(tmp_path: Path) -> None:
    path = tmp_path / "config.yml"
    path.write_text("a: [unclosed\n")
    with pytest.raises(ConfigReadError):
        load_config(path)
    path.write_bytes(b"\xff\xfe")
    with pytest.raises(ConfigReadError):
        load_config(path)
    rdata = tmp_path / "config.Rdata"
    rdata.write_bytes(b"not an R data file")
    with pytest.raises(ConfigReadError):
        load_config(rdata)
    rdata.write_bytes(b"")
    with pytest.raises(ConfigReadError):
        load_config(rdata)
    with pytest.raises(ConfigReadError):
        load_config(tmp_path / "missing" / "config.yml")


def test_r_greater_than() -> None:
    assert (
        r_greater_than(1, 0) and r_greater_than(0.5, 0) and r_greater_than("1", 0) and r_greater_than(True, 0)
    )
    assert not r_greater_than(0, 0) and not r_greater_than("abc", 0) and not r_greater_than(None, 0)
    assert not r_greater_than([1], 0)


def test_r_as_number() -> None:
    from modelstats._config import r_as_number, r_greater_than

    assert r_as_number("1e2") == 100.0 and r_as_number(" 5 ") == 5.0 and r_as_number(True) == 1.0
    assert r_as_number("1_000") is None and r_as_number("abc") is None and r_as_number(None) is None
    assert r_greater_than("1_000", 0) is False and r_greater_than("2", 1)
