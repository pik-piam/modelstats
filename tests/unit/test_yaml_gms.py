"""PyYAML reads a ``config.yml`` written by ``gms::saveConfig``.

Converted from ``migration/harness/smoke/yaml_test.py``. ``gms::loadConfig`` is
``yaml::read_yaml(file, handlers = list(namedVector = unlist, character = as.character))``; the
file carries ``!<namedVector>`` mappings, ``!<character> []`` for empty character vectors, ``yes``
for ``TRUE`` and ``~`` for ``NULL`` (``migration/02-python-libraries.md`` section 4.3). The file
under test is written by R (``migration/harness/checks/write_testdata.R``); ``MODELSTATS_R_TESTDATA``
points at a freshly written copy when the check runner regenerates it.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

import pytest
import yaml


class GmsLoader(yaml.SafeLoader):
    """SafeLoader plus the two tags gms::saveConfig emits."""


GmsLoader.add_constructor("namedVector", lambda loader, node: loader.construct_mapping(node))  # type: ignore[arg-type]
GmsLoader.add_constructor("character", lambda loader, node: loader.construct_sequence(node))  # type: ignore[arg-type]


@pytest.fixture(scope="module")
def config_yml(r_testdata_dir: Path) -> Path:
    return r_testdata_dir / "config.yml"


@pytest.fixture(scope="module")
def cfg(config_yml: Path) -> dict[str, Any]:
    with config_yml.open(encoding="utf-8") as handle:
        loaded: dict[str, Any] = yaml.load(handle, Loader=GmsLoader)  # noqa: S506 - SafeLoader subclass
    return loaded


def test_plain_safe_load_rejects_the_gms_tags(config_yml: Path) -> None:
    with config_yml.open(encoding="utf-8") as handle, pytest.raises(yaml.constructor.ConstructorError):
        yaml.safe_load(handle)


def test_named_vector_becomes_a_dict(cfg: dict[str, Any]) -> None:
    assert cfg["gms"]["vec"] == {"a": 1.0, "b": 2.0}


def test_empty_character_vector_becomes_an_empty_list(cfg: dict[str, Any]) -> None:
    assert cfg["gms"]["empty"] == []


def test_yaml_1_1_scalars(cfg: dict[str, Any]) -> None:
    gms = cfg["gms"]
    assert gms["some_flag"] is True  # 'yes'
    assert gms["nullval"] is None  # '~'
    assert gms["s15_elastic_demand"] == 0 and isinstance(gms["s15_elastic_demand"], int)
    assert gms["big"] == 1_000_000.0 and isinstance(gms["big"], float)
    assert gms["dbl"] == 2.5
    assert gms["date"] == "2026-09-25"
    assert gms["chars"] == ["x", "y"]
    assert gms["optimization"] == "nlp_apr17"


def test_top_level_keys(cfg: dict[str, Any]) -> None:
    assert cfg["title"] == "default"
    assert cfg["model_name"] == "MAgPIE"
    assert cfg["results_folder"] == "output/default_2026-09-25_10.11.12"
