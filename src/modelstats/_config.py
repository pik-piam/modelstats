"""Run configurations: ``config.Rdata`` (object ``cfg``) and MAgPIE's ``config.yml``.

R source: ``gms::loadConfig()`` (YAML with the ``!namedVector`` and ``!character`` handlers) and
``load("config.Rdata")`` in colRunType.R and getRunStatus.R.

YAML is read as R's ``yaml`` package reads it: YAML 1.1 booleans including ``y``/``n``, mapping
keys turned into text (``on`` -> ``"TRUE"``, ``1.0`` -> ``"1"``), numbers with ``_`` or ``:``
and timestamps left as text, unknown tags ignored. ``gms::saveConfig`` writes the tags in the
verbatim form ``!<namedVector>``; both forms are accepted. ``!character`` items are stringified
like ``as.character``; a one-item ``!character`` is the scalar. Not reproduced: ``!namedVector``
flattening nested mappings (``unlist``), R's partial matching of ``$`` names.
"""

from __future__ import annotations

import re
from collections.abc import Mapping
from pathlib import Path
from typing import Any

import yaml

from modelstats._rdata import read_rda
from modelstats._timefmt import r_num_str, r_str
from modelstats.errors import ConfigReadError

__all__ = [
    "CONFIG_NAMES",
    "ConfigReadError",
    "cfg_get",
    "find_config",
    "load_config",
    "r_as_number",
    "r_equals",
    "r_equals_number",
    "r_greater_than",
]

CONFIG_NAMES = ("config.Rdata", "config.yml")
_TEXT_NUMBER = re.compile(r"[_:]|^[-+]?0[bB]")


def _key_text(key: Any) -> str:
    """A mapping key as R names it: ``TRUE``/``FALSE``, ``1`` for ``1.0``, ``""`` for null."""
    if key is None:
        return ""
    if isinstance(key, bool | int | float):
        return r_num_str(key)
    return str(key)


class GmsLoader(yaml.SafeLoader):
    """YAML 1.1 like R's ``yaml`` package, plus the tags ``gms::loadConfig`` understands."""

    def construct_mapping(self, node: yaml.MappingNode, deep: bool = False) -> dict[Any, Any]:
        """Mapping keys become text, as R turns them into list names."""
        self.flatten_mapping(node)
        mapping: dict[Any, Any] = {}
        for key_node, value_node in node.value:
            key = _key_text(self.construct_object(key_node, deep=deep))
            mapping[key] = self.construct_object(value_node, deep=deep)
        return mapping


def _bool(loader: GmsLoader, node: yaml.ScalarNode) -> bool:
    value = loader.construct_scalar(node)
    if value in ("y", "Y", "n", "N"):
        return value in ("y", "Y")
    return yaml.SafeLoader.construct_yaml_bool(loader, node)


def _int(loader: GmsLoader, node: yaml.ScalarNode) -> Any:
    if _TEXT_NUMBER.search(node.value):  # 1_000, 1:30, 0b101 are text in R
        return node.value
    return yaml.SafeLoader.construct_yaml_int(loader, node)


def _float(loader: GmsLoader, node: yaml.ScalarNode) -> Any:
    if _TEXT_NUMBER.search(node.value):
        return node.value
    return yaml.SafeLoader.construct_yaml_float(loader, node)


def _text(loader: GmsLoader, node: yaml.ScalarNode) -> str:
    return loader.construct_scalar(node)


def _collapse(items: list[Any]) -> Any:
    """A one-item sequence of scalars is the scalar: R's yaml turns sequences of scalars into vectors."""
    if len(items) == 1 and isinstance(items[0], str | int | float | bool | None):
        return items[0]
    return items


def _seq(loader: GmsLoader, node: yaml.SequenceNode) -> Any:
    return _collapse(loader.construct_sequence(node, deep=True))


def _any(loader: GmsLoader, tag_suffix: str, node: yaml.Node) -> Any:
    """An unknown tag: the plain value of the node."""
    if isinstance(node, yaml.MappingNode):
        return loader.construct_mapping(node, deep=True)
    if isinstance(node, yaml.SequenceNode):
        return loader.construct_sequence(node, deep=True)
    if isinstance(node, yaml.ScalarNode):
        return loader.construct_scalar(node)
    return None


def _named_vector(loader: GmsLoader, node: yaml.Node) -> Any:
    return _any(loader, "", node)


def _character_item(item: Any) -> str:
    return "NULL" if item is None else r_str(item)


def _character(loader: GmsLoader, node: yaml.Node) -> Any:
    if isinstance(node, yaml.SequenceNode):
        return _collapse([_character_item(item) for item in loader.construct_sequence(node, deep=True)])
    if isinstance(node, yaml.ScalarNode):
        return str(node.value)
    return _any(loader, "", node)


GmsLoader.add_constructor("tag:yaml.org,2002:bool", _bool)
GmsLoader.add_constructor("tag:yaml.org,2002:int", _int)
GmsLoader.add_constructor("tag:yaml.org,2002:float", _float)
GmsLoader.add_constructor("tag:yaml.org,2002:timestamp", _text)
GmsLoader.add_constructor("tag:yaml.org,2002:seq", _seq)
for _tag in ("!namedVector", "namedVector"):
    GmsLoader.add_constructor(_tag, _named_vector)
for _tag in ("!character", "character"):
    GmsLoader.add_constructor(_tag, _character)
GmsLoader.add_multi_constructor("", _any)
# R's yaml package also reads the YAML 1.1 short forms y/n as logicals; PyYAML does not by default
GmsLoader.add_implicit_resolver("tag:yaml.org,2002:bool", re.compile(r"^(?:y|Y|n|N)$"), list("yYnN"))


def find_config(run_dir: Path) -> Path | None:
    """The configuration file of a run folder; ``config.Rdata`` wins over ``config.yml``."""
    for name in CONFIG_NAMES:
        path = run_dir / name
        if path.is_file():
            return path
    return None


def load_config(path: Path) -> dict[str, Any]:
    """The ``cfg`` list of a run: ``config.Rdata`` through rdata, ``config.yml`` through PyYAML.

    Raises:
        ConfigReadError: The file cannot be read or parsed (any failure of the parsers counts,
            they see arbitrary bytes - the library-boundary rule of PORTING.md section 4.1).
    """
    try:
        if path.suffix in (".yml", ".yaml"):
            loaded = yaml.load(path.read_text(encoding="utf-8"), Loader=GmsLoader)
            return loaded if isinstance(loaded, dict) else {}
        cfg = read_rda(path).get("cfg")
    except Exception as error:  # total boundary around the parsers
        raise ConfigReadError(f"{path}: {error}") from error
    return cfg if isinstance(cfg, dict) else {}


def cfg_get(cfg: Mapping[str, Any] | None, *keys: str) -> Any:
    """``cfg$a$b``: the nested value, ``None`` when any level is absent."""
    value: Any = cfg
    for key in keys:
        if not isinstance(value, Mapping):
            return None
        value = value.get(key)
    return value


def r_equals(value: Any, text: str) -> bool:
    """``isTRUE(value == "text")``: R turns numbers and logicals into character before comparing.

    ``cm_nash_mode`` is ``"debug"`` or ``1`` depending on the REMIND version: ``r_equals(1.0, "1")``
    is true, ``r_equals(None, "1")`` and ``r_equals(["a", "b"], "a")`` are false.
    """
    if value is None or isinstance(value, list | dict):
        return False
    return r_str(value) == text


def r_equals_number(value: Any, number: float) -> bool:
    """``isTRUE(value == number)``: numbers and logicals compare numerically, text as text."""
    if value is None or isinstance(value, list | dict):
        return False
    if isinstance(value, bool | int | float):
        return float(value) == number
    return str(value) == r_num_str(number)


def r_as_number(value: Any) -> float | None:
    """``as.numeric(value)``: a number, or a string holding one; anything else (``NA``) is ``None``.

    Like R, an underscore is no digit separator (``"1_000"`` is ``NA``); unlike R, nothing warns.
    """
    if isinstance(value, bool | int | float):
        return float(value)
    if isinstance(value, str) and "_" not in value:
        try:
            return float(value)
        except ValueError:
            return None
    return None


def r_greater_than(value: Any, number: float) -> bool:
    """``isTRUE(value > number)`` for a number, or a string holding one (``cm_nash_autoconverge > 0``)."""
    as_number = r_as_number(value)
    return as_number is not None and as_number > number
