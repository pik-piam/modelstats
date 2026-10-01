"""Run configuration files: ``config.Rdata`` (REMIND) and ``config.yml`` (MAgPIE).

Discovery mirrors ``R/getRunStatus.R:41`` and ``R/colRunType.R:14`` literally:
``grep("config.Rdata|config.yml", dir(mydir), value = TRUE)`` is an unanchored
regex in which ``.`` matches any character, so ``config.Rdata.bak`` or
``old_config.yml`` match too (BUG-003, decision D-01 pending -> parity).
Loading mirrors ``R/getRunStatus.R:72``: a name ending in ``yml`` goes through
``gms::loadConfig`` (YAML 1.1 as R's ``yaml`` package types it, with the
``!namedVector`` and ``!character`` handlers, 02 section 4.3), anything else
through ``load()`` and the ``cfg`` object it defines.
"""

from __future__ import annotations

import math
import os
import re
import warnings
from collections.abc import Mapping
from typing import TYPE_CHECKING, Any

import yaml

from modelstats.errors import RParityError
from modelstats.rdata_io import pythonize, read_rda

if TYPE_CHECKING:
    from modelstats.env import Effects

__all__ = [
    "CONFIG_PATTERN",
    "GmsLoader",
    "config_matches",
    "find_config_file",
    "load_config",
    "load_yaml",
    "r_as_character",
    "r_double_str",
    "r_unlist",
]

#: R's pattern, verbatim: unanchored, ``.`` matches any character.
CONFIG_PATTERN = re.compile(r"config.Rdata|config.yml")


def _effects(effects: Effects | None) -> Effects:
    if effects is not None:
        return effects
    from modelstats.env import default_effects

    return default_effects()


# ---------------------------------------------------------------------------
# Discovery
# ---------------------------------------------------------------------------
def config_matches(mydir: str | os.PathLike[str], effects: Effects | None = None) -> list[str]:
    """``grep("config.Rdata|config.yml", dir(mydir), value = TRUE)``: every matching name, in ``dir()`` order."""
    names = _effects(effects).listdir_like_r(os.fspath(mydir))
    return [name for name in names if CONFIG_PATTERN.search(name)]


def find_config_file(mydir: str | os.PathLike[str], effects: Effects | None = None) -> str | None:
    """The config file name ``getRunStatus`` loads, or ``None`` when no name matches.

    Two or more matches reproduce what ``R/getRunStatus.R:72`` then does with a
    vector ``cfgf``: ``ifelse(grepl("yml$", cfgf), cfg <- loadConfig(...), load(...))``
    evaluates the ``no`` branch as soon as one test is FALSE, and ``load()`` of a
    length-2 path fails in ``gzfile()`` with ``invalid 'description' argument``
    (golden ``config-bak``). When every match ends in ``yml`` only the ``yes``
    branch runs: ``loadConfig()`` pastes the paths into one YAML string without
    complaint and the call fails one line later in ``colRunType()`` line 18,
    ``if (file.exists(<length 2>))`` -> ``the condition has length > 1``.
    ``colRunType()`` itself fails that way for any two matches; it uses
    :func:`config_matches`.
    """
    matches = config_matches(mydir, effects)
    if not matches:
        return None
    if len(matches) == 1:
        return matches[0]
    if all(name.endswith("yml") for name in matches):
        raise RParityError("the condition has length > 1")
    raise RParityError("invalid 'description' argument")


# ---------------------------------------------------------------------------
# YAML as R's yaml package reads it (gms::loadConfig)
# ---------------------------------------------------------------------------
_R_INT_MAX = 2**31 - 1
_NULL_RE = re.compile(r"^(?:~|null|Null|NULL|)$")
_BOOL_RE = re.compile(r"^(?:y|Y|yes|Yes|YES|n|N|no|No|NO|true|True|TRUE|false|False|FALSE|on|On|ON|off|Off|OFF)$")
_INT_RE = re.compile(r"^(?:[-+]?0x[0-9a-fA-F]+|[-+]?0[0-7]+|[-+]?(?:0|[1-9][0-9,]*))$")
_FLOAT_RE = re.compile(
    r"^(?:[-+]?(?:[0-9][0-9,]*)?\.[0-9,]*(?:[eE][-+][0-9]+)?|[-+]?\.(?:inf|Inf|INF)|\.(?:nan|NaN|NAN))$"
)
_NA_RE = re.compile(r"^\.na(?:\.real|\.integer|\.character)?$")
_MERGE_RE = re.compile(r"^(?:<<)$")
_TRUE = frozenset({"y", "Y", "yes", "Yes", "YES", "true", "True", "TRUE", "on", "On", "ON"})
_NA_TAG = "tag:r-yaml:na"


class _RNA:
    """R's ``NA`` while a YAML document is being built (``NULL`` is ``None``); ``None`` once loaded."""

    __slots__ = ()

    def __repr__(self) -> str:
        return "NA"


NA = _RNA()


def _coercion_warning(text: str) -> None:
    warnings.warn(f"NAs introduced by coercion: {text}", RuntimeWarning, stacklevel=3)


class GmsLoader(yaml.SafeLoader):
    """``yaml::read_yaml(handlers = list(namedVector = unlist, character = as.character))``.

    PyYAML's implicit typing is replaced by the rules of R's ``yaml`` package
    (verified against R 4.6.1 / yaml 2.3.12, pinned by ``tests/unit/data/yaml_scalars.json``):
    ``y``/``n`` are logical, integers are decimal, octal (``017``) or hex
    (``0x1A``) without underscores and must fit a 32-bit R integer (else ``NA``),
    floats need a dot and a *signed* exponent (``1.5e+3`` yes, ``1e5`` and
    ``1.5e3`` no), ``.inf``/``.nan`` only with the dot, ``.na`` (``.na.real``,
    ``.na.integer``, ``.na.character``) is ``NA``, no timestamps and no
    sexagesimals (``1:30`` stays a string). A tagged scalar reaches its handler
    as raw text, a tagged sequence or mapping with typed elements. Duplicate
    keys are an error, as in R.
    """

    yaml_implicit_resolvers: dict[str | None, list[tuple[str, re.Pattern[str]]]] = {}

    def construct_mapping(self, node: yaml.Node, deep: bool = False) -> dict[Any, Any]:
        """A mapping as R's yaml package builds it: typed keys become names, merges keep document order.

        Keys go through the same implicit typing as values and are then named by
        ``as.character`` (``yes:`` is the name ``"TRUE"``, ``017:`` is ``"15"``,
        ``~:`` is ``""``). ``<<`` merges are expanded in place and the first
        occurrence of a name wins (``merge.precedence = "order"``); a literal
        duplicate key is an error, as in R.
        """
        if not isinstance(node, yaml.MappingNode):
            msg = f"expected a mapping node, but found {node.tag}"
            raise yaml.constructor.ConstructorError(None, None, msg, node.start_mark)
        result: dict[str, Any] = {}
        explicit: set[str] = set()
        for key_node, value_node, merged in self._flatten_pairs(node):
            name = self._key_name(key_node)
            if not merged:
                if name in explicit:
                    msg = f"Duplicate map key: '{name}'"
                    raise ValueError(msg)
                explicit.add(name)
            if name in result:
                continue
            result[name] = self.construct_object(value_node, deep=deep)
        return result

    def _flatten_pairs(self, node: yaml.MappingNode) -> list[tuple[yaml.Node, yaml.Node, bool]]:
        pairs: list[tuple[yaml.Node, yaml.Node, bool]] = []
        for key_node, value_node in node.value:
            if key_node.tag != "tag:yaml.org,2002:merge":
                pairs.append((key_node, value_node, False))
                continue
            targets = value_node.value if isinstance(value_node, yaml.SequenceNode) else [value_node]
            for target in targets:
                if not isinstance(target, yaml.MappingNode):
                    msg = f"expected a mapping for merging, but found {target.tag}"
                    raise yaml.constructor.ConstructorError(None, None, msg, target.start_mark)
                pairs.extend((k, v, True) for k, v, _ in self._flatten_pairs(target))
        return pairs

    def _key_name(self, key_node: yaml.Node) -> str:
        key = self.construct_object(key_node, deep=True)
        if key is None:
            return ""
        if isinstance(key, (dict, list)):
            msg = "complex mapping keys are not supported"
            raise yaml.constructor.ConstructorError(None, None, msg, key_node.start_mark)
        return r_as_character(key)


def _text(loader: yaml.SafeLoader, node: yaml.Node) -> str:
    assert isinstance(node, yaml.ScalarNode)
    return str(loader.construct_scalar(node))


def _construct_null(loader: yaml.SafeLoader, node: yaml.Node) -> None:
    return None


def _construct_na(loader: yaml.SafeLoader, node: yaml.Node) -> _RNA:
    return NA


def _construct_bool(loader: yaml.SafeLoader, node: yaml.Node) -> bool:
    return _text(loader, node) in _TRUE


def _construct_int(loader: yaml.SafeLoader, node: yaml.Node) -> int | _RNA:
    text = _text(loader, node)
    sign = -1 if text.startswith("-") else 1
    digits = text.lstrip("+-")
    try:
        if digits.lower().startswith("0x"):
            value = int(digits[2:], 16)
        elif len(digits) > 1 and digits.startswith("0"):
            value = int(digits, 8)
        else:
            value = int(digits)  # a comma (allowed by the pattern) fails here, like as.integer() in R
    except ValueError:
        _coercion_warning(text)
        return NA
    value *= sign
    if abs(value) > _R_INT_MAX:
        _coercion_warning(f"{text} is out of integer range")
        return NA
    return value


def _construct_float(loader: yaml.SafeLoader, node: yaml.Node) -> float | _RNA:
    text = _text(loader, node)
    lowered = text.lower()
    if lowered.endswith(".inf"):
        return -math.inf if text.startswith("-") else math.inf
    if lowered == ".nan":
        return math.nan
    try:
        value = float(text)
    except ValueError:
        _coercion_warning(text)
        return NA
    if math.isinf(value):  # strtod overflow is NA in R's yaml package
        _coercion_warning(text)
        return NA
    return value


def r_double_str(value: float) -> str:
    """``as.character()`` of one R double: 15 significant digits, fixed unless scientific is narrower.

    R keeps the fewest significant digits (at most 15) that reproduce the value
    and then prints the fixed notation unless the scientific one is shorter
    (``scipen = 0``): ``123456`` stays, ``100000`` becomes ``1e+05``, ``0.0001``
    becomes ``1e-04``, ``1/3`` is ``0.333333333333333``.
    """
    if math.isnan(value):
        return "NaN"
    if math.isinf(value):
        return "Inf" if value > 0 else "-Inf"
    if value == 0:
        return "0"
    target = float(f"{value:.14e}")
    sci_py = f"{value:.14e}"
    for sig in range(1, 16):
        candidate = f"{value:.{sig - 1}e}"
        if float(candidate) == target:
            sci_py = candidate
            break
    mantissa, exponent = sci_py.split("e")
    exp = int(exponent)
    sci = f"{mantissa}e{'+' if exp >= 0 else '-'}{abs(exp):02d}"
    fixed = f"{value:.{max(0, sig - 1 - exp)}f}"
    return fixed if len(fixed) <= len(sci) else sci


def r_as_character(value: Any) -> str:
    """``as.character()`` of one element of a YAML sequence (``!character`` handler, mixed ``!namedVector``).

    ``NA`` is ``"NA"``, a ``NULL`` element (``~``) is ``"NULL"`` as in
    ``as.character(list(NULL))``; ``None`` stands for ``NULL`` here.
    """
    if isinstance(value, _RNA):
        return "NA"
    if value is None:
        return "NULL"
    if isinstance(value, bool):
        return "TRUE" if value else "FALSE"
    if isinstance(value, int):
        return str(value)
    if isinstance(value, float):
        return r_double_str(value)
    if isinstance(value, (list, dict)):
        return str(value)
    return str(value)


def _leaves(prefix: str, value: Any, out: list[tuple[str, Any]]) -> None:
    if isinstance(value, Mapping):
        for key, sub in value.items():
            _leaves(f"{prefix}.{key}" if prefix else str(key), sub, out)
    elif isinstance(value, (list, tuple)):
        for i, sub in enumerate(value, start=1):
            _leaves(f"{prefix}{i}" if len(value) > 1 else prefix, sub, out)
    elif value is not None:  # unlist() drops NULL but keeps NA
        out.append((prefix, value))


def _coerce(values: list[Any]) -> list[Any]:
    """The common R type of ``unlist()``: character > double > integer > logical; ``NA`` stays ``NA``."""
    present = [v for v in values if not isinstance(v, _RNA)]
    if any(isinstance(v, str) for v in present):
        return [NA if isinstance(v, _RNA) else r_as_character(v) for v in values]
    if any(isinstance(v, float) for v in present):
        return [NA if isinstance(v, _RNA) else float(v) for v in values]
    if any(isinstance(v, int) and not isinstance(v, bool) for v in present):
        return [NA if isinstance(v, _RNA) else int(v) for v in values]
    return list(values)


def r_unlist(value: Mapping[str, Any] | list[Any]) -> dict[str, Any] | list[Any] | None:
    """``unlist()`` of a YAML mapping (named list) or sequence (list): one atomic vector.

    A mapping gives a dict of name -> value (nested elements flattened with R's
    dotted/numbered names), a sequence a list; ``NULL`` entries are dropped,
    ``NA`` is kept, and no element at all is ``NULL`` (``None``).
    """
    leaves: list[tuple[str, Any]] = []
    _leaves("", value, leaves)
    if not leaves:
        return None
    coerced = _coerce([v for _, v in leaves])
    if isinstance(value, Mapping):
        return {name: v for (name, _), v in zip(leaves, coerced, strict=True)}
    return coerced


def _construct_named_vector(loader: yaml.SafeLoader, node: yaml.Node) -> Any:
    if isinstance(node, yaml.MappingNode):
        return r_unlist({str(k): v for k, v in loader.construct_mapping(node, deep=True).items()})
    if isinstance(node, yaml.SequenceNode):
        return r_unlist(loader.construct_sequence(node, deep=True))
    return _text(loader, node)  # unlist("5") is "5": the handler sees the raw text


def _construct_character(loader: yaml.SafeLoader, node: yaml.Node) -> Any:
    if isinstance(node, yaml.SequenceNode):
        return [r_as_character(v) for v in loader.construct_sequence(node, deep=True)]
    if isinstance(node, yaml.MappingNode):
        return [r_as_character(v) for v in loader.construct_mapping(node, deep=True).values()]
    return _text(loader, node)  # as.character("yes") is "yes": the handler sees the raw text


GmsLoader.add_implicit_resolver("tag:yaml.org,2002:null", _NULL_RE, ["~", "n", "N", ""])
GmsLoader.add_implicit_resolver("tag:yaml.org,2002:bool", _BOOL_RE, list("yYnNtTfFoO"))
GmsLoader.add_implicit_resolver("tag:yaml.org,2002:int", _INT_RE, list("-+0123456789"))
GmsLoader.add_implicit_resolver("tag:yaml.org,2002:float", _FLOAT_RE, list("-+0123456789."))
GmsLoader.add_implicit_resolver(_NA_TAG, _NA_RE, ["."])
GmsLoader.add_implicit_resolver("tag:yaml.org,2002:merge", _MERGE_RE, ["<"])
GmsLoader.add_constructor("tag:yaml.org,2002:null", _construct_null)
GmsLoader.add_constructor("tag:yaml.org,2002:bool", _construct_bool)
GmsLoader.add_constructor("tag:yaml.org,2002:int", _construct_int)
GmsLoader.add_constructor("tag:yaml.org,2002:float", _construct_float)
GmsLoader.add_constructor(_NA_TAG, _construct_na)
GmsLoader.add_constructor("namedVector", _construct_named_vector)
GmsLoader.add_constructor("character", _construct_character)


def _strip_na(value: Any) -> Any:
    if isinstance(value, _RNA):
        return None
    if isinstance(value, dict):
        return {k: _strip_na(v) for k, v in value.items()}
    if isinstance(value, list):
        return [_strip_na(v) for v in value]
    return value


def load_yaml(text: str) -> Any:
    """``gms::loadConfig`` on a YAML string (``NA`` and ``NULL`` both come back as ``None``)."""
    return _strip_na(yaml.load(text, Loader=GmsLoader))  # noqa: S506 - GmsLoader derives from SafeLoader


# ---------------------------------------------------------------------------
# Loading
# ---------------------------------------------------------------------------
def load_config(path: str | os.PathLike[str], effects: Effects | None = None) -> dict[str, Any]:
    """The ``cfg`` of a run: ``R/getRunStatus.R:72``.

    A name ending in ``yml`` is parsed as YAML (``gms::loadConfig``), anything
    else is ``load()``-ed and its ``cfg`` object returned as plain Python
    (:func:`modelstats.rdata_io.pythonize`: R integer -> ``int``, double ->
    ``float``, character -> ``str``, logical -> ``bool``, ``NA``/``NULL`` ->
    ``None``, named vectors -> dicts). An RData file that defines no ``cfg``
    leaves R's ``cfg`` at ``NULL``, so it gives ``{}`` here. Error texts are
    R's: a missing file is ``cannot open the connection``, a non-RData file
    ``bad restore file magic number (file may be corrupted) -- no data loaded``.
    """
    name = os.fspath(path)
    if name.endswith("yml"):
        if effects is None:
            # yaml.load_file() / file() apply path.expand() (a leading ~ only)
            with open(os.path.expanduser(name), "rb") as handle:
                raw = handle.read()
        else:
            raw = effects.read_bytes(name)
        loaded = load_yaml(raw.decode("utf-8"))
        if loaded is None:
            return {}
        if not isinstance(loaded, Mapping):
            msg = f"{name}: top level of the YAML config is not a mapping"
            raise TypeError(msg)
        return {str(key): value for key, value in loaded.items()}
    cfg = pythonize(read_rda(name, effects).get("cfg"))
    if cfg is None:
        return {}
    if not isinstance(cfg, Mapping):
        msg = f"{name}: the cfg object is not a list"
        raise TypeError(msg)
    return dict(cfg)
