"""Run configuration files: ``config.Rdata`` (REMIND) and ``config.yml`` (MAgPIE).

R reference: ``R/getRunStatus.R:41,72`` and ``R/colRunType.R:14,19`` find the file with the
*unanchored* regular expression ``config.Rdata|config.yml`` over ``dir(run)`` (register BUG-003,
decision D-01: parity; the listing is ``Effects.listdir_like_r`` with ``run_type.CONFIG_FILE_PATTERN``,
in R's collation order) and load it with ``load()`` (object ``cfg``) or ``gms::loadConfig()``
(``yaml::read_yaml`` with the ``namedVector`` and ``character`` handlers,
``migration/02-python-libraries.md`` section 4.3). This module is the loading half: every field the R
package reads from the resulting ``cfg`` list is an attribute of :class:`RunConfig`; the whole list
stays in ``RunConfig.raw``.
"""

from __future__ import annotations

import enum
from collections.abc import Mapping
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import yaml

from modelstats.errors import ModelstatsError
from modelstats.rdata_io import RDataError, RScalar, r_as_character, read_rda, vector


class ConfigError(ModelstatsError):
    """A configuration file is missing, ambiguous or does not hold what the R code expects."""


class ConfigKind(enum.Enum):
    """Which loader a configuration file name selects."""

    RDATA = "Rdata"
    YAML = "yml"


def config_kind(name: str) -> ConfigKind:
    """``grepl("yml$", cfgf)`` (``R/getRunStatus.R:72``): ``loadConfig()`` for ``yml``, else ``load()``.

    ``colRunType`` tests the looser ``grepl("yml", cfgf)`` and then loads the literal
    ``config.yml`` (``R/colRunType.R:19``); for the real file names the two agree.
    """
    return ConfigKind.YAML if name.endswith("yml") else ConfigKind.RDATA


# ---------------------------------------------------------------------------------------------
# Loading
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True, slots=True)
class RunConfig:
    """The ``cfg`` list of a run, with the fields the R package reads as typed attributes.

    The names and paths R only pastes (``cfg$title`` in ``REMIND_generic_<title>.mif``,
    ``cfg$results_folder``) are strings as ``as.character()`` makes them. The ``gms`` switches
    keep their R type, because R's ``==`` coerces (``"1" == 1`` is ``TRUE``) and the rules of
    ``colRunType`` (``R/colRunType.R:25-32``) are applied by the caller; a zero-length vector is
    ``None`` like ``NULL``, which is what every ``isTRUE(... == ...)`` test and every ``if`` on it
    amounts to in R. Attributes keep the R spelling of the switches so that a REMIND developer can
    grep for them.
    """

    source: Path
    kind: ConfigKind
    raw: Mapping[str, Any]
    gms: Mapping[str, Any]
    model_name: str | None  # cfg$model_name (R/colRunType.R:20)
    title: str | None  # cfg$title (R/getRunStatus.R:128-129, 319-320)
    results_folder: str | None  # cfg$results_folder (R/modeltests.R:317-320)
    remind_folder: str | None  # cfg$remind_folder (R/modeltests.R:354)
    path_magpie: str | None  # cfg$path_magpie (R/getRunStatus.R:192)
    cfg_mag_results_folder: str | None  # cfg$cfg_mag$results_folder (R/getRunStatus.R:192-194)
    optimization: RScalar  # cfg$gms$optimization (R/colRunType.R:21,23)
    cm_iteration_max: RScalar  # cfg$gms$cm_iteration_max (R/getRunStatus.R:141)
    cm_nash_autoconverge: RScalar  # cfg$gms$cm_nash_autoconverge (R/getRunStatus.R:142)
    cm_nash_mode: RScalar  # cfg$gms$cm_nash_mode (R/colRunType.R:25,28)
    cm_quick_mode: RScalar  # cfg$gms$cm_quick_mode (R/colRunType.R:26)
    c_testOneRegi_region: RScalar  # noqa: N815 - cfg$gms$c_testOneRegi_region (R/colRunType.R:26)
    CES_parameters: RScalar  # noqa: N815 - cfg$gms$CES_parameters (R/colRunType.R:29, getRunStatus.R:298)
    cm_MAgPIE_coupling: RScalar  # noqa: N815 - cfg$gms$cm_MAgPIE_coupling (R/colRunType.R:30)
    cm_MAgPIE_Nash: RScalar  # noqa: N815 - cfg$gms$cm_MAgPIE_Nash (R/colRunType.R:30)
    c_empty_model: RScalar  # cfg$gms$c_empty_model (R/colRunType.R:32)

    def gms_scalar(self, name: str) -> RScalar:
        """``cfg$gms$<name>`` as a scalar, for switches beyond the typed attributes."""
        return _scalar_field(self.source, self.gms, name, prefix="gms$")


def _scalar_field(source: Path, mapping: Mapping[str, Any], name: str, prefix: str = "") -> RScalar:
    """``cfg$<name>`` unwrapped: ``NULL``, ``NA`` and a zero-length vector are ``None``."""
    try:
        elements = vector(mapping.get(name))
    except RDataError as error:
        raise ConfigError(f"{source}: cfg${prefix}{name} is not an atomic vector: {error}") from error
    if len(elements) > 1:
        raise ConfigError(f"{source}: cfg${prefix}{name} is a vector of length {len(elements)}, not a scalar")
    return elements[0] if elements else None


def _string_field(source: Path, mapping: Mapping[str, Any], name: str, prefix: str = "") -> str | None:
    """``cfg$<name>`` as ``paste0()``/``file.path()`` would coerce it (``as.character``)."""
    return r_as_character(_scalar_field(source, mapping, name, prefix))


def _sub_list(source: Path, raw: Mapping[str, Any], name: str) -> Mapping[str, Any]:
    """``cfg$<name>`` as a mapping; ``NULL`` (absent) is an empty mapping, anything else an error."""
    value = raw.get(name)
    if value is None:
        return {}
    if not isinstance(value, Mapping):
        raise ConfigError(f"{source}: cfg${name} is not a list")
    return {str(key): item for key, item in value.items()}


def _run_config(source: Path, kind: ConfigKind, raw: Mapping[str, Any]) -> RunConfig:
    gms = _sub_list(source, raw, "gms")
    cfg_mag = _sub_list(source, raw, "cfg_mag")

    def string(name: str) -> str | None:
        return _string_field(source, raw, name)

    def switch(name: str) -> RScalar:
        return _scalar_field(source, gms, name, prefix="gms$")

    return RunConfig(
        source=source,
        kind=kind,
        raw=raw,
        gms=gms,
        model_name=string("model_name"),
        title=string("title"),
        results_folder=string("results_folder"),
        remind_folder=string("remind_folder"),
        path_magpie=string("path_magpie"),
        cfg_mag_results_folder=_string_field(source, cfg_mag, "results_folder", prefix="cfg_mag$"),
        optimization=switch("optimization"),
        cm_iteration_max=switch("cm_iteration_max"),
        cm_nash_autoconverge=switch("cm_nash_autoconverge"),
        cm_nash_mode=switch("cm_nash_mode"),
        cm_quick_mode=switch("cm_quick_mode"),
        c_testOneRegi_region=switch("c_testOneRegi_region"),
        CES_parameters=switch("CES_parameters"),
        cm_MAgPIE_coupling=switch("cm_MAgPIE_coupling"),
        cm_MAgPIE_Nash=switch("cm_MAgPIE_Nash"),
        c_empty_model=switch("c_empty_model"),
    )


def load_config_rdata(path: Path) -> RunConfig:
    """``load(config.Rdata)`` and the ``cfg`` object it defines (``R/getRunStatus.R:72``)."""
    try:
        objects = read_rda(path)
    except (OSError, RDataError) as error:
        raise ConfigError(f"cannot load {path}: {error}") from error
    cfg = objects.get("cfg")
    if not isinstance(cfg, Mapping):
        raise ConfigError(f"{path} does not define a 'cfg' list (objects: {', '.join(objects) or 'none'})")
    return _run_config(path, ConfigKind.RDATA, {str(key): value for key, value in cfg.items()})


class GmsLoader(yaml.SafeLoader):
    """``yaml::read_yaml(file, handlers = list(namedVector = unlist, character = as.character))``.

    ``gms::saveConfig`` tags named vectors ``!<namedVector>`` (a mapping) and empty character vectors
    ``!<character> []``; R's libyaml writer emits ``yes`` for ``TRUE`` and ``~`` for ``NULL``, which
    PyYAML's YAML 1.1 ``SafeLoader`` reads back as ``True`` and ``None`` (``02`` section 4.3).
    """


def _named_vector(loader: yaml.SafeLoader, node: yaml.Node) -> dict[Any, Any]:
    if not isinstance(node, yaml.MappingNode):
        raise yaml.constructor.ConstructorError(
            None, None, "!<namedVector> must be a mapping", node.start_mark
        )
    return loader.construct_mapping(node)


def _character(loader: yaml.SafeLoader, node: yaml.Node) -> list[Any]:
    if not isinstance(node, yaml.SequenceNode):
        raise yaml.constructor.ConstructorError(
            None, None, "!<character> must be a sequence", node.start_mark
        )
    return loader.construct_sequence(node)


GmsLoader.add_constructor("namedVector", _named_vector)
GmsLoader.add_constructor("character", _character)


def load_config_yaml(path: Path) -> RunConfig:
    """``gms::loadConfig(config.yml)`` (``R/getRunStatus.R:72``, ``R/colRunType.R:19``)."""
    try:
        with path.open(encoding="utf-8") as handle:
            loaded = yaml.load(handle, Loader=GmsLoader)  # noqa: S506 - SafeLoader subclass
    except (OSError, UnicodeDecodeError, yaml.YAMLError) as error:
        raise ConfigError(f"cannot load {path}: {error}") from error
    if not isinstance(loaded, Mapping):
        raise ConfigError(f"{path} does not hold a mapping")
    return _run_config(path, ConfigKind.YAML, {str(key): value for key, value in loaded.items()})


def load_config(path: Path) -> RunConfig:
    """Load a configuration file with the loader its name selects (:func:`config_kind`).

    Every failure (missing or unreadable file, no ``cfg`` object, malformed YAML, a non-scalar in a
    field R uses as a scalar) raises :class:`ConfigError`, the counterpart of the R error that
    aborts ``getRunStatus()`` and ``colRunType()`` at this point.
    """
    if config_kind(path.name) is ConfigKind.YAML:
        return load_config_yaml(path)
    return load_config_rdata(path)
