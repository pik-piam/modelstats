"""The type of a run: ``nash``, ``testOneRegi EUR``, ``Calib_nash debug + mag``, ``nlp_apr17``, ...

R source: R/colRunType.R.

Deviations and fixes (details in PORTING-BUGS.md):

- the configuration file is found by its exact name, ``config.Rdata`` before ``config.yml``
  (BUG-001); a loaded ``cfg`` may be passed in to spare a second read;
- without a configuration file R stopped with an error, so its ``full.lst`` branch never ran;
  the port reads the ``$setGlobal optimization`` and ``$setglobal CES_parameters`` switches from
  ``full.lst`` as that branch intended, and gives ``"NA"`` when neither file tells (BUG-002);
- a configuration without ``gms$optimization`` gives ``"NA"`` (R: an error for REMIND, ``NULL``
  for MAgPIE), one without ``model_name`` follows the REMIND rules instead of an R error; a list
  where a scalar is expected counts as missing; ``NA`` values count as missing where R printed
  ``NA`` (BUG-003);
- ``paste()`` with a missing ``c_testOneRegi_region`` leaves no trailing space (BUG-007).
"""

from __future__ import annotations

import re
from pathlib import Path
from typing import Any

from modelstats._config import cfg_get, find_config, load_config, r_equals, r_equals_number
from modelstats._textscan import first_match
from modelstats._timefmt import r_str

__all__ = ["run_type"]

_OPTIMIZATION = re.compile(r"(?i)^\s*(?:\d+\s+)?\$setglobal\s+optimization\s+(\S+)")
_CES_PARAMETERS = re.compile(r"(?i)^\s*(?:\d+\s+)?\$setglobal\s+CES_parameters\s+(\S+)")


def _scalar_text(value: Any) -> str | None:
    """A configuration value as text; ``None`` for a missing value or a list."""
    if value is None or isinstance(value, list | dict):
        return None
    return r_str(value)


def _is_debug(cfg: dict[str, Any]) -> bool:
    """Is the nash mode ``"debug"`` (or its numeric code 1)?"""
    mode = cfg_get(cfg, "gms", "cm_nash_mode")
    return r_equals(mode, "debug") or r_equals_number(mode, 1)


def _from_config(cfg: dict[str, Any]) -> str:
    """The run type from the switches of a loaded configuration."""
    out = _scalar_text(cfg_get(cfg, "gms", "optimization"))
    if out is None:
        return "NA"
    if r_equals(cfg_get(cfg, "model_name"), "MAgPIE"):
        return out
    if out.startswith("testOneRegi"):
        if _is_debug(cfg):
            kind = "debug"
        elif r_equals(cfg_get(cfg, "gms", "cm_quick_mode"), "on"):
            kind = "quick"
        else:
            kind = "testOneRegi"
        region = _scalar_text(cfg_get(cfg, "gms", "c_testOneRegi_region"))
        out = kind if region is None else f"{kind} {region}"
    else:
        if _is_debug(cfg):
            out = f"{out} debug"
        if r_equals(cfg_get(cfg, "gms", "CES_parameters"), "calibrate"):
            out = f"Calib_{out}"
        coupled = r_equals(cfg_get(cfg, "gms", "cm_MAgPIE_coupling"), "on")
        nash_coupled = r_equals_number(cfg_get(cfg, "gms", "cm_MAgPIE_Nash"), 1)
        if coupled or nash_coupled:
            out = f"{out} + mag"
    if r_equals(cfg_get(cfg, "gms", "c_empty_model"), "on"):
        out = "empty model"
    return out


def _from_full_lst(full_lst: Path) -> str:
    """The optimization switch of ``full.lst`` (a run without configuration file)."""
    line = first_match(full_lst, _OPTIMIZATION.pattern)
    match = _OPTIMIZATION.search(line) if line is not None else None
    if match is None:
        return "NA"
    out = match[1]
    ces_line = first_match(full_lst, _CES_PARAMETERS.pattern)
    ces = _CES_PARAMETERS.search(ces_line) if ces_line is not None else None
    if ces is not None and ces[1] == "calibrate":
        out = f"Calib_{out}"
    return out


def run_type(run_dir: Path, cfg: dict[str, Any] | None = None) -> str:
    """What is the type of this run?

    Args:
        run_dir: Path to the folder where the run is performed.
        cfg: Its configuration, when the caller has loaded it already.

    Returns:
        The optimization switch, decorated with the debug, quick, calibration, coupling and
        empty-model switches of a REMIND run; ``"NA"`` when neither a configuration file nor
        ``full.lst`` tells.

    Raises:
        ConfigReadError: The configuration file cannot be read.
    """
    if cfg is None:
        config = find_config(run_dir)
        if config is not None:
            cfg = load_config(config)
    if cfg is not None:
        return _from_config(cfg)
    full_lst = run_dir / "full.lst"
    if full_lst.exists():
        return _from_full_lst(full_lst)
    return "NA"
