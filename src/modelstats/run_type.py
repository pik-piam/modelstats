"""``col_run_type``: the run type of a REMIND or MAgPIE run folder (``R/colRunType.R``).

The function follows ``R/colRunType.R`` line by line, including its error paths
(BUG-003, BUG-004 and BUG-017 of ``migration/04-bug-register.md``; D-01 and D-02
of the decision table are pending, so the port reproduces R):

* the config file is discovered by the unanchored regex ``config.Rdata|config.yml``
  over R's ``dir()`` listing (``modelstats.config.config_matches``); two or more
  matches make R's ``if (file.exists(...))`` fail with ``the condition has length > 1``;
* without a config file ``paste0(mydir, "/", character(0))`` is ``"<mydir>/"``, which
  exists for a directory, so R enters the branch with ``cfg = NULL`` and fails with
  ``argument is of length zero`` (BUG-004); the ``full.lst`` fallback is unreachable
  for an existing directory (BUG-017) and a non-existent path yields ``"NA"``;
* a config file whose name contains ``yml`` is loaded as the literal
  ``<mydir>/config.yml`` regardless of the matched name (BUG-003);
* a config whose ``gms$optimization`` is ``NA`` returns a real NA (``None``, R's
  ``NA_character_``, ``{"value": null}`` in the runtype goldens) unless a ``paste()`` step
  turned it into text (``NA debug``, ``Calib_NA``, ``NA + mag``); the string ``"NA"`` is
  R's initial ``out``, returned for a path that is not a directory.
"""

from __future__ import annotations

import math
from typing import TYPE_CHECKING, Any

from modelstats.errors import RParityError

if TYPE_CHECKING:
    from collections.abc import Mapping

    from modelstats.env import Effects

__all__ = ["col_run_type"]


def _effects(effects: Effects | None) -> Effects:
    if effects is not None:
        return effects
    from modelstats.env import default_effects

    return default_effects()


def _config_matches(mydir: str, effects: Effects) -> list[str]:
    """``grep("config.Rdata|config.yml", dir(mydir), value = TRUE)`` (``modelstats.config``, lazily imported)."""
    from modelstats.config import config_matches

    return config_matches(mydir, effects)


def _load_config(path: str, effects: Effects) -> Mapping[str, Any]:
    """``modelstats.config.load_config`` (imported lazily: another unit owns it)."""
    from modelstats.config import load_config

    return load_config(path, effects)


# --- R value semantics on the loaded config -------------------------------------------------------

_NULL = object()  # R's NULL: a missing list element


def _r_get(mapping: object, key: str) -> object:
    """``x[[key]]`` / ``x$key`` on an R list: NULL when absent or not a list.

    A key that is present with ``None`` is a length-1 NA (that is how the loaders
    represent ``NA``); only an absent key is NULL.
    """
    if not isinstance(mapping, dict):
        return _NULL
    return mapping.get(key, _NULL)


def _r_vector(x: object) -> list[object]:
    """The elements of an R value as a Python list (NULL -> [], NA -> [None], scalar -> [scalar])."""
    if x is _NULL:
        return []
    if x is None:
        return [None]
    if isinstance(x, str | bytes | bool | int | float | dict):
        return [x]
    try:
        items = list(x)  # type: ignore[call-overload]
    except TypeError:
        items = [x]
    out: list[object] = []
    for item in items:
        if hasattr(item, "item") and not isinstance(item, str | bytes):
            item = item.item()  # numpy scalar or 0-d array -> Python scalar
        out.append(item)
    return out


def _is_na(v: object) -> bool:
    return v is None or (isinstance(v, float) and math.isnan(v))


def _r_num_str(v: float) -> str:
    """``as.character()`` of an R number (15 significant digits, R's fixed/scientific choice)."""
    if math.isnan(v):
        return "NA"
    if math.isinf(v):
        return "Inf" if v > 0 else "-Inf"
    if v == int(v) and abs(v) < 1e15:
        fixed = str(int(v))
    else:
        fixed = f"{v:.15g}" if "e" not in f"{v:.15g}" else ""
    mantissa, exponent = f"{v:.14e}".split("e")
    mantissa = mantissa.rstrip("0").rstrip(".")
    exp = int(exponent)
    sci = f"{mantissa}e{'+' if exp >= 0 else '-'}{abs(exp):02d}"
    if fixed and len(fixed) <= len(sci):
        return fixed
    return sci


def _as_character(v: object) -> str:
    """``as.character()`` of one R scalar."""
    if isinstance(v, bool):
        return "TRUE" if v else "FALSE"
    if isinstance(v, int | float):
        return _r_num_str(float(v))
    if isinstance(v, bytes):
        return v.decode("utf-8", errors="replace")
    return str(v)


def _r_eq(v: object, target: str | int) -> bool | None:
    """``v == target`` for one R scalar: True, False or None (NA)."""
    if _is_na(v):
        return None
    if isinstance(target, str):
        return _as_character(v) == target
    if isinstance(v, bool | int | float):
        return float(v) == float(target)
    return _as_character(v) == _as_character(target)


def _is_true_eq(x: object, target: str | int) -> bool | None:
    """``x == target`` as R evaluates it before ``isTRUE``: NA/None for NULL or length != 1."""
    vec = _r_vector(x)
    if len(vec) != 1:
        return None
    return _r_eq(vec[0], target)


def _is_true(v: bool | None) -> bool:
    """``isTRUE()``: only a length-1, non-NA TRUE passes."""
    return v is True


def _r_or(a: bool | None, b: bool | None) -> bool | None:
    """R's ``|`` with NA: TRUE wins over NA, NA wins over FALSE."""
    if a is True or b is True:
        return True
    if a is None or b is None:
        return None
    return False


def _condition_scalar(x: object) -> object:
    """The scalar an ``if ()`` condition is built from; the two R errors for bad lengths."""
    vec = _r_vector(x)
    if len(vec) == 0:
        raise RParityError("argument is of length zero")
    if len(vec) > 1:
        raise RParityError("the condition has length > 1")
    return vec[0]


def _remind_run_type(gms: object) -> str | None:
    """The REMIND composition of ``R/colRunType.R`` lines 23-34; ``None`` is a real NA.

    ``paste()`` / ``paste0()`` coerce an NA optimization to the text ``NA`` (``NA debug``,
    ``Calib_NA``, ``NA + mag``, verified in R), so the result is a real NA only when no
    composition step touched it.
    """
    optimization = _condition_scalar(_r_get(gms, "optimization"))
    na = _is_na(optimization)
    # grepl("^testOneRegi", out): grepl on NA is FALSE, numbers are coerced to character
    out = "NA" if na else _as_character(optimization)
    nash_mode = _r_get(gms, "cm_nash_mode")
    debug = _is_true(_r_or(_is_true_eq(nash_mode, "debug"), _is_true_eq(nash_mode, 1)))
    if not _is_na(optimization) and out.startswith("testOneRegi"):
        if debug:
            mode = "debug"
        elif _is_true(_is_true_eq(_r_get(gms, "cm_quick_mode"), "on")):
            mode = "quick"
        else:
            mode = "testOneRegi"
        # paste(mode, cfg$gms$c_testOneRegi_region): a NULL or zero-length region is recycled
        # to "" and the separator is still emitted (trailing space kept); NA prints as NA
        region = _r_vector(_r_get(gms, "c_testOneRegi_region"))
        region_text = "" if not region else ("NA" if _is_na(region[0]) else _as_character(region[0]))
        out = f"{mode} {region_text}"
    else:
        if debug:
            out = f"{out} debug"
        if _is_true(_is_true_eq(_r_get(gms, "CES_parameters"), "calibrate")):
            out = f"Calib_{out}"
        coupled = _is_true(_is_true_eq(_r_get(gms, "cm_MAgPIE_coupling"), "on"))
        nash_coupled = _is_true(_is_true_eq(_r_get(gms, "cm_MAgPIE_Nash"), 1))
        if coupled or nash_coupled:
            out = f"{out} + mag"
    if _is_true(_is_true_eq(_r_get(gms, "c_empty_model"), "on")):
        out = "empty model"
    # still the untouched NA only when nothing was pasted to it (a literal string "NA" has na False)
    return None if na and out == "NA" else out


def col_run_type(mydir: str = ".", effects: Effects | None = None) -> str | None:
    """What is the type of this run? (``R/colRunType.R``, parity including its bugs.)

    Returns ``cfg$gms$optimization`` for MAgPIE, the REMIND composition otherwise
    (``nash``, ``nash debug``, ``Calib_nash``, ``nash + mag``, ``testOneRegi EUR``,
    ``debug EUR``, ``quick EUR``, ``empty model``) and the string ``"NA"`` for a path that
    is not a directory. ``None`` is R's real ``NA``: a config whose ``gms$optimization`` is
    ``NA`` and that no ``paste()`` step turned into text (``getRunStatus`` stores it as an
    NA cell, blank in ``printOutput``). Raises :class:`RParityError` exactly where R errors:

    * ``argument is of length zero`` for a directory without a config file (BUG-004) or
      a config without ``model_name`` / ``gms$optimization`` (REMIND);
    * ``replacement has length zero`` for a MAgPIE config without ``gms$optimization``:
      R returns ``NULL`` and its only caller fails at the assignment (PORT-009);
    * ``the condition has length > 1`` when two or more entries match the config regex
      (BUG-003; raised by ``find_config_file``);
    * ``missing value where TRUE/FALSE needed`` when ``model_name`` is NA.
    """
    eff = _effects(effects)
    matches = _config_matches(mydir, eff)
    if len(matches) > 1:
        # if (file.exists(paste0(mydir, "/", cfgf))) with a length-2 cfgf (BUG-003)
        raise RParityError("the condition has length > 1")
    if not matches:
        # R: paste0(mydir, "/", character(0)) == "<mydir>/" exists for a directory, so the
        # branch is entered with cfg NULL and `if (cfg[["model_name"]] == "MAgPIE")` fails
        # (BUG-004). For a non-existent path neither "<mydir>/" nor "<mydir>/full.lst"
        # exists and R returns the initial "NA" (the full.lst branch is dead, BUG-017).
        if eff.is_dir(mydir):
            raise RParityError("argument is of length zero")
        return "NA"
    cfgf = matches[0]
    if "yml" in cfgf:
        # ifelse(grepl("yml", cfgf), cfg <- loadConfig(file.path(mydir, "config.yml")), ...):
        # the literal name config.yml, whatever matched
        cfg: object = _load_config(f"{mydir}/config.yml", eff)
    else:
        # load(paste0(mydir, "/", cfgf)) defines `cfg` in the function environment
        cfg = _load_config(f"{mydir}/{cfgf}", eff)
    model_name = _condition_scalar(_r_get(cfg, "model_name"))
    is_magpie = _r_eq(model_name, "MAgPIE")
    if is_magpie is None:
        raise RParityError("missing value where TRUE/FALSE needed")
    gms = _r_get(cfg, "gms")
    if is_magpie:
        # out <- cfg$gms$optimization (returned as is)
        optimization = _r_vector(_r_get(gms, "optimization"))
        if not optimization:
            # R returns NULL here; the only caller (getRunStatus) then fails assigning it
            raise RParityError("replacement has length zero")
        return None if _is_na(optimization[0]) else _as_character(optimization[0])
    return _remind_run_type(gms)
