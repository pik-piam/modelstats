"""The run type of a REMIND or MAgPIE run folder: ``R/colRunType.R`` (``colRunType()``).

Parity port (``migration/03-migration-plan.md`` section 3.2). The R function finds the configuration
file with an unanchored regular expression over ``dir(mydir)``, loads it and composes the type from
``cfg$gms``. Its failure modes are reproduced on purpose, because ``getRunStatus()`` guards the call
(``R/getRunStatus.R:69-74``) and ``rs`` users never see them, while the goldens under
``migration/goldens/runtype`` record them:

* a directory without a configuration file raises ``argument is of length zero`` (BUG-004, decision
  D-02 pending): ``paste0(mydir, "/", character(0))`` is ``"<dir>/"``, which exists, so the branch runs
  with ``cfg = NULL`` and ``if (NULL == "MAgPIE")`` fails at ``R/colRunType.R:20``;
* two matching names raise ``the condition has length > 1`` (BUG-003, D-01 pending), from the ``if``
  over a length-2 ``file.exists()`` at line 18;
* a path that is not a directory yields ``"NA"`` (line 17); the ``full.lst`` fallback of lines 36-42 is
  unreachable (BUG-017) and is not ported;
* a MAgPIE configuration without ``cfg$gms$optimization`` returns ``NULL`` (line 21), ``None`` here
  (BUG-046, decision D-28); the only caller then fails on ``out[i, "RunType"] <- NULL`` with
  ``replacement has length zero`` (``R/getRunStatus.R:73``), which the ``get_run_status`` port raises
  at that assignment, not here.

The directory listing goes through :class:`modelstats.env.Effects` (``dir()`` order and dot-file rule
live there); the configuration file is read by a :data:`ConfigLoader`, ``modelstats.config.load_config``
unless the caller injects another one (tests, or a loader that resolves an overlay).
"""

from __future__ import annotations

import os
from collections.abc import Callable
from pathlib import Path
from typing import Final, Protocol

from modelstats.config import load_config
from modelstats.env import Effects, PathLike
from modelstats.rdata_io import RScalar, r_as_character

__all__ = [
    "ARGUMENT_OF_LENGTH_ZERO",
    "CONDITION_LENGTH_ABOVE_ONE",
    "CONFIG_FILE_PATTERN",
    "NO_RUN_TYPE",
    "EXISTS_CALL",
    "MODEL_NAME_CALL",
    "ONE_REGION_CALL",
    "ConfigLoader",
    "RunConfigLike",
    "RunTypeError",
    "col_run_type",
    "compose_run_type",
    "config_path_for",
]

CONFIG_FILE_PATTERN: Final = "config.Rdata|config.yml"
"""``R/colRunType.R:14``: ``grep("config.Rdata|config.yml", dir(mydir))``, unanchored, ``.`` any character."""

ARGUMENT_OF_LENGTH_ZERO: Final = "argument is of length zero"
"""R's message for ``if (logical(0))``: ``R/colRunType.R:20`` with ``cfg = NULL`` (BUG-004)."""

CONDITION_LENGTH_ABOVE_ONE: Final = "the condition has length > 1"
"""R's message for ``if (c(TRUE, TRUE))``: ``R/colRunType.R:18`` with two matching names (BUG-003)."""

NO_RUN_TYPE: Final = "NA"
"""``R/colRunType.R:17``: the value for a path that is not a directory."""

_MAGPIE: Final = "MAgPIE"
_ONE_REGION_PREFIX: Final = "testOneRegi"


class RunTypeError(RuntimeError):
    """An R error of ``colRunType()``; ``str(error)`` is the R condition message (the golden ``error``).

    ``call`` is the failing R statement as ``deparse(conditionCall(e), nlines = 1L)`` prints it (the
    ``if`` of ``R/colRunType.R:18``, ``:20`` or ``:24``), what ``try()`` puts after ``Error in``.
    """

    def __init__(self, message: str, *, call: str | None = None) -> None:
        """``message`` is R's condition message, ``call`` the deparsed call it is reported under."""
        super().__init__(message)
        self.call = call


EXISTS_CALL: Final = 'if (file.exists(paste0(mydir, "/", cfgf))) {'
"""``R/colRunType.R:18`` as R deparses it: the call of the ``the condition has length > 1`` error."""

MODEL_NAME_CALL: Final = 'if (cfg[["model_name"]] == "MAgPIE") {'
"""``R/colRunType.R:20``: the call of the ``argument is of length zero`` error for a ``NULL`` ``cfg``."""

ONE_REGION_CALL: Final = 'if (grepl("^testOneRegi", out)) {'
"""``R/colRunType.R:24``: the call of the ``argument is of length zero`` error for a ``NULL`` optimization."""


class RunConfigLike(Protocol):
    """The ``cfg`` fields ``colRunType()`` reads, as R scalars (``NULL`` is ``None``).

    ``modelstats.config.RunConfig`` satisfies this; tests pass a small dataclass. Values keep their R
    type (a numeric ``cm_nash_mode`` of ``1`` is a number, a character ``"1"`` a string), because R's
    ``==`` coerces the number to character and the port does the same.
    """

    @property
    def model_name(self) -> RScalar:
        """``cfg$model_name`` (``R/colRunType.R:20``)."""
        ...

    @property
    def optimization(self) -> RScalar:
        """``cfg$gms$optimization`` (lines 21, 23)."""
        ...

    @property
    def cm_nash_mode(self) -> RScalar:
        """``cfg$gms$cm_nash_mode`` (lines 25, 28)."""
        ...

    @property
    def cm_quick_mode(self) -> RScalar:
        """``cfg$gms$cm_quick_mode`` (line 26)."""
        ...

    @property
    def c_testOneRegi_region(self) -> RScalar:  # noqa: N802 - the R spelling of the switch
        """``cfg$gms$c_testOneRegi_region`` (line 26)."""
        ...

    @property
    def CES_parameters(self) -> RScalar:  # noqa: N802 - the R spelling of the switch
        """``cfg$gms$CES_parameters`` (line 29)."""
        ...

    @property
    def cm_MAgPIE_coupling(self) -> RScalar:  # noqa: N802 - the R spelling of the switch
        """``cfg$gms$cm_MAgPIE_coupling`` (line 30)."""
        ...

    @property
    def cm_MAgPIE_Nash(self) -> RScalar:  # noqa: N802 - the R spelling of the switch
        """``cfg$gms$cm_MAgPIE_Nash`` (line 30)."""
        ...

    @property
    def c_empty_model(self) -> RScalar:
        """``cfg$gms$c_empty_model`` (line 32)."""
        ...


type ConfigLoader = Callable[[Path], RunConfigLike]
"""Reads the configuration file at a path; the default is ``modelstats.config.load_config``."""


def col_run_type(mydir: PathLike, env: Effects, load: ConfigLoader = load_config) -> str | None:
    """``colRunType(mydir)`` (``R/colRunType.R:11-45``).

    Args:
        mydir: the run folder, as the R caller passes it (relative paths resolve against ``env.cwd``).
        env: the environment door, for ``dir()`` and the directory test.
        load: the configuration reader; it receives the path R would load.

    Returns:
        the run type string, ``"NA"`` when ``mydir`` is not a directory, or ``None`` (R's ``NULL``) for a
        MAgPIE configuration without ``cfg$gms$optimization``.

    Raises:
        RunTypeError: with R's message, for a directory without a configuration file
            (``argument is of length zero``) or with two matching names (``the condition has length > 1``).
    """
    directory = os.fspath(mydir)
    matches = env.listdir_like_r(directory, pattern=CONFIG_FILE_PATTERN)  # R:14
    if len(matches) > 1:
        raise RunTypeError(CONDITION_LENGTH_ABOVE_ONE, call=EXISTS_CALL)  # R:18: length-2 logical (BUG-003)
    if not matches:
        # R:18 tests file.exists("<mydir>/"), true exactly for a directory; R:20 then fails on cfg = NULL.
        if env.is_dir(directory):
            raise RunTypeError(ARGUMENT_OF_LENGTH_ZERO, call=MODEL_NAME_CALL)  # BUG-004
        return NO_RUN_TYPE  # R:17; the full.lst branch (R:36-42) is unreachable (BUG-017)
    return compose_run_type(load(config_path_for(directory, matches[0])))


def config_path_for(directory: str, matched_name: str) -> Path:
    """The file R loads for the matched name (``R/colRunType.R:19``).

    A name containing ``yml`` makes R call ``loadConfig(file.path(mydir, "config.yml"))``, the literal
    name regardless of what matched (BUG-003); any other name is ``load()``-ed as matched.
    """
    if "yml" in matched_name:
        return Path(directory, "config.yml")
    return Path(directory, matched_name)


def compose_run_type(cfg: RunConfigLike) -> str | None:
    """The run type of a loaded configuration (``R/colRunType.R:20-35``).

    MAgPIE returns ``cfg$gms$optimization`` as is, ``NULL`` (``None``) included (line 21; the caller
    fails on the assignment, ``R/getRunStatus.R:73``, BUG-046, D-28); REMIND composes it
    (:func:`_remind_run_type`).

    Raises:
        RunTypeError: ``argument is of length zero`` when ``model_name`` is ``NULL`` (line 20) or, for
            REMIND, when ``optimization`` is ``NULL`` (``grepl()`` on ``NULL`` at line 24).
    """
    model_name = r_as_character(cfg.model_name)
    if model_name is None:
        raise RunTypeError(ARGUMENT_OF_LENGTH_ZERO, call=MODEL_NAME_CALL)  # R:20: NULL == "MAgPIE"
    if model_name == _MAGPIE:
        return _magpie_run_type(cfg)
    return _remind_run_type(cfg)


def _magpie_run_type(cfg: RunConfigLike) -> str | None:
    """``R/colRunType.R:21``: the optimization switch, uncomposed; ``NULL`` stays ``NULL`` (``None``)."""
    return r_as_character(cfg.optimization)


def _remind_run_type(cfg: RunConfigLike) -> str:
    """``R/colRunType.R:23-34``: debug, calibration and coupling markers, one-region modes, empty model."""
    optimization = r_as_character(cfg.optimization)
    if optimization is None:
        raise RunTypeError(ARGUMENT_OF_LENGTH_ZERO, call=ONE_REGION_CALL)  # R:24: grepl() on NULL
    if optimization.startswith(_ONE_REGION_PREFIX):  # R:24-26
        out = _r_paste(_one_region_mode(cfg), cfg.c_testOneRegi_region)
    else:  # R:28-30
        out = optimization
        if _is_debug(cfg):
            out = f"{out} debug"
        if _is_true_equal(cfg.CES_parameters, "calibrate"):
            out = f"Calib_{out}"
        if _is_true_equal(cfg.cm_MAgPIE_coupling, "on") or _is_true_equal(cfg.cm_MAgPIE_Nash, 1):
            out = f"{out} + mag"
    if _is_true_equal(cfg.c_empty_model, "on"):  # R:32-34
        out = "empty model"
    return out


def _one_region_mode(cfg: RunConfigLike) -> str:
    """``R/colRunType.R:25-26``: ``debug`` beats ``quick`` beats the plain ``testOneRegi``."""
    if _is_debug(cfg):
        return "debug"
    if _is_true_equal(cfg.cm_quick_mode, "on"):
        return "quick"
    return _ONE_REGION_PREFIX


def _is_debug(cfg: RunConfigLike) -> bool:
    """``isTRUE(cfg$gms$cm_nash_mode == "debug" | cfg$gms$cm_nash_mode == 1)`` (lines 25 and 28)."""
    return _is_true_equal(cfg.cm_nash_mode, "debug") or _is_true_equal(cfg.cm_nash_mode, 1)


def _is_true_equal(value: RScalar, target: str | int) -> bool:
    """R's ``isTRUE(value == target)`` for a scalar ``value``.

    ``NULL`` (``None``) compares to ``logical(0)``, never TRUE. With a character on either side R
    compares the ``as.character()`` forms (``"1" == 1`` is TRUE, ``"1.0" == 1`` is FALSE); otherwise
    numerically (``TRUE == 1`` is TRUE).
    """
    if value is None:
        return False
    if isinstance(target, str):
        return r_as_character(value) == target
    if isinstance(value, str):
        return value == str(target)
    return float(value) == float(target)


def _r_paste(first: str, second: RScalar) -> str:
    """``paste(first, second)`` with the default separator: ``NULL`` is dropped but the space stays.

    ``paste("testOneRegi", NULL)`` is ``"testOneRegi "`` (verified in R 4.6.1).
    """
    text = r_as_character(second)
    return f"{first} " if text is None else f"{first} {text}"
