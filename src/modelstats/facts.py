"""The reading layer of a run folder: every file ``getRunStatus()`` looks at, read once and cached.

``migration/06-user-requests.md`` section 5.2 (decision D-25): one reading layer, two views. :class:`RunFiles`
composes the paths ``R/getRunStatus.R:40-50`` defines for one run folder and :class:`RunFacts` exposes what
those files contain as lazily cached *facts* - the configuration, the ``stats`` list of
``runstatistics.rda``, the selected GDX and the symbols read from it, the ``*** Status:`` lines, the last
``LOOPS`` value, the ``cm_iteration_max`` line of ``full.gms``, the ``slurmstepd`` errors, the warning scans
of both models, the mif and sanity inputs, the grid-log mtimes, the SLURM lookup - with no R vocabulary:
nothing here knows what ``"NA"``, ``"Run interrupted"`` or ``"sumErr"`` mean. ``run_status.py`` applies the
R rules of ``migration/03-migration-plan.md`` section 3.1 to these facts (the parity view pinned by the
status goldens); ``run.py`` presents the same facts as the public ``Run`` object (the clean view). Each fact
names the R lines it reads.

Every look at the environment goes through :class:`modelstats.env.Effects` (listings, existence, ``stat``,
``glob``, streams for the text scanners, the clock, ``squeue``). Libraries that open files themselves (the
GDX library, ``rdata``, the CSV counters) receive the physical file behind a logical path
(:func:`physical_path`): the same path in production, the overlay's answer under the ``FakeEffects`` of
the golden harness.

Errors: what R raises outside ``try()`` aborts ``getRunStatus()`` as a whole (``03`` 3.1, "Errors"), and
so does a damaged GDX, which kills the R process instead of raising (BUG-037, decision D-20). Both become a
:class:`RunReadError` (the package's, from ``modelstats.errors``, re-exported here), raised from the fact that
reads the file, with R's condition message where R has one; the views let it propagate. A result R's
``try()`` or ``react = "silent"`` turns into ``NULL`` is ``None`` or an empty list, never an exception.
A log that exists but cannot be opened is what ``system(..., intern = TRUE)`` makes of a grep, tac or tail
that cannot read it: ``character(0)`` and no error, so every text fact takes its no-match value
(:meth:`RunFacts._scan`); the one exception is the ``awk`` line of ``R/getRunStatus.R:189``, whose empty
result ``:190`` compares outside ``try()`` (:attr:`RunFacts.last_logtxt_line` raises R's abort).
"""

from __future__ import annotations

import io
import os
from collections.abc import Callable, Mapping
from dataclasses import dataclass
from functools import cached_property
from pathlib import Path, PurePosixPath
from typing import Any, BinaryIO, Final, Protocol, runtime_checkable

import pandas as pd

from modelstats import textscan
from modelstats.config import ConfigError, RunConfig, load_config
from modelstats.env import Effects, FileInfo, OverlayPaths, PathLike
from modelstats.errors import DeferredWarning, RunReadError
from modelstats.gdx import (
    ITERATION_SYMBOL,
    MODELSTAT_SYMBOLS,
    GdxCandidate,
    GdxFile,
    GdxReadError,
    flat_values,
    read_first_found,
    read_param,
    read_scalar,
    read_typed_scalar,
    select_latest_gdx,
)
from modelstats.rdata_io import RDataError, read_rds
from modelstats.run_type import CONFIG_FILE_PATTERN, EXISTS_CALL, RunTypeError, col_run_type
from modelstats.runfolders import r_basename
from modelstats.runstats import RunStatistics, read_run_statistics
from modelstats.slurm import SlurmError, SlurmLookup, lookup_slurm
from modelstats.tables import EmptyCsvError, fix_error_count, range_error_count, summation_error_count

__all__ = [
    "ARGUMENT_OF_LENGTH_ZERO",
    "CONDITION_LENGTH_ABOVE_ONE",
    "INVALID_DESCRIPTION",
    "MISSING_VALUE",
    "NO_LINES_IN_INPUT",
    "ABSENT_SYMBOL_WARNING",
    "ABSENT_TYPED_SYMBOL_WARNING",
    "CONVERGENCE_READ_CALL",
    "EXPAND_PATTERN_CALL",
    "GZFILE_CALL",
    "ITERATION_READ_CALL",
    "LAST_LINE_CALL",
    "LOAD_CONFIG_CALL",
    "LOAD_STATISTICS_CALL",
    "LOAD_YAML_CALL",
    "READ_CSV_CALL",
    "READ_RDS_CALL",
    "REPY_READ_CALL",
    "ABORT_THRESHOLD_SYMBOL",
    "ABORT_TRACK_SYMBOL",
    "CONVERGENCE_SYMBOL",
    "REPY_SYMBOL",
    "OverlayEffects",
    "RunFacts",
    "RunFiles",
    "RunReadError",
    "physical_path",
]

ARGUMENT_OF_LENGTH_ZERO: Final = "argument is of length zero"
"""R's message for ``if (logical(0))``: ``R/getRunStatus.R:91, 108`` with a ``NULL`` model name, ``:282``."""

MISSING_VALUE: Final = "missing value where TRUE/FALSE needed"
"""R's message for ``if (NA)``: ``R/getRunStatus.R:284`` without ``o_iterationNumber`` (BUG-038)."""

INVALID_DESCRIPTION: Final = "invalid 'description' argument"
"""R's message for ``load()`` on a vector of paths: ``R/getRunStatus.R:72`` with two matches (BUG-003)."""

CONDITION_LENGTH_ABOVE_ONE: Final = "the condition has length > 1"

# The R calls the errors above are reported under, as ``deparse(conditionCall(e), nlines = 1L)`` prints them
# (what ``try()`` puts after ``Error in``, ``R/loopRuns.R:57``); each is cited at its raise site.
GZFILE_CALL: Final = "gzfile(file)"
"""``load()`` on a vector of paths fails in ``gzfile(file)`` (``R/getRunStatus.R:72``, BUG-003)."""
LOAD_CONFIG_CALL: Final = 'load(paste0(ii, "/", cfgf))'
"""The ``.Rdata`` branch of ``R/getRunStatus.R:72``."""
LOAD_YAML_CALL: Final = 'loadConfig(paste0(ii, "/", cfgf))'
"""The ``.yml`` branch of ``R/getRunStatus.R:72``."""
LOAD_STATISTICS_CALL: Final = "load(fle, envir = runstatistics)"
"""``R/getRunStatus.R:78``."""
LAST_LINE_CALL: Final = 'if (lastLine == "Starting MAgPIE...") {'
"""``R/getRunStatus.R:190`` (the double blank of the source is not kept by ``deparse``)."""
READ_CSV_CALL: Final = "read.table(file = file, header = header, sep = sep, quote = quote, "
"""``read.csv2()`` fails inside ``read.table()``; the first ``deparse`` line of that call ends in a blank."""
READ_RDS_CALL: Final = "readRDS(projectSumFile)"
"""``R/getRunStatus.R:353``."""

# The readGDX() calls whose absent symbol R only warns about (react defaults to "warning"), deparsed, and
# the warning texts of gdx2 0.6.2 (verified 2026-10-01): the plain-name reads of ``:280`` and ``:288`` warn
# from readGDX itself, the typed read of ``:281`` from ``.expandPattern``.
ITERATION_READ_CALL: Final = 'readGDX(gdx = latest_gdx, "o_iterationNumber", format = "simplest")'
CONVERGENCE_READ_CALL: Final = (
    'readGDX(gdx = latest_gdx, "s80_bool", type = "Parameter", format = "simplest")'
)
REPY_READ_CALL: Final = 'readGDX(gdx = latest_gdx, "p80_repy")'
EXPAND_PATTERN_CALL: Final = ".expandPattern(c(...), gdx, format, type, react)"
ABSENT_SYMBOL_WARNING: Final = (
    "Error : User specified to read symbol {name}, but it does not exist in the source file"
)
ABSENT_TYPED_SYMBOL_WARNING: Final = "No element of {name} found in GDX! NULL returned"
"""R's message for ``if (c(TRUE, TRUE))``: ``R/colRunType.R:18`` after two ``yml`` matches (BUG-003)."""

NO_LINES_IN_INPUT: Final = "no lines available in input"
"""R's message for ``read.csv2()`` on an empty file: ``R/getRunStatus.R:327, 336, 345``."""

ABORT_THRESHOLD_SYMBOL: Final = "cm_abortOnConsecFail"
"""``R/getRunStatus.R:212``: the consecutive-infeasibility threshold stored in ``abort.gdx``."""

ABORT_TRACK_SYMBOL: Final = "p80_trackConsecFail"
"""``R/getRunStatus.R:213``: the per-region counter stored in ``abort.gdx``."""

CONVERGENCE_SYMBOL: Final = "s80_bool"
"""``R/getRunStatus.R:281``: the Nash convergence flag, read as a typed parameter."""

REPY_SYMBOL: Final = "p80_repy"
"""``R/getRunStatus.R:288``: the per-region solve report."""

_RUN_STATISTICS: Final = "runstatistics.rda"
_FULLDATA_GDX: Final = "fulldata.gdx"
_NON_OPTIMAL_GDX: Final = "non_optimal.gdx"
_FULL_GMS: Final = "full.gms"
_FULL_LOG: Final = "full.log"
_SLURM_LOG: Final = "slurm.log"
_LOG_TXT: Final = "log.txt"
_LOG_MAG_TXT: Final = "log-mag.txt"
_ABORT_GDX: Final = "abort.gdx"
_VALIDATION_MIF: Final = "validation.mif"
_FIX_ON_REF_CSV: Final = "log_fixOnRef.csv"
_PROJECT_SUMMATIONS_RDS: Final = "projectSummations.rds"
_GRID_LOG_GLOB: Final = "225*/grid*/gmsgrid.log"
_FULLDATA_ITERATION_GLOB: Final = "fulldata_*.gdx"
_INPUT_ITERATION_GLOB: Final = "input_*.gdx"
_OUTPUT: Final = "output"
_REMIND_ITERATION: Final = "-rem-"
_MAGPIE_ITERATION: Final = "-mag-"
_HOURS: Final = 3600.0


# ---------------------------------------------------------------------------------------------
# Logical and physical paths
# ---------------------------------------------------------------------------------------------


@runtime_checkable
class OverlayEffects(Protocol):
    """An ``Effects`` whose paths are logical: it exposes the overlay that maps them onto real files.

    ``modelstats.env.FakeEffects`` is the one implementation (its ``paths`` property); production effects
    have no overlay and their paths are physical already.
    """

    @property
    def paths(self) -> OverlayPaths | None:
        """The overlay mapping, if any."""
        ...

    @property
    def cwd(self) -> str:
        """The directory relative paths resolve against."""
        ...


def physical_path(path: PathLike, effects: Effects) -> Path:
    """The real file behind ``path`` for a library that opens files by name (GDX, R data, CSV).

    In production the path is returned as given (relative to the process's working directory, as R
    resolves it). Under an :class:`OverlayEffects` a relative path is first resolved against its logical
    working directory and the overlay, when there is one, answers with the physical entry it shows; a path
    the overlay does not show maps to itself, so that the caller's existence test or open fails the way
    R's would.
    """
    text = os.fspath(path)
    if not isinstance(effects, OverlayEffects):
        return Path(text)
    logical = PurePosixPath(effects.cwd) / text
    if effects.paths is None:
        return Path(logical)
    found = effects.paths.first_existing(logical)
    return found if found is not None else Path(logical)


# ---------------------------------------------------------------------------------------------
# The files of one run folder
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True)
class RunFiles:
    """The paths ``getRunStatus()`` composes for one run folder (``R/getRunStatus.R:40-51``).

    ``directory`` is R's ``ii``: the folder after ``normalizePath()`` (``:25``) or, for a path that does
    not exist under ``sort = "nf"``, the row name ``"NA"`` R's data-frame indexing produces (``:30-32``).
    Paths are strings pasted the way the R code pastes them (``file.path`` and ``paste0`` both put one
    ``/`` between ``ii`` and the name), because the displayed and logged values must equal R's. Existence,
    ``stat`` and listings go through ``effects``; :meth:`physical` and :meth:`opened` hand the files to
    libraries and scanners.
    """

    directory: str
    effects: Effects

    @property
    def name(self) -> str:
        """``basename(ii)`` (``R/getRunStatus.R:37``): the row name of the run."""
        return r_basename(self.directory)

    def path(self, name: str) -> str:
        """``file.path(ii, name)`` / ``paste0(ii, "/", name)``."""
        return f"{self.directory}/{name}"

    # -- the files the R code names (R/getRunStatus.R:42-51) ---------------------------------------

    @property
    def statistics(self) -> str:
        """``runstatistics.rda`` (``:42``)."""
        return self.path(_RUN_STATISTICS)

    @property
    def fulldata_gdx(self) -> str:
        """``fulldata.gdx`` (``:43``), the first GDX candidate."""
        return self.path(_FULLDATA_GDX)

    @property
    def non_optimal_gdx(self) -> str:
        """``non_optimal.gdx`` (``:44``), the second GDX candidate."""
        return self.path(_NON_OPTIMAL_GDX)

    @property
    def full_gms(self) -> str:
        """``full.gms`` (``:45``)."""
        return self.path(_FULL_GMS)

    @property
    def full_log(self) -> str:
        """``full.log`` (``:46``)."""
        return self.path(_FULL_LOG)

    @property
    def slurm_log(self) -> str:
        """``slurm.log`` (``:47``)."""
        return self.path(_SLURM_LOG)

    @property
    def log_txt(self) -> str:
        """``log.txt`` (``:48``)."""
        return self.path(_LOG_TXT)

    @property
    def log_mag_txt(self) -> str:
        """``log-mag.txt`` (``:49``), the MAgPIE log of an old-style coupled run."""
        return self.path(_LOG_MAG_TXT)

    @property
    def abort_gdx(self) -> str:
        """``abort.gdx`` (``:50``)."""
        return self.path(_ABORT_GDX)

    @cached_property
    def magpie_log(self) -> str:
        """``logmagtxt`` after ``:51``: ``log-mag.txt`` when it exists, else ``log.txt``."""
        return self.log_mag_txt if self.exists(self.log_mag_txt) else self.log_txt

    @property
    def validation_mif(self) -> str:
        """``validation.mif`` (``:125``), the MAgPIE report."""
        return self.path(_VALIDATION_MIF)

    def remind_mif(self, title: str | None) -> str:
        """``REMIND_generic_<cfg$title>.mif`` (``:128, 319``); a ``NULL`` title pastes as nothing."""
        return self.path(f"REMIND_generic_{title or ''}.mif")

    def summation_errors_csv(self, title: str | None) -> str:
        """``REMIND_generic_<title>_summation_errors.csv`` (``:129, 320``)."""
        return self.path(f"REMIND_generic_{title or ''}_summation_errors.csv")

    def range_errors_txt(self, title: str | None) -> str:
        """``REMIND_generic_<title>_range_errors.txt`` (``:334``)."""
        return self.path(f"REMIND_generic_{title or ''}_range_errors.txt")

    @property
    def fix_on_ref_csv(self) -> str:
        """``log_fixOnRef.csv`` (``:343``)."""
        return self.path(_FIX_ON_REF_CSV)

    @property
    def project_summations_rds(self) -> str:
        """``projectSummations.rds`` (``:351``)."""
        return self.path(_PROJECT_SUMMATIONS_RDS)

    @cached_property
    def config_matches(self) -> tuple[str, ...]:
        """``grep("config.Rdata|config.yml", dir(ii), value = TRUE)`` (``:41``): unanchored (BUG-003)."""
        return tuple(self.effects.listdir_like_r(self.directory, pattern=CONFIG_FILE_PATTERN))

    @cached_property
    def grid_logs(self) -> tuple[str, ...]:
        """``Sys.glob(file.path(ii, "225*", "grid*", "gmsgrid.log"))`` (``:179``)."""
        return tuple(self.effects.glob(self.path(_GRID_LOG_GLOB)))

    @cached_property
    def coupled_magpie_full_log(self) -> str:
        """The ``full.log`` of the MAgPIE iteration of an old-style coupled run (``:226-228``).

        ``gsub("-rem-", "-mag-", gsub("output", file.path("magpie", "output"), fulllog))``, every
        occurrence in the whole path replaced; when that file does not exist, the same with
        ``../magpie/output``, existing or not.
        """
        full_log = self.full_log
        sibling = full_log.replace(_OUTPUT, f"magpie/{_OUTPUT}").replace(_REMIND_ITERATION, _MAGPIE_ITERATION)
        if self.exists(sibling):
            return sibling
        return full_log.replace(_OUTPUT, f"../magpie/{_OUTPUT}").replace(_REMIND_ITERATION, _MAGPIE_ITERATION)

    # -- the environment -------------------------------------------------------------------------

    def exists(self, path: str) -> bool:
        """``file.exists(path)``."""
        return self.effects.exists(path)

    def stat(self, path: str) -> FileInfo | None:
        """``file.info(path)``, ``None`` for a missing path."""
        return self.effects.stat(path)

    def physical(self, path: str) -> Path:
        """The real file behind ``path`` (:func:`physical_path`)."""
        return physical_path(path, self.effects)

    def read_bytes(self, path: str) -> bytes | None:
        """The whole of an existing ``path``; ``None`` when it cannot be opened.

        ``file.exists()`` is true for a file the process may not read, and the grep, tac, tail and awk
        commands ``getRunStatus()`` runs on it then print nothing: ``system(..., intern = TRUE)`` returns
        ``character(0)`` with a status attribute, not an error. The facts decide what no output means.
        """
        try:
            stream = self.effects.open_binary(path)
        except OSError:
            return None
        with stream:
            return stream.read()


# ---------------------------------------------------------------------------------------------
# The facts
# ---------------------------------------------------------------------------------------------


class RunFacts:
    """What the files of one run contain, each read once on first use (``06`` 5.2).

    ``user`` is the SLURM user of ``getRunStatus(user)``; ``None`` is ``Sys.info()[["user"]]``. The clock
    is read once (:attr:`now`): R calls ``Sys.time()`` at lines 181, 183 and 313 separately, which only
    differs by the microseconds between them.
    """

    def __init__(self, files: RunFiles, effects: Effects, user: str | None = None) -> None:
        """Bind the files, the environment and the SLURM user; nothing is read here."""
        self.files = files
        self.effects = effects
        self.user = user
        self._configs: dict[Path, RunConfig] = {}
        self._csv_counts: dict[str, int] = {}
        self._contents: dict[str, bytes | None] = {}
        self.deferred_warnings: list[DeferredWarning] = []
        """The R warnings this run's reads raised so far, oldest first (``readGDX`` on an absent symbol).

        R defers them (``options(warn = 0)``) and prints them with the next error (``try()``'s ``In
        addition:`` block) or at the top level; the callers that raise :class:`RunReadError` attach a copy.
        """

    def __repr__(self) -> str:
        """``RunFacts('/path/to/run')``."""
        return f"RunFacts({self.files.directory!r})"

    @cached_property
    def now(self) -> float:
        """``Sys.time()`` as seconds since the epoch (``R/getRunStatus.R:181, 183, 313``)."""
        return self.effects.now().timestamp()

    # -- configuration (R/getRunStatus.R:41, 67-74) -------------------------------------------------

    @cached_property
    def config(self) -> RunConfig | None:
        """The ``cfg`` list R loads from the one matching configuration file (``:69-74``); ``None`` without.

        Two or more matches (BUG-003, decision D-01 pending) abort as R does: ``ifelse`` evaluates
        ``load()`` on the vector of paths when any name does not end in ``yml`` (``invalid 'description'
        argument``), and ``loadConfig()`` on the vector otherwise, which parses the pasted names as YAML
        without complaint, so the abort then comes from ``colRunType()`` at the next line (``the condition
        has length > 1``, ``R/colRunType.R:18``).
        """
        matches = self.files.config_matches
        if not matches:
            return None
        if len(matches) > 1:
            if all(name.endswith("yml") for name in matches):
                raise RunReadError(CONDITION_LENGTH_ABOVE_ONE, call=EXISTS_CALL)
            raise RunReadError(INVALID_DESCRIPTION, call=GZFILE_CALL)
        return self._load_config(Path(self.files.path(matches[0])))

    def _load_config(self, path: Path) -> RunConfig:
        """``load()`` / ``loadConfig()`` of one path, once per path; a failure aborts as R's would.

        The error is reported under the ``R/getRunStatus.R:72`` call of the file's kind (the message is the
        library's, not R's: an unreadable file has no R golden).
        """
        if path not in self._configs:
            try:
                self._configs[path] = load_config(self.files.physical(str(path)))
            except ConfigError as error:
                call = LOAD_YAML_CALL if path.name.endswith("yml") else LOAD_CONFIG_CALL
                raise RunReadError(str(error), call=call) from error
        return self._configs[path]

    @cached_property
    def run_type(self) -> str | None:
        """``colRunType(ii)`` (``:73``), called only when a configuration file exists (``:69-74``).

        ``None`` without a configuration file (R never calls ``colRunType`` then) and for a MAgPIE
        configuration without ``cfg$gms$optimization`` (R's ``NULL``, BUG-046); the parity view turns the
        latter into R's assignment error. An R error of ``colRunType`` aborts the call.
        """
        if self.config is None:
            return None
        try:
            return col_run_type(self.files.directory, self.effects, self._load_config)
        except RunTypeError as error:
            raise RunReadError(str(error), call=error.call) from error

    # -- runstatistics.rda (R/getRunStatus.R:76-79) --------------------------------------------------

    @cached_property
    def statistics(self) -> RunStatistics | None:
        """The ``stats`` list of ``runstatistics.rda`` (``:76-79``); ``None`` when the file is absent."""
        if not self.files.exists(self.files.statistics):
            return None
        try:
            return read_run_statistics(self.files.physical(self.files.statistics))
        except RDataError as error:
            raise RunReadError(str(error), call=LOAD_STATISTICS_CALL) from error

    @property
    def is_magpie(self) -> bool:
        """``isTRUE(runstatistics$stats[["config"]][["model_name"]] == "MAgPIE")`` (``:124, 257, 266, 318``).

        A run without ``runstatistics.rda`` is not MAgPIE by this test (``03`` 3.1, "Config and model
        detection").
        """
        return self.statistics is not None and self.statistics.is_magpie

    # -- the GDX files (R/getRunStatus.R:52-64) ---------------------------------------------------------

    @cached_property
    def gdx_candidates(self) -> tuple[GdxCandidate, ...]:
        """``c(gdx, gdx_non_optimal)[file.exists(...)]`` with their ``file.info()$mtime`` (``:53, 61``)."""
        candidates: list[GdxCandidate] = []
        for path in (self.files.fulldata_gdx, self.files.non_optimal_gdx):
            info = self.files.stat(path)
            if info is not None:
                candidates.append(GdxCandidate(GdxFile(self.files.physical(path)), info.mtime_epoch))
        return tuple(candidates)

    @cached_property
    def selected_gdx(self) -> GdxFile | None:
        """``latest_gdx`` (``:54-64``): the candidate with the highest ``o_iterationNumber``.

        One candidate is taken unread; with two, both are read outside any ``try()``, so a damaged file
        aborts (BUG-037, D-20); ties go to the first, a missing value to the newest mtime
        (``gdx.select_latest_gdx``). ``None`` without a candidate.
        """
        try:
            return select_latest_gdx(self.gdx_candidates)
        except GdxReadError as error:
            raise RunReadError(str(error)) from error

    @cached_property
    def selected_gdx_mtime(self) -> float | None:
        """``file.info(latest_gdx)$mtime`` (``:183``) as epoch seconds; ``None`` without a selected GDX."""
        selected = self.selected_gdx
        if selected is None:
            return None
        return next(c.mtime for c in self.gdx_candidates if c.file is selected)

    def _gdx_read[T](self, read: Callable[[], T]) -> T:
        """Run one GDX read; a library failure aborts like R's process abort (BUG-037, D-20).

        The error carries no R call: R dies in the GDX library and prints no condition at all.
        """
        try:
            return read()
        except GdxReadError as error:
            raise RunReadError(str(error)) from error

    @cached_property
    def modelstat_values(self) -> list[float] | None:
        """``c(readGDX(latest_gdx, c("o_modelstat", "p80_modelstat"), format = "first_found"))`` (``:84``).

        The values in magclass order (one per MAgPIE year, one for REMIND); ``None`` without a selected
        GDX or when neither symbol exists.
        """
        selected = self.selected_gdx
        if selected is None:
            return None
        frame = self._gdx_read(lambda: read_first_found(selected, MODELSTAT_SYMBOLS))
        return None if frame is None else flat_values(frame)

    @cached_property
    def iteration_number(self) -> float | None:
        """``as.numeric(readGDX(latest_gdx, "o_iterationNumber", format = "simplest"))`` (``:280``).

        ``None`` is R's ``numeric(0)`` for an absent symbol (``react`` defaults to a warning, not an
        error; the warning is deferred, :attr:`deferred_warnings`); a GAMS special value arrives as ``nan``.
        """
        selected = self.selected_gdx
        if selected is None:
            return None
        value = self._gdx_read(lambda: read_scalar(selected, ITERATION_SYMBOL))
        if value is None:
            self._warn(ITERATION_READ_CALL, ABSENT_SYMBOL_WARNING.format(name=ITERATION_SYMBOL))
        return value

    @cached_property
    def convergence_flag(self) -> float | None:
        """``as.numeric(readGDX(latest_gdx, "s80_bool", type = "Parameter", format = "simplest"))``.

        ``R/getRunStatus.R:281``: exact-case, parameters only (``gdx.read_typed_scalar``); ``None`` is
        ``numeric(0)``, with gdx2's ``.expandPattern`` warning deferred (:attr:`deferred_warnings`).
        """
        selected = self.selected_gdx
        if selected is None:
            return None
        value = self._gdx_read(lambda: read_typed_scalar(selected, CONVERGENCE_SYMBOL))
        if value is None:
            self._warn(EXPAND_PATTERN_CALL, ABSENT_TYPED_SYMBOL_WARNING.format(name=CONVERGENCE_SYMBOL))
        return value

    @cached_property
    def solve_report(self) -> pd.DataFrame | None:
        """``readGDX(latest_gdx, "p80_repy")`` (``:288``) as a dense frame; ``None`` when absent (warned)."""
        selected = self.selected_gdx
        if selected is None:
            return None
        frame = self._gdx_read(lambda: read_param(selected, REPY_SYMBOL))
        if frame is None:
            self._warn(REPY_READ_CALL, ABSENT_SYMBOL_WARNING.format(name=REPY_SYMBOL))
        return frame

    def _warn(self, call: str, message: str) -> None:
        """Record one R warning the way ``options(warn = 0)`` defers it (:attr:`deferred_warnings`)."""
        self.deferred_warnings.append(DeferredWarning(call, message))

    @cached_property
    def abort_gdx(self) -> GdxFile | None:
        """``abort.gdx`` (``:50``) as one reader behind both symbols of ``:212-213``; ``None`` when absent.

        R opens the file twice; one :class:`GdxFile` reads the symbol table once for both.
        """
        if not self.files.exists(self.files.abort_gdx):
            return None
        return GdxFile(self.files.physical(self.files.abort_gdx))

    @cached_property
    def abort_threshold(self) -> list[float] | None:
        """``as.numeric(readGDX(abortgdx, "cm_abortOnConsecFail", format = "simplest"))`` (``:212``).

        A numeric vector of whatever length the symbol has: REMIND stores a scalar, but R accepts any
        shape here and leaves the length to ``isTRUE(maxinfes > 0)`` at ``:214``. ``None`` is R's
        ``numeric(0)`` for a missing file or symbol; a damaged file aborts (BUG-037, D-20).
        """
        gdx = self.abort_gdx
        if gdx is None:
            return None
        frame = self._gdx_read(lambda: read_param(gdx, ABORT_THRESHOLD_SYMBOL))
        return None if frame is None else flat_values(frame)

    @cached_property
    def abort_track(self) -> pd.DataFrame | None:
        """``readGDX(abortgdx, "p80_trackConsecFail")`` (``:213``) as a dense frame; ``None`` when absent."""
        gdx = self.abort_gdx
        if gdx is None:
            return None
        return self._gdx_read(lambda: read_param(gdx, ABORT_TRACK_SYMBOL))

    # -- full.gms and full.log (R/getRunStatus.R:144-157) ------------------------------------------------

    def _snapshot(self, path: str) -> bytes | None:
        """The bytes of the existing file ``path``, read once per path; ``None`` when it cannot be opened.

        Every text fact of a file scans this one snapshot (``06`` 5.2: nothing is read twice), so the
        ``LOOPS`` and ``*** Status:`` facts of ``full.log`` and the seven facts of ``log.txt`` cannot
        disagree about a log that is still being written.
        """
        if path not in self._contents:
            self._contents[path] = self.files.read_bytes(path)
        return self._contents[path]

    def _scan[T](self, path: str, scan: Callable[[BinaryIO], T], absent: T) -> T:
        """``scan`` over the snapshot of ``path``; ``absent`` when the file does not exist.

        A file that exists but cannot be opened scans as empty: the shell command printed nothing
        (``character(0)``), and the scanner's value for no output is its no-match value, which R's
        ``try()``, ``length()`` or ``isTRUE()`` then handles without an error (``R/getRunStatus.R:144,
        154, 157, 162, 204-205, 223-235, 240, 258-273, 299``).
        """
        if not self.files.exists(path):
            return absent
        contents = self._snapshot(path)
        return scan(io.BytesIO(b"" if contents is None else contents))

    @cached_property
    def iteration_max_lines(self) -> list[str]:
        """The last ``cm_iteration_max = N;`` line of ``full.gms``, reduced to ``N`` (``:144-145``).

        ``[]`` when ``full.gms`` is absent or has no such line (R's ``character(0)``).
        """
        return self._scan(self.files.full_gms, textscan.scan_iteration_max, [])

    @cached_property
    def last_loops(self) -> list[str]:
        """The last ``LOOPS`` value of ``full.log`` (``:154``); ``[]`` without the file or such a line."""
        return self._scan(self.files.full_log, textscan.scan_last_loops, [])

    @cached_property
    def status_lines(self) -> list[str]:
        """Every ``*** Status:`` line of ``full.log`` cut to 17 characters (``:157``); ``[]`` without it."""
        return self._scan(self.files.full_log, textscan.scan_status_lines, [])

    # -- log.txt (R/getRunStatus.R:162, 189, 204-205, 240, 267-273, 299) ---------------------------------

    @cached_property
    def slurmstepd_errors(self) -> list[str]:
        """``grep 'slurmstepd.*error' log.txt`` (``:162``); ``[]`` without the file."""
        return self._scan(self.files.log_txt, textscan.scan_slurmstepd_errors, [])

    @cached_property
    def last_logtxt_line(self) -> str | None:
        """``awk 'NF{s=$0}END{print s}' log.txt`` (``:189``): the last non-blank line; ``None`` without it.

        The one scan whose empty output R does not survive: ``:190`` compares awk's result with ``==``
        outside any ``try()``, and when ``log.txt`` exists but cannot be read awk prints nothing, so
        ``if (character(0) == "Starting MAgPIE...")`` aborts with ``argument is of length zero``.
        """
        log_txt = self.files.log_txt
        if not self.files.exists(log_txt):
            return None
        contents = self._snapshot(log_txt)
        if contents is None:
            raise RunReadError(ARGUMENT_OF_LENGTH_ZERO, call=LAST_LINE_CALL)
        return textscan.last_nonempty_line(io.BytesIO(contents))

    @cached_property
    def logtxt_tail(self) -> list[str]:
        """``tail -1 log.txt`` (``:240``) as R received it; ``[]`` without the file or for an empty one."""
        return self._scan(self.files.log_txt, textscan.scan_last_line, [])

    @cached_property
    def output_generation_started(self) -> bool:
        """``tac log.txt | grep -m 1 'Starting output generation for'`` found a line (``:204``)."""
        return self._last_match_exists(self.files.log_txt, textscan.OUTPUT_GENERATION_START)

    @cached_property
    def output_generation_finished(self) -> bool:
        """``tac log.txt | grep -m 1 'Finished output generation for'`` found a line (``:205``)."""
        return self._last_match_exists(self.files.log_txt, textscan.OUTPUT_GENERATION_END)

    @cached_property
    def warning_summary(self) -> list[str]:
        """The digits of the ``There were N warnings`` line of ``log.txt`` (``:267-269``); ``[]`` without."""
        return self._scan(self.files.log_txt, textscan.scan_warning_summary, [])

    @cached_property
    def numbered_warning_count(self) -> int:
        """The numbered lines of the ``Warning messages:`` blocks of ``log.txt`` (``:272-273``)."""
        return self._scan(self.files.log_txt, textscan.count_numbered_warning_lines, 0)

    @cached_property
    def calibration_iteration(self) -> list[str]:
        """The last one-or-two-digit group of the ``CES calibration iteration`` lines (``:299``)."""
        return self._scan(self.files.log_txt, textscan.scan_calibration_iteration, [])

    # -- slurm.log (R/getRunStatus.R:258-261) -------------------------------------------------------------

    @cached_property
    def magpie_warning_block(self) -> list[str]:
        """What survives of ``grep -zoP "Warning messages:..." slurm.log | tail -1`` (``:258``, BUG-011)."""
        return self._scan(self.files.slurm_log, textscan.scan_magpie_warnings, [])

    @cached_property
    def slurm_log_has_warning_message(self) -> bool:
        """``grep "Warning message:" slurm.log | tail -1`` found a line (``:261``)."""
        return self._scan(self.files.slurm_log, textscan.has_warning_message, False)

    # -- the MAgPIE log of a coupled run (R/getRunStatus.R:222-235) ---------------------------------------

    @cached_property
    def magpie_preparing(self) -> bool:
        """``tac logmagtxt | grep -m 1 'Preparing MAgPIE'`` found a line (``:223``)."""
        return self._last_match_exists(self.files.magpie_log, textscan.MAGPIE_PREPARING)

    @cached_property
    def magpie_output_stored(self) -> bool:
        """``tac logmagtxt | grep -m 1 'MAgPIE output was stored'`` found a line (``:224``)."""
        return self._last_match_exists(self.files.magpie_log, textscan.MAGPIE_OUTPUT_STORED)

    @cached_property
    def magpie_report_started(self) -> bool:
        """``tac logmagtxt | grep -m 1 'Start getReport'`` found a line (``:232``)."""
        return self._last_match_exists(self.files.magpie_log, textscan.MAGPIE_REPORT_START)

    @cached_property
    def magpie_log_tail(self) -> list[str]:
        """``tail -1 logmagtxt`` (``:235``) as R received it; ``[]`` without the file or for an empty one."""
        return self._scan(self.files.magpie_log, textscan.scan_last_line, [])

    @cached_property
    def coupled_magpie_loops(self) -> list[str]:
        """The last ``LOOPS`` value of the MAgPIE iteration's ``full.log`` (``:226-231``); ``[]`` without."""
        return self._scan(self.files.coupled_magpie_full_log, textscan.scan_last_loops, [])

    def _last_match_exists(self, path: str, pattern: textscan.Pattern) -> bool:
        """``length(tac path | grep -m 1 pattern) > 0``, ``False`` without the file."""
        return self._scan(path, lambda stream: textscan.last_match(stream, pattern) is not None, False)

    # -- the conopt grid logs (R/getRunStatus.R:179-181) ------------------------------------------------

    @cached_property
    def grid_log_newest_mtime(self) -> float | None:
        """``max(file.info(gridfiles)$mtime)`` as epoch seconds (``:181``); ``None`` without a grid log."""
        mtimes = [info.mtime_epoch for path in self.files.grid_logs if (info := self.files.stat(path))]
        return max(mtimes) if mtimes else None

    def hours_since(self, epoch: float) -> float:
        """``difftime(Sys.time(), <instant>, units = "hours")`` (``:181, 183``)."""
        return (self.now - epoch) / _HOURS

    # -- the mif files (R/getRunStatus.R:124-136, 319-322) -------------------------------------------------

    @cached_property
    def validation_mif_size(self) -> int | None:
        """``file.info(validation.mif)[["size"]]`` (``:126``); ``None`` without the file."""
        info = self.files.stat(self.files.validation_mif)
        return None if info is None else info.size

    def remind_mif_exists(self, title: str | None) -> bool:
        """``file.exists(REMIND_generic_<title>.mif)`` (``:130, 321``)."""
        return self.files.exists(self.files.remind_mif(title))

    def summation_errors_exist(self, title: str | None) -> bool:
        """``file.exists(REMIND_generic_<title>_summation_errors.csv)`` (``:132, 326``)."""
        return self.files.exists(self.files.summation_errors_csv(title))

    # -- the sanity files (R/getRunStatus.R:326-360) ------------------------------------------------------

    def summation_error_count(self, title: str | None) -> int:
        """``length(unique(read.csv2(sumErrFile, sep = ",")$variable))`` (``:327-328``)."""
        return self._csv_count(self.files.summation_errors_csv(title), summation_error_count)

    def range_errors_exist(self, title: str | None) -> bool:
        """``file.exists(rangeErrFile)`` (``:335``)."""
        return self.files.exists(self.files.range_errors_txt(title))

    def range_error_count(self, title: str | None) -> int:
        """``nrow(read.csv2(rangeErrFile))`` (``:336-337``)."""
        return self._csv_count(self.files.range_errors_txt(title), range_error_count)

    @property
    def fix_errors_exist(self) -> bool:
        """``file.exists(log_fixOnRef.csv)`` (``:344``)."""
        return self.files.exists(self.files.fix_on_ref_csv)

    @cached_property
    def fix_error_count(self) -> int:
        """``length(unique(read.csv2(fixErrFile, sep = ",")$variable))`` (``:345-346``)."""
        return self._csv_count(self.files.fix_on_ref_csv, fix_error_count)

    def _csv_count(self, path: str, count: Callable[[Path], int]) -> int:
        """One ``read.csv2`` count, read once per file; an empty file is ``no lines available in input``."""
        if path not in self._csv_counts:
            try:
                self._csv_counts[path] = count(self.files.physical(path))
            except EmptyCsvError as error:
                raise RunReadError(NO_LINES_IN_INPUT, call=READ_CSV_CALL) from error
        return self._csv_counts[path]

    @cached_property
    def project_summations(self) -> Mapping[str, Any] | None:
        """``readRDS(projectSummations.rds)`` (``:352-353``) as a mapping; ``None`` without the file."""
        if not self.files.exists(self.files.project_summations_rds):
            return None
        try:
            value = read_rds(self.files.physical(self.files.project_summations_rds))
        except RDataError as error:
            raise RunReadError(str(error), call=READ_RDS_CALL) from error
        return value if isinstance(value, Mapping) else {}

    @cached_property
    def fulldata_iteration_count(self) -> int:
        """``length(system("find ii -name 'fulldata_*.gdx'", intern = TRUE))`` (``:302``)."""
        return textscan.count_files_named(self.files.physical(self.files.directory), _FULLDATA_ITERATION_GLOB)

    @cached_property
    def input_iteration_count(self) -> int:
        """``length(system("find ii -name 'input_*.gdx'", intern = TRUE))`` (``:303``)."""
        return textscan.count_files_named(self.files.physical(self.files.directory), _INPUT_ITERATION_GLOB)

    # -- the runtime inputs (R/getRunStatus.R:309-313) ----------------------------------------------------

    @cached_property
    def gams_wall_seconds(self) -> float | None:
        """``difftime(stats$timeGAMSEnd, stats$timeGAMSStart, units = "secs")`` (``:311``), or ``None``."""
        stats = self.statistics
        return None if stats is None else stats.gams_wall_seconds()

    @cached_property
    def seconds_since_prepare_start(self) -> float | None:
        """``difftime(Sys.time(), stats$timePrepareStart, units = "secs")`` (``:313``); ``None`` without."""
        stats = self.statistics
        if stats is None or stats.time_prepare_start is None or stats.time_prepare_start.epoch is None:
            return None
        return self.now - stats.time_prepare_start.epoch

    # -- the results archive (R/getRunStatus.R:106-115) --------------------------------------------------

    def archived_result_is_current(self, archive: str) -> bool:
        """``file.exists(id) && all(file.info(Sys.glob(overview.rds))$mtime + 600 > file.info(id)$mtime)``.

        ``R/getRunStatus.R:112-113``: ``id`` is ``paste0(archive, stats$id, ".rds")`` (a ``NULL`` id pastes
        as nothing) and ``overview.rds`` is looked up in ``archive``; the ``all()`` over no ``overview.rds``
        is vacuously true (BUG-010).
        """
        stats = self.statistics
        result = f"{archive}{'' if stats is None or stats.id is None else stats.id}.rds"
        info = self.files.stat(result)
        if info is None:
            return False
        overviews = [self.files.stat(path) for path in self.effects.glob(f"{archive}overview.rds")]
        return all(o.mtime_epoch + 600 > info.mtime_epoch for o in overviews if o is not None)

    # -- SLURM (R/getRunStatus.R:38) ------------------------------------------------------------------

    @cached_property
    def slurm(self) -> SlurmLookup | None:
        """``foundInSlurm(ii, user)`` with its working state (``:38``); ``None`` off the cluster.

        One ``squeue`` call per run; an R error of ``foundInSlurm`` aborts the call. Only the ``squeue``
        call and the line matching happen here: the user regex of ``R/foundInSlurm.R:33`` and the field
        parsing run when ``lookup.text`` or ``lookup.job`` is read, and their :class:`SlurmError` is
        turned into a :class:`RunReadError` at those use sites (``run_status.column_job_in_slurm``,
        ``Run.slurm``, ``Run._jobs``).
        """
        if not self.effects.on_cluster:
            return None
        try:
            return lookup_slurm(self.files.directory, self.effects, self.user)
        except SlurmError as error:
            raise RunReadError(str(error), call=error.call) from error
