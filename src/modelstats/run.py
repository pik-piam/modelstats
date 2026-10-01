"""The public ``Run`` object: a REMIND or MAgPIE run folder read once, presented with clean semantics.

``migration/06-user-requests.md`` section 5 (request 6, decision D-25): other repositories import
:class:`Run` instead of re-implementing ``R/getRunStatus.R``. A ``Run`` sits on one
:class:`modelstats.facts.RunFacts`, the reading layer that ``run_status.py`` (the parity view pinned by the
status goldens) also uses, so both views see the same files, each read once. Every property is lazy and
cached; nothing is opened before a property needs it; errors raise :class:`modelstats.errors.RunReadError`
from the property and never abort the process (D-20). :meth:`Run.to_status_record` is the parity record
of ``03`` section 5 computed on the same facts.

Example::

    from modelstats import Run, RunState

    run = Run("/p/projects/remind/modeltests/remind/output/SSP2-NPi-AMT_2026-09-28_10.30.27")
    if run.status is RunState.COMPLETED and run.convergence.is_converged:
        print(run.iterations.current, run.warnings, run.summation_errors, run.runtime)

The clean view reads the same facts as the parity view through the same rule functions wherever the R
vocabulary is a faithful summary of the files; where it deliberately says something else, the difference
is listed here and asserted by ``tests/unit/test_run.py`` on a synthetic case (D-25). ``rs`` is never
affected: it prints the parity view.

**R says / Run says / why (decision D-25)**

* Several ``*** Status:`` lines in ``full.log``. R says: ``RunStatus`` ``"NA"`` and the interrupt fallback
  (``R/getRunStatus.R:157``, BUG-036). Run says: the last line is :attr:`Run.status_detail`. Why: GAMS
  appends one status per restart; the last one is the current state.
* ``modelstat``. R says: ``"2: Locally Optimal"``, ``"222222"``, the string ``"NA"`` or a real ``NA``
  (``:82-102``). Run says: ``ModelStat(2, "Locally Optimal", "2")``, ``ModelStat(None, None, "222222")``,
  ``ModelStat(None, None, None)``. Why: real ``None`` and numbers, not string conventions.
* ``modelstat`` fallback from ``runstatistics.rda`` whose ``stats$config`` has no ``model_name``. R says:
  the call aborts with ``argument is of length zero`` (``:91``, the BUG-005 input). Run says: the REMIND
  fallback (``stats$modelstat`` when it is one value). Why: a missing field is a missing input.
* ``RunType`` of a MAgPIE configuration without ``cfg$gms$optimization``. R says: the call aborts with
  ``replacement has length zero`` (``:73``, BUG-046, D-28). Run says: ``run_type`` is ``None``.
* ``Runtime``. R says: whole seconds; ``now - timePrepareStart`` whenever ``jobInSLURM`` is not ``"no"``,
  which off the cluster (``"NA"``) is every unfinished run (``:309-313``). Run says: a ``timedelta`` with
  its fraction; the live value only while a job of the run is listed by ``squeue``; ``None`` otherwise.
  Why: a live runtime without a job is the age of an abandoned run.
* ``Conv`` for a Nash run whose ``s80_bool`` is 0 below ``cm_iteration_max``. R says: the ``p80_repy``
  modelstat digits over all regions (``:288-290``). Run says: :attr:`Convergence.NOT_CONVERGED`. Why:
  the flag is the model's own verdict; the digits are a diagnostic, not a state.
* ``Conv`` when the selected GDX lacks ``s80_bool``, ``o_iterationNumber`` or ``p80_repy``. R says: the
  call aborts (``argument is of length zero``, ``missing value where TRUE/FALSE needed`` BUG-038 / D-23,
  ``incorrect number of dimensions``). Run says: :attr:`Convergence.UNKNOWN` without the flag,
  :attr:`Convergence.NOT_CONVERGED` for a 0 flag whatever else is missing. Why: missing inputs degrade.
* A pending job with a QOS containing a digit (``R/foundInSlurm.R:32``, BUG-031). R says: ``"qos1
  startup"`` and ``"Run in progress"``. Run says: ``slurm.pending`` is ``True`` and ``status`` is
  :attr:`RunState.PENDING`. Why: ``SlurmJob`` reads the state field, not a regex on the line.
* Two or more ``squeue`` hits for one run (BUG-019, BUG-035). R says: ``"<user>"`` or ``"<n> users"``,
  never a suffix. Run says: ``slurm`` is ``None`` (``facts.slurm.jobs`` has every hit); ``status`` is
  :attr:`RunState.PENDING` when every hit is pending.
* ``full.log missing`` while a job of the run is listed. R says: ``"full.log missing"``. Run says:
  ``status_detail`` keeps the text, ``status`` is :attr:`RunState.RUNNING`. Why: the job is alive and GAMS
  has not started yet (input preparation).
* The plausibility block. R says: ``summationErrors`` etc. only with a mif, and the call aborts with
  ``replacement has length zero`` when ``projectSummations.rds`` lacks ``ScenarioMIP`` or one of its
  elements (``:351-357``). Run says: the three counts as ``int`` under the same mif rule;
  ``project_summation`` whenever the file exists, a missing element being ``None``.
* ``runInAppResults``. R says: ``"yes"`` / ``"no"``, tested when any ``stats`` name contains ``id``
  (``:107``), and the call aborts on a ``NULL`` or ``NA`` ``stats$config$model_name`` (``:108``). Run says:
  ``in_app_results`` is ``True`` / ``False`` when ``stats$id`` exists, ``None`` off the cluster; the MAgPIE
  archive by ``isTRUE(model_name == "MAgPIE")``. The vacuous ``overview.rds`` rule (BUG-010) is kept.
* ``Warnings``, ``Iter``, ``Mif``, ``Conv``. R says: strings (``"27"``, ``"26/50 Clb: 3"``, ``"sumErr"``,
  ``"converged (had INFES)"``). Run says: ``int | None``, :class:`Iterations`, :class:`MifState`,
  :class:`Convergence`; the ``"NA"`` string is ``None`` / ``UNKNOWN``.
* A path that does not exist. R says: the row ``"NA"`` under ``sort = "nf"`` (``:30-32``). Run says: a
  ``Run`` named after the path whose properties are ``None`` / ``UNKNOWN``.

Kept as R has them (rule-level, ``06`` 4.2): ``Clb_converged`` only above ten calibration files
(BUG-001, D-09), the ``Abort ... *Infes`` equality test (BUG-002, D-10), the REMIND warning count that
misses ``Warning message:`` and ``50 or more`` (BUG-029), the ``overview.rds`` rule (BUG-010), the
unexpanded ``cfg_mag$results_folder`` of the ``mag-<i>`` status (BUG-021, a text in ``status_detail``).
"""

from __future__ import annotations

import math
import os
import re
from collections.abc import Callable, Iterator, Mapping, Sequence
from dataclasses import asdict, dataclass
from datetime import timedelta
from enum import Enum
from functools import cached_property, wraps
from pathlib import Path
from typing import Any, Final

from modelstats.config import RunConfig
from modelstats.env import Effects, PathLike, ProductionEffects
from modelstats.errors import RunReadError
from modelstats.facts import RunFacts, RunFiles
from modelstats.facts import RunReadError as _FactsReadError
from modelstats.gdx import MODELSTAT_EXPLANATIONS, modelstat_text
from modelstats.rdata_io import RDataError, r_as_character, scalar
from modelstats.run_status import (
    NA,
    RunStatus,
    calibration,
    column_conv,
    column_iter,
    column_job_in_slurm,
    column_mif,
    column_modelstat,
    column_run_in_app_results,
    column_run_status,
    column_run_type,
    column_runtime,
    column_warnings,
    iteration_maximum,
    sanity_counts,
    status_overrides,
)
from modelstats.runfolders import is_main_folder, is_run_folder, natural_order
from modelstats.runstats import RunStatistics
from modelstats.slurm import SlurmError, SlurmJob

__all__ = [
    "Convergence",
    "Iterations",
    "MifState",
    "ModelStat",
    "ProjectSummation",
    "Run",
    "RunState",
    "iter_runs",
    "run_state",
    "state_of_detail",
]

_NASH: Final = "nash"
_CALIB: Final = "Calib"
_CALIBRATE: Final = "calibrate"
_PROJECT_BLOCK: Final = "ScenarioMIP"
_CALIBRATION_FILES_ABOVE: Final = 10
"""``R/getRunStatus.R:302-303``: more than ten ``fulldata_*.gdx`` or ``input_*.gdx`` (BUG-001, kept)."""
_INTEGER: Final = re.compile(r"-?[0-9]+")


# ---------------------------------------------------------------------------------------------
# The vocabulary
# ---------------------------------------------------------------------------------------------


class RunState(Enum):
    """What a run is doing, from the R ``RunStatus`` text and the SLURM queue (:func:`run_state`)."""

    PENDING = "pending"
    """A job of the run waits in the queue."""
    RUNNING = "running"
    """A job of the run is working: GAMS, the conopt grid, the coupled MAgPIE step or the reporting."""
    COMPLETED = "completed"
    """GAMS wrote ``*** Status: Normal completion``."""
    ERROR = "error"
    """GAMS stopped on an error (compilation, execution, consecutive infeasibilities, any other status)."""
    INTERRUPTED = "interrupted"
    """The run was stopped from outside: SLURM time or memory limit, preemption, cancel, user interrupt."""
    UNKNOWN = "unknown"
    """The files say nothing: no ``full.log``, no status line and no job, or no run folder at all."""


class Convergence(Enum):
    """The Nash convergence verdict of ``s80_bool`` (``R/getRunStatus.R:278-304``)."""

    CONVERGED = "converged"
    CONVERGED_HAD_INFES = "converged_had_infes"
    """Converged after an infeasible iteration (``non_optimal.gdx`` exists)."""
    NOT_CONVERGED = "not_converged"
    CALIBRATION_CONVERGED = "calibration_converged"
    """Converged with more than ten calibration iterations on disk (BUG-001 kept)."""
    UNKNOWN = "unknown"
    """Not a Nash run, no GDX, or no ``s80_bool`` in it."""

    @property
    def is_converged(self) -> bool:
        """``converged``, ``converged (had INFES)`` or ``Clb_converged`` in R's words."""
        converged = (
            Convergence.CONVERGED,
            Convergence.CONVERGED_HAD_INFES,
            Convergence.CALIBRATION_CONVERGED,
        )
        return self in converged


class MifState(Enum):
    """The report file (``R/getRunStatus.R:121-136``)."""

    MISSING = "missing"
    PRESENT = "present"
    SUMMATION_ERRORS = "summation_errors"
    """The REMIND mif exists next to a ``_summation_errors.csv``."""
    UNKNOWN = "unknown"
    """No configuration file, so no title to look for."""


_MIF_BY_TEXT: Final[Mapping[str, MifState]] = {
    "no": MifState.MISSING,
    "yes": MifState.PRESENT,
    "sumErr": MifState.SUMMATION_ERRORS,
    NA: MifState.UNKNOWN,
}

_STATE_BY_DETAIL: Final[Mapping[str, RunState]] = {
    "Normal completion": RunState.COMPLETED,
    "Execution error": RunState.ERROR,
    "Compilation error": RunState.ERROR,
    "Terminated by use": RunState.INTERRUPTED,
    NA: RunState.UNKNOWN,
    "Timeout interrupt": RunState.INTERRUPTED,
    "Memory interrupt": RunState.INTERRUPTED,
    "Preempt interrupt": RunState.INTERRUPTED,
    "Run requeued": RunState.INTERRUPTED,
    "Run cancelled": RunState.INTERRUPTED,
    "Run interrupted": RunState.INTERRUPTED,
    "Run restarted": RunState.PENDING,
    "Run in progress": RunState.RUNNING,
    "Running reporting": RunState.RUNNING,
    "Wait MAgPIE lock": RunState.RUNNING,
    "Wait REMIND lock": RunState.RUNNING,
    "full.log missing": RunState.UNKNOWN,
}
"""Every fixed ``RunStatus`` string of ``migration/03-migration-plan.md`` 3.1 and its state."""

_STATE_BY_PREFIX: Final[tuple[tuple[str, RunState], ...]] = (
    ("conoptspy >", RunState.RUNNING),
    ("mag-", RunState.RUNNING),
    ("Run MAgPIE", RunState.RUNNING),
    ("Abort ", RunState.ERROR),
)
"""The parameterised ``RunStatus`` strings: ``conoptspy >Nh``, ``mag-<i> <Iter>``, ``Run MAgPIE <loop>``,
``Abort <regi> <n>*Infes`` / ``Abort <k>R*<n>*Infes``."""


def state_of_detail(detail: str) -> RunState:
    """The :class:`RunState` of one R ``RunStatus`` string, by the table of ``03`` 3.1.

    A status line GAMS wrote that is none of the known ones (``Terminated due to``, ``System error``,
    ...) is :attr:`RunState.ERROR`: GAMS did not complete normally and nobody interrupted it.
    """
    known = _STATE_BY_DETAIL.get(detail)
    if known is not None:
        return known
    for prefix, state in _STATE_BY_PREFIX:
        if detail.startswith(prefix):
            return state
    return RunState.ERROR


def run_state(detail: str, jobs: Sequence[SlurmJob] = ()) -> RunState:
    """:func:`state_of_detail` refined by the queue: the jobs ``squeue`` lists for the run.

    Every job pending means :attr:`RunState.PENDING` whatever the logs say (they are from an earlier
    attempt); a listed job turns an :attr:`RunState.UNKNOWN` (``full.log missing``: GAMS has not started)
    into :attr:`RunState.RUNNING`. Off the cluster ``jobs`` is empty and the text alone decides.
    """
    if jobs and all(job.pending for job in jobs):
        return RunState.PENDING
    state = state_of_detail(detail)
    if state is RunState.UNKNOWN and jobs:
        return RunState.RUNNING
    return state


# ---------------------------------------------------------------------------------------------
# The structured values
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True, slots=True)
class ModelStat:
    """The GAMS model status of the selected GDX (``R/getRunStatus.R:82-102``).

    ``raw`` is the text R builds: one code for REMIND (``"2"``), one digit per year for MAgPIE
    (``"222222"``, ``0`` printed as ``.``), ``None`` when no GDX and no ``runstatistics.rda`` has it.
    ``code`` and ``label`` are filled for the single codes R explains (``2`` / ``Locally Optimal``).
    """

    code: int | None
    label: str | None
    raw: str | None

    @classmethod
    def from_text(cls, text: str | None) -> ModelStat:
        """From the unexplained R text; ``"NA"`` and ``None`` are the unknown model status."""
        if text is None or text == NA:
            return cls(None, None, None)
        label = MODELSTAT_EXPLANATIONS.get(text)
        return cls(int(text) if label is not None else None, label, text)


@dataclass(frozen=True, slots=True)
class Iterations:
    """The ``Iter`` column as numbers (``R/getRunStatus.R:141-155, 296-300``).

    ``current`` is the last ``LOOPS`` value of ``full.log`` when it is a number (a MAgPIE run counts
    years, kept as ``loop``, e.g. ``"y2050"``); ``maximum`` is ``cm_iteration_max`` (from ``full.gms`` for an
    auto-converging Nash run); ``calibration`` the last CES calibration iteration of a calibrating run.
    """

    current: int | None
    maximum: int | None
    calibration: int | None
    loop: str | None = None


@dataclass(frozen=True, slots=True)
class ProjectSummation:
    """The ``ScenarioMIP`` block of ``projectSummations.rds`` (``R/getRunStatus.R:351-357``)."""

    missing_vars: int | None
    errors: int | None
    errors_regional: int | None


# ---------------------------------------------------------------------------------------------
# Lazy, cached, error-converting properties
# ---------------------------------------------------------------------------------------------


def _lazy[T](method: Callable[[Run], T]) -> cached_property[T]:
    """A cached property whose reading errors are the package's :class:`RunReadError`.

    The facts layer raises the same class (``modelstats.facts`` re-exports it from ``modelstats.errors``);
    the re-raise here keeps the facts error as ``__cause__`` and goes in the quality pass together with
    the ``_FactsReadError`` alias. An error is not cached: the next access reads again, as R would on the
    next call.
    """

    @wraps(method)
    def read(self: Run) -> T:
        try:
            return method(self)
        except _FactsReadError as error:
            raise RunReadError(str(error)) from error

    return cached_property(read)


def _as_int(text: str | None) -> int | None:
    """An R text that holds a whole number, else ``None`` (``"NA"``, ``"y2050"``, ``None``)."""
    return int(text) if text is not None and _INTEGER.fullmatch(text) else None


# ---------------------------------------------------------------------------------------------
# The object
# ---------------------------------------------------------------------------------------------


class Run:
    """One run folder, read lazily through one :class:`modelstats.facts.RunFacts` (``06`` 5.1).

    ``path`` is normalised as ``getRunStatus()`` normalises it (``R/getRunStatus.R:25``: the real path
    when it exists, the input otherwise); ``effects`` defaults to the production environment; ``user`` is
    the SLURM user whose jobs are the run's own (``Sys.info()[["user"]]`` by default). Nothing but the
    normalisation touches the environment at construction.
    """

    def __init__(self, path: PathLike, effects: Effects | None = None, *, user: str | None = None) -> None:
        """Bind the run folder to an environment; no file is read here."""
        env = effects if effects is not None else ProductionEffects()
        directory = env.normalize_path(os.fspath(path))
        self._facts = RunFacts(RunFiles(directory, env), env, user)

    def __repr__(self) -> str:
        """``Run('/path/to/run')``."""
        return f"Run({self._facts.files.directory!r})"

    # -- identity --------------------------------------------------------------------------------

    @property
    def facts(self) -> RunFacts:
        """The reading layer behind this run (shared with :meth:`to_status_record`)."""
        return self._facts

    @property
    def files(self) -> RunFiles:
        """The resolved paths of the run's files (``config``, ``full.log``, ``log.txt``, the GDX files...)."""
        return self._facts.files

    @property
    def path(self) -> Path:
        """The run folder as normalised."""
        return Path(self._facts.files.directory)

    @property
    def name(self) -> str:
        """``basename(dir)``: the run name ``rs`` shows."""
        return self._facts.files.name

    # -- the parsed files (R/getRunStatus.R:41, 69-79) -----------------------------------------------

    @_lazy
    def config(self) -> RunConfig | None:
        """``cfg`` of ``config.Rdata`` / ``config.yml``; ``None`` without a configuration file."""
        return self._facts.config

    @_lazy
    def statistics(self) -> RunStatistics | None:
        """``stats`` of ``runstatistics.rda``; ``None`` without the file."""
        return self._facts.statistics

    @_lazy
    def title(self) -> str | None:
        """``cfg$title``; ``None`` without a configuration file."""
        config = self.config
        return None if config is None else config.title

    @_lazy
    def model(self) -> str | None:
        """``stats$config$model_name`` of ``runstatistics.rda``, else ``cfg$model_name``, else ``None``."""
        stats = self.statistics
        if stats is not None and stats.model_name is not None:
            return stats.model_name
        config = self.config
        return None if config is None else config.model_name

    @_lazy
    def run_type(self) -> str | None:
        """``colRunType()`` (``R/colRunType.R``): ``"nash"``, ``"Calib_nash"``, ``"testOneRegi EUR"``, ...

        ``None`` without a configuration file and for a MAgPIE configuration without
        ``cfg$gms$optimization`` (BUG-046, where the parity view aborts).
        """
        return self._facts.run_type

    # -- status (R/getRunStatus.R:38, 152-243) --------------------------------------------------------

    @_lazy
    def _job_text(self) -> str:
        """``jobInSLURM`` (``R/getRunStatus.R:38``): ``foundInSlurm()``'s string, ``"NA"`` off the cluster."""
        return column_job_in_slurm(self._facts)

    @_lazy
    def _jobs(self) -> tuple[SlurmJob, ...]:
        """Every ``squeue`` line ``foundInSlurm()`` kept for the run, typed; empty off the cluster."""
        lookup = self._facts.slurm
        if lookup is None:
            return ()
        try:
            return lookup.jobs
        except SlurmError as error:
            raise RunReadError(str(error)) from error

    @_lazy
    def _parity_status(self) -> str:
        """The R ``RunStatus`` (``column_run_status``), computed once for both views."""
        return column_run_status(self._facts, self._job_text)

    @_lazy
    def status_detail(self) -> str:
        """The R ``RunStatus`` text with the last ``*** Status:`` line winning (D-25).

        ``"Normal completion"``, ``"Execution error"``, ``"Timeout interrupt"``, ``"Run in progress"``,
        ``"conoptspy >2h"``, ``"Abort EUR 2*Infes"``, ``"full.log missing"`` ... (``03`` 3.1). With one
        ``*** Status:`` line or none this is the parity value itself; with several (the first D-25
        difference) R's cell assignment fails and the fallbacks run (BUG-036), whereas here the last line is
        the status and only the overrides of ``R/getRunStatus.R:201-238`` apply
        (:func:`modelstats.run_status.status_overrides`).
        """
        lines = self._facts.status_lines
        if len(lines) <= 1:
            return self._parity_status
        return status_overrides(self._facts, lines[-1], self._job_text)

    @_lazy
    def status(self) -> RunState:
        """:func:`run_state` of :attr:`status_detail` and the run's jobs in the queue."""
        return run_state(self.status_detail, self._jobs)

    @_lazy
    def slurm(self) -> SlurmJob | None:
        """The run's one job in ``squeue`` (cluster only); ``None`` for no hit or several.

        A line ``foundInSlurm()`` could not parse (fewer than six fields) is a :class:`RunReadError`, as
        in :attr:`_jobs`.
        """
        lookup = self._facts.slurm
        if lookup is None:
            return None
        try:
            return lookup.job
        except SlurmError as error:
            raise RunReadError(str(error)) from error

    # -- the model's verdicts (R/getRunStatus.R:82-102, 278-304) --------------------------------------

    @_lazy
    def modelstat(self) -> ModelStat:
        """The GAMS model status of the selected GDX, or of ``runstatistics.rda`` (:class:`ModelStat`)."""
        return ModelStat.from_text(_modelstat_text(self._facts))

    @_lazy
    def convergence(self) -> Convergence:
        """The Nash convergence verdict (:class:`Convergence`)."""
        return _convergence(self._facts, self.run_type, self.iterations.maximum)

    @property
    def feasibility(self) -> Convergence:
        """Alias of :attr:`convergence` (the user's word)."""
        return self.convergence

    # -- progress (R/getRunStatus.R:141-155, 256-273, 296-313) -----------------------------------------

    @_lazy
    def iterations(self) -> Iterations:
        """The loop counters (:class:`Iterations`)."""
        facts = self._facts
        run_type = self.run_type or ""
        loops = facts.last_loops
        loop = loops[0] if loops else None
        return Iterations(
            current=_as_int(loop),
            maximum=_as_int(iteration_maximum(facts, run_type)),
            calibration=_calibration_iteration(facts, run_type),
            loop=loop,
        )

    @_lazy
    def warnings(self) -> int | None:
        """The warning count of ``log.txt`` (REMIND) or ``slurm.log`` (MAgPIE); ``None`` without the log."""
        return _as_int(column_warnings(self._facts))

    @_lazy
    def runtime(self) -> timedelta | None:
        """``timeGAMSEnd - timeGAMSStart``, or ``now - timePrepareStart`` while a job of the run is listed."""
        facts = self._facts
        wall = facts.gams_wall_seconds
        if wall is not None:
            return timedelta(seconds=wall)
        elapsed = facts.seconds_since_prepare_start if self._jobs else None
        return None if elapsed is None else timedelta(seconds=elapsed)

    # -- the reports (R/getRunStatus.R:121-136, 316-360) --------------------------------------------------

    @_lazy
    def mif(self) -> MifState:
        """The report file (:class:`MifState`)."""
        return _MIF_BY_TEXT[column_mif(self._facts)]

    @_lazy
    def _plausibility(self) -> tuple[int, int, int] | None:
        """The three REMIND plausibility counts (``R/getRunStatus.R:316-348``); ``None`` where R has none.

        A configuration file, a REMIND run (by ``runstatistics.rda``) and the mif are needed, as in R; each
        count is 0 without its file.
        """
        facts = self._facts
        config = self.config
        if config is None or facts.is_magpie or not facts.remind_mif_exists(config.title):
            return None
        title = config.title
        return (
            facts.summation_error_count(title) if facts.summation_errors_exist(title) else 0,
            facts.range_error_count(title) if facts.range_errors_exist(title) else 0,
            facts.fix_error_count if facts.fix_errors_exist else 0,
        )

    @property
    def summation_errors(self) -> int | None:
        """Distinct variables of ``REMIND_generic_<title>_summation_errors.csv``; ``None`` without a mif."""
        counts = self._plausibility
        return None if counts is None else counts[0]

    @property
    def range_errors(self) -> int | None:
        """Rows of ``REMIND_generic_<title>_range_errors.txt``; ``None`` without a mif."""
        counts = self._plausibility
        return None if counts is None else counts[1]

    @property
    def fix_errors(self) -> int | None:
        """Distinct variables of ``log_fixOnRef.csv``; ``None`` without a mif."""
        counts = self._plausibility
        return None if counts is None else counts[2]

    @_lazy
    def project_summation(self) -> ProjectSummation | None:
        """The ``ScenarioMIP`` block of ``projectSummations.rds``; ``None`` without the file."""
        project = self._facts.project_summations
        if project is None:
            return None
        block = project.get(_PROJECT_BLOCK)
        return ProjectSummation(
            _count_of(block, "missingVars"),
            _count_of(block, "checkSummations"),
            _count_of(block, "checkSummationsRegional"),
        )

    @_lazy
    def in_app_results(self) -> bool | None:
        """Whether the results archive holds a current ``<stats$id>.rds``; ``None`` off the cluster."""
        return _in_app_results(self._facts)

    # -- the parity view and the snapshot ------------------------------------------------------------

    def to_status_record(self, detailed: bool = True) -> RunStatus:
        """The R-compatible record of ``getRunStatus()`` for this run (``03`` section 5), on the same facts.

        The columns are evaluated in R's read order (``R/getRunStatus.R:34-361``), so that where two reads
        would abort, the error R reports first is the one raised, as :class:`RunReadError`. The row name
        is the run name; the call-level rules of ``get_run_status`` (order, the ``"NA"`` row of a missing
        path, the merge of repeated names, BUG-022) are not applied here.
        """
        try:
            return self._status_record(detailed)
        except _FactsReadError as error:
            raise RunReadError(str(error)) from error

    def _status_record(self, detailed: bool) -> RunStatus:
        """``run_status.run_status_record`` over this run's facts (one copy of its read order)."""
        facts = self._facts
        job = self._job_text  # :38
        facts.selected_gdx  # noqa: B018 - forced: R reads the GDX selection (:54-64) before the configuration
        run_type = column_run_type(facts)  # :69-74
        facts.statistics  # noqa: B018 - forced: R loads runstatistics.rda (:76-79) before the modelstat column
        modelstat = column_modelstat(facts)
        in_app_results = column_run_in_app_results(facts) if facts.effects.on_cluster else None
        mif = column_mif(facts)
        maximum = iteration_maximum(facts, run_type)
        iteration = column_iter(facts, maximum)
        status = self._parity_status
        if not detailed:  # :248-250
            return RunStatus(
                run=facts.files.name,
                path=facts.files.directory,
                jobInSLURM=job,
                RunType=run_type,
                modelstat=modelstat,
                runInAppResults=in_app_results,
                Mif=mif,
                Iter=iteration,
                RunStatus=status,
                detailed=False,
            )
        warnings = column_warnings(facts)
        conv = column_conv(facts, run_type, maximum)
        iteration, conv = calibration(facts, run_type, iteration, conv)
        return RunStatus(
            run=facts.files.name,
            path=facts.files.directory,
            jobInSLURM=job,
            RunType=run_type,
            modelstat=modelstat,
            runInAppResults=in_app_results,
            Mif=mif,
            Iter=iteration,
            RunStatus=status,
            detailed=True,
            Warnings=warnings,
            Conv=conv,
            Runtime=column_runtime(facts, job),
            sanity=sanity_counts(facts),
        )

    def to_dict(self) -> dict[str, Any]:
        """A JSON-able snapshot of the properties: enums as their values, the runtime in seconds."""
        runtime = self.runtime
        slurm = self.slurm
        project = self.project_summation
        return {
            "path": str(self.path),
            "name": self.name,
            "title": self.title,
            "model": self.model,
            "run_type": self.run_type,
            "status": self.status.value,
            "status_detail": self.status_detail,
            "convergence": self.convergence.value,
            "modelstat": asdict(self.modelstat),
            "iterations": asdict(self.iterations),
            "warnings": self.warnings,
            "runtime_seconds": None if runtime is None else runtime.total_seconds(),
            "mif": self.mif.value,
            "summation_errors": self.summation_errors,
            "range_errors": self.range_errors,
            "fix_errors": self.fix_errors,
            "project_summation": None if project is None else asdict(project),
            "in_app_results": self.in_app_results,
            "slurm": None if slurm is None else asdict(slurm),
        }

    # -- finding runs ------------------------------------------------------------------------------

    @staticmethod
    def is_run_folder(path: PathLike, effects: Effects | None = None) -> bool:
        """The ``rs`` detection rule (``R/commandLineInterface.R:22-26``, ``03`` 3.4).

        At least four of ``full.gms``, ``log.txt``, ``config.Rdata``, ``prepare_and_run.R``,
        ``prepareAndRun.R`` (REMIND), or all of ``full.gms``, ``submit.R``, ``config.yml``,
        ``magpie_y1995.gdx`` (MAgPIE).
        """
        env = effects if effects is not None else ProductionEffects()
        return is_run_folder(os.fspath(path), env)


def iter_runs(
    directory: PathLike, effects: Effects | None = None, *, user: str | None = None
) -> Iterator[Run]:
    """A :class:`Run` for every run folder below ``directory``, in ``rs`` order.

    ``directory`` is expanded as the ``rs`` positional argument is (``R/commandLineInterface.R:192-202``
    without the comma splitting): a run folder is itself; a model main folder contributes the directories
    below its ``output``; anything else its own sub-directories. Only run folders (:meth:`Run.is_run_folder`)
    are yielded, ordered like ``rs`` (``R/commandLineInterface.R:238``, natural order of the basenames).
    """
    env = effects if effects is not None else ProductionEffects()
    text = os.fspath(directory)
    if is_run_folder(text, env):
        folders = [text]
    elif is_main_folder(text, env):
        folders = env.list_dirs_like_r(f"{text}/output")
    else:
        folders = env.list_dirs_like_r(text)
    for folder in natural_order(folders, env):
        if is_run_folder(folder, env):
            yield Run(folder, env, user=user)


# ---------------------------------------------------------------------------------------------
# The clean rules over the facts
# ---------------------------------------------------------------------------------------------


def _modelstat_text(facts: RunFacts) -> str | None:
    """The unexplained ``modelstat`` text (``R/getRunStatus.R:84-97``) with missing fields degrading.

    The first-found ``o_modelstat``/``p80_modelstat`` of the selected GDX, pasted with ``0`` as ``.``;
    without it the ``stats$modelstat`` of ``runstatistics.rda``: every value pasted for a MAgPIE run, the
    one value of a REMIND run (R's ``try()`` fails on a vector). ``None`` when neither has it.
    """
    values = facts.modelstat_values
    text = modelstat_text(values) if values is not None else NA
    if text != NA:
        return text
    stats = facts.statistics
    if stats is None or not stats.has_name("modelstat"):
        return None
    if stats.is_magpie:
        return "".join(r_as_character(value) or NA for value in stats.modelstat)
    return r_as_character(stats.modelstat[0]) if len(stats.modelstat) == 1 else None


def _convergence(facts: RunFacts, run_type: str | None, maximum: int | None) -> Convergence:
    """``Conv`` as an enum (``R/getRunStatus.R:278-304``) with missing symbols degrading.

    ``s80_bool`` of the selected GDX of a Nash run: 1 is converged (``converged (had INFES)`` with a
    ``non_optimal.gdx``), 0 is not converged (R prints the ``p80_repy`` digits below the maximum); a
    converged calibration run with more than ten iteration files is ``Clb_converged`` (BUG-001 kept).
    """
    if run_type is None or _NASH not in run_type or facts.selected_gdx is None:
        return Convergence.UNKNOWN
    flag = facts.convergence_flag
    if flag is None or math.isnan(flag):
        return Convergence.UNKNOWN
    if flag == 0:
        return Convergence.NOT_CONVERGED
    if flag != 1:
        return Convergence.UNKNOWN
    if _is_calibrating(facts, run_type) and facts.files.exists(facts.files.log_txt):  # :297-304
        iteration_files = max(facts.fulldata_iteration_count, facts.input_iteration_count)
        if iteration_files > _CALIBRATION_FILES_ABOVE:
            return Convergence.CALIBRATION_CONVERGED
    if facts.files.exists(facts.files.non_optimal_gdx):
        return Convergence.CONVERGED_HAD_INFES
    return Convergence.CONVERGED


def _is_calibrating(facts: RunFacts, run_type: str) -> bool:
    """``grepl("Calib", RunType) || isTRUE(cfg$gms$CES_parameters == "calibrate")`` (``:297``)."""
    config = facts.config
    return _CALIB in run_type or (config is not None and r_as_character(config.CES_parameters) == _CALIBRATE)


def _calibration_iteration(facts: RunFacts, run_type: str) -> int | None:
    """The last ``CES calibration iteration`` number of a calibrating run (``R/getRunStatus.R:297-300``).

    ``None`` for a run that does not calibrate, without ``log.txt``, or when the number is not positive.
    """
    if not (_is_calibrating(facts, run_type) and facts.files.exists(facts.files.log_txt)):
        return None
    digits = facts.calibration_iteration
    number = _as_int(digits[0]) if digits else None
    return number if number is not None and number > 0 else None


def _count_of(block: Any, name: str) -> int | None:
    """``tmp[["ScenarioMIP"]][[name]]`` as a count; ``None`` for a missing element, ``NA`` or a non-count."""
    value = block.get(name) if isinstance(block, Mapping) else None
    if value is None:
        return None
    try:
        element = scalar(value)
    except RDataError:
        return None
    if isinstance(element, bool | int):
        return int(element)
    if isinstance(element, float) and element.is_integer():
        return int(element)
    return None


def _in_app_results(facts: RunFacts) -> bool | None:
    """``runInAppResults`` (``R/getRunStatus.R:105-116``) as a boolean, ``None`` off the cluster.

    ``<archive>/<stats$id>.rds`` exists and no ``overview.rds`` of the archive is older than ten minutes
    before it (vacuously true without one, BUG-010); the archive is ``$MAGPIE_RESULTS_ARCHIVE_PATH/`` for
    a MAgPIE run and the REMIND results archive otherwise. ``False`` without a ``stats$id``.
    """
    env = facts.effects
    if not env.on_cluster:
        return None
    stats = facts.statistics
    if stats is None or stats.id is None:
        return False
    archive = (
        f"{env.magpie_results_archive}/" if facts.is_magpie else env.cluster_paths.remind_results_archive
    )
    return facts.archived_result_is_current(archive)
