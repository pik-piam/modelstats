"""The parity view of a run: ``getRunStatus()`` (``R/getRunStatus.R``) column by column over the facts.

``migration/03-migration-plan.md`` section 3.1 is the contract and the goldens under
``migration/goldens/status/<mode>/<detailed|brief>`` pin it; ``06`` 5.2 asks for one small function per
column. Every rule of the R loop (``R/getRunStatus.R:34-363``) is reproduced in R's order, quirks included
and recorded in ``migration/04-bug-register.md`` (every ``rs`` decision of ``03`` 3.5 is pending, so parity
holds everywhere):

* rows keyed by ``basename(dir)``, a repeated name overwriting the earlier row (BUG-022, D-13);
* a path that does not exist becomes the row ``"NA"`` under ``sort = "nf"`` (the data-frame indexing of
  ``:30-32``); ``sort = "nf"`` lists directories only, newest ``mtime`` first;
* the configuration file by the unanchored regex, ``colRunType()`` only when one exists (BUG-003, D-01);
* ``runInAppResults`` ``"yes"`` without an ``overview.rds`` (BUG-010);
* ``RunStatus`` ``"NA"`` for two or more ``*** Status:`` lines (BUG-036, D-19), ``"NA"`` kept on the
  cluster for an unrecognised ``slurmstepd`` error (BUG-030), the coupled-run lookup on the unexpanded
  ``cfg_mag$results_folder`` template (BUG-021), ``Abort ... *Infes`` on equality (BUG-002, D-10);
* REMIND ``Warnings`` missing the singular ``Warning message:`` and ``50 or more`` (BUG-029);
* ``Clb_converged`` only above ten calibration files (BUG-001, D-09); an abort when the selected GDX
  lacks ``o_iterationNumber`` (BUG-038, D-23);
* a live, growing ``Runtime`` off the cluster for every unfinished run (``"NA" != "no"``).

Anything R raises outside ``try()`` propagates as :class:`modelstats.facts.RunReadError` with R's message.

:class:`RunStatus` is the record of ``03`` section 5: the R column names as fields, string columns holding
the literal ``"NA"`` where R does, ``Runtime`` in whole seconds, the sanity counts as integers, and every
column R never assigns (brief mode, a MAgPIE row, a REMIND row without a mif, ``runInAppResults`` off the
cluster) absent from :meth:`RunStatus.to_row` rather than ``None``. :func:`to_dataframe` builds the pandas
equivalent of the R data frame.
"""

from __future__ import annotations

import math
import os
import re
from collections.abc import Iterable, Mapping, Sequence
from dataclasses import dataclass, replace
from typing import Any, Final

import pandas as pd

from modelstats.env import Effects, PathLike, ProductionEffects, r_collation_key
from modelstats.facts import (
    ARGUMENT_OF_LENGTH_ZERO,
    MISSING_VALUE,
    RunFacts,
    RunFiles,
    RunReadError,
)
from modelstats.formatting import niceround, r_round
from modelstats.gdx import (
    VALUE,
    GdxReadError,
    explain_modelstat,
    magpie_roles,
    modelstat_text,
    repy_modelstat_text,
)
from modelstats.rdata_io import RDataError, RScalar, r_as_character, scalar
from modelstats.rdata_io import r_round as r_round_seconds
from modelstats.runstats import MAGPIE, RunStatistics
from modelstats.slurm import SlurmError
from modelstats.tables import Cell
from modelstats.tables import to_dataframe as _frame_of

__all__ = [
    "INCORRECT_DIMENSIONS",
    "NA",
    "NEWEST_FIRST",
    "NO_JOB",
    "REPLACEMENT_LENGTH_ZERO",
    "RunStatus",
    "SanityCounts",
    "calibration",
    "column_conv",
    "column_iter",
    "column_job_in_slurm",
    "column_mif",
    "column_modelstat",
    "column_run_in_app_results",
    "column_run_status",
    "column_run_type",
    "column_runtime",
    "column_warnings",
    "get_run_status",
    "iteration_maximum",
    "merge_records",
    "ordered_directories",
    "run_status_record",
    "sanity_counts",
    "status_overrides",
    "to_dataframe",
]

NA: Final = "NA"
"""What R assigns before it knows better (``R/getRunStatus.R:38, 70, 82, 121, 151, 256, 278``)."""

NEWEST_FIRST: Final = "nf"
"""``sort = "nf"`` (``R/getRunStatus.R:32``): newest directory first; anything else keeps the input order."""

NO_JOB: Final = "no"
"""``foundInSlurm()`` for no job (``R/foundInSlurm.R:46``; ``R/getRunStatus.R:160, 169, 201, 222, 312``)."""

REPLACEMENT_LENGTH_ZERO: Final = "replacement has length zero"
"""R's message for assigning ``NULL`` to one cell: ``R/getRunStatus.R:73`` (BUG-046, D-28), ``:355-357``."""

INCORRECT_DIMENSIONS: Final = "incorrect number of dimensions"
"""R's message for ``NULL[, , "modelstat"]``: ``R/getRunStatus.R:290`` without ``p80_repy``."""

_NASH: Final = "nash"
_CALIB: Final = "Calib"
_CALIBRATE: Final = "calibrate"
_NORMAL_COMPLETION: Final = "Normal completion"
_EXECUTION_ERROR: Final = "Execution error"
_RUN_INTERRUPTED: Final = "Run interrupted"
_RUN_RESTARTED: Final = "Run restarted"
_RUN_IN_PROGRESS: Final = "Run in progress"
_RUNNING_REPORTING: Final = "Running reporting"
_WAIT_MAGPIE_LOCK: Final = "Wait MAgPIE lock"
_WAIT_REMIND_LOCK: Final = "Wait REMIND lock"
_FULL_LOG_MISSING: Final = "full.log missing"
_STARTING_MAGPIE: Final = "Starting MAgPIE..."
_MODEL_LOCK: Final = "try to acquire model lock"
_MAGPIE_LOG_NAME: Final = "log-mag.txt"
_CONVERGED: Final = "converged"
_CONVERGED_INFES: Final = "converged (had INFES)"
_NOT_CONVERGED: Final = "not_converged"
_CALIBRATION_CONVERGED: Final = "Clb_converged"
_CALIBRATION_FILES_ABOVE: Final = 10
_CONOPT_DELAY_OFFSET: Final = 0.049
_CONOPT_DELAY_HOURS: Final = 0.1
_GDX_DELAY_HOURS: Final = 0.25
_MIF_SIZE_ABOVE: Final = 99999
_PROJECT_BLOCK: Final = "ScenarioMIP"
_COUPLING_ITERATION: Final = re.compile(r".*-mag-([0-9]{1,2})$")
"""``gsub(".*-mag-([0-9]{1,2})$", "\\\\1", cfg$cfg_mag$results_folder)`` (``R/getRunStatus.R:194``)."""

_INTERRUPTS: Final[tuple[tuple[re.Pattern[str], str], ...]] = (
    (re.compile("DUE TO TIME LIMIT"), "Timeout interrupt"),
    (re.compile("memory|oom-kill"), "Memory interrupt"),
    (re.compile("DUE TO PREEMPTION"), "Preempt interrupt"),
    (re.compile("DUE TO JOB REQUEUE"), "Run requeued"),
    (re.compile("CANCELLED"), "Run cancelled"),
)
"""``R/getRunStatus.R:164-174``: the ``slurmstepd`` patterns in R's order; no match keeps ``NA`` (BUG-030)."""


# ---------------------------------------------------------------------------------------------
# The record
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True, slots=True)
class SanityCounts:
    """The REMIND plausibility columns (``R/getRunStatus.R:326-360``), assigned only with a config and a mif.

    The three counts are integers; the three project-summation values come from ``projectSummations.rds``
    and are ``None`` (R's ``NA``) when that file is absent.
    """

    summationErrors: int  # noqa: N815 - the R column name
    rangeErrors: int  # noqa: N815 - the R column name
    fixErrors: int  # noqa: N815 - the R column name
    missingProjVars: int | None  # noqa: N815 - the R column name
    projSummationErrors: int | None  # noqa: N815 - the R column name
    projSummationErrorsRegional: int | None  # noqa: N815 - the R column name

    def to_row(self) -> dict[str, Cell]:
        """The six cells by their R names, in R's order."""
        return {
            "summationErrors": self.summationErrors,
            "rangeErrors": self.rangeErrors,
            "fixErrors": self.fixErrors,
            "missingProjVars": self.missingProjVars,
            "projSummationErrors": self.projSummationErrors,
            "projSummationErrorsRegional": self.projSummationErrorsRegional,
        }


@dataclass(frozen=True, slots=True)
class RunStatus:
    """One row of ``getRunStatus()``: the R columns as fields (``03`` section 5, "RunStatus dataclass").

    ``run`` is the row name (``basename(dir)``) and ``path`` the directory it was read from (the resolved
    path, or ``"NA"`` for a missing one under ``sort = "nf"``). String columns hold the literal ``"NA"``
    where R assigns it; ``modelstat`` is ``None`` only for the real ``NA`` a ``runstatistics.rda`` can
    carry (``:93``). ``runInAppResults`` is ``None`` off the cluster, where R never assigns the column;
    ``Warnings``, ``Conv``, ``Runtime`` are ``None`` in brief mode (``detailed=False``), and ``Runtime``
    ``None`` in detailed mode is R's ``NA``; ``sanity`` is ``None`` where R never reaches the block.
    :meth:`to_row` contains exactly the cells R assigned, in R's column order.
    """

    run: str
    path: str
    jobInSLURM: str  # noqa: N815 - the R column name
    RunType: str
    modelstat: str | None
    runInAppResults: str | None  # noqa: N815 - the R column name
    Mif: str
    Iter: str
    RunStatus: str
    detailed: bool = True
    Warnings: str | None = None
    Conv: str | None = None
    Runtime: int | None = None
    sanity: SanityCounts | None = None

    def to_row(self) -> dict[str, Cell]:
        """The cells R assigned, by column name and in R's order; a column R never assigned is absent."""
        row: dict[str, Cell] = {
            "jobInSLURM": self.jobInSLURM,
            "RunType": self.RunType,
            "modelstat": self.modelstat,
        }
        if self.runInAppResults is not None:
            row["runInAppResults"] = self.runInAppResults
        row["Mif"] = self.Mif
        row["Iter"] = self.Iter
        row["RunStatus"] = self.RunStatus
        if not self.detailed:
            return row
        row["Warnings"] = self.Warnings
        row["Conv"] = self.Conv
        row["Runtime"] = self.Runtime
        if self.sanity is not None:
            row.update(self.sanity.to_row())
        return row

    @property
    def cells(self) -> Mapping[str, Cell]:
        """:meth:`to_row`, under the name the presentation layer (``formatting.StatusRow``) reads."""
        return self.to_row()


def merge_records(earlier: RunStatus, later: RunStatus) -> RunStatus:
    """Two records of one row name: the later overwrites every column it assigns (BUG-022, D-13 pending).

    ``out[i, col] <- value`` on an existing row name replaces that cell, so the columns the later record
    never assigned keep the earlier values: only the sanity block can differ, since both records come
    from one call (same mode, same ``detailed``). ``path`` is the later record's.
    """
    return replace(later, sanity=later.sanity if later.sanity is not None else earlier.sanity)


def to_dataframe(records: Iterable[RunStatus]) -> pd.DataFrame:
    """The ``data.frame`` ``getRunStatus()`` returns (``tables.to_dataframe``): run names as the index."""
    return _frame_of(records)


# ---------------------------------------------------------------------------------------------
# The call: directories, order, identity (R/getRunStatus.R:22-37, 362-365)
# ---------------------------------------------------------------------------------------------


def get_run_status(
    dirs: PathLike | Sequence[PathLike],
    sort: str = NEWEST_FIRST,
    user: str | None = None,
    detailed: bool = True,
    effects: Effects | None = None,
) -> list[RunStatus]:
    """``getRunStatus(mydir, sort, user, detailed)`` (``R/getRunStatus.R:22-365``) as a list of records.

    ``user`` defaults to ``Sys.info()[["user"]]`` (``:24``); the paths are normalised (``:25``) and ordered
    by :func:`ordered_directories`; each is read by :func:`run_status_record`; a repeated row name merges
    into the earlier record (:func:`merge_records`). An R error outside ``try()`` raises
    :class:`RunReadError` and nothing is returned, as the R call aborts.
    """
    env = effects if effects is not None else ProductionEffects()
    paths = [dirs] if isinstance(dirs, str | os.PathLike) else list(dirs)
    who = env.user if user is None else user
    normalized = [env.normalize_path(path) for path in paths]
    records: list[RunStatus] = []
    position: dict[str, int] = {}
    for directory in ordered_directories(normalized, sort, env):
        record = run_status_record(directory, who, detailed, env)
        index = position.get(record.run)
        if index is None:
            position[record.run] = len(records)
            records.append(record)
        else:
            records[index] = merge_records(records[index], record)
    return records


def ordered_directories(paths: Sequence[str], sort: str, env: Effects) -> list[str]:
    """The directories in R's loop order (``R/getRunStatus.R:30-32``).

    With ``sort = "nf"`` the rows of ``file.info(mydir)`` are filtered by ``isdir == TRUE`` and ordered by
    ``mtime`` decreasing: files drop out, existing directories come newest first (ties in input order), and
    a missing path, whose ``isdir`` is ``NA``, survives the filter as an all-``NA`` row named ``"NA"``
    (``"NA.1"``, ``"NA.2"`` ... for several) that sorts last; the loop then runs on that name as a path.
    Any other ``sort`` keeps the input as given, files and missing paths included.
    """
    if sort != NEWEST_FIRST:
        return list(paths)
    directories: list[tuple[str, float]] = []
    missing = 0
    for path in paths:
        info = env.stat(path)
        if info is None:
            missing += 1
        elif info.is_dir:
            directories.append((path, info.mtime_epoch))
    ordered = [path for path, _ in sorted(directories, key=lambda item: -item[1])]
    return ordered + [_na_row_name(index) for index in range(missing)]


def _na_row_name(index: int) -> str:
    """``make.unique(rep("NA", n))``: ``NA``, ``NA.1``, ``NA.2`` ..."""
    return NA if index == 0 else f"{NA}.{index}"


# ---------------------------------------------------------------------------------------------
# One row (R/getRunStatus.R:34-361)
# ---------------------------------------------------------------------------------------------


def run_status_record(directory: str, user: str, detailed: bool, env: Effects) -> RunStatus:
    """One iteration of the directory loop (``R/getRunStatus.R:34-361``), the columns in R's order.

    The facts are forced in the order R reads the files, so that when two reads would abort the call,
    the error R reports first is the one raised: ``squeue`` (``:38``), the GDX selection (``:54-64``),
    the configuration (``:69-74``), ``runstatistics.rda`` (``:76-79``), then the columns.
    """
    facts = RunFacts(RunFiles(directory, env), env, user)
    job = column_job_in_slurm(facts)
    facts.selected_gdx  # noqa: B018 - forced here: R reads the GDX selection (:54-64) before the configuration
    run_type = column_run_type(facts)
    facts.statistics  # noqa: B018 - forced here: R loads runstatistics.rda (:76-79) before the modelstat column
    modelstat = column_modelstat(facts)
    in_app_results = column_run_in_app_results(facts) if env.on_cluster else None
    mif = column_mif(facts)
    maximum = iteration_maximum(facts, run_type)
    iteration = column_iter(facts, maximum)
    status = column_run_status(facts, job)
    record = RunStatus(
        run=facts.files.name,
        path=directory,
        jobInSLURM=job,
        RunType=run_type,
        modelstat=modelstat,
        runInAppResults=in_app_results,
        Mif=mif,
        Iter=iteration,
        RunStatus=status,
        detailed=detailed,
    )
    if not detailed:  # :248-250
        return record
    warnings = column_warnings(facts)
    conv = column_conv(facts, run_type, maximum)
    iteration, conv = calibration(facts, run_type, iteration, conv)
    return replace(
        record,
        Iter=iteration,
        Warnings=warnings,
        Conv=conv,
        Runtime=column_runtime(facts, job),
        sanity=sanity_counts(facts),
    )


# ---------------------------------------------------------------------------------------------
# The columns, in order of first assignment
# ---------------------------------------------------------------------------------------------


def column_job_in_slurm(facts: RunFacts) -> str:
    """``jobInSLURM`` (``R/getRunStatus.R:38``): ``foundInSlurm()`` on the cluster, ``"NA"`` elsewhere.

    The user regex of ``R/foundInSlurm.R:33`` is evaluated here, after the lookup; an invalid ``user``
    aborts the call with R's message, as the package's :class:`RunReadError`.
    """
    lookup = facts.slurm
    if lookup is None:
        return NA
    try:
        return lookup.text
    except SlurmError as error:
        raise RunReadError(str(error)) from error


def column_run_type(facts: RunFacts) -> str:
    """``RunType`` (``R/getRunStatus.R:69-74``): ``"NA"`` without a configuration file, else ``colRunType()``.

    A ``NULL`` from ``colRunType`` (a MAgPIE configuration without ``cfg$gms$optimization``, BUG-046) fails
    the cell assignment with ``replacement has length zero``.
    """
    if facts.config is None:
        return NA
    run_type = facts.run_type
    if run_type is None:
        raise RunReadError(REPLACEMENT_LENGTH_ZERO)
    return run_type


def column_modelstat(facts: RunFacts) -> str | None:
    """``modelstat`` (``R/getRunStatus.R:82-102``).

    The first-found ``o_modelstat``/``p80_modelstat`` of the selected GDX, pasted with ``0`` as ``.``
    (``:86``); when that leaves ``"NA"``, the ``modelstat`` of ``runstatistics.rda``
    (:func:`_modelstat_from_statistics`); a value among ``1, 2, 3, 4, 5, 6, 7, 13`` is explained
    (``:98-102``).
    """
    text: str | None = NA
    values = facts.modelstat_values
    if values is not None:
        text = modelstat_text(values)
    if text == NA:
        text = _modelstat_from_statistics(facts)
    return None if text is None else explain_modelstat(text)


def _modelstat_from_statistics(facts: RunFacts) -> str | None:
    """``R/getRunStatus.R:89-97``: the ``stats$modelstat`` fallback, by model.

    MAgPIE pastes ``as.character(stats$modelstat)`` into one string (``NA`` elements as ``"NA"``); REMIND
    assigns the value inside ``try()``, which only succeeds for a length-1 value, so a vector or ``NULL``
    leaves ``"NA"`` and a real ``NA`` becomes the cell's ``NA`` (``None``). The model test of ``:91`` is the
    strict ``==``: a ``NULL`` or ``NA`` model name aborts the call.
    """
    stats = facts.statistics
    if stats is None or not stats.has_name("config"):
        return NA
    if _is_magpie_strict(stats):
        if stats.has_name("modelstat"):
            return "".join(r_as_character(value) or NA for value in stats.modelstat)
        return NA
    if stats.has_name("modelstat") and len(stats.modelstat) == 1:
        return r_as_character(stats.modelstat[0])
    return NA


def _is_magpie_strict(stats: RunStatistics) -> bool:
    """``if (stats[["config"]][["model_name"]] == "MAgPIE")`` (``R/getRunStatus.R:91, 108``), no ``isTRUE``.

    ``NULL`` (no such element) is ``argument is of length zero``; ``NA`` is ``missing value where
    TRUE/FALSE needed``.
    """
    config = stats.config
    if config is None or config.get("model_name") is None:
        raise RunReadError(ARGUMENT_OF_LENGTH_ZERO)
    if stats.model_name is None:
        raise RunReadError(MISSING_VALUE)
    return stats.model_name == MAGPIE


def column_run_in_app_results(facts: RunFacts) -> str:
    """``runInAppResults`` (``R/getRunStatus.R:105-116``), cluster only.

    ``"yes"`` when ``<archive>/<stats$id>.rds`` exists and no ``overview.rds`` of the archive is older than
    ten minutes before it (vacuously true without one, BUG-010); the archive is
    ``$MAGPIE_RESULTS_ARCHIVE_PATH/`` for a MAgPIE run (strict model test, ``:108``) and the REMIND results
    archive otherwise. ``"no"`` without a ``stats`` name matching ``id``.
    """
    stats = facts.statistics
    if stats is None or not stats.has_name("id"):
        return "no"
    if stats.has_name("config") and _is_magpie_strict(stats):
        archive = f"{facts.effects.magpie_results_archive}/"
    else:
        archive = facts.effects.cluster_paths.remind_results_archive
    return "yes" if facts.archived_result_is_current(archive) else "no"


def column_mif(facts: RunFacts) -> str:
    """``Mif`` (``R/getRunStatus.R:121-136``).

    ``"NA"`` without a configuration file; MAgPIE (by ``runstatistics.rda``): ``validation.mif`` larger than
    99 999 bytes gives ``"yes"``, else ``"no"``; REMIND: ``"no"`` without ``REMIND_generic_<title>.mif``,
    ``"sumErr"`` with the summation-errors file next to it, ``"yes"`` otherwise.
    """
    config = facts.config
    if config is None:
        return NA
    if facts.is_magpie:
        size = facts.validation_mif_size
        return "yes" if size is not None and size > _MIF_SIZE_ABOVE else "no"
    if not facts.remind_mif_exists(config.title):
        return "no"
    if facts.summation_errors_exist(config.title):
        return "sumErr"
    return "yes"


def iteration_maximum(facts: RunFacts, run_type: str) -> str | None:
    """``cm_iteration_max`` (``R/getRunStatus.R:141-147``) as the text ``Iter`` and ``Conv`` use.

    ``cfg$gms$cm_iteration_max`` pasted as text (``None`` for ``NULL``; a stored ``NA`` is not told apart
    from ``NULL`` by the configuration reader and gives ``None`` too, where R would print ``NA``), replaced
    for a ``nash`` run type with ``cm_nash_autoconverge > 0`` by the last ``cm_iteration_max = N;`` line of
    ``full.gms`` when that file exists: ``N``, or ``None`` when no line matches (R's ``character(0)``).
    """
    config = facts.config
    if config is None:
        return None
    maximum = r_as_character(config.cm_iteration_max)
    autoconverging = _is_true_positive(config.cm_nash_autoconverge) and _NASH in run_type
    if autoconverging and facts.files.exists(facts.files.full_gms):
        lines = facts.iteration_max_lines
        maximum = lines[0] if lines else None
    return maximum


def column_iter(facts: RunFacts, maximum: str | None) -> str:
    """``Iter`` before the calibration suffix (``R/getRunStatus.R:151-155``).

    ``"NA"`` without ``full.log``; else the last ``LOOPS`` value (``"NA"`` without one) with ``/<maximum>``
    appended when :func:`iteration_maximum` found one.
    """
    if not facts.files.exists(facts.files.full_log):
        return NA
    loops = facts.last_loops
    text = loops[0] if loops else NA
    return text if maximum is None else f"{text}/{maximum}"


# -- RunStatus (R/getRunStatus.R:152-243) ----------------------------------------------------------------


def column_run_status(facts: RunFacts, job: str) -> str:
    """``RunStatus`` (``R/getRunStatus.R:152-243``): the status line, its fallbacks and overrides, R's order.

    Without ``full.log``: ``"Wait REMIND lock"`` when the last line of ``log.txt`` says so, else
    ``"full.log missing"`` (``:239-243``). With it: exactly one ``*** Status:`` line is the status
    (``:157``; none or several leave ``"NA"``, BUG-036); a ``"NA"`` then takes the cluster fallbacks
    (:func:`_interrupt_status` for ``"no"``/pending jobs, :func:`_in_progress_status` otherwise) or
    ``"Run interrupted"`` off the cluster (``:159-199``); then the overrides ``"Running reporting"``
    (``:201-208``), ``"Abort ... *Infes"`` (``:210-220``), ``"Run MAgPIE ..."`` and ``"Wait MAgPIE lock"``
    (``:222-238``).
    """
    files = facts.files
    if not files.exists(files.full_log):
        return _lock_or_missing(facts)
    lines = facts.status_lines
    status = lines[0] if len(lines) == 1 else NA
    if facts.effects.on_cluster and status == NA:
        if job == NO_JOB or job.endswith("pending"):
            status = _interrupt_status(facts, job)
        else:
            status = _in_progress_status(facts)
    elif status == NA:
        status = _RUN_INTERRUPTED
    return status_overrides(facts, status, job)


def status_overrides(facts: RunFacts, status: str, job: str) -> str:
    """The overrides of ``R/getRunStatus.R:201-238`` on one status: the tail of :func:`column_run_status`.

    ``"Running reporting"`` after completion while the job still runs (``:201-208``), ``"Abort <region>
    <n>*Infes"`` / ``"Abort <k>R*<n>*Infes"`` from ``abort.gdx`` (``:210-220``, BUG-002), then ``"Run MAgPIE
    ..."`` and ``"Wait MAgPIE lock"`` from the MAgPIE log of a coupled run (``:222-238``). Public so that the
    clean view (``Run.status_detail``) can apply them to the last ``*** Status:`` line (D-25).
    """
    status = _reporting_override(facts, status, job)
    status = _abort_override(facts, status)
    return _magpie_override(facts, status, job)


def _lock_or_missing(facts: RunFacts) -> str:
    """``R/getRunStatus.R:239-243``: ``"Wait REMIND lock"`` or ``"full.log missing"``."""
    files = facts.files
    tail = facts.logtxt_tail
    if files.exists(files.log_txt) and tail and _MODEL_LOCK in tail[0]:
        return _WAIT_REMIND_LOCK
    return _FULL_LOG_MISSING


def _interrupt_status(facts: RunFacts, job: str) -> str:
    """``R/getRunStatus.R:160-176``: why a run without a status line and without a running job stopped.

    With ``log.txt``, the first ``slurmstepd`` pattern of :data:`_INTERRUPTS` found in its error lines, or
    ``"NA"`` when none matches (BUG-030); without ``log.txt``, ``"Run interrupted"`` for no job and
    ``"Run restarted"`` for a pending one.
    """
    files = facts.files
    if not files.exists(files.log_txt):
        return _RUN_INTERRUPTED if job == NO_JOB else _RUN_RESTARTED
    errors = facts.slurmstepd_errors
    for pattern, label in _INTERRUPTS:
        if any(pattern.search(line) for line in errors):
            return label
    return NA


def _in_progress_status(facts: RunFacts) -> str:
    """``R/getRunStatus.R:177-198``: ``"Run in progress"``, the conopt-spy delay, the coupled MAgPIE step."""
    status = _RUN_IN_PROGRESS
    delay = _conopt_delay(facts)
    if delay is not None:
        status = delay
    files = facts.files
    if files.exists(files.log_txt) and facts.last_logtxt_line == _STARTING_MAGPIE:
        status = _magpie_iteration_status(facts)
    return status


def _conopt_delay(facts: RunFacts) -> str | None:
    """``R/getRunStatus.R:179-186``: ``"conoptspy ><h>h"`` when the grid logs are stale.

    ``conoptdelay`` is the hours since the newest ``225*/grid*/gmsgrid.log`` minus 0.049, rounded to one
    decimal (R's ``round``); ``gdxdelay`` the hours since the selected GDX (1 without one); the label fires
    for ``conoptdelay > 0.1`` and ``gdxdelay > 0.25`` with ``piamutils::niceround(conoptdelay, 1)``.
    """
    newest = facts.grid_log_newest_mtime
    if newest is None:
        return None
    conopt_delay = r_round(facts.hours_since(newest) - _CONOPT_DELAY_OFFSET, 1)
    gdx_mtime = facts.selected_gdx_mtime
    gdx_delay = 1.0 if gdx_mtime is None else facts.hours_since(gdx_mtime)
    if conopt_delay > _CONOPT_DELAY_HOURS and gdx_delay > _GDX_DELAY_HOURS:
        return f"conoptspy >{niceround(conopt_delay, 1)}h"
    return None


def _magpie_iteration_status(facts: RunFacts) -> str:
    """``R/getRunStatus.R:188-196``: ``"mag-<i> <Iter of the MAgPIE run>"`` while MAgPIE runs.

    The MAgPIE folder is ``file.path(cfg$path_magpie, cfg$cfg_mag$results_folder)``, which in the Nash
    coupling layout is the unexpanded template ``output/:title::date:`` (BUG-021): ``getRunStatus()`` on
    that path yields the ``"NA"`` row, whose ``Iter`` is ``"NA"``, and the coupling iteration is the
    template itself. A ``NULL`` in either field makes ``file.path`` empty, the recursive call empty, and
    the pasted ``Iter`` nothing. The recursive call uses the R defaults: ``sort = "nf"``, the session's
    user, ``detailed = TRUE``.
    """
    config = facts.config
    path_magpie = None if config is None else config.path_magpie
    folder = None if config is None else config.cfg_mag_results_folder
    coupling = "" if folder is None else _COUPLING_ITERATION.sub(r"\1", folder)
    magpie_iter = ""
    if path_magpie is not None and folder is not None:
        rows = get_run_status([f"{path_magpie}/{folder}"], effects=facts.effects)
        magpie_iter = rows[0].Iter if rows else ""
    return f"mag-{coupling} {magpie_iter}"


def _reporting_override(facts: RunFacts, status: str, job: str) -> str:
    """``R/getRunStatus.R:201-208``: ``"Running reporting"`` after completion while the job still runs."""
    files = facts.files
    completed = status == _NORMAL_COMPLETION and files.exists(files.log_txt) and job != NO_JOB
    if completed and facts.output_generation_started and not facts.output_generation_finished:
        return _RUNNING_REPORTING
    return status


def _abort_override(facts: RunFacts, status: str) -> str:
    """``R/getRunStatus.R:210-220``: ``"Abort <region> <n>*Infes"`` / ``"Abort <k>R*<n>*Infes"``.

    For an execution error with ``abort.gdx``: the regions whose ``p80_trackConsecFail`` *equals*
    ``cm_abortOnConsecFail`` in some iteration (BUG-002, D-10), one named with a trailing blank, several
    counted; nothing when the threshold is missing, not a single value or not positive (``isTRUE(maxinfes
    > 0)`` at ``:214`` is false for a vector of any other length and for ``NA``), the counter is missing,
    or no region reaches it.
    """
    files = facts.files
    if status != _EXECUTION_ERROR or not files.exists(files.abort_gdx):
        return status
    threshold = facts.abort_threshold
    track = facts.abort_track
    if threshold is None or len(threshold) != 1 or not threshold[0] > 0 or track is None:
        return status
    maxinfes = threshold[0]
    regions = _regions_at(track, maxinfes)
    if not regions:
        return status
    label = f"{regions[0]} " if len(regions) == 1 else f"{len(regions)}R*"
    return f"Abort {label}{r_as_character(maxinfes)}*Infes"


def _regions_at(track: pd.DataFrame, threshold: float) -> list[str]:
    """``unique(cf[cf$value == maxinfes, ]$region)`` (``R/getRunStatus.R:216``) for ``quitte::as.quitte(x)``.

    ``region`` is the spatial dimension ``as.magpie`` detects (the first column otherwise); the names come
    in first-appearance order.
    """
    labels = [list(dict.fromkeys(track[column].astype(str))) for column in track.columns[:-1]]
    spatial, _ = magpie_roles(labels)
    column = track.columns[0 if spatial is None else spatial]
    hits = track.loc[track[VALUE] == threshold, column].astype(str)
    return list(dict.fromkeys(hits))


def _magpie_override(facts: RunFacts, status: str, job: str) -> str:
    """``R/getRunStatus.R:222-238``: ``"Run MAgPIE <loop|report|>"`` and ``"Wait MAgPIE lock"``.

    Applies while the job runs, for a completed run or whenever the MAgPIE log is the dedicated
    ``log-mag.txt``: a ``Preparing MAgPIE`` not followed by ``MAgPIE output was stored`` gives
    ``"Run MAgPIE "`` plus the last ``LOOPS`` of the MAgPIE iteration's ``full.log`` (nothing without),
    ``report`` once ``Start getReport`` appears; a last line asking for the model lock gives
    ``"Wait MAgPIE lock"``.
    """
    files = facts.files
    log = files.magpie_log
    applies = status == _NORMAL_COMPLETION or _MAGPIE_LOG_NAME in log
    if not (files.exists(log) and job != NO_JOB and applies):
        return status
    if facts.magpie_preparing and not facts.magpie_output_stored:
        loops = facts.coupled_magpie_loops
        loop = loops[0] if loops else None
        if facts.magpie_report_started:
            loop = "report"
        status = f"Run MAgPIE {loop or ''}"
    tail = facts.magpie_log_tail
    if tail and _MODEL_LOCK in tail[0]:
        status = _WAIT_MAGPIE_LOCK
    return status


# -- detailed mode (R/getRunStatus.R:252-361) -------------------------------------------------------------


def column_warnings(facts: RunFacts) -> str:
    """``Warnings`` (``R/getRunStatus.R:256-273``), detailed only.

    MAgPIE with ``slurm.log``: the number of the last numbered warning of the last ``Warning messages:``
    block, ``"1"`` for a bare ``Warning message:``, else ``"0"``. REMIND with ``log.txt``: the digits of
    ``There were N warnings``, else the count of numbered lines in the ``Warning messages:`` blocks
    (misses ``Warning message:`` and ``50 or more``, BUG-029). ``"NA"`` otherwise.
    """
    files = facts.files
    if files.exists(files.slurm_log) and facts.is_magpie:
        block = facts.magpie_warning_block
        if block:
            return block[0]
        return "1" if facts.slurm_log_has_warning_message else "0"
    if files.exists(files.log_txt) and not facts.is_magpie:
        summary = facts.warning_summary
        if summary:
            return summary[0]
        return str(facts.numbered_warning_count)
    return NA


def column_conv(facts: RunFacts, run_type: str, maximum: str | None) -> str:
    """``Conv`` (``R/getRunStatus.R:278-291``), detailed only, for ``nash`` run types with a selected GDX.

    ``s80_bool == 1`` is ``"converged"`` (``"converged (had INFES)"`` with a ``non_optimal.gdx``);
    ``s80_bool == 0`` with ``o_iterationNumber`` at ``cm_iteration_max`` is ``"not_converged"``; otherwise
    the ``modelstat`` digits of ``p80_repy`` over all regions. The ``if`` tests abort as R's do: a missing
    ``s80_bool`` (``argument is of length zero``), a missing ``o_iterationNumber`` or maximum in the second
    test (``missing value where TRUE/FALSE needed``, BUG-038, D-23), a missing ``p80_repy``
    (``incorrect number of dimensions``). ``"NA"`` otherwise.
    """
    if _NASH not in run_type or facts.selected_gdx is None:
        return NA
    iteration = facts.iteration_number
    flag = facts.convergence_flag
    if flag is None:
        raise RunReadError(ARGUMENT_OF_LENGTH_ZERO)
    if math.isnan(flag):
        raise RunReadError(MISSING_VALUE)
    if flag == 1:
        return _CONVERGED_INFES if facts.files.exists(facts.files.non_optimal_gdx) else _CONVERGED
    if flag == 0:
        limit = _as_numeric(maximum)
        if limit is None or iteration is None or math.isnan(limit) or math.isnan(iteration):
            raise RunReadError(MISSING_VALUE)
        if limit == iteration:
            return _NOT_CONVERGED
    report = facts.solve_report
    if report is None:
        raise RunReadError(INCORRECT_DIMENSIONS)
    try:
        return repy_modelstat_text(report)
    except GdxReadError as error:
        raise RunReadError(str(error)) from error


def calibration(facts: RunFacts, run_type: str, iteration: str, conv: str) -> tuple[str, str]:
    """The calibration additions to ``Iter`` and ``Conv`` (``R/getRunStatus.R:296-304``), detailed only.

    For a ``Calib`` run type or ``CES_parameters == "calibrate"`` with ``log.txt``: ``" Clb: <k>"`` is
    appended to ``Iter`` when the last ``CES calibration iteration`` number is positive, and a converged
    run with more than ten ``fulldata_*.gdx`` or ``input_*.gdx`` files becomes ``"Clb_converged"`` (exactly
    ten today, BUG-001, D-09).
    """
    config = facts.config
    calibrating = _CALIB in run_type or (
        config is not None and r_as_character(config.CES_parameters) == _CALIBRATE
    )
    if not (calibrating and facts.files.exists(facts.files.log_txt)):
        return iteration, conv
    digits = facts.calibration_iteration
    number = _as_numeric(digits[0]) if digits else None
    if number is not None and number > 0:
        iteration = f"{iteration} Clb: {digits[0]}"
    if conv in (_CONVERGED, _CONVERGED_INFES) and (
        facts.fulldata_iteration_count > _CALIBRATION_FILES_ABOVE
        or facts.input_iteration_count > _CALIBRATION_FILES_ABOVE
    ):
        conv = _CALIBRATION_CONVERGED
    return iteration, conv


def column_runtime(facts: RunFacts, job: str) -> int | None:
    """``Runtime`` (``R/getRunStatus.R:309-313``), detailed only, in whole seconds (``round(x, 0)``).

    ``timeGAMSEnd - timeGAMSStart`` when ``stats`` has a name matching ``GAMSEnd``; else ``now -
    timePrepareStart`` while ``jobInSLURM`` is not ``"no"`` (so off the cluster, where it is ``"NA"``,
    for every unfinished run); ``None`` (R's ``NA``) otherwise or when a time is missing.
    """
    stats = facts.statistics
    if stats is None:
        return None
    if stats.has_name("GAMSEnd"):
        seconds = facts.gams_wall_seconds
    elif stats.has_name("timePrepareStart") and job != NO_JOB:
        seconds = facts.seconds_since_prepare_start
    else:
        seconds = None
    return None if seconds is None else r_round_seconds(seconds)


def sanity_counts(facts: RunFacts) -> SanityCounts | None:
    """The plausibility block (``R/getRunStatus.R:316-360``), detailed only.

    ``None`` without a configuration file, for a MAgPIE run (by ``runstatistics.rda``) and without
    ``REMIND_generic_<title>.mif`` (the ``next`` of ``:322``). Each count is 0 without its file; the three
    project values are ``NA`` without ``projectSummations.rds`` and abort as R's assignment does when the
    file lacks ``ScenarioMIP`` or one of its elements (``replacement has length zero``).
    """
    config = facts.config
    if config is None or facts.is_magpie:
        return None
    title = config.title
    if not facts.remind_mif_exists(title):
        return None
    summation = facts.summation_error_count(title) if facts.summation_errors_exist(title) else 0
    ranges = facts.range_error_count(title) if facts.range_errors_exist(title) else 0
    fixes = facts.fix_error_count if facts.fix_errors_exist else 0
    project = facts.project_summations
    if project is None:
        return SanityCounts(summation, ranges, fixes, None, None, None)
    block = project.get(_PROJECT_BLOCK)
    return SanityCounts(
        summation,
        ranges,
        fixes,
        _project_count(block, "missingVars"),
        _project_count(block, "checkSummations"),
        _project_count(block, "checkSummationsRegional"),
    )


def _project_count(block: Any, name: str) -> int | None:
    """``tmp[["ScenarioMIP"]][[name]]`` (``R/getRunStatus.R:355-357``) assigned to one cell.

    ``NULL`` (no block, no element) fails the assignment; a vector of another length fails it too; ``NA``
    is ``None``; a whole number is the count.
    """
    value = block.get(name) if isinstance(block, Mapping) else None
    if value is None:
        raise RunReadError(REPLACEMENT_LENGTH_ZERO)
    try:
        element = scalar(value)
    except RDataError as error:
        raise RunReadError(f"projectSummations.rds: ScenarioMIP${name}: {error}") from error
    if element is None:
        return None
    if isinstance(element, bool | int):
        return int(element)
    if isinstance(element, float) and element.is_integer():
        return int(element)
    raise RunReadError(f"projectSummations.rds: ScenarioMIP${name} is not a count: {element!r}")


# ---------------------------------------------------------------------------------------------
# R coercions
# ---------------------------------------------------------------------------------------------


def _is_true_positive(value: RScalar) -> bool:
    """``isTRUE(x > 0)`` for a configuration scalar (``R/getRunStatus.R:142``).

    ``NULL`` and ``NA`` are not TRUE; a character value is compared with ``"0"`` under R's collation
    (``"1" > 0`` is TRUE, as in the fixture configurations where the switch is the string ``"1"``).
    """
    if value is None:
        return False
    if isinstance(value, str):
        return r_collation_key(value) > r_collation_key("0")
    return not math.isnan(float(value)) and float(value) > 0


def _as_numeric(text: str | None) -> float | None:
    """``as.numeric(x)`` of a character scalar: ``None`` for ``NULL``/``character(0)``, ``nan`` for ``NA``."""
    if text is None:
        return None
    try:
        return float(text)
    except ValueError:
        return math.nan
