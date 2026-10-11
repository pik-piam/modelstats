"""modelstats: run analysis tools for REMIND and MAgPIE.

The Python port of the R package of the same name (``DESCRIPTION``, version mirrored in
``__version__``). The package grows phase by phase (``migration/06-user-requests.md`` section 7):
the pure helpers (phase 1), the run inspection layer and the public ``Run`` object (phase 2), the
tables (phase 3), the ``rs`` command line (phase 4) and the REMIND-only automated model tests
(phase 5).

Public API (``06`` section 5.1)::

    from modelstats import Run, RunState, Convergence

    run = Run("/p/projects/remind/modeltests/remind/output/SSP2-NPi-AMT_2026-09-28_10.30.27")
    run.status, run.convergence, run.iterations, run.warnings, run.summation_errors

``get_run_status``, ``found_in_slurm``, ``loop_runs`` and ``get_sanity_checks`` are the parity functions
behind ``rs`` (``R/getRunStatus.R``, ``R/foundInSlurm.R``, ``R/loopRuns.R``, ``R/getSanityChecks.R``).
"""

from modelstats.errors import ModelstatsError, RunReadError
from modelstats.loop_runs import LoopRunsError, loop_runs
from modelstats.run import (
    Convergence,
    Iterations,
    MifState,
    ModelStat,
    ProjectSummation,
    Run,
    RunState,
    iter_runs,
)
from modelstats.run_status import RunStatus, SanityCounts, get_run_status
from modelstats.sanity import SanityError, get_sanity_checks
from modelstats.slurm import SlurmJob, found_in_slurm

__version__ = "0.31.0"

__all__ = [
    "Convergence",
    "Iterations",
    "LoopRunsError",
    "MifState",
    "ModelStat",
    "ModelstatsError",
    "ProjectSummation",
    "Run",
    "RunReadError",
    "RunState",
    "RunStatus",
    "SanityCounts",
    "SanityError",
    "SlurmJob",
    "__version__",
    "found_in_slurm",
    "get_run_status",
    "get_sanity_checks",
    "iter_runs",
    "loop_runs",
]
