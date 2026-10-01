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

``get_run_status`` and ``found_in_slurm`` are the parity functions behind ``rs`` (``R/getRunStatus.R``,
``R/foundInSlurm.R``); ``loop_runs`` joins them in phase 3.
"""

from modelstats.errors import ModelstatsError, RunReadError
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
from modelstats.slurm import SlurmJob, found_in_slurm

__version__ = "0.31.0"

__all__ = [
    "Convergence",
    "Iterations",
    "MifState",
    "ModelStat",
    "ModelstatsError",
    "ProjectSummation",
    "Run",
    "RunReadError",
    "RunState",
    "RunStatus",
    "SanityCounts",
    "SlurmJob",
    "__version__",
    "found_in_slurm",
    "get_run_status",
    "iter_runs",
]
