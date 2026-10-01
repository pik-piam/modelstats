"""modelstats: run analysis tools for REMIND and MAgPIE model runs.

Python port of the R package modelstats (same version number). The public
functions ``get_run_status``, ``loop_runs``, ``found_in_slurm``, ``col_run_type``,
``print_output`` and ``get_sanity_checks`` are exported here (see
``migration/03-migration-plan.md``).
"""

from modelstats.formatting import print_output
from modelstats.loop_runs import loop_runs
from modelstats.run_status import RunStatus, StatusTable, get_run_status
from modelstats.run_type import col_run_type
from modelstats.sanity import get_sanity_checks
from modelstats.slurm import found_in_slurm

__version__ = "0.31.0"

__all__ = [
    "RunStatus",
    "StatusTable",
    "__version__",
    "col_run_type",
    "found_in_slurm",
    "get_run_status",
    "get_sanity_checks",
    "loop_runs",
    "print_output",
]
