"""modelstats: run analysis tools for REMIND and MAgPIE model runs.

Python port of the R package modelstats (same version number). The public
functions ``get_run_status``, ``loop_runs``, ``found_in_slurm``,
``col_run_type``, ``print_output`` and ``get_sanity_checks`` are exported here
once their modules are ported; see ``migration/03-migration-plan.md``.
"""

from modelstats.formatting import print_output
from modelstats.run_type import col_run_type

__version__ = "0.31.0"

__all__ = ["__version__", "col_run_type", "print_output"]
