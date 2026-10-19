"""Run analysis tools for REMIND and MAgPIE (Python port of the R package modelstats).

R source: R/modelstats-package.R. The public names of the ported modules are re-exported here
as they land (PORTING.md section 2).
"""

from __future__ import annotations

from modelstats.errors import ConfigReadError, GdxReadError, ModelstatsError, RDataReadError
from modelstats.found_in_slurm import SqueueJob, found_in_slurm
from modelstats.loop_runs import print_run_table, runtime_cell
from modelstats.modeltests import modeltests
from modelstats.print_output import TableRow, default_columns, format_row
from modelstats.prompt_and_run import prompt_and_run
from modelstats.run_status import EXPLAIN_MODELSTAT, RunStatus, get_run_status, to_dataframe
from modelstats.run_type import run_type
from modelstats.sanity_checks import SANITY_COLUMNS, print_sanity_checks

__version__ = "0.31.0"

__all__ = [
    "EXPLAIN_MODELSTAT",
    "SANITY_COLUMNS",
    "ConfigReadError",
    "GdxReadError",
    "ModelstatsError",
    "RDataReadError",
    "RunStatus",
    "SqueueJob",
    "TableRow",
    "__version__",
    "default_columns",
    "format_row",
    "found_in_slurm",
    "get_run_status",
    "modeltests",
    "print_run_table",
    "print_sanity_checks",
    "prompt_and_run",
    "run_type",
    "runtime_cell",
    "to_dataframe",
]
