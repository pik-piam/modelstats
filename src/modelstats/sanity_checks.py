"""The ``rs -s`` table: summation, range, fixing and project-summation checks of REMIND runs.

R source: R/getSanityChecks.R.

Deviations and fixes (details in PORTING-BUGS.md):

- the rows are joined with ``format_row``'s default separator of three spaces while the title
  line uses two, as in R (BUG-036, open);
- the latest AMT folders are those of the cluster output folder whose name matches the pattern
  in ``runcode.rds``, in the package's run order rather than the locale's collation (BUG-054); a
  missing or unreadable output folder yields no folders, as R's ``dir()`` did;
- a missing run folder raises ``ModelstatsError`` (R stopped with its own message); so do an
  unusable ``runcode.rds`` and an invalid pattern, which is a Python regular expression (BUG-072);
  a run whose status fails still aborts the table after the rows printed so far, as in R (BUG-055).
"""

from __future__ import annotations

from collections.abc import Sequence
from pathlib import Path

from rich.console import Console
from rich.text import Text

from modelstats import _console, _shell
from modelstats._rdata import read_rds_string
from modelstats._sorting import run_sort_key
from modelstats._textscan import compile_pattern
from modelstats.errors import ModelstatsError
from modelstats.print_output import format_row
from modelstats.run_status import get_run_status

__all__ = [
    "EXPLANATION_URL",
    "SANITY_COLUMNS",
    "SANITY_TITLES",
    "latest_amt_dirs",
    "print_sanity_checks",
    "read_runcode",
    "results_from_amt",
]

SANITY_COLUMNS = (
    "summation_errors",
    "range_errors",
    "fix_errors",
    "missing_proj_vars",
    "proj_summation_errors",
    "proj_summation_errors_regional",
)
SANITY_TITLES = ("SumErr", "RangeErr", "FixingErr", "MissingVar", "ProjSumErr", "ProjSumErrReg")
EXPLANATION_URL = (
    "https://github.com/remindmodel/remind/blob/develop/tutorials/05_AnalysingModelOutputs.md"
    "#7-visualizing-run-status-and-summation-checks-for-runs"
)
_TITLE_SEPARATOR = "  "


def read_runcode(path: Path | None = None) -> str:
    """The regular expression of the latest AMT runs, saved by ``modeltests`` in ``runcode.rds``.

    Raises:
        ModelstatsError: The file does not hold one string, or the pattern is invalid.
    """
    runcode_rds = path or _shell.AMT_RUNCODE
    runcode = read_rds_string(runcode_rds)
    compile_pattern(runcode, source=str(runcode_rds))
    return runcode


def results_from_amt() -> Text:
    """The ``Results from <AMT output>/ `` line of the AMT sanity table and of ``rs2 -t``.

    R's ``cat()`` wrote it to stdout; it is part of the AMT Mattermost message.
    """
    return Text(f"Results from {_shell.AMT_OUTPUT}/ ")


def latest_amt_dirs(pattern: str | None = None, *, source: str = "pattern") -> list[Path]:
    """The entries of the cluster AMT output folder whose names match ``pattern`` (default: ``runcode.rds``).

    Like R's ``dir()`` this lists files as well as folders and skips dot-entries; an output
    folder that does not exist or cannot be read yields nothing. The pattern is a Python regular
    expression (the runcode written by ``modeltests`` is one); POSIX classes such as
    ``[[:digit:]]`` are rejected (BUG-072). ``source`` names the pattern's origin in the error of
    an invalid one.
    """
    regex = compile_pattern(read_runcode() if pattern is None else pattern, source=source)
    try:
        entries = [entry for entry in _shell.AMT_OUTPUT.iterdir() if not entry.name.startswith(".")]
    except OSError:  # missing, or not readable: dir() gave character(0)
        return []
    return sorted((entry for entry in entries if regex.search(entry.name)), key=run_sort_key)


def print_sanity_checks(dirs: Sequence[Path] | None = None, *, out: Console | None = None) -> None:
    """Print the overview of the sanity check results of REMIND runs.

    The columns are: ``SumErr`` summation errors found when running ``remind2::convGDX2MIF``,
    ``RangeErr`` range errors found when running ``remind2::convGDX2MIF``, ``FixingErr`` fixing
    errors found when running ``piamInterfaces::fixOnRef``, ``MissingVar`` missing variables found
    when running ``piamInterfaces::checkMissingVars`` for ScenarioMIP, ``ProjSumErr`` summation
    errors found when running ``piamInterfaces::checkSummations`` for ScenarioMIP, and
    ``ProjSumErrReg`` regional summation errors found when running
    ``piamInterfaces::checkSummationsRegional`` for ScenarioMIP. Only runs whose status holds these
    columns (REMIND runs with a configuration and a mif) get a row.

    Args:
        dirs: The run folders. ``None``: the latest AMTs (only works on the PIK cluster), after a
            ``Results from`` line on ``out`` - that line is part of the AMT Mattermost message.
        out: The console to print to (default: stdout).

    Raises:
        ModelstatsError: One of the folders does not exist, ``runcode.rds`` is unusable, or the
            status of a run cannot be read (the rows before it have been printed).
        OSError: A file of a run cannot be opened.
    """
    console = out or _console.out
    if dirs is None:
        console.print(results_from_amt())
        dirs = latest_amt_dirs()
    names = [_folder_name(path) for path in dirs]
    first_width = min(67, max([15, *(len(name) for name in names)]))
    titles = ["Folder".ljust(first_width), *SANITY_TITLES]
    widths = [len(title) for title in titles]
    console.print()
    console.print(_console.styled(f"For column explanations see: {EXPLANATION_URL}", "cyan"))
    console.print(Text.assemble((_TITLE_SEPARATOR.join(titles), "underline"), " "))
    console.print()
    for run_dir in dirs:
        for row in get_run_status(run_dir):
            # R: all(cols %in% names(status)); the plausibility block sets the six columns together,
            # summation_errors as a number whenever the run has a mif
            if row.summation_errors is not None:
                console.print(Text(format_row(row, widths, cols=SANITY_COLUMNS)))


def _folder_name(path: Path) -> str:
    """``basename(normalizePath(path, mustWork = TRUE))``."""
    try:
        return path.resolve(strict=True).name
    except OSError as error:
        raise ModelstatsError(f"{path}: {error.strerror}") from error
