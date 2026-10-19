"""The ``rs`` status table: one coloured line per run.

R source: R/loopRuns.R.

Deviations and fixes (details in PORTING-BUGS.md):

- the "skipped because of error" test comes before the "skipped." test, which read the failed
  status in R and crashed (BUG-040); the error itself is reported on stderr as R did;
- an empty list of folders prints nothing (R returned the string ``"No runs found"``, which a
  top-level caller saw printed) and the ``"exit"`` sentinel of rs2 does not exist (BUG-041); paths
  that are not directories are left out (BUG-010);
- the "looks like a run" test wants a file named exactly ``log.txt`` (BUG-057); the "skipped."
  line prints the normalised path (BUG-058); a run twice in the list prints twice (BUG-010).
"""

from __future__ import annotations

import re
from collections.abc import Sequence
from pathlib import Path

from rich.console import Console
from rich.text import Text

from modelstats import _console, _shell
from modelstats._console import styled
from modelstats._sorting import existing_dirs
from modelstats._timefmt import format_runtime
from modelstats.errors import ModelstatsError
from modelstats.print_output import default_columns, format_row
from modelstats.run_status import RunStatus, get_run_status

__all__ = ["print_run_table", "runtime_cell"]

COLUMN_SEPARATOR = "  "
_CLUSTER_TITLES = (
    "Runtime    ",
    "inSlurm ",
    "RunType    ",
    "RunStatus        ",
    "Warnings ",
    "Iter            ",
    "Conv                 ",
    "modelstat            ",
    "Mif   ",
    "AppResults",
)
_LOCAL_TITLES = (
    "Runtime    ",
    "RunType    ",
    "RunStatus        ",
    "Warnings ",
    "Iter            ",
    "Conv                 ",
    "modelstat          ",
    "Mif   ",
)
_REMIND_ERRORS = re.compile("not_converged|Execution erro|Compilation er|interrupted|Intermed Infes")
_MAGPIE_ERRORS = re.compile("not_converged|Execution erro|Compilation er|missing|interrupted|Abort")
_CONVERGED = ("converged", "Clb_converged", "converged (had INFES)")


def runtime_cell(row: RunStatus) -> str:
    """The Runtime cell of a run.

    Args:
        row: The status of the run.

    Returns:
        ``pending``/``startup`` from the slurm state, else the formatted seconds, prefixed with
        ``>`` (and a space when shorter than ten characters) while the job is in SLURM; ``NA``
        without a runtime.
    """
    if row.job_in_slurm.endswith("pending"):
        return "pending"
    if row.runtime is not None:
        text = format_runtime(row.runtime)
        if row.job_in_slurm != "no":
            text = ">" + (" " if len(text) < 10 else "") + text
        return text
    if row.job_in_slurm.endswith("startup"):
        return "startup"
    return "NA"


def _legend() -> Text:
    """``# Color code:`` line of the table."""
    return Text.assemble(
        "# Color code: ",
        styled("pending", "yellow"),
        "/",
        styled("startup", "yellow"),
        ", ",
        styled("running", "cyan"),
        ", ",
        styled("converged", "green"),
        "/",
        styled("finished", "green"),
        ", ",
        styled("no mif", "orange"),
        ", ",
        styled("conopt stalled?", "magenta"),
        ", ",
        styled("error", "orangered"),
        ".",
    )


def _layout(run_dirs: Sequence[Path], on_cluster: bool) -> tuple[str, list[int]]:
    """The header line and the column widths for these folders."""
    longest = max([15, *(len(path.resolve().name) for path in run_dirs)])
    first_width = min(67, longest)
    titles = ["Folder".ljust(first_width), *(_CLUSTER_TITLES if on_cluster else _LOCAL_TITLES)]
    widths = [len(title) for title in titles]
    if on_cluster:
        widths[-1] = 3
    return COLUMN_SEPARATOR.join(titles), widths


_LOOKS_LIKE_RUN = re.compile(r"^config.*|^log\.txt$")


def _has_config_or_log(run_dir: Path) -> bool:
    """Does the folder look like a run: its first ``config*``/``log.txt`` entry exists (not a broken link)?"""
    try:
        names = sorted(entry.name for entry in run_dir.iterdir() if not entry.name.startswith("."))
    except OSError:
        return False
    first = next((name for name in names if _LOOKS_LIKE_RUN.search(name)), None)
    return first is not None and (run_dir / first).exists()


def _is_magpie(row: RunStatus, run_type: str) -> bool:
    return re.match("^y[12]", row.iter) is not None or run_type.startswith("nlp_")


def _remind_style(row: RunStatus, runtime: str, in_slurm: str, line: str) -> str:
    """R's if-chain for REMIND rows: the first match wins, the order matters."""
    if runtime in ("pending", "startup"):
        return "yellow"
    if "conoptspy >" in line:
        return "magenta"
    if in_slurm not in ("no", "NA"):
        return "cyan"
    if row.conv == "converged (had INFES)" and row.mif != "no":
        return "green"
    if _REMIND_ERRORS.search(row.run_status):
        return "orangered"
    if row.conv in ("converged", "Clb_converged") and row.mif != "no":
        return "green"
    if "2: Locally Optimal" in row.modelstat and "nash" not in row.run_type:
        return "green"
    if row.mif == "no" and row.conv in _CONVERGED:
        return "orange"
    if in_slurm == "no":
        return "orangered"
    return "cyan"


def _magpie_style(row: RunStatus, runtime: str, line: str) -> str | None:
    """R's if-chain for MAgPIE rows: the first match wins, the order matters; ``None`` is plain."""
    if runtime == "pending":
        return "yellow"
    if _MAGPIE_ERRORS.search(row.run_status):
        return "orangered"
    if re.search("converged|Clb_converged", row.run_status):
        return "green"
    if ("222" in row.modelstat and "." not in row.modelstat) or row.modelstat == "2: Locally Optimal":
        return "green"
    if "conoptspy >" in line:
        return "magenta"
    if "Run in progress" in line:
        return "cyan"
    if " NA " in line and "FALSE" in line:
        return "orangered"
    return None


def _print_row(
    row: RunStatus, widths: Sequence[int], columns: Sequence[str], color: bool, console: Console
) -> None:
    runtime = runtime_cell(row)
    in_slurm = re.sub(r" *startup$| *pending$", "", row.job_in_slurm)
    run_type = row.run_type.replace("testOneRegi", "1Regi")
    cells = {"runtime": runtime, "job_in_slurm": in_slurm, "run_type": run_type}
    line = format_row(row, widths, COLUMN_SEPARATOR, columns, cells=cells).rstrip(" ")
    style = None
    if color:
        if _is_magpie(row, run_type):
            style = _magpie_style(row, runtime, line)
        else:
            style = _remind_style(row, runtime, in_slurm, line)
    console.print(styled(line, style))


def print_run_table(
    dirs: Sequence[Path],
    user: str | None = None,
    color: bool = True,
    sort_by_time: bool = True,
    *,
    out: Console | None = None,
) -> None:
    """Print the status table of the given run folders.

    Args:
        dirs: The run folders; paths that are not directories are left out.
        user: The user whose runs will be shown (default: the current user).
        color: Colour the lines by status and print the colour legend.
        sort_by_time: Order the folders by modification time, newest first; else as given.
        out: The console to print to (default: stdout).
    """
    console = out or _console.out
    if user is None:
        user = _shell.current_user()
    if not dirs:
        return
    run_dirs = existing_dirs(dirs, newest_first=sort_by_time)
    if color:
        console.print(_legend())
        console.print()
    on_cluster = _shell.on_cluster()
    header, widths = _layout(run_dirs, on_cluster)
    columns = default_columns(on_cluster)
    console.print(Text.assemble((header, "underline"), " "))
    for run_dir in run_dirs:
        try:
            rows = get_run_status(run_dir, user=user)
        except (ModelstatsError, OSError) as error:
            _console.err.print(Text(f"Error in {run_dir}: {error}"))
            console.print(Text(f"{run_dir.name or run_dir} skipped because of error"))
            continue
        if not rows:
            continue
        row = rows[0]
        if not _has_config_or_log(run_dir) and row.job_in_slurm == "no":
            console.print(Text(f"{run_dir} skipped."))
            continue
        _print_row(row, widths, columns, color, console)
