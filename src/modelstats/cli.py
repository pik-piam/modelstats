"""The ``rs`` command: run status or sanity checks of model runs from the command line.

R source: R/commandLineInterface.R.

Deviations and fixes (details in PORTING-BUGS.md):

- ``-m`` adds the run folders *inside* ``magpie/output``; R added the folder ``magpie/output`` itself,
  which its own coupled-run filter then dropped (BUG-049);
- ``-l`` keeps the highest coupling iteration; R kept the alphabetically last folder, so ``-rem-9``
  won over ``-rem-10`` (BUG-050);
- the help text is typer's, not optparse's, and usage errors exit with status 2 (BUG-051);
- a job name that is not a valid regular expression is matched as text in the current-runs logic
  (BUG-052); ``squeue``/``sacct`` are still asked twice, as in R (BUG-053); a failing ``squeue``
  means no current runs (BUG-026);
- the run folders are sorted by the package's run order, not ``stri_order`` or ``list.dirs()``
  (BUG-054, BUG-074); the filter sees normalised paths (BUG-073); ``-A -C`` asks for the current
  user's jobs (BUG-075); ``-p`` numbers the folders from 1 (BUG-076) and ``Ctrl-C`` there exits
  with status 130 (BUG-094); ``-b`` leaves the sanity table's colours as R did (BUG-077);
- an invalid ``-f`` pattern (a Python regular expression, BUG-072) and other errors end with exit
  status 1 and a message on stderr.
"""

from __future__ import annotations

import os
import random
from collections.abc import Sequence
from datetime import timedelta
from pathlib import Path
from typing import Annotated

import typer
from rich.text import Text

from modelstats import _console, _shell, _timefmt
from modelstats._choose import choose_from_list
from modelstats._sorting import is_coupled, last_iterations, run_sort_key, subdirs
from modelstats._textscan import compile_pattern, r_strsplit
from modelstats.errors import ModelstatsError
from modelstats.loop_runs import print_run_table
from modelstats.sanity_checks import print_sanity_checks, read_runcode

__all__ = [
    "HINTS",
    "MAGPIE_OUTPUT",
    "app",
    "current_jobs",
    "current_run_dirs",
    "expand_paths",
    "is_main_folder",
    "is_run_folder",
    "main",
    "output_folder",
    "run_dirs_of_jobs",
    "user_path",
]

HINTS = (
    "Show (l)ast iterations of (m)agpie-coupled runs with: rs -ml",
    "Show all (m)agpie-coupled runs with: rs -m",
    "Show your runs (C)urrently running with: rs -C",
    "Show your (C)urrent runs from the last 5 (d)ays with: rs -C -d 5",
    "Sort runs by (t)ime of last change with: rs -t",
    "List runs found and (p)rompt me to select for which the status should be printed with: rs -p",
    "Show results from specific folders with: rs folder1,folder2",
    "(f)ilter runs by regular expression: rs -f PkBudg500,EU21",
    "Remove the coloring and print the table in (b)lack and white with -b.",
    "Show (C)urrent runs for one or more (u)sers with: rs -C -u user1,user2",
    "Show a (h)elp text with: rs -h",
    "To understand why your pending runs don't start, run: sq -s",
    "To get info about the current run output folder, simply run: rs",
    "List most recent (A)utomated model tests with: rs -A",
    "To get info about a specific AMT scenario, run: rs -A -f SSP2EU-Base",
    "Get a more detailed assessment of a specific run: remindstatus folder",
)

_DESCRIPTION = """\
\b
[PATH] is optional and can be any comma separated combination of individual run folders, the main
folder, or the output folder.
[OPTION] see below.

Description:

\b
Print run status or sanity of model runs.
Flags in capital letters list runs from special locations and ignore any path provided.
Short flags may be bundled together, sharing a single leading -, but only the final short flag is able
to have a corresponding argument.

\b
The following flags only take effect if used together with other flag:
  -u, -d only with -C
  -l     only with -m
The following flags can be combined with all other flags:
  -b, -f, -p, -t

Examples:

\b
1. rs /p/projects/remind/runs/REMIND-MAgPIE-2025-04-24/remind/ -mltpbf PkBudg
   From the given path list coupled runs (m) and only last iteration (l), order by time (t), prompt me
   to select the runs (p), print in black&white (b), filter (f) by 'PkBudg'.

\b
2. rs -As
   Show sanity checks (s) for lastest AMTs (A).

\b
3. rs -C -u alf -d 5
   Show run status of current runs (-C) from the user alf (-u) of the last 5 days (-d 5).
"""
_EPILOGUE = "Bugs and feedback: https://github.com/pik-piam/modelstats/issues"

_HELP = {
    "amt": (
        "print most recent REMIND automated model test runs. With -f: of all AMTs, show only those that "
        "match the regular expression REGEX -f REGEX"
    ),
    "nocolor": "black&white: print table without colors",
    "current": (
        "print currently running runs no matter where they are, i.e. you don't need to be in or right "
        "above a run to see it. Add user with -u USER and include recent runs with -d N"
    ),
    "daysback": "only with -C: show recent runs of the last N days",
    "filter": (
        "print runs that match the regular expression REGEX. Specify multiple REGEX separated by comma."
    ),
    "last": "only with -m: print the last coupling iterations only",
    "magpie": "print all coupling iterations. Add -l to print the last iterations only",
    "prompt": "let the user choose individual runs from a list before printing the status table",
    "sanity": "show overview of sanity check",
    "time": "sort runs chronologically",
    "user": "only with -C: show runs of user USER",
}
_RUN_FILES_REMIND = ("full.gms", "log.txt", "config.Rdata", "prepare_and_run.R", "prepareAndRun.R")
_RUN_FILES_MAGPIE = ("full.gms", "submit.R", "config.yml", "magpie_y1995.gdx")
_MAIN_FILES = ("output", "output.R", "start.R", "main.gms")
MAGPIE_OUTPUT = Path("magpie") / "output"

app = typer.Typer(
    add_completion=False, rich_markup_mode=None, context_settings={"help_option_names": ["-h", "--help"]}
)


def is_run_folder(folder: Path) -> bool:
    """Is this a REMIND or MAgPIE run folder?"""
    remind = sum((folder / name).exists() for name in _RUN_FILES_REMIND)
    magpie = sum((folder / name).exists() for name in _RUN_FILES_MAGPIE)
    return remind >= 4 or magpie == 4


def is_main_folder(folder: Path) -> bool:
    """Is this the REMIND or MAgPIE main folder?"""
    return all((folder / name).exists() for name in _MAIN_FILES)


def output_folder(folder: Path) -> Path:
    """``output`` of a main folder, else the folder itself."""
    return folder / "output" if is_main_folder(folder) else folder


def user_path(text: str) -> Path:
    """A path typed by the user: ``~`` is expanded as R's file functions did (an unknown user stays)."""
    return Path(os.path.expanduser(text))


def expand_paths(paths: Sequence[Path]) -> list[Path]:
    """Run folders for the given paths: the run folder itself, a main folder's ``output``, else subfolders."""
    run_dirs: list[Path] = []
    for folder in paths:
        if is_run_folder(folder):
            run_dirs.append(folder)
        else:
            run_dirs.extend(subdirs(output_folder(folder)))
    return run_dirs


def current_jobs(user: str, days_back: int | None = None) -> list[tuple[str, str | None]]:
    """The (work dir, job name) pairs of the user's jobs in SLURM, plus the finished ones.

    ``days_back`` (not ``None``) adds the jobs that ended in the last days (``sacct``). ``batch``
    jobs are dropped, and ``default`` ones while a ``mag-run`` job exists.
    """
    work_dirs = _shell.squeue("%Z", user=user)
    job_names = _shell.squeue("%j", user=user)
    if days_back is not None:
        try:
            since = _timefmt.today() - timedelta(days=days_back)
        except OverflowError:
            raise ModelstatsError(f"{days_back} days back is before the beginning of time") from None
        work_dirs += _shell.sacct(user, since, "WorkDir")
        job_names += _shell.sacct(user, since, "JobName")
    dropped = {"default", "batch"} if any("mag-run" in name for name in job_names) else {"batch"}
    jobs: list[tuple[str, str | None]] = [
        (work, name) for work, name in zip(work_dirs, job_names, strict=False) if name not in dropped
    ]
    # R indexed the work dirs by the job names: extra work dirs stayed, extra names stopped R (BUG-053)
    jobs += [(work, None) for work in work_dirs[len(job_names) :]]
    return jobs


def _is_run_dir(work_dir: str, job_name: str | None) -> bool:
    """``grepl(runnames[[i]], myruns[[i]])`` (the job name as a pattern, BUG-052) or a ``mag-run`` job.

    An unpaired work dir (``None``) stays as it is (BUG-053).
    """
    if job_name is None or "mag-run" in job_name:
        return True
    try:
        pattern = compile_pattern(job_name, source="job name")
    except ModelstatsError:  # no valid pattern: matched as text (BUG-052)
        return job_name in work_dir
    return pattern.search(work_dir) is not None


def run_dirs_of_jobs(jobs: Sequence[tuple[str, str | None]]) -> list[Path]:
    """The existing run folders of SLURM jobs, in run order.

    A job whose work dir is not its run folder is replaced by ``<work dir>/output/<job name>``, the
    run folder of a coupled run; for other such jobs (preprocessing, ...) that folder does not exist
    and they drop out.
    """
    runs = {work if _is_run_dir(work, name) else f"{work}/output/{name}" for work, name in jobs}
    existing = sorted(run for run in runs if run and Path(run).exists())  # R: sort(unique(myruns))
    return sorted((Path(run) for run in existing), key=run_sort_key)


def current_run_dirs(user: str, days_back: int | None = None) -> list[Path]:
    """The run folders of the user's jobs in SLURM, plus the finished ones of the last ``days_back`` days."""
    return run_dirs_of_jobs(current_jobs(user, days_back))


def _file_list(paths: Sequence[Path]) -> Text:
    """``{.file {paths}}`` of ``cli_alert_info``: blue paths on a terminal, quoted otherwise."""
    terminal = _console.err.color_system is not None and not _console.err.no_color
    shown = Text()
    for position, folder in enumerate(paths):
        if position:
            shown.append(" and ")
        name = f"{folder}/" if folder == _shell.AMT_OUTPUT else str(folder)
        if terminal:
            shown.append(name, style="blue")
        else:
            shown.append(f"'{name}'")
    return shown


def _hint() -> str:
    """``sample(hints, 1)``."""
    return random.choice(HINTS)


def _news_and_hint() -> None:
    _console.info("Update 1: The underline has been removed for converged runs (green).")
    _console.info(
        "Update 2: Runs that showed INFES but finally converged are now displayed in the same way "
        "(now green, previously blue)."
    )
    _console.info(f"Did you know? {_hint()}")


@app.command(help=_DESCRIPTION, epilog=_EPILOGUE)
def rs(
    path: Annotated[str | None, typer.Argument(metavar="[PATH]", show_default=False)] = None,
    amt: Annotated[bool, typer.Option("-A", "--amt", help=_HELP["amt"])] = False,
    nocolor: Annotated[bool, typer.Option("-b", "--nocolor", help=_HELP["nocolor"])] = False,
    current: Annotated[bool, typer.Option("-C", "--current", help=_HELP["current"])] = False,
    days_back: Annotated[
        int, typer.Option("-d", "--daysback", metavar="N", help=_HELP["daysback"], show_default=False)
    ] = 0,
    filter_: Annotated[
        str, typer.Option("-f", "--filter", metavar="REGEX", help=_HELP["filter"], show_default=False)
    ] = ".*",
    last: Annotated[bool, typer.Option("-l", "--last", help=_HELP["last"])] = False,
    magpie: Annotated[bool, typer.Option("-m", "--magpie", help=_HELP["magpie"])] = False,
    prompt: Annotated[bool, typer.Option("-p", "--prompt", help=_HELP["prompt"])] = False,
    sanity: Annotated[bool, typer.Option("-s", "--sanity", help=_HELP["sanity"])] = False,
    time: Annotated[bool, typer.Option("-t", "--time", help=_HELP["time"])] = False,
    user: Annotated[
        str, typer.Option("-u", "--user", metavar="USER", help=_HELP["user"], show_default=False)
    ] = "you",
) -> None:
    """Print run status or sanity of model runs."""
    _console.tolerate_undecodable_names()
    try:
        _rs(
            path,
            amt=amt,
            nocolor=nocolor,
            current=current,
            days_back=days_back,
            filter_=filter_,
            last=last,
            magpie=magpie,
            prompt=prompt,
            sanity=sanity,
            time=time,
            user=user,
        )
    except ModelstatsError as error:
        _console.error(error)
        raise typer.Exit(1) from error


def _rs(
    path: str | None,
    *,
    amt: bool,
    nocolor: bool,
    current: bool,
    days_back: int,
    filter_: str,
    last: bool,
    magpie: bool,
    prompt: bool,
    sanity: bool,
    time: bool,
    user: str,
) -> None:
    """The main decision tree: evaluate the command line arguments and decide what to do."""
    _news_and_hint()
    shown_user: str | None = _shell.current_user() if user in ("you", "") else user  # BUG-075
    given = r_strsplit("." if path is None else path, ",")
    paths = [user_path(p) for p in given if p != ""]
    pattern = "|".join(r_strsplit(filter_, ","))

    if amt:
        # AMT runs: the AMT path and the regular expression from 'runcode.rds' select the latest AMTs
        shown_user = None
        paths = [_shell.AMT_OUTPUT]
        if pattern == ".*":
            pattern = read_runcode()
        else:
            paths.append(_shell.AMT_OUTPUT / "archive")
        _console.info(Text.assemble("Results from ", _file_list(paths)))

    if current:
        # OPTION A: get current runs
        run_dirs = current_run_dirs(shown_user or _shell.current_user(), days_back if days_back > 0 else None)
        if not run_dirs:
            if days_back < 1:
                _console.warn(
                    "No currently running runs found. To include recent runs please expand the time "
                    "horizon by adding -d DAYS."
                )
            else:
                _console.warn(f"No runs found in the past {days_back} days. Try to expand the time horizon.")
            raise typer.Exit(0)
    else:
        # OPTION B: create list with run folders from path supplied by user or AMTs
        run_dirs = expand_paths(paths)

    if magpie and (current or amt or given):  # R: opt$magpie & !is.null(runfolders)
        # add magpie run folders if 'magpie/output' exists inside current folder
        if MAGPIE_OUTPUT.is_dir():
            listed = {d.resolve() for d in run_dirs}
            run_dirs += [d for d in subdirs(MAGPIE_OUTPUT) if d.resolve() not in listed]  # BUG-049: once
        run_dirs = [d for d in run_dirs if d.name.startswith("C_") or is_coupled(d.name)]
        if last:
            run_dirs = last_iterations(run_dirs)
            if not run_dirs:
                _console.warn("No coupled runs found")
                raise typer.Exit(0)

    regex = compile_pattern(pattern, source="-f")
    run_dirs = [run_dir for run_dir in run_dirs if regex.search(str(run_dir))]
    run_dirs = sorted(run_dirs, key=run_sort_key)
    if not run_dirs:
        _console.warn("No runs found")
        return
    if len(run_dirs) > 40 and pattern == ".*" and not prompt:
        _console.info(
            "To reduce the number of runs, filter the runs with -f REGEX or select manually from the list "
            "with -p."
        )
    if prompt:
        chosen = choose_from_list([str(run_dir) for run_dir in run_dirs], kind="folders")
        run_dirs = [Path(name) for name in chosen]
    _console.info(f"Runs found: {len(run_dirs)}")
    if sanity:
        print_sanity_checks(run_dirs)  # R's getSanityChecks() ignored -b (BUG-077)
    else:
        print_run_table(run_dirs, user=shown_user, color=not nocolor, sort_by_time=time)


def main(argv: Sequence[str] | None = None) -> int:
    """Run ``rs`` with the given arguments and return its exit status (for tests and scripts)."""
    return _console.run_app(app, argv, prog_name="rs")
