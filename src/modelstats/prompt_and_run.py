"""The ``rs2`` command: run status of the runs around you, chosen by a few short flags.

R source: R/promptAndRun.R.

Deviations and fixes (details in PORTING-BUGS.md):

- ``-s`` keeps one last iteration per coupling kind; R kept one per stem (BUG-060);
- the "No runs found for this user" texts are warnings on stderr, also when a pattern search fell
  back to them; R returned them and ``Rscript -e`` printed them as ``[1] "..."`` (BUG-061); the
  list of folders ``print()``-ed before the table goes to stderr, one per line (BUG-062); the
  ``-t`` line ``Results from ...`` stays on stdout as R's ``cat()`` wrote it;
- a search pattern that found no folder is used as the user of ``rs2 -c``, as in R, and also as
  the user whose runs are shown (BUG-063); comma lists of flags or patterns work (BUG-078);
  the pattern sees normalised folder paths (BUG-073); an invalid pattern, a ``DAYS`` argument that
  is no whole number up to 99999, and arguments after it end with exit status 1 (BUG-072, BUG-079);
- the folder prompt, ``-d``, ``-p``/``-s``, the current runs and the pattern matches are listed in
  the package's run order (BUG-054, BUG-080); the current-runs logic is shared with ``rs``
  (``cli.current_jobs``), job names that are no pattern are matched as text (BUG-052);
- ``Ctrl-C`` at the prompt ends with exit status 130 (BUG-094).
"""

from __future__ import annotations

import re
import shlex
import sys
from collections.abc import Sequence
from pathlib import Path

from rich.text import Text

from modelstats import _console, _shell
from modelstats._choose import choose_from_list
from modelstats._sorting import is_coupled, last_iterations, last_timestamps, run_sort_key, subdirs
from modelstats._textscan import compile_pattern, r_strsplit
from modelstats.cli import (
    MAGPIE_OUTPUT,
    current_jobs,
    is_run_folder,
    output_folder,
    run_dirs_of_jobs,
    user_path,
)
from modelstats.errors import ModelstatsError
from modelstats.loop_runs import print_run_table
from modelstats.sanity_checks import latest_amt_dirs, results_from_amt

__all__ = ["main", "prompt_and_run"]

_FLAG_WITH_B = re.compile(r"^-.*b.*")
_FLAGS = re.compile(r"^-[a-zA-Z]+")
_CURRENT_FLAGS = ("-cr", "-a", "-c")
_FINISHED_FLAGS = ("-cr", "-c")
_DIGITS = re.compile(r"[0-9]+")
_NUMBER_LIKE = re.compile(r"-?[0-9.]+")  # a second argument that is no user name (names start with a letter)
_MAX_DAYS = 99999


def _is_dir(path: str) -> bool:
    """``dir.exists(path)``: ``""`` is no directory (``Path("")`` would be the current folder)."""
    return path != "" and user_path(path).is_dir()


def _coupled_runs(folders: Sequence[Path], *, last_only: bool) -> list[Path]:
    """The coupled runs (all iterations or the last) and the other runs (all or the latest timestamp)."""
    dirs: list[Path] = []
    for folder in folders:
        entries = subdirs(folder)
        coupled = [entry for entry in entries if is_coupled(entry.name)]
        plain = [entry for entry in entries if not is_coupled(entry.name)]
        if last_only:
            dirs += last_iterations(coupled) + last_timestamps(plain)
        else:
            dirs += coupled + plain
    return sorted(dirs, key=run_sort_key)


def _current_runs(user: str, *, finished: bool, active: bool, days_back: int, color: bool) -> None:
    """``rs2 -c`` / ``-cr`` / ``-a``: the runs of the user's SLURM jobs (``-a``: the active ones only)."""
    jobs = current_jobs(user, days_back if finished else None)
    if not jobs:
        _console.warn(
            "No runs found for this user. You can change the reporting period (here: 5 days) "
            f"by running 'rs2 -c {user} 5"
        )
        return
    run_dirs = run_dirs_of_jobs(jobs)
    if not run_dirs:
        _console.warn(
            "No runs found for this user. To change the reporting period (days) you need to specify "
            "also a user, e.g. rs2 -c USER 1"
        )
        return
    _console.err.print()
    many = days_back == 0 or len(run_dirs) / days_back > 20  # R: n / 0 is Inf
    excuse = " Excuse me? You need a cluster only for yourself it seems." if many else ""
    _console.err.print(Text(f"Found {len(run_dirs)}{' active' if active else ''} runs.{excuse}"))
    for run_dir in run_dirs[:100]:
        _console.err.print(Text(str(run_dir)))
    print_run_table(run_dirs, user=user, color=color, sort_by_time=False)


def _paths_or_patterns(args: Sequence[str], *, color: bool, days_back: int) -> None:
    """``rs2 PATH,...`` / ``rs2 REGEX``: folders by path, by pattern in the output folder, or a user name."""
    given: list[str] = []  # the arguments, a folder inside output/ with its prefix (R: mydir)
    run_dirs: list[Path] = []
    patterns: list[str] = []
    for arg in args:
        if _is_dir(arg):
            given.append(arg)
            run_dirs.append(user_path(arg))
        elif _is_dir(f"output/{arg}"):
            given.append(f"output/{arg}")
            run_dirs.append(user_path(f"output/{arg}"))
        else:
            given.append(arg)
            patterns.append(arg)
    user = ",".join(given)  # R: user <- mydir (BUG-063)
    shown_user = given[0]  # R's foundInSlurm() used the first element of the vector (with a warning)
    if patterns:
        regex = compile_pattern("|".join(patterns), source="rs2")
        found = [entry for entry in subdirs(output_folder(Path())) if regex.search(str(entry))]
        run_dirs += sorted(found, key=run_sort_key)
    if not run_dirs:
        # if mydir yields no results it may be a user name -> try with username
        _console.err.print(Text(f"Did not find any runs for {user}. Looking for runs of user {user}"))
        prompt_and_run("-c", user=user, days_back=days_back)
        return
    print_run_table(run_dirs, user=shown_user, color=color, sort_by_time=False)


def prompt_and_run(paths: str = ".", user: str | None = None, days_back: int = 3) -> None:
    """Prompt for runs and print their status (the ``rs2`` tool).

    Args:
        paths: A comma separated list of folders, a pattern, a user name, or one of the flags
            ``-t`` (latest AMTs), ``-d``/``-f`` (all runs of the output folder, alphabetically / by
            time), ``-p``/``-s`` (coupled runs, all / last iterations), ``-c``/``-cr``/``-a``
            (the user's current runs, with / with / without the finished ones of the last days),
            ``.`` (this run) or nothing (this run, or a choice from the output folder). A ``b`` in a
            flag switches the colours off.
        user: The user whose runs will be shown (paths and patterns are shown as their own user,
            BUG-063); for ``-t``, the pattern used for matching.
        days_back: How many days ``-c`` looks back for finished runs.

    Raises:
        ModelstatsError: An invalid pattern, an unreadable ``runcode.rds`` (``-t``), or a
            ``days_back`` before the beginning of time.
    """
    args = r_strsplit(paths, ",")
    color = not any(_FLAG_WITH_B.match(arg) for arg in args)
    if args == ["-t"]:
        # AMTs: rs2 -t
        _console.out.print(results_from_amt())
        amt_dirs = latest_amt_dirs(user or None, source="rs2 -t")
        print_run_table(amt_dirs, user=None, color=color, sort_by_time=False)
        return
    shown_user = user or _shell.current_user()
    if not color:
        args = [arg.replace("b", "") if _FLAGS.match(arg) else arg for arg in args]
    here = Path()
    match args:
        case ["."]:
            # CURRENT FOLDER: list single run: rs2 .
            print_run_table([here], user=shown_user, color=color)
        case [] | [""]:
            if is_run_folder(here):
                # IN RUN FOLDER: list single run: rs2
                print_run_table([here], user=shown_user, color=color)
            else:
                # IN MAIN FOLDER: prompt list of all runs in "output": rs2
                folder = output_folder(here)
                choices = [folder, *sorted(subdirs(folder), key=run_sort_key)]
                chosen = choose_from_list([str(choice) for choice in choices], kind="folders")
                print_run_table([Path(name) for name in chosen], user=shown_user, sort_by_time=False)
        case ["-d" | "-f" as flag]:
            # IN CURRENT OR IN MAIN FOLDER: list alphabetically (-d) or by time (-f)
            folder = output_folder(here)
            run_dirs = sorted(subdirs(folder), key=run_sort_key)
            print_run_table(run_dirs, user=shown_user, color=color, sort_by_time=flag == "-f")
        case ["-p" | "-s" as flag]:
            # COUPLED RUNS
            folders = [output_folder(here)]
            if flag == "-p" and MAGPIE_OUTPUT.is_dir():
                folders.append(MAGPIE_OUTPUT)
            run_dirs = _coupled_runs(folders, last_only=flag == "-s")
            print_run_table(run_dirs, user=shown_user, color=color, sort_by_time=False)
        case _ if all(arg in _CURRENT_FLAGS for arg in args):
            # CURRENT OR ACTIVE RUNS
            finished = all(arg in _FINISHED_FLAGS for arg in args)
            _current_runs(
                shown_user, finished=finished, active=args == ["-a"], days_back=days_back, color=color
            )
        case _:
            # PATHs or REGEX
            _paths_or_patterns(args, color=color, days_back=days_back)


def _parse_args(args: Sequence[str]) -> tuple[str, str | None, int]:
    """``rs2 [PATHS|FLAG] [USER] [DAYS]``: a number-like second argument is DAYS (the wrapper's rule)."""
    paths, *rest = args or [""]
    user = None
    if rest and not _NUMBER_LIKE.fullmatch(rest[0]):
        user, *rest = rest
    days, *extra = rest or [""]
    if extra:
        raise ModelstatsError(f"too many arguments: {shlex.join(extra)}")
    digits = days.lstrip("0")  # the value counts, not the digits (int() refuses 4300-digit strings)
    if days and not (_DIGITS.fullmatch(days) and len(digits) <= len(str(_MAX_DAYS))):
        raise ModelstatsError(f"DAYS must be a whole number up to {_MAX_DAYS}, not {days!r}")
    return paths, user or None, int(digits or "0") if days else 3


def main(argv: Sequence[str] | None = None) -> int:
    """The ``rs2`` console script."""
    _console.tolerate_undecodable_names()
    try:
        paths, user, days_back = _parse_args(sys.argv[1:] if argv is None else argv)
        prompt_and_run(paths, user=user, days_back=days_back)
    except ModelstatsError as error:
        _console.error(error)
        return 1
    except KeyboardInterrupt:
        return 130
    return 0
