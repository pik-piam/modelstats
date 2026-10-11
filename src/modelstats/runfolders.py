"""Finding, filtering and ordering run folders: the path logic of ``R/commandLineInterface.R``.

Parity port of the helpers the ``rs`` command line applies before it prints a table
(``migration/03-migration-plan.md`` section 3.4): the run-folder and main-folder tests, the expansion
of the positional argument into run folders, the ``-m`` filter for coupled runs, the ``-l`` reduction to
the last coupling iteration and the final natural ordering. Strings are kept exactly as R composes
them (``"./name"``, ``"dir//name"`` after a trailing slash), because the ``-f`` regular expression is
later matched against the whole string and the prompt menu prints it.

Register bugs reproduced on purpose (``migration/04-bug-register.md``): BUG-025 (``-l`` keeps the last
entry in listing order, not the highest iteration number) and BUG-026 (``-m`` appends ``magpie/output``
and filters it away again). Every look at the filesystem goes through :class:`modelstats.env.Effects`,
whose ``list_dirs_like_r`` and ``normalize_path`` reproduce ``list.dirs()`` and ``normalizePath()``.
Sorting follows decision D-14: ``natsort`` with the fixed ``ns.DEFAULT`` flags stands in for
``stringi::stri_order(numeric = TRUE)`` under the cluster locale (``migration/02-python-libraries.md``
section 4.4); the goldens under ``migration/goldens/sort`` pin it.
"""

from __future__ import annotations

import os
import re
from collections.abc import Iterable, Sequence
from typing import Final

from natsort import index_natsorted, ns

from modelstats.env import Effects

__all__ = [
    "COUPLED_RUN_NAME",
    "DEFAULT_PATH",
    "ITERATION_SUFFIX",
    "MAGPIE_RUN_MARKERS",
    "MAIN_FOLDER_MARKERS",
    "NATURAL_SORT_FLAGS",
    "REMIND_RUN_MARKERS",
    "expand_paths",
    "filter_coupled",
    "is_main_folder",
    "is_run_folder",
    "last_iterations",
    "natural_order",
    "natural_order_indices",
    "natural_sorted",
    "r_basename",
    "r_dirname",
    "split_comma_list",
]

REMIND_RUN_MARKERS: Final = ("full.gms", "log.txt", "config.Rdata", "prepare_and_run.R", "prepareAndRun.R")
"""``R/commandLineInterface.R:23``: at least four of these make a REMIND run folder."""

MAGPIE_RUN_MARKERS: Final = ("full.gms", "submit.R", "config.yml", "magpie_y1995.gdx")
"""``R/commandLineInterface.R:24``: all four make a MAgPIE run folder."""

MAIN_FOLDER_MARKERS: Final = ("output", "output.R", "start.R", "main.gms")
"""``R/commandLineInterface.R:30``: all four make a model main folder."""

COUPLED_RUN_NAME: Final = re.compile(r"(^C_)|(-(rem|mag)-[0-9]+$)")
"""``R/commandLineInterface.R:212``: a coupled run, by its basename."""

ITERATION_SUFFIX: Final = re.compile(r"-(rem|mag)-[0-9]+$")
"""``R/commandLineInterface.R:218-219``: the iteration suffix removed to group the iterations of one run."""

NATURAL_SORT_FLAGS: Final = ns.DEFAULT
"""Decision D-14: the one flag set standing in for ``stri_order(numeric = TRUE)`` (case-sensitive)."""

DEFAULT_PATH: Final = "."
"""``R/commandLineInterface.R:112``: the positional argument defaults to the working directory."""

_MAGPIE_OUTPUT: Final = "magpie/output"
"""``R/commandLineInterface.R:209``: ``file.path("magpie", "output")``, relative to the working directory."""


def is_run_folder(directory: str, env: Effects) -> bool:
    """``is.runfolder(dir)`` (``R/commandLineInterface.R:22-26``).

    ``file.exists`` is true for directories too, so a sub-directory named like a marker counts.
    """
    remind = sum(env.exists(f"{directory}/{name}") for name in REMIND_RUN_MARKERS)
    magpie = sum(env.exists(f"{directory}/{name}") for name in MAGPIE_RUN_MARKERS)
    return remind >= 4 or magpie == 4


def is_main_folder(directory: str, env: Effects) -> bool:
    """``is.mainfolder(dir)`` (``R/commandLineInterface.R:29-31``)."""
    return all(env.exists(f"{directory}/{name}") for name in MAIN_FOLDER_MARKERS)


def split_comma_list(text: str) -> list[str]:
    """``strsplit(text, ",")[[1]]`` (``R/commandLineInterface.R:115-116``).

    R drops exactly one trailing empty piece and gives no piece at all for the empty string:
    ``"a,b,"`` is ``["a", "b"]``, ``"a,,"`` is ``["a", ""]``, ``",a"`` is ``["", "a"]`` and ``""`` is ``[]``.
    """
    if text == "":
        return []
    pieces = text.split(",")
    if pieces[-1] == "":
        pieces.pop()
    return pieces


def expand_paths(positional: str | None, env: Effects) -> list[str]:
    """Run folders for the ``rs`` positional argument (``R/commandLineInterface.R:112-115,192-202``).

    No argument means ``"."``; the argument is a comma-separated list. A run folder is kept as given; a
    main folder contributes the directories below its ``output``; anything else contributes its own
    sub-directories, which is nothing for a missing path (BUG-027: a run that failed before ``full.gms``
    existed falls into this branch).
    """
    runfolders: list[str] = []
    for directory in split_comma_list(DEFAULT_PATH if positional is None else positional):
        if is_run_folder(directory, env):
            runfolders.append(directory)
        elif is_main_folder(directory, env):
            runfolders.extend(env.list_dirs_like_r(f"{directory}/output"))  # file.path(dir, "output")
        else:
            runfolders.extend(env.list_dirs_like_r(directory))
    return runfolders


def filter_coupled(runfolders: Iterable[str], env: Effects) -> list[str]:
    """The ``-m`` filter (``R/commandLineInterface.R:208-213``).

    When ``magpie/output`` exists below the working directory it is appended, relative as in R, and then
    removed again by the basename test because ``output`` is not a coupled run name (BUG-026). Kept are
    the folders whose basename starts with ``C_`` or ends in ``-rem-N`` / ``-mag-N``.
    """
    folders = list(runfolders)
    if env.is_dir(_MAGPIE_OUTPUT):
        folders.append(_MAGPIE_OUTPUT)
    return [folder for folder in folders if COUPLED_RUN_NAME.search(r_basename(folder))]


def last_iterations(runfolders: Sequence[str]) -> list[str]:
    """The ``-l`` reduction (``R/commandLineInterface.R:216-222``).

    For every run (the path without its iteration suffix, in order of first appearance) the entry at the
    largest list position is kept: the listing order decides, not the iteration number, so
    ``rem-1, rem-10, rem-2`` keeps ``rem-2`` (BUG-025).
    """
    prefixes = [ITERATION_SUFFIX.sub("", folder) for folder in runfolders]
    kept: list[str] = []
    for prefix in dict.fromkeys(prefixes):
        last_position = max(index for index, candidate in enumerate(prefixes) if candidate == prefix)
        kept.append(runfolders[last_position])
    return kept


def natural_order(runfolders: Sequence[str], env: Effects) -> list[str]:
    """The final ordering (``R/commandLineInterface.R:238``).

    The sort key is ``basename(normalizePath(folder))``: the name of the resolved directory (so ``"."``
    sorts under the working directory's name); an unresolvable path keeps its own basename.
    """
    keys = [r_basename(env.normalize_path(folder)) for folder in runfolders]
    return [runfolders[index] for index in natural_order_indices(keys)]


def natural_sorted(names: Sequence[str]) -> list[str]:
    """``names[stri_order(names, numeric = TRUE)]`` with the fixed flags of D-14."""
    return [names[index] for index in natural_order_indices(names)]


def natural_order_indices(names: Sequence[str]) -> list[int]:
    """``stri_order(names, numeric = TRUE)`` as 0-based positions (stable for equal names)."""
    return list(index_natsorted(names, alg=NATURAL_SORT_FLAGS))


def r_basename(path: str) -> str:
    """``basename(path)``: trailing slashes removed first, so ``"a/b/"`` is ``"b"`` and ``"/"`` is ``""``."""
    stripped = path.rstrip("/")
    return os.path.basename(stripped) if stripped else ""


def r_dirname(path: str) -> str:
    """``dirname(path)``: trailing slashes removed first; ``"/a"`` gives ``"/"``, ``"a"`` gives ``"."``.

    ``"/a/b/"`` is ``"/a"``, ``"/a//b"`` is ``"/a"``, ``"/"`` is ``"/"`` and ``""`` stays ``""``. Verified in
    R 4.6.1 (``R/commandLineInterface.R:160`` via ``modelstats.slurm``).
    """
    if path == "":
        return ""
    stripped = path.rstrip("/")
    if stripped == "":
        return "/"
    if "/" not in stripped:
        return "."
    head = stripped.rpartition("/")[0].rstrip("/")
    return head or "/"
