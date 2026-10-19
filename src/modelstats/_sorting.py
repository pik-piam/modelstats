"""Run folder names: coupling iterations, timestamps and the one sort order of the package.

R source: ``stringi::stri_order(numeric = TRUE)`` (rs), ``gtools::mixedsort`` and
``mixedorder(gsub("-", "", x))`` (rs2), and the ``gsub("-(rem|mag)-[0-9]+$", "", x)`` groupings.
The three R sorts disagree with each other; this module provides a single natural order in
which ``-rem-10`` follows ``-rem-9`` and ``PkBudg500`` precedes ``PkBudg1000``.
"""

from __future__ import annotations

import re
import stat
from collections.abc import Sequence
from dataclasses import dataclass
from pathlib import Path
from typing import Any

__all__ = [
    "ITERATION",
    "TIMESTAMP",
    "RunName",
    "existing_dirs",
    "is_coupled",
    "last_iterations",
    "last_timestamps",
    "natural_key",
    "parse_run_name",
    "run_sort_key",
    "subdirs",
]

# \Z and DOTALL: like R's grepl, a trailing newline is not the end and "." matches a newline
ITERATION = re.compile(r"^(?P<stem>.*)-(?P<kind>rem|mag)-(?P<iteration>[0-9]+)\Z", re.DOTALL)
TIMESTAMP = re.compile(
    r"^(?P<stem>.*)_(?P<ts>[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}\.[0-9]{2}\.[0-9]{2})\Z", re.DOTALL
)
_DIGITS = re.compile(r"(\d+)")


@dataclass(frozen=True)
class RunName:
    """The parts of a run folder name."""

    name: str
    stem: str
    kind: str | None
    iteration: int | None
    timestamp: str | None


def parse_run_name(name: str) -> RunName:
    """Split ``C_SSP2-rem-10`` into stem/kind/iteration and ``X_2026-01-02_10.00.00`` into stem/timestamp."""
    stem, kind, iteration, timestamp = name, None, None, None
    if match := ITERATION.match(stem):
        stem, kind, iteration = match["stem"], match["kind"], int(match["iteration"])
    if match := TIMESTAMP.match(stem):
        stem, timestamp = match["stem"], match["ts"]
    return RunName(name, stem, kind, iteration, timestamp)


def natural_key(text: str) -> tuple[Any, ...]:
    """Sort key that compares embedded numbers numerically and text case-insensitively."""
    parts = _DIGITS.split(text)
    return tuple(int(part) if index % 2 else part.lower() for index, part in enumerate(parts))


def run_sort_key(path: Path) -> tuple[Any, ...]:
    """The order of run folders everywhere in the package (by folder name only)."""
    run = parse_run_name(path.name)
    return (natural_key(run.stem), run.timestamp or "", run.iteration or 0, run.name)


def is_coupled(name: str) -> bool:
    """``grepl("-(rem|mag)-[0-9]+$", name)``."""
    return ITERATION.match(name) is not None


def _keep_best(paths: Sequence[Path], pattern: re.Pattern[str], rank: str) -> list[Path]:
    """One path per (parent, stem[, kind]): the highest ``rank`` group of the pattern.

    A path without the suffix is its own group unless a suffixed sibling of the same stem exists
    (then it is dropped, as R's ``gsub`` grouping did); groups keep the order of their first
    appearance.
    """
    best: dict[tuple[Path, str, str | None], tuple[Any, Path]] = {}
    plain_marker = "plain"
    for path in paths:
        match = pattern.match(path.name)
        if match is None:
            if any(key[:2] == (path.parent, path.name) and key[2] != plain_marker for key in best):
                continue
            best.setdefault((path.parent, path.name, plain_marker), (None, path))
            continue
        kind = match.groupdict().get("kind") or rank
        key = (path.parent, match["stem"], kind)
        best.pop((path.parent, match["stem"], plain_marker), None)
        value: Any = int(match[rank]) if rank == "iteration" else match[rank]
        current = best.get(key)
        if current is None or current[0] < value:
            best[key] = (value, path)
    return [path for _, path in best.values()]


def last_iterations(paths: Sequence[Path]) -> list[Path]:
    """One path per (parent, stem, kind): the highest coupling iteration; other paths unchanged."""
    return _keep_best(paths, ITERATION, "iteration")


def last_timestamps(paths: Sequence[Path]) -> list[Path]:
    """One path per (parent, stem): the latest ``_YYYY-MM-DD_HH.MM.SS`` timestamp; other paths unchanged."""
    return _keep_best(paths, TIMESTAMP, "ts")


def existing_dirs(paths: Sequence[Path], *, newest_first: bool) -> list[Path]:
    """The paths that are directories, optionally newest first (``file.info()`` once per path).

    A path that vanishes between listing and reading is left out; equal modification times keep
    the given order.
    """
    found = []
    for path in paths:
        try:
            info = path.stat()
        except OSError:
            continue
        if stat.S_ISDIR(info.st_mode):
            found.append((info.st_mtime_ns, path))
    if newest_first:
        found.sort(key=lambda item: item[0], reverse=True)
    return [path for _, path in found]


def subdirs(folder: Path) -> list[Path]:
    """``list.dirs(folder, recursive = FALSE)``: the direct subfolders (dot-folders included), by name.

    A folder that cannot be listed yields nothing. The order is by code point, R's under a C locale;
    callers sort with ``run_sort_key`` where the order is visible (BUG-054).
    """
    try:
        return sorted((entry for entry in folder.iterdir() if entry.is_dir()), key=lambda entry: entry.name)
    except OSError:
        return []
