"""The previous run of a scenario, found on disk: ``find_previous_run`` (``06`` section 4.2, decision D-24).

``R/modeltests.R:312-324`` picked the run to compare against from the ``gRS.rds`` history (every run
``getRunStatus()`` had ever returned for ``output/``): the rows that were converged with a mif, whose name
contained ``cfg$title`` and was not the current run, then ``max()`` of those sorting before the current run,
then a guess whether that folder was still in ``output/`` or had moved to ``archive/``. Two defects came with
it: ``max(character(0))`` is ``NA`` when no earlier sibling exists (BUG-014) and a history row whose folder
has left both directories, or sits there gutted, makes ``.readRuntime()`` abort the whole evaluation
(BUG-039). ``migration/06-user-requests.md`` 4.2 replaces the history by a lookup on disk:

    the newest directory in ``output/`` or ``output/archive/`` named ``<title>_<date>_<time>`` that sorts
    before the current run *and* is converged with a mif according to ``Run``.

:func:`find_previous_run` implements that sentence: :func:`candidate_runs` lists the directories of the two
locations whose name is the scenario title followed by an earlier timestamp (:class:`RunName`, the
``_YYYY-MM-DD_hh.mm.ss`` suffix of ``R/modeltests.R:371``), newest first, ``output/`` before ``archive/``
on a tie; :func:`is_usable_previous` keeps R's rule for the history rows (``Conv`` converged or converged
(had INFES), ``Mif`` yes or sumErr, ``:314-315``) in :class:`modelstats.run.Run` vocabulary. A candidate
``Run`` cannot read (:class:`modelstats.errors.RunReadError`) is not "converged according to ``Run``": it is
logged and skipped instead of aborting the evaluation. Nothing here mutates anything.
"""

from __future__ import annotations

import logging
import os
import re
from dataclasses import dataclass
from datetime import datetime
from typing import Final

from modelstats.amt.logs import LOGGER_NAME
from modelstats.env import Effects, PathLike
from modelstats.errors import RunReadError
from modelstats.run import Convergence, MifState, Run

__all__ = [
    "ARCHIVE_DIR",
    "RUN_NAME",
    "TIMESTAMP_FORMAT",
    "USABLE_CONVERGENCE",
    "USABLE_MIF",
    "RunName",
    "candidate_runs",
    "find_previous_run",
    "is_usable_previous",
    "run_name",
]

ARCHIVE_DIR: Final = "archive"
"""``output/archive``: where ``evaluateRuns`` moves runs older than 90 days (``R/modeltests.R:417``)."""

RUN_NAME: Final = re.compile(
    r"^(?P<title>.+)_(?P<date>[0-9]{4}-[0-9]{2}-[0-9]{2})_(?P<time>[0-9]{2}\.[0-9]{2}\.[0-9]{2})$"
)
"""``<title>_<date>_<time>``: the ``datetimepattern`` of ``R/modeltests.R:371`` anchored after a title."""

TIMESTAMP_FORMAT: Final = "%Y-%m-%d_%H.%M.%S"
"""The ``:date::time:`` suffix REMIND's ``start.R`` gives a run folder (``2026-09-28_10.30.27``)."""

USABLE_CONVERGENCE: Final = frozenset({Convergence.CONVERGED, Convergence.CONVERGED_HAD_INFES})
"""``Conv %in% c("converged", "converged (had INFES)")`` of ``R/modeltests.R:314``.

``Clb_converged`` (:attr:`modelstats.run.Convergence.CALIBRATION_CONVERGED`) was not in R's list and stays
out: the rule is the model owners' (``06`` 4.2).
"""

USABLE_MIF: Final = frozenset({MifState.PRESENT, MifState.SUMMATION_ERRORS})
"""``Mif %in% c("yes", "sumErr")`` of ``R/modeltests.R:315``."""


@dataclass(frozen=True, slots=True)
class RunName:
    """A run folder name taken apart: the scenario title and the moment ``start.R`` stamped onto it.

    ``started`` is naive (the stamp has no zone); it only orders runs of one title.
    """

    title: str
    started: datetime

    @property
    def stamp(self) -> str:
        """The ``<date>_<time>`` suffix as it appears in the name."""
        return self.started.strftime(TIMESTAMP_FORMAT)


def run_name(name: str) -> RunName | None:
    """Parse ``<title>_<date>_<time>``; ``None`` for any other name (``testOneRegi-AMT``, ``archive``...).

    A suffix that looks like a stamp but is not a valid moment (``2026-13-45_99.00.00``) is no stamp.
    """
    match = RUN_NAME.match(name)
    if match is None:
        return None
    try:
        started = datetime.strptime(f"{match['date']}_{match['time']}", TIMESTAMP_FORMAT)
    except ValueError:
        return None
    return RunName(match["title"], started)


def candidate_runs(
    effects: Effects, output_dir: PathLike, title: str, *, before: datetime, user: str | None = None
) -> list[Run]:
    """Every run of ``title`` in ``output/`` and ``output/archive/`` stamped before ``before``, newest first.

    Directories only (``list.dirs``, dot-names included as R lists them, but no dot-name carries a stamp);
    the name must be exactly ``<title>_<date>_<time>`` (R's ``grepl(cfg$title, rownames(gRS))`` of ``:316``
    matched any name containing the title, which is the kind of implementation detail D-24 drops). On an
    equal stamp the ``output/`` copy comes first, as R looked there first (``:322``).
    """
    output = os.fspath(output_dir)
    locations = (output, f"{output}/{ARCHIVE_DIR}")
    found: list[tuple[datetime, int, str]] = []
    for position, location in enumerate(locations):
        for name in effects.list_dirs_like_r(location, full_names=False):
            parsed = run_name(name)
            if parsed is None or parsed.title != title or not parsed.started < before:
                continue
            found.append((parsed.started, position, f"{location}/{name}"))
    found.sort(key=lambda item: (-item[0].timestamp(), item[1]))
    return [Run(path, effects, user=user) for _, _, path in found]


def is_usable_previous(run: Run) -> bool:
    """Converged with a mif according to ``Run``: the history rule of ``R/modeltests.R:314-315``."""
    return run.convergence in USABLE_CONVERGENCE and run.mif in USABLE_MIF


def find_previous_run(
    run: Run,
    output_dir: PathLike,
    effects: Effects,
    *,
    user: str | None = None,
    logger: logging.Logger | None = None,
) -> Run | None:
    """The run ``run`` is compared with (``06`` 4.2): the newest usable earlier run of the same title.

    ``run`` must carry a configuration (``cfg$title``, ``:316``) and a stamped folder name; without either
    there is nothing to look for and the answer is ``None`` (R's ``sameRuns`` were empty, ``:319``). A
    candidate that cannot be read is skipped with a warning in ``logger`` (the cycle logger by default).
    """
    log = logger if logger is not None else logging.getLogger(LOGGER_NAME)
    title = run.title
    current = run_name(run.name)
    if title is None or current is None:
        return None
    for candidate in candidate_runs(effects, output_dir, title, before=current.started, user=user):
        try:
            usable = is_usable_previous(candidate)
        except RunReadError as error:
            log.warning("previous run candidate %s skipped: %s", candidate.path, error)
            continue
        if usable:
            return candidate
    return None
