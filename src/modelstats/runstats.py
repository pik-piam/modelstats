"""``runstatistics.rda``: the ``stats`` list a run writes, as ``getRunStatus()`` reads it.

R reference: ``R/getRunStatus.R:76-79`` loads the file into its own environment
(``suppressWarnings(load(fle, envir = runstatistics))``) and then tests the *names* of
``runstatistics$stats`` with unanchored ``grepl`` calls before touching a field:
``any(grepl("config", names(...)))`` (line 90), ``"modelstat"`` (92-94), ``"id"`` (107),
``"GAMSEnd"`` (310) and ``"timePrepareStart"`` (312). :class:`RunStatistics` keeps those names
and the raw list so that the parity view can reproduce every test, and exposes the fields the R
code reads as typed attributes. The file is XZ-compressed RData and contains ``POSIXct`` values
without a ``tzone`` on the cluster (``migration/01-test-data-inventory.md``).
"""

from __future__ import annotations

import re
from collections.abc import Mapping
from dataclasses import dataclass
from pathlib import Path
from typing import Any

from modelstats.rdata_io import RDataError, RScalar, RTime, r_as_character, read_rda, scalar, vector

MAGPIE = "MAgPIE"
"""``stats$config$model_name`` of a MAgPIE run (``R/getRunStatus.R:91,108,124,257,266,318``)."""


@dataclass(frozen=True, slots=True)
class RunStatistics:
    """The ``stats`` list of ``runstatistics.rda``.

    ``names`` and ``raw`` are the list as R sees it; the typed attributes are ``None`` when R would
    find ``NULL`` (``stats[["id"]]`` of a list without ``id``). ``config`` is ``stats$config`` when
    that is a list, and ``model_name`` its ``model_name`` element as ``==`` and ``paste0()`` coerce
    it, so that ``isTRUE(model_name == "MAgPIE")`` is :attr:`is_magpie`; the unguarded comparisons
    of lines 91 and 108 error in R on ``NULL`` and are the caller's to reproduce.
    """

    path: Path
    raw: Mapping[str, Any]
    names: tuple[str, ...]
    config: Mapping[str, Any] | None
    model_name: str | None
    id: str | None
    modelstat: tuple[RScalar, ...]
    time_prepare_start: RTime | None
    time_gams_start: RTime | None
    time_gams_end: RTime | None

    def has_name(self, pattern: str) -> bool:
        """``any(grepl(pattern, names(stats)))``: an unanchored regular expression over the names."""
        return any(re.search(pattern, name) for name in self.names)

    @property
    def is_magpie(self) -> bool:
        """``isTRUE(stats[["config"]][["model_name"]] == "MAgPIE")``."""
        return self.model_name == MAGPIE

    def gams_wall_seconds(self) -> float | None:
        """``as.numeric(difftime(timeGAMSEnd, timeGAMSStart, units = "secs"))`` (``R/getRunStatus.R:311``).

        ``None`` when either instant is missing or NA; the caller rounds (``rdata_io.r_round``) and
        decides, as R does by its name tests, whether the value applies.
        """
        if self.time_gams_end is None or self.time_gams_start is None:
            return None
        return self.time_gams_end.elapsed_since(self.time_gams_start)


def _time(path: Path, raw: Mapping[str, Any], name: str) -> RTime | None:
    """``stats$<name>`` as one ``POSIXct`` instant; ``NULL`` is ``None``."""
    value = raw.get(name)
    if value is None:
        return None
    if isinstance(value, RTime):
        return value
    raise RDataError(f"{path}: stats${name} is not a single POSIXct value but {type(value).__name__}")


def _config(value: Any) -> Mapping[str, Any] | None:
    if not isinstance(value, Mapping):
        return None
    return {str(key): item for key, item in value.items()}


def _string(value: Any) -> str | None:
    return r_as_character(scalar(value))


def run_statistics_from(path: Path, stats: Mapping[str, Any]) -> RunStatistics:
    """Build :class:`RunStatistics` from an already loaded ``stats`` list (tests, other readers)."""
    raw = {str(key): value for key, value in stats.items()}
    config = _config(raw.get("config"))
    return RunStatistics(
        path=path,
        raw=raw,
        names=tuple(raw),
        config=config,
        model_name=None if config is None else _string(config.get("model_name")),
        id=_string(raw.get("id")),
        modelstat=vector(raw.get("modelstat")),
        time_prepare_start=_time(path, raw, "timePrepareStart"),
        time_gams_start=_time(path, raw, "timeGAMSStart"),
        time_gams_end=_time(path, raw, "timeGAMSEnd"),
    )


def read_run_statistics(path: Path) -> RunStatistics | None:
    """``load("runstatistics.rda")`` and its ``stats`` object (``R/getRunStatus.R:76-79``).

    ``None`` when the file does not exist or defines no ``stats`` list: in both cases every R test
    on ``names(runstatistics$stats)`` is false. Unreadable content raises :class:`RDataError`, as
    the unguarded ``load()`` aborts ``getRunStatus()``.
    """
    if not path.is_file():
        return None
    stats = read_rda(path).get("stats")
    if not isinstance(stats, Mapping):
        return None
    return run_statistics_from(path, stats)
