"""``runstatistics.rda``: the ``stats`` list a run writes (``R/getRunStatus.R:76-79``).

``getRunStatus`` loads the file into its own environment when it exists and
afterwards only looks at ``runstatistics$stats``: ``names(stats)`` through
``any(grepl(<name>, names(stats)))``, ``stats$config$model_name`` (the
MAgPIE/REMIND switch), ``stats$id`` (the results archive file name),
``stats$modelstat`` (the GDX fallback; a vector over years for MAgPIE, stored
as a magclass object) and the three POSIXct fields that make ``Runtime``.
"""

from __future__ import annotations

import dataclasses
import os
import re
from collections.abc import Mapping
from typing import TYPE_CHECKING, Any

from modelstats.rdata_io import RTime, read_rda, scalar, vector

if TYPE_CHECKING:
    from modelstats.env import Effects

__all__ = ["RunStatistics"]


def _effects(effects: Effects | None) -> Effects:
    if effects is not None:
        return effects
    from modelstats.env import default_effects

    return default_effects()


@dataclasses.dataclass(frozen=True)
class RunStatistics:
    """The ``stats`` list of a ``runstatistics.rda`` (values as :func:`modelstats.rdata_io.read_rda` gives them)."""

    raw: Mapping[str, Any]

    @classmethod
    def load(cls, path: str | os.PathLike[str], effects: Effects | None = None) -> RunStatistics | None:
        """``if (file.exists(fle)) suppressWarnings(load(fle, envir = runstatistics))``.

        ``None`` when the file does not exist or defines no ``stats`` object
        (``runstatistics$stats`` is then ``NULL``, and every later check treats
        the run as REMIND without statistics). A file that exists but is not an
        RData file raises like R does, outside any ``try()``.
        """
        fle = os.fspath(path)
        if not _effects(effects).exists(fle):
            return None
        stats = read_rda(fle, effects).get("stats")
        if stats is None:
            return None
        if not isinstance(stats, Mapping):
            msg = f"{fle}: the stats object is not a list"
            raise TypeError(msg)
        return cls({str(key): value for key, value in stats.items()})

    @property
    def names(self) -> list[str]:
        """``names(stats)``."""
        return list(self.raw)

    def has(self, name: str) -> bool:
        """``any(grepl(name, names(stats)))``: ``name`` is a regex searched in every element name."""
        pattern = re.compile(name)
        return any(pattern.search(n) for n in self.raw)

    @property
    def config(self) -> Mapping[str, Any] | None:
        """``stats[["config"]]`` (the cfg at run time) or ``None``."""
        config = self.raw.get("config")
        return config if isinstance(config, Mapping) else None

    @property
    def model_name(self) -> str | None:
        """``stats[["config"]][["model_name"]]``; ``None`` when absent (R: ``NULL``).

        The callers decide what R does with ``NULL``: ``isTRUE(NULL == "MAgPIE")``
        is FALSE, but ``if (NULL == "MAgPIE")`` (lines 91 and 108) raises
        ``argument is of length zero``.
        """
        config = self.config
        if config is None:
            return None
        value = scalar(config.get("model_name"))
        return None if value is None else str(value)

    @property
    def id(self) -> Any:
        """``stats[["id"]]`` as a scalar (a character string in every fixture) or ``None``."""
        return scalar(self.raw.get("id"))

    @property
    def modelstat(self) -> list[Any]:
        """``stats[["modelstat"]]`` as a list (one element for REMIND, one per year for MAgPIE; NA -> ``None``)."""
        return vector(self.raw.get("modelstat"))

    def _time(self, name: str) -> RTime | None:
        value = self.raw.get(name)
        if value is None:
            return None
        if isinstance(value, RTime):
            return value
        if isinstance(value, list) and value and all(isinstance(v, RTime) for v in value):
            if len(value) == 1:
                first = value[0]
                assert isinstance(first, RTime)
                return first
            msg = f"stats${name} is a POSIXct of length {len(value)}"
            raise ValueError(msg)
        msg = f"stats${name} is not a POSIXct: {type(value).__name__}"
        raise TypeError(msg)

    @property
    def timeGAMSStart(self) -> RTime | None:  # noqa: N802 - R field name
        return self._time("timeGAMSStart")

    @property
    def timeGAMSEnd(self) -> RTime | None:  # noqa: N802 - R field name
        return self._time("timeGAMSEnd")

    @property
    def timePrepareStart(self) -> RTime | None:  # noqa: N802 - R field name
        return self._time("timePrepareStart")
