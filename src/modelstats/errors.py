"""The exception hierarchy of the package: one base class, :class:`ModelstatsError`.

``migration/06-user-requests.md`` section 5.1: errors while reading a run raise :class:`RunReadError`, a
subclass of :class:`ModelstatsError`, from the property that needed the file and never abort the process
(decision D-20). This module imports nothing from the package so that every other module can import it.

Transition note (phase 2). The phase-1 helpers still define their own error classes (``config.ConfigError``,
``gdx.GdxReadError``, ``rdata_io.RDataError``, ``run_type.RunTypeError``, ``slurm.SlurmError``,
``tables.EmptyCsvError``, ``formatting.PrintOutputError``, ``colors.ColourRuleError``,
``env.CommandFailedError``, ``choose.NoMoreInputError``) outside this hierarchy; the reading layer
(``modelstats.facts``) raises and re-exports this module's :class:`RunReadError` since the phase 2
bookkeeping, so the parity view (``get_run_status``) and the ``Run`` object raise the same class. Moving the
helper classes onto :class:`ModelstatsError` is a cross-unit change for the quality phase.
"""

from __future__ import annotations

__all__ = ["ModelstatsError", "RunReadError"]


class ModelstatsError(Exception):
    """Base class of every error the package raises on purpose."""


class RunReadError(ModelstatsError):
    """A run folder could not be read the way ``getRunStatus()`` reads it.

    Raised by ``get_run_status`` (``modelstats.run_status``, what R raises outside ``try()`` and aborts the
    whole call), by the properties of :class:`modelstats.run.Run` and by ``Run.to_status_record()``;
    ``str(error)`` is R's condition message where R has one (``argument is of length zero``, ``invalid
    'description' argument``, ``missing value where TRUE/FALSE needed``, ...) and the library's message
    where R aborts the process instead of raising (a damaged GDX, BUG-037, decision D-20).
    """
