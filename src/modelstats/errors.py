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

from collections.abc import Sequence
from dataclasses import dataclass

__all__ = ["DeferredWarning", "ModelstatsError", "RunReadError"]


class ModelstatsError(Exception):
    """Base class of every error the package raises on purpose."""


@dataclass(frozen=True, slots=True)
class DeferredWarning:
    """One R warning raised while a run was read and, under ``options(warn = 0)``, still unprinted.

    ``call`` is the warning's call as ``deparse()`` prints it (``None`` for a call-less warning) and
    ``message`` its condition message. ``getRunStatus()`` raises exactly one kind: ``readGDX()``'s warning
    for a symbol absent from the selected GDX (``R/getRunStatus.R:280-281, 288``, ``react`` defaults to
    ``"warning"``). R prints the deferred warnings where the surrounding error is printed: ``try()``'s
    ``In addition: Warning message:`` block (``R/loopRuns.R:57``,
    :func:`modelstats.loop_runs.try_error_message`) and the top-level handler of ``Rscript`` (the CLI).
    """

    call: str | None
    message: str


class RunReadError(ModelstatsError):
    """A run folder could not be read the way ``getRunStatus()`` reads it.

    Raised by ``get_run_status`` (``modelstats.run_status``, what R raises outside ``try()`` and aborts the
    whole call), by the properties of :class:`modelstats.run.Run` and by ``Run.to_status_record()``;
    ``str(error)`` is R's condition message where R has one (``argument is of length zero``, ``invalid
    'description' argument``, ``missing value where TRUE/FALSE needed``, ...) and the library's message
    where R aborts the process instead of raising (a damaged GDX, BUG-037, decision D-20).

    ``call`` is R's ``conditionCall`` as ``deparse(call, nlines = 1L)`` prints it, the text ``try()`` puts
    after ``Error in`` (``gzfile(file)``, ``if (s80_bool == 1) {``, ...); ``None`` where R has no call
    (``try()`` then prints ``Error : ``) or where the port cannot know which R statement failed (a library
    failure that aborts R's process). ``warnings`` are the :class:`DeferredWarning` records R had collected
    during the call, oldest first; they are attached by the layer that owns the run's facts
    (``run_status.run_status_record``, ``Run``).
    """

    def __init__(
        self, message: str, *, call: str | None = None, warnings: Sequence[DeferredWarning] = ()
    ) -> None:
        """``message`` is R's condition message, ``call`` its deparsed call, ``warnings`` the deferred."""
        super().__init__(message)
        self.call = call
        self.warnings: tuple[DeferredWarning, ...] = tuple(warnings)

    @property
    def message(self) -> str:
        """R's condition message (``str(error)``)."""
        return str(self)
