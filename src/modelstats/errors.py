"""The exception hierarchy of the package, and the R condition messages several modules reproduce.

One base class, :class:`ModelstatsError`: every error the package raises on purpose derives from it, the
reading errors (``config.ConfigError``, ``gdx.GdxReadError``, ``rdata_io.RDataError``,
``run_type.RunTypeError``, ``slurm.SlurmError``, ``tables.EmptyCsvError``), the presentation errors
(``formatting.PrintOutputError``, ``colors.ColourRuleError``, ``loop_runs.LoopRunsError``,
``sanity.SanityError``), the environment's (``env.CommandFailedError``, ``env.LockHeldError``), the command
lines' (``choose.NoMoreInputError``, ``cli.OptparseError``, ``cli.RError``) and the AMT's
(``amt.state.AmtStateError``, ``amt.context.CycleFailedError``, ``amt.evaluate.EvaluateError``,
``amt.bridges.BridgeError``), so that a caller can catch the package as a whole.
``migration/06-user-requests.md`` section 5.1: errors while reading a run raise :class:`RunReadError` from
the property that needed the file and never abort the process (decision D-20).

The constants are R's own condition messages where more than one module raises them (``conditionMessage``
of the base R error, verified in R 4.6.1); the modules that reproduce one R statement each keep their
message next to that statement. This module imports nothing from the package so that every other module
can import it.
"""

from __future__ import annotations

from collections.abc import Sequence
from dataclasses import dataclass
from typing import Final

__all__ = [
    "ARGUMENT_OF_LENGTH_ZERO",
    "CONDITION_LENGTH_ABOVE_ONE",
    "MISSING_VALUE_IN_IF",
    "SUBSCRIPT_OUT_OF_BOUNDS",
    "DeferredWarning",
    "ModelstatsError",
    "RunReadError",
]

ARGUMENT_OF_LENGTH_ZERO: Final = "argument is of length zero"
"""R's message for ``if (logical(0))``: ``R/getRunStatus.R:91, 108`` with a ``NULL`` model name, ``:190``
with an unreadable ``log.txt``, ``:283`` without ``s80_bool``; ``R/colRunType.R:20, 24`` with a ``NULL``
``cfg`` or optimization (BUG-004); ``R/printOutput.R:48`` for a single column (``rownames()`` of a vector)."""

MISSING_VALUE_IN_IF: Final = "missing value where TRUE/FALSE needed"
"""R's message for ``if (NA)``: ``R/getRunStatus.R:91, 108`` with an ``NA`` model name, ``:283, 285``
without ``o_iterationNumber`` (BUG-038); ``R/loopRuns.R:94-135`` on an ``NA`` status field; crayon's
``if (nc > 1)`` on an unparsable ``R_CLI_NUM_COLORS``; ``R/commandLineInterface.R:144`` on ``-d`` ``NA``."""

SUBSCRIPT_OUT_OF_BOUNDS: Final = "subscript out of bounds"
"""R's message for ``x[[i]]`` past the end or a missing name: ``R/foundInSlurm.R:29`` on a short
``squeue`` line, ``R/commandLineInterface.R:165`` when the ``%Z`` and ``%j`` answers differ in length
(BUG-028), ``R/loopRuns.R:58`` on a ``try-error`` (BUG-005), ``status[["name"]]`` on a vector without that
name in the colour rules."""

CONDITION_LENGTH_ABOVE_ONE: Final = "the condition has length > 1"
"""R's message for ``if (c(TRUE, TRUE))``: ``R/colRunType.R:18`` after two configuration matches
(BUG-003), reached from ``R/getRunStatus.R:72-73`` as well."""


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

    Raised by the reading layer (``modelstats.facts``, what R raises outside ``try()`` and aborts the whole
    ``getRunStatus()`` call), so by ``get_run_status`` (``modelstats.run_status``), by the properties of
    :class:`modelstats.run.Run` and by ``Run.to_status_record()``; ``str(error)`` is R's condition message
    where R has one (``argument is of length zero``, ``invalid 'description' argument``, ``missing value
    where TRUE/FALSE needed``, ...) and the library's message where R aborts the process instead of
    raising (a damaged GDX, BUG-037, decision D-20).

    ``call`` is R's ``conditionCall`` as ``deparse(call, nlines = 1L)`` prints it, the text ``try()`` puts
    after ``Error in`` (``gzfile(file)``, ``if (s80_bool == 1) {``, ...); ``None`` where R has no call
    (``try()`` then prints ``Error : ``) or where the port cannot know which R statement failed (a library
    failure that aborts R's process). ``warnings`` are the :class:`DeferredWarning` records R had collected
    during the call, oldest first; the layer that owns the run's facts attaches them
    (``RunFacts.attaching_warnings``).
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
