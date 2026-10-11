"""The context shared by ``modeltests start`` and ``modeltests evaluate``, and the failure path they share.

``migration/06-user-requests.md`` section 4.3: "``start.py``/``evaluate.py`` receive one ``AmtContext``
(paths, options, ``Effects``, logger, state) and are plain functions; no module-level state".
:class:`AmtContext` is that object: the REMIND checkout (the R ``mydir``, ``R/modeltests.R:24``), the report
repository (the R ``gitdir``, ``:25``), the SLURM user (``:27``), the options of ``06`` 3.1
(:class:`AmtOptions`), the :class:`modelstats.env.Effects` that is the only door to the environment (``03``
2.3), the cycle logger of ``modelstats.amt.logs``, the state the command loaded under the lock
(``modelstats.amt.state.StateStore.load_or_migrate``) and, derived from the checkout, the state store
(``<dir>/amt-state.json``, ``06`` 3.2) and the cycle log location (``<log-dir>/amt-<cycle id>.log``, ``06``
3.3). The flat accessors ``checkout``, ``publish``, ``compare_scenarios`` and ``mattermost_variable`` are the
names ``modelstats.amt.evaluate.EvaluateContext`` reads, so one context serves both commands.

The one behaviour both commands share is the last row of the transition table of ``06`` 3.2: "any |
crash inside start/evaluate | failed | ``last_error`` filled; the cycle is kept; a Mattermost notification
is sent if a URL is configured (the R version sent none)". :func:`record_failure` is that row: it logs the
error with its traceback, saves the ``failed`` state through the store and sends the failure message of
``modelstats.amt.notify``; the command then raises the :class:`CycleFailedError` it returns, which carries
the saved state for the CLI. The R version left the error sentence in ``../.testsstatus``
(``R/modeltests.R:172``) and stopped silently.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass
from pathlib import Path
from typing import ClassVar, Final

from modelstats.amt import notify
from modelstats.amt.logs import cycle_log_path
from modelstats.amt.state import AMT_STATE_FILE, AmtState, Command, Cycle, LastError, StateStore, fail
from modelstats.env import Effects
from modelstats.errors import ModelstatsError

__all__ = [
    "DEFAULT_LOG_DIR",
    "OUTPUT_DIR",
    "AmtContext",
    "AmtOptions",
    "CycleFailedError",
    "record_failure",
]

DEFAULT_LOG_DIR: Final = "logs"
"""``--log-dir`` default: ``<dir>/logs`` (``06`` 3.1)."""

OUTPUT_DIR: Final = "output"
"""The run folders below the checkout (``withr::with_dir("output", ...)``, ``R/modeltests.R:45``)."""


@dataclass(frozen=True)
class AmtOptions:
    """The global options of ``06`` 3.1 that ``start`` and ``evaluate`` consult.

    ``publish`` is the R ``email`` flag (``R/modeltests.R:28, 433``): commit and push ``README.md`` to the
    report repository. ``compare_scenarios`` is the R ``compScen`` (``:29, 334``). ``mattermost_url_variable``
    names the environment variable holding the webhook URL (``--mattermost-url-env``; the R
    ``mattermostToken`` was the URL itself, ``:30``); unset means no notification. ``log_dir`` is where the
    cycle logs go (``None``: ``<dir>/logs``). ``dry_run`` says the effects are a
    :class:`modelstats.env.DryRunEffects` (the commands only mention it in the log). ``force`` is
    ``start --force`` (``06`` 3.2: ``started -> started``, the abandoned cycle recorded).
    """

    publish: bool = True
    compare_scenarios: bool = True
    mattermost_url_variable: str = notify.MATTERMOST_URL_VARIABLE
    log_dir: Path | None = None
    dry_run: bool = False
    force: bool = False


@dataclass(frozen=True)
class AmtContext:
    """One invocation's environment: checkout, report repository, user, options, effects, logger, state.

    ``dir`` is the REMIND checkout as ``--dir`` named it (relative paths are resolved by the effects
    against their working directory); ``report_repo`` the clone that receives ``README.md`` (``None``
    with ``--no-publish``); ``user`` the SLURM user whose jobs ``evaluate`` awaits (``R/modeltests.R:182``);
    ``logger`` the cycle logger the CLI configured with ``modelstats.amt.logs.cycle_log`` for the command
    (the commands log through it only); ``state`` the state the CLI loaded under the lock, the one the
    command's transition starts from. The state store, the state file, the output directory and the log
    location are derived (:attr:`store`, :attr:`state_path`, :attr:`output_dir`, :meth:`cycle_log_path`),
    so that ``start`` and ``evaluate`` agree on them by construction.
    """

    dir: Path
    report_repo: Path | None
    user: str
    options: AmtOptions
    effects: Effects
    logger: logging.Logger
    state: AmtState

    @property
    def checkout(self) -> Path:
        """``dir`` under the name ``modelstats.amt.evaluate.EvaluateContext`` uses."""
        return self.dir

    @property
    def publish(self) -> bool:
        """``options.publish`` (``--publish`` / ``--no-publish``)."""
        return self.options.publish

    @property
    def compare_scenarios(self) -> bool:
        """``options.compare_scenarios`` (``--compare-scenarios`` / ``--no-compare-scenarios``)."""
        return self.options.compare_scenarios

    @property
    def mattermost_variable(self) -> str:
        """``options.mattermost_url_variable`` (``--mattermost-url-env``)."""
        return self.options.mattermost_url_variable

    @property
    def store(self) -> StateStore:
        """``amt-state.json``, ``amt-state.lock`` and ``runcode.rds`` of the checkout (``06`` 3.2, 3.4)."""
        return StateStore.at(self.effects, self.dir)

    @property
    def state_path(self) -> Path:
        """``<dir>/amt-state.json``."""
        return self.dir / AMT_STATE_FILE

    @property
    def output_dir(self) -> Path:
        """``<dir>/output``: the run folders (``R/modeltests.R:45``)."""
        return self.dir / OUTPUT_DIR

    @property
    def log_dir(self) -> Path:
        """``--log-dir``, or ``<dir>/logs`` (``06`` 3.1)."""
        return self.options.log_dir if self.options.log_dir is not None else self.dir / DEFAULT_LOG_DIR

    def cycle_log_path(self, cycle_id: str) -> Path:
        """``<log-dir>/amt-<cycle id>.log`` (``06`` 3.3): what ``cycle.log`` records and the CLI opens."""
        return cycle_log_path(self.log_dir, cycle_id)


class CycleFailedError(ModelstatsError):
    """A command crashed and the ``failed`` state has been saved (``06`` 3.2, last row of the table).

    ``state`` is the saved state (``last_error`` filled, the cycle kept), ``command`` the command that
    crashed and ``__cause__`` the original error. ``str(error)`` is the line for the terminal; the process
    exits with :attr:`exit_status`.
    """

    exit_status: ClassVar[int] = 1

    def __init__(self, command: Command, state: AmtState) -> None:
        """Name the command and where the details are (the log of the cycle, the state file)."""
        error = state.last_error
        message = error.message if error is not None else "unknown error"
        where = f"; log {error.log}" if error is not None and error.log is not None else ""
        super().__init__(f"'{command.value}' failed: {message}{where}; the AMT state is now 'failed'")
        self.command = command
        self.state = state


def record_failure(
    ctx: AmtContext, command: Command, error: BaseException, *, cycle: Cycle | None = None
) -> CycleFailedError:
    """``any -> failed``: log the error, save ``ctx.state`` with ``last_error`` and notify (``06`` 3.2).

    ``cycle`` is the record ``start`` had built before it crashed (the state still says ``idle`` then);
    ``evaluate`` leaves it ``None`` and the cycle in the state is kept (``modelstats.amt.state.fail``).
    The log path recorded with the error is the kept cycle's. The notification goes through
    :func:`modelstats.amt.notify.send`, which does nothing without a configured URL and never raises.
    Returns the :class:`CycleFailedError` for the caller to raise ``from error``.

    When the ``failed`` state itself cannot be written (an ``OSError`` of the state file: a full or
    read-only disk), the error is logged, the notification is still sent (``06`` 3.2 promises one for
    every crash) and that ``OSError`` is raised: the state on disk is then whatever the atomic write left,
    which the next invocation reads as it is.
    """
    at = ctx.effects.now()
    kept = cycle if cycle is not None else ctx.state.cycle
    message = str(error) or repr(error)
    last_error = LastError(command=command.value, message=message, at=at, log=kept.log if kept else None)
    ctx.logger.error("'%s' failed: %s", command.value, message, exc_info=error)
    failed = fail(ctx.state, last_error, cycle=cycle)
    report = notify.FailureReport(
        command=command.value, message=message, runs_path=str(ctx.dir), at=at, log=last_error.log
    )
    try:
        ctx.store.save(failed)
    except OSError as save_error:
        ctx.logger.error("cannot save the 'failed' state to %s: %s", ctx.state_path, save_error)
        _notify_failure(ctx, report)
        raise
    ctx.logger.error("the AMT state is now 'failed' (%s); the cycle is kept for a retry", ctx.state_path)
    _notify_failure(ctx, report)
    return CycleFailedError(command, failed)


def _notify_failure(ctx: AmtContext, report: notify.FailureReport) -> None:
    """Post the failure message when a webhook is configured (``notify.send`` never raises)."""
    notify.send(
        ctx.effects,
        notify.failure_message(report),
        variable=ctx.mattermost_variable,
        log=ctx.logger,
    )
