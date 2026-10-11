"""Console script ``modeltests``: the REMIND automated model tests (AMT), one command per step of a cycle.

The rewrite of ``R/modeltests.R``'s entry point ``modeltests()`` (``R/modeltests.R:24-55``, modelstats
0.31.0) under ``migration/06-user-requests.md`` sections 3.1 (commands and options), 3.2 (state machine and
exit statuses) and 3.3 (one log per cycle), REMIND only (section 2, D-27). The R function read
``../.testsstatus`` and called ``startRuns`` or ``evaluateRuns``; here five subcommands of one typer
application share the global options of ``06`` 3.1 (:class:`GlobalOptions`) and one :class:`Session`
(the :class:`modelstats.env.Effects`, the streams, stdin):

* ``start`` and ``evaluate`` are :func:`modelstats.amt.start.start_cycle` and
  :func:`modelstats.amt.evaluate.evaluate_cycle` on one :class:`modelstats.amt.context.AmtContext`;
* ``run`` is the drop-in for the old toggle: ``idle`` starts, ``started`` evaluates and ``failed`` is
  refused with the message (``06`` 3.2, :func:`modelstats.amt.state.next_command`);
* ``status`` prints ``amt-state.json`` on stdout and where the cycle log is on stderr, exit 0;
* ``reset`` clears a ``failed`` state after a human looked at it, asking on the terminal unless ``--yes``.

Every command runs :func:`run_command`: the effects are selected (production, or
:class:`modelstats.env.DryRunEffects` around them under ``--dry-run``), the exclusive ``flock`` on
``<dir>/amt-state.lock`` is held for the whole command (``--no-wait`` fails fast), the state is loaded or
migrated once from the R files (:meth:`modelstats.amt.state.StateStore.load_or_migrate`, the derived facts
logged), and the cycle log of ``06`` 3.3 is opened at ``<log-dir>/amt-<cycle id>.log`` before anything else
is logged (:func:`log_path_for`: ``start`` creates today's, ``evaluate`` appends to the cycle's, ``run``
follows the command it dispatches, ``reset`` notes itself in the failed cycle's log, ``status`` logs to
stderr only, a dry run writes no log file). Subprocess output is mirrored to stderr by the log setup, so
cron mail and ``nohup`` see what the file sees.

Exit statuses (``06`` 3.2): ``0`` when the command did what it says; ``1`` when ``start`` or ``evaluate``
crashed and the state is now ``failed`` (:class:`modelstats.amt.context.CycleFailedError`), or when a
``reset`` was declined at the prompt; ``2`` for a refused transition, a held lock under ``--no-wait``, a
damaged state file or a missing ``--report-repo``, all before any effect and with the state unchanged
(:class:`modelstats.amt.state.AmtStateError`, :class:`modelstats.amt.evaluate.EvaluateError`), and for a
usage error (click's own). :func:`main` is the in-process entry for tests and harnesses: it takes the
argument vector and an injected ``Effects`` (the sandbox runner ``migration/harness/run_amt_case.sh``
captures the webhook through it) and returns the exit status; :data:`app` is the console script.
"""

from __future__ import annotations

import logging
import sys
from collections.abc import Callable, Sequence
from dataclasses import dataclass, replace
from pathlib import Path
from typing import Annotated, Any, Final, TextIO

import typer
from typer._click.exceptions import ClickException

from modelstats.amt.context import AmtContext, AmtOptions, CycleFailedError
from modelstats.amt.evaluate import EvaluateError, cycle_to_evaluate, evaluate_cycle
from modelstats.amt.logs import LOGGER_NAME, close_cycle_log, setup_cycle_log
from modelstats.amt.notify import MATTERMOST_URL_VARIABLE
from modelstats.amt.start import start_cycle
from modelstats.amt.state import (
    AmtState,
    AmtStateError,
    Command,
    StateStore,
    cycle_id,
    describe,
    next_command,
    reset,
    target_phase,
)
from modelstats.choose import LineReader, read_stdin_line
from modelstats.env import DryRunEffects, Effects, ProductionEffects

__all__ = [
    "CANCELLED_EXIT_STATUS",
    "CRASHED_EXIT_STATUS",
    "DEFAULT_DIR",
    "GLOBAL_OPTION_HELP",
    "PROGRAM",
    "REFUSED_EXIT_STATUS",
    "USAGE_EXIT_STATUS",
    "GlobalOptions",
    "Invocation",
    "ModeltestsApp",
    "Session",
    "app",
    "confirm_reset",
    "evaluate_command",
    "log_path_for",
    "main",
    "make_app",
    "reset_command",
    "run_command",
    "select_effects",
    "start_command",
    "status_command",
    "transition_allowed",
]

PROGRAM: Final = "modeltests"
"""The console script's name (``pyproject.toml`` ``[project.scripts]``)."""

DEFAULT_DIR: Final = "."
"""``--dir`` default: the R ``mydir = "."`` (``R/modeltests.R:24``)."""

CRASHED_EXIT_STATUS: Final = CycleFailedError.exit_status
"""``1``: ``start`` or ``evaluate`` crashed, the state is ``failed`` (``06`` 3.2, last row of the table)."""

REFUSED_EXIT_STATUS: Final = AmtStateError.exit_status
"""``2``: a refused transition or a held lock; nothing changed (``06`` 3.2)."""

CANCELLED_EXIT_STATUS: Final = 1
"""``reset`` without ``--yes`` was answered with anything but ``y``: nothing changed."""

USAGE_EXIT_STATUS: Final = 2
"""click's exit status for a usage error (an unknown command or option)."""

HELP_DESCRIPTION: Final = (
    "Start and evaluate the REMIND automated model tests (AMT).\n"
    "\n"
    "The state of the cycle lives in <dir>/amt-state.json (idle, started or failed), every command "
    "holds <dir>/amt-state.lock, and each cycle writes one log <log-dir>/amt-<date>.log that 'start' "
    "creates and 'evaluate' appends to. 'run' is the cron entry point: it starts a cycle when the state "
    "is idle, evaluates it when it is started, and stops with status 2 when a previous command failed, "
    "until a human runs 'evaluate' again or 'reset'. See docs/amt.md."
)
"""The help page's description (typer's layout)."""

HELP_EPILOGUE: Final = "Bugs and feedback: https://github.com/pik-piam/modelstats/issues"

GLOBAL_OPTION_HELP: Final[dict[str, str]] = {
    "dir": "the REMIND checkout (the R 'mydir')",
    "report-repo": (
        "the git clone that receives README.md (the R 'gitdir'); required for 'evaluate' unless --no-publish"
    ),
    "user": "SLURM user whose jobs are awaited; default: the current user",
    "mattermost-url-env": (
        "environment variable holding the Mattermost webhook URL; unset variable = no notification"
    ),
    "publish": "commit and push README.md to --report-repo (the R 'email' flag)",
    "compare-scenarios": "run compareScenarios2 for converged runs (the R 'compScen')",
    "log-dir": "where the cycle logs and the README copies go; default <dir>/logs",
    "dry-run": "reads execute, every mutation is logged and answered synthetically; no log file is written",
    "force": "'start' although a cycle is still 'started' (its evaluate never ran); the cycle is abandoned",
    "no-wait": "fail with status 2 when another modeltests command holds the lock instead of waiting",
}
"""The option help of ``06`` 3.1."""

COMMAND_HELP: Final[dict[Command, str]] = {
    Command.START: "Start the AMT cycle: update the checkout, submit the runs, record the cycle.",
    Command.EVALUATE: "Wait for the runs, write the README, publish it, notify, close the cycle.",
    Command.RUN: "'start' when idle, 'evaluate' when started; the drop-in for the old toggle file.",
    Command.STATUS: "Print the state file (stdout) and the log paths (stderr); exit 0.",
    Command.RESET: "Clear a 'failed' state after a human looked at it (asks unless --yes).",
}

_RESET_QUESTION: Final = "Clear the 'failed' state and drop the recorded cycle? [y/N] "
_YES: Final = frozenset({"y", "yes"})
_DRY_RUN_PREFIX: Final = "dry run: %s"


# ---------------------------------------------------------------------------------------------
# What one invocation carries: the global options, the session, the subcommand
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True)
class GlobalOptions:
    """The ``GLOBAL OPTIONS`` of ``06`` 3.1 as parsed, plus ``--no-wait`` of ``06`` 3.2.

    The three paths are made absolute against the effects' working directory by :meth:`absolute` before a
    command runs, so that the state file records paths that any later invocation can open from any
    directory; ``user`` ``None`` means the effects' user (``Sys.info()[["user"]]``); ``report_repo``
    ``None`` is allowed with ``--no-publish``.
    """

    dir: Path = Path(DEFAULT_DIR)
    report_repo: Path | None = None
    user: str | None = None
    mattermost_url_variable: str = MATTERMOST_URL_VARIABLE
    publish: bool = True
    compare_scenarios: bool = True
    log_dir: Path | None = None
    dry_run: bool = False
    force: bool = False
    no_wait: bool = False

    def absolute(self, effects: Effects) -> GlobalOptions:
        """The options with ``dir``, ``report_repo`` and ``log_dir`` resolved against ``effects.cwd``."""
        base = Path(effects.cwd)
        return replace(
            self,
            dir=base / self.dir,
            report_repo=None if self.report_repo is None else base / self.report_repo,
            log_dir=None if self.log_dir is None else base / self.log_dir,
        )

    def amt_options(self) -> AmtOptions:
        """The part the commands read (``modelstats.amt.context.AmtOptions``)."""
        return AmtOptions(
            publish=self.publish,
            compare_scenarios=self.compare_scenarios,
            mattermost_url_variable=self.mattermost_url_variable,
            log_dir=self.log_dir,
            dry_run=self.dry_run,
            force=self.force,
        )


@dataclass(frozen=True)
class Session:
    """Where one ``modeltests`` invocation runs: the environment, the two streams and stdin.

    ``effects`` is the production environment or an injected double (``main(argv, effects=...)``); ``out``
    receives the one-line results (``start``, ``evaluate``) and the state document (``status``), ``err``
    the messages, the prompt of ``reset`` and the mirror of the cycle log; ``read_line`` answers the
    ``reset`` prompt.
    """

    effects: Effects
    out: TextIO
    err: TextIO
    read_line: LineReader = read_stdin_line

    @classmethod
    def production(cls) -> Session:
        """The real environment and streams; stdout and stderr pass undecodable names through."""
        out, err = sys.stdout, sys.stderr
        for stream in (out, err):
            reconfigure = getattr(stream, "reconfigure", None)
            if reconfigure is not None:
                reconfigure(errors="surrogateescape")
        return cls(effects=ProductionEffects(), out=out, err=err)


@dataclass(frozen=True)
class Invocation:
    """The global options and the session, handed from the typer callback to the subcommand."""

    options: GlobalOptions
    session: Session


# ---------------------------------------------------------------------------------------------
# The common frame of every command: effects, lock, state, log (06 sections 3.2 and 3.3)
# ---------------------------------------------------------------------------------------------


def select_effects(session: Session, options: GlobalOptions) -> Effects:
    """The session's effects, wrapped in :class:`modelstats.env.DryRunEffects` under ``--dry-run``.

    The dry run's ``would ...`` lines go to the cycle logger (``06`` 3.1: "every mutation is logged"); the
    logger is looked up per message because its handlers are configured after the effects exist.
    """
    if not options.dry_run:
        return session.effects

    def log(message: str) -> None:
        logging.getLogger(LOGGER_NAME).info(_DRY_RUN_PREFIX, message)

    return DryRunEffects(session.effects, log=log)


def log_path_for(command: Command, state: AmtState, ctx: AmtContext) -> Path | None:
    """The cycle log a command writes (``06`` 3.3), ``None`` for stderr only.

    ``start`` creates today's ``<log-dir>/amt-<date>.log`` (the cycle id is the start date); ``evaluate``
    appends to the log the cycle recorded, or to ``amt-<cycle id>.log`` for a cycle migrated from the R
    files (which recorded none); ``reset`` notes itself in the log the failed cycle or the last error
    recorded, and creates none; ``status`` only prints; ``run`` is resolved by the caller. A dry run writes
    no file.
    """
    if ctx.options.dry_run or command is Command.STATUS:
        return None
    if command is Command.START:
        return _under_cwd(ctx.cycle_log_path(cycle_id(ctx.effects.now().date())), ctx.effects)
    cycle = state.cycle
    if command is Command.EVALUATE and cycle is not None:
        recorded = Path(cycle.log) if cycle.log is not None else ctx.cycle_log_path(cycle.id)
        return _under_cwd(recorded, ctx.effects)
    if command is Command.RESET:
        error = state.last_error
        kept = cycle.log if cycle is not None and cycle.log is not None else None
        noted = kept if kept is not None else (error.log if error is not None else None)
        return None if noted is None else _under_cwd(Path(noted), ctx.effects)
    return None


def _under_cwd(path: Path, effects: Effects) -> Path:
    """A relative log path (a relative ``--dir``, or a state file written with one) under the effects' cwd.

    The state records the path as ``--dir`` named it; the file is opened where that name resolves.
    """
    return path if path.is_absolute() else Path(effects.cwd) / path


def _with_cycle_log(state: AmtState, path: Path | None) -> AmtState:
    """The state with a migrated cycle's missing ``log`` filled, so a failure record names the file."""
    cycle = state.cycle
    if cycle is None or cycle.log is not None or path is None:
        return state
    return replace(state, cycle=replace(cycle, log=str(path)))


def transition_allowed(command: Command, state: AmtState, *, force: bool) -> None:
    """The pre-check of ``06`` 3.2 before any effect; :class:`modelstats.amt.state.AmtStateError` refuses.

    ``start`` and ``reset`` consult the transition table (``force`` is ``start --force``); ``evaluate``
    additionally needs a recorded cycle (:func:`modelstats.amt.evaluate.cycle_to_evaluate`); ``status``
    is always allowed. The commands repeat their own check, so nothing depends on this one being first;
    it only keeps a refused command from opening a log file.
    """
    if command is Command.START:
        target_phase(state, Command.START, force=force)
    elif command is Command.EVALUATE:
        cycle_to_evaluate(state)
    elif command is Command.RESET:
        target_phase(state, Command.RESET)


def run_command(command: Command, options: GlobalOptions, session: Session, *, yes: bool = False) -> int:
    """Run ``command`` under the lock with the state loaded and the cycle log open; the exit status.

    The order of ``06`` 3.2 and 3.3: lock, load or migrate the state (its notes are kept until the log
    is open and then logged first), resolve ``run`` to ``start`` or ``evaluate``, check the transition
    (:func:`transition_allowed`), open the cycle log for the resolved command, build the
    :class:`modelstats.amt.context.AmtContext` and dispatch. A refusal
    (:class:`modelstats.amt.state.AmtStateError`) prints its message and returns status 2 with nothing
    changed, not even a log file; the migration notes are still printed so the derived state is explained.
    ``yes`` is the ``reset`` command's ``--yes``.
    """
    effects = select_effects(session, options)
    options = options.absolute(effects)
    store = StateStore.at(effects, options.dir)
    logger = setup_cycle_log(None, command.value, clock=effects.now, stream=session.err)
    notes: list[str] = []
    try:
        if not effects.is_dir(options.dir):
            raise AmtStateError(f"--dir {options.dir} is not a directory (the REMIND checkout)")
        with store.locked(wait=not options.no_wait):
            try:
                state = store.load_or_migrate(notes.append)
                resolved = next_command(state) if command is Command.RUN else command
                transition_allowed(resolved, state, force=options.force)
            except AmtStateError:
                _log_lines(logger, notes)
                raise
            ctx = _context(options, effects, logger, state)
            path = log_path_for(resolved, state, ctx)
            logger = setup_cycle_log(path, resolved.value, clock=effects.now, stream=session.err)
            _log_lines(logger, notes)
            if command is Command.RUN:
                logger.info("'run' found %s: running '%s'", describe(state), resolved.value)
            ctx = replace(ctx, logger=logger, state=_with_cycle_log(state, path))
            return _dispatch(resolved, ctx, session, yes=yes)
    except AmtStateError as error:
        _report(session, logger, error)
        return error.exit_status
    finally:
        close_cycle_log(logger)


def _context(options: GlobalOptions, effects: Effects, logger: logging.Logger, state: AmtState) -> AmtContext:
    return AmtContext(
        dir=options.dir,
        report_repo=options.report_repo,
        user=options.user if options.user is not None else effects.user,
        options=options.amt_options(),
        effects=effects,
        logger=logger,
        state=state,
    )


def _log_lines(logger: logging.Logger, lines: Sequence[str]) -> None:
    for line in lines:
        logger.info("%s", line)


def _report(session: Session, logger: logging.Logger, error: Exception) -> None:
    """The one line of a refusal or a crash: on stderr, and in the cycle log when one is open."""
    if any(isinstance(handler, logging.FileHandler) for handler in logger.handlers):
        logger.error("%s", error)
    else:
        session.err.write(f"{PROGRAM}: {error}\n")


def _dispatch(command: Command, ctx: AmtContext, session: Session, *, yes: bool) -> int:
    if command is Command.START:
        return start_command(ctx, session)
    if command is Command.EVALUATE:
        return evaluate_command(ctx, session)
    if command is Command.STATUS:
        return status_command(ctx, session)
    if command is Command.RESET:
        return reset_command(ctx, session, yes=yes)
    raise AssertionError(f"{command} is resolved before dispatch")


# ---------------------------------------------------------------------------------------------
# The commands
# ---------------------------------------------------------------------------------------------


def start_command(ctx: AmtContext, session: Session) -> int:
    """``modeltests start``: :func:`modelstats.amt.start.start_cycle`; one line on stdout, 0 or 1."""
    try:
        state = start_cycle(ctx)
    except CycleFailedError as error:
        _report(session, ctx.logger, error)
        return error.exit_status
    cycle = state.cycle
    assert cycle is not None  # a 'started' state carries its cycle
    count = len(cycle.runs_to_start)
    session.out.write(
        f"started AMT cycle {cycle.id} with {count} scenario{'' if count == 1 else 's'}; log {cycle.log}\n"
    )
    return 0


def evaluate_command(ctx: AmtContext, session: Session) -> int:
    """``modeltests evaluate``: :func:`modelstats.amt.evaluate.evaluate_cycle`; the summary on stdout.

    A missing ``--report-repo`` while publishing is refused before any effect (status 2, like a refused
    transition); a crash inside the evaluation is status 1 with the state ``failed``.
    """
    try:
        evaluation = evaluate_cycle(ctx)
    except CycleFailedError as error:
        _report(session, ctx.logger, error)
        return error.exit_status
    except EvaluateError as error:
        _report(session, ctx.logger, error)
        return REFUSED_EXIT_STATUS
    session.out.write(f"evaluated AMT cycle {evaluation.cycle.id}: {evaluation.summary}\n")
    return 0


def status_command(ctx: AmtContext, session: Session) -> int:
    """``modeltests status``: the state document on stdout, where things are on stderr, exit 0."""
    state = ctx.state
    session.out.write(state.to_json())
    lines = [describe(state), f"state file: {ctx.state_path}"]
    if state.cycle is not None and state.cycle.log is not None:
        lines.append(f"cycle log: {state.cycle.log}")
    if state.last_error is not None and state.last_error.log is not None:
        lines.append(f"last error log: {state.last_error.log}")
    session.err.write("".join(f"{line}\n" for line in lines))
    return 0


def confirm_reset(session: Session) -> bool:
    """Ask on stderr and read one line; ``y``/``yes`` (any case) confirms, anything else or EOF declines."""
    session.err.write(_RESET_QUESTION)
    session.err.flush()
    answer = session.read_line()
    session.err.write("\n" if answer is None else "")
    return answer is not None and answer.strip().lower() in _YES


def reset_command(ctx: AmtContext, session: Session, *, yes: bool) -> int:
    """``modeltests reset``: ``failed -> idle`` after confirmation (``06`` 3.2).

    The transition is checked first (anything but ``failed`` is refused with status 2); a declined prompt
    changes nothing and returns :data:`CANCELLED_EXIT_STATUS`.
    """
    state = ctx.state
    target_phase(state, Command.RESET)
    ctx.logger.info("reset requested while %s", describe(state))
    if not yes and not confirm_reset(session):
        ctx.logger.info("reset cancelled; the state is unchanged")
        session.err.write(f"{PROGRAM}: reset cancelled; the state is unchanged\n")
        return CANCELLED_EXIT_STATUS
    cleared = reset(state)
    ctx.store.save(cleared)
    ctx.logger.info("the AMT state is now 'idle' (%s); the failed cycle was dropped", ctx.state_path)
    session.out.write(f"the AMT state is now 'idle' ({ctx.state_path})\n")
    return 0


# ---------------------------------------------------------------------------------------------
# The typer application
# ---------------------------------------------------------------------------------------------


class ModeltestsApp(typer.Typer):
    """The ``modeltests`` application: ``app()`` returns the exit status instead of calling ``sys.exit``.

    The console script wrapper passes the return value to ``sys.exit``; a usage error prints click's
    message on stderr and returns its status (2); ``--help`` returns 0; without a command the help page is
    printed on stderr with status 2 (``no_args_is_help``).
    """

    def __call__(self, *args: Any, **kwargs: Any) -> int:
        """Parse ``kwargs["args"]`` (default ``sys.argv[1:]``) and run; returns the exit status."""
        argv = kwargs.pop("args", None)
        tokens = list(sys.argv[1:] if argv is None else argv)
        try:
            status = super().__call__(*args, args=tokens, prog_name=PROGRAM, standalone_mode=False, **kwargs)
        except ClickException as error:
            error.show()
            return int(error.exit_code)
        return int(status or 0)


def _invocation(ctx: typer.Context) -> Invocation:
    invocation = ctx.obj
    if not isinstance(invocation, Invocation):
        raise AssertionError("the callback did not run")
    return invocation


def make_app(session_factory: Callable[[], Session]) -> ModeltestsApp:
    """The typer application whose commands run :func:`run_command` in ``session_factory()``.

    The module-level :data:`app` uses :meth:`Session.production`; :func:`main` passes a session over an
    injected ``Effects``.
    """
    application = ModeltestsApp(
        name=PROGRAM,
        help=HELP_DESCRIPTION,
        epilog=HELP_EPILOGUE,
        add_completion=False,
        rich_markup_mode=None,
        pretty_exceptions_enable=False,
        no_args_is_help=True,
        context_settings={"help_option_names": ["-h", "--help"]},
    )

    @application.callback()
    def modeltests(
        ctx: typer.Context,
        dir_: Annotated[
            Path, typer.Option("--dir", metavar="DIR", help=GLOBAL_OPTION_HELP["dir"], show_default=True)
        ] = Path(DEFAULT_DIR),
        report_repo: Annotated[
            Path | None,
            typer.Option("--report-repo", metavar="DIR", help=GLOBAL_OPTION_HELP["report-repo"]),
        ] = None,
        user: Annotated[
            str | None, typer.Option("--user", metavar="USER", help=GLOBAL_OPTION_HELP["user"])
        ] = None,
        mattermost_url_env: Annotated[
            str,
            typer.Option(
                "--mattermost-url-env", metavar="NAME", help=GLOBAL_OPTION_HELP["mattermost-url-env"]
            ),
        ] = MATTERMOST_URL_VARIABLE,
        publish: Annotated[
            bool, typer.Option("--publish/--no-publish", help=GLOBAL_OPTION_HELP["publish"])
        ] = True,
        compare_scenarios: Annotated[
            bool,
            typer.Option(
                "--compare-scenarios/--no-compare-scenarios", help=GLOBAL_OPTION_HELP["compare-scenarios"]
            ),
        ] = True,
        log_dir: Annotated[
            Path | None, typer.Option("--log-dir", metavar="DIR", help=GLOBAL_OPTION_HELP["log-dir"])
        ] = None,
        dry_run: Annotated[bool, typer.Option("--dry-run", help=GLOBAL_OPTION_HELP["dry-run"])] = False,
        force: Annotated[bool, typer.Option("--force", help=GLOBAL_OPTION_HELP["force"])] = False,
        no_wait: Annotated[bool, typer.Option("--no-wait", help=GLOBAL_OPTION_HELP["no-wait"])] = False,
    ) -> None:
        """Start and evaluate the REMIND automated model tests (AMT)."""
        options = GlobalOptions(
            dir=dir_,
            report_repo=report_repo,
            user=user,
            mattermost_url_variable=mattermost_url_env,
            publish=publish,
            compare_scenarios=compare_scenarios,
            log_dir=log_dir,
            dry_run=dry_run,
            force=force,
            no_wait=no_wait,
        )
        ctx.obj = Invocation(options, session_factory())

    @application.command(name=Command.START.value, help=COMMAND_HELP[Command.START])
    def start(
        ctx: typer.Context,
        force: Annotated[bool, typer.Option("--force", help=GLOBAL_OPTION_HELP["force"])] = False,
    ) -> int:
        invocation = _invocation(ctx)
        options = replace(invocation.options, force=invocation.options.force or force)
        return run_command(Command.START, options, invocation.session)

    @application.command(name=Command.EVALUATE.value, help=COMMAND_HELP[Command.EVALUATE])
    def evaluate(ctx: typer.Context) -> int:
        invocation = _invocation(ctx)
        return run_command(Command.EVALUATE, invocation.options, invocation.session)

    @application.command(name=Command.RUN.value, help=COMMAND_HELP[Command.RUN])
    def run(ctx: typer.Context) -> int:
        invocation = _invocation(ctx)
        return run_command(Command.RUN, invocation.options, invocation.session)

    @application.command(name=Command.STATUS.value, help=COMMAND_HELP[Command.STATUS])
    def status(ctx: typer.Context) -> int:
        invocation = _invocation(ctx)
        return run_command(Command.STATUS, invocation.options, invocation.session)

    @application.command(name=Command.RESET.value, help=COMMAND_HELP[Command.RESET])
    def reset_(
        ctx: typer.Context,
        yes: Annotated[bool, typer.Option("--yes", "-y", help="do not ask for confirmation")] = False,
    ) -> int:
        invocation = _invocation(ctx)
        return run_command(Command.RESET, invocation.options, invocation.session, yes=yes)

    return application


def main(
    argv: Sequence[str] | None = None,
    *,
    effects: Effects | None = None,
    out: TextIO | None = None,
    err: TextIO | None = None,
    read_line: LineReader | None = None,
) -> int:
    """Run ``modeltests argv`` in process and return the exit status.

    ``effects`` is the environment the commands run in (production when ``None``); ``out``, ``err`` and
    ``read_line`` default to the process's streams. Help and usage errors are printed by click on the
    process's streams. This is the entry the harnesses use (``migration/harness/run_amt_case.sh`` injects
    an ``Effects`` that records the webhook calls instead of posting them).
    """
    session = Session(
        effects=effects if effects is not None else ProductionEffects(),
        out=out if out is not None else sys.stdout,
        err=err if err is not None else sys.stderr,
        read_line=read_line if read_line is not None else read_stdin_line,
    )
    return make_app(lambda: session)(args=list(argv) if argv is not None else None)


app = make_app(Session.production)
"""The console script ``modeltests`` (``pyproject.toml`` ``[project.scripts]``)."""
