"""Console script ``modeltests``: the cron entry point of the AMT (plan 03 sections 2.3 and 5).

The command line carries the arguments of ``modeltests()`` (``--mydir --gitdir --model --user --email/--no-email
--comp-scen/--no-comp-scen``), the Mattermost webhook as the NAME of an environment variable
(``--mattermost-token-env NAME``: the cron script loads the variable from a file only the cron user can read; the
URL itself never appears on a command line) and ``--dry-run``. :func:`run_cli` is what ``Rscript -e
'modelstats::modeltests(...)'`` adds around the function: the deferred ``Warning message(s):`` block at the end,
and for an R error ``Error in <call> : <message>`` (``Error: <message>`` without a call), the ``In addition:``
warnings and ``Execution halted`` with exit status 1 (the ``Calls:`` traceback line is not reproduced: no golden
pins it, the AMT goldens record R's error through ``try()``).

Colours: :func:`modelstats.colors.enable_from_environment` once on stdout, as ``rs`` does, so that the tables
captured into the Mattermost message carry escapes exactly where crayon would (``R_CLI_NUM_COLORS`` first, then
``NO_COLOR``, then the tty check: a cron job without a tty gets none).

``--dry-run`` installs :class:`AmtDryRunEffects`: reads execute for real, every mutation (writes, copies, renames,
deletions, subprocesses, the HTTP POST, sleeping) is logged as ``would ...`` on stderr and answered synthetically
(an empty scheduler, a fixed commit for ``git log -1``, status 0 for everything else; see
:class:`modelstats.env.DryRunEffects`), with one exception documented here: the two ``Rscript`` bridges
(``select_scenarios.R``, ``add_to_data_changelog.R``) are executed for real, because they are reads of the model
directory whose only output is a file under ``tempdir()`` (the list of scenarios to start, the changelog copy) and a
dry run that reported zero scenarios would be useless to the operator. Nothing below ``mydir``, ``gitdir`` or the
state directory changes in a dry run; the directories must exist (``chdir`` is real).
"""

from __future__ import annotations

import dataclasses
import os
import shlex
import subprocess
import sys
import warnings
from collections.abc import Mapping, Sequence
from typing import Annotated, TextIO

import typer

from modelstats import colors
from modelstats.amt import modeltests
from modelstats.amt.bridges import ADD_TO_DATA_CHANGELOG_SCRIPT, SELECT_SCENARIOS_SCRIPT, BridgeError
from modelstats.env import DryRunEffects, Effects, PathLike, ProductionEffects, default_effects
from modelstats.errors import RWarning
from modelstats.loop_runs import r_try_message, r_warnings_text

__all__ = [
    "OPTION_FLAGS",
    "AmtDryRunEffects",
    "Options",
    "app",
    "main",
    "run_cli",
]

HELP_MYDIR = "path to the folder where the model is found (the AMT checkout; the cron job passes a trailing slash)"
HELP_GITDIR = "path to the git clone that sends the report via email (the testing_suite repository)"
HELP_MODEL = "model name: REMIND or MAgPIE"
HELP_USER = "the user that starts the jobs and commits the changes (squeue -u USER)"
HELP_EMAIL = "whether the README (and the MAgPIE data changelog) is committed and pushed to gitdir"
HELP_COMP_SCEN = "whether compareScenarios2 runs for converged REMIND runs"
HELP_TOKEN_ENV = (
    "NAME of the environment variable holding the Mattermost webhook URL; unset or empty: no notification is sent"
)
HELP_DRY_RUN = (
    "execute the reads, log every mutation as 'would ...' on stderr instead of performing it, send nothing "
    "(the two Rscript bridges still run: they only read the checkout and write under the temporary directory)"
)

DESCRIPTION = """\b
Run the automated model tests (AMT) of REMIND or MAgPIE: the cron entry point modeltests().
Reads <mydir>/../.testsstatus and either starts the test runs ('next:start') or evaluates them ('next:evaluate'),
then writes the next state; anything else does nothing.
"""

#: The option flags, what ``modeltests -h`` must list.
OPTION_FLAGS: tuple[str, ...] = (
    "--mydir",
    "--gitdir",
    "--model",
    "--user",
    "--email",
    "--no-email",
    "--comp-scen",
    "--no-comp-scen",
    "--mattermost-token-env",
    "--dry-run",
    "--help",
)

_BRIDGE_SCRIPTS = (SELECT_SCENARIOS_SCRIPT, ADD_TO_DATA_CHANGELOG_SCRIPT)


@dataclasses.dataclass(frozen=True)
class Options:
    """The parsed command line, named like the arguments of ``modeltests()``."""

    mydir: str = "."
    gitdir: str | None = None
    model: str | None = None
    user: str | None = None
    email: bool = True
    comp_scen: bool = True
    mattermost_token_env: str | None = None
    dry_run: bool = False


# ---------------------------------------------------------------------------
# dry run
# ---------------------------------------------------------------------------


def is_bridge_argv(argv: Sequence[str]) -> bool:
    """Whether ``argv`` runs one of the package's ``bridge_scripts`` (``Rscript <dir>/<script>.R ...``)."""
    return len(argv) >= 2 and os.path.basename(argv[1]) in _BRIDGE_SCRIPTS


class AmtDryRunEffects(DryRunEffects):
    """``modeltests --dry-run``: :class:`~modelstats.env.DryRunEffects` with the two bridges executed for real.

    The bridges read the model directory and write only under ``tempdir()`` (the ``--out`` RDS of
    ``select_scenarios.R``, the changelog copy of ``add_to_data_changelog.R``); every other subprocess is logged
    and answered synthetically. The ``report`` and ``answer`` arguments are those of the base class. File targets
    of the logged events are made absolute against the working directory at the time of the call (the AMT writes
    ``gRS.rds`` from inside ``output/`` and ``../.testsstatus`` from ``mydir``: a relative ``would write`` line
    would be ambiguous for the operator), the base class keeps them as given.
    """

    _FILE_ACTIONS = frozenset({"write", "delete", "create directory"})
    _PAIR_ACTIONS = frozenset({"copy", "rename"})

    def _would(self, action: str, target: str, detail: str = "", data: bytes | None = None) -> None:
        if action in self._FILE_ACTIONS:
            target = os.path.abspath(target)
        elif action in self._PAIR_ACTIONS:
            target = " -> ".join(os.path.abspath(part) for part in target.split(" -> "))
        super()._would(action, target, detail, data)

    def run(
        self,
        argv: Sequence[str] | str,
        cwd: PathLike | None = None,
        env: Mapping[str, str] | None = None,
        input: str | None = None,
        *,
        shell: bool = False,
        capture: bool = True,
    ) -> subprocess.CompletedProcess[str]:
        if not shell and not isinstance(argv, str) and is_bridge_argv(argv):
            where = os.getcwd() if cwd is None else os.path.abspath(os.fspath(cwd))
            self._would("run (executed: a read-only bridge)", shlex.join(argv), f"cwd {where}")
            return ProductionEffects.run(self, argv, cwd, env, input, shell=shell, capture=capture)
        return super().run(argv, cwd, env, input, shell=shell, capture=capture)


# ---------------------------------------------------------------------------
# the process around modeltests() (Rscript)
# ---------------------------------------------------------------------------


def _r_warnings(caught: Sequence[warnings.WarningMessage]) -> list[warnings.WarningMessage]:
    """The recorded warnings R would have deferred (the port's :class:`RWarning` ones); Python's own are dropped."""
    return [item for item in caught if isinstance(item.message, RWarning)]


def _error_text(exc: BaseException) -> str:
    """Rscript's top-level error line: ``Error in <call> : <message>`` with a call, ``Error: <message>`` without."""
    call = getattr(exc, "call", None)
    if isinstance(call, str) and call:
        return r_try_message(exc)
    return f"Error: {exc}\n"


def _resolve_token(opt: Options, effects: Effects, stderr: TextIO) -> str | None:
    """The webhook URL from the environment variable named on the command line (``None``: no notification)."""
    if not opt.mattermost_token_env:
        return None
    value = effects.getenv(opt.mattermost_token_env)
    if value == "":
        stderr.write(
            f"modeltests: the environment variable {opt.mattermost_token_env} is not set or empty: "
            "no Mattermost notification will be sent\n"
        )
        return None
    return value


def run_cli(opt: Options, effects: Effects | None = None) -> int:
    """Run ``modeltests`` with parsed options; returns the exit status (0, or 1 for an R error).

    Streams are ``sys.stdout`` / ``sys.stderr`` as they are at call time. ``effects`` defaults to the production
    Effects, or to :class:`AmtDryRunEffects` reporting to stderr under ``--dry-run``.
    """
    stdout, stderr = sys.stdout, sys.stderr
    if effects is not None:
        eff = effects
    elif opt.dry_run:

        def report(line: str) -> None:
            stderr.write(f"dry run: {line}\n")
            stderr.flush()

        eff = AmtDryRunEffects(report=report)
    else:
        eff = default_effects()
    colors.enable_from_environment(stdout)
    token = _resolve_token(opt, eff, stderr)
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        try:
            modeltests(
                mydir=opt.mydir,
                gitdir=opt.gitdir,
                model=opt.model,
                user=opt.user,
                email=opt.email,
                comp_scen=opt.comp_scen,
                mattermost_token=token,
                effects=eff,
            )
        except BridgeError as exc:
            # R has no counterpart (the bridged code ran in-process): the run stops like any uncaught error
            stdout.flush()
            stderr.write(f"Error: bridge {shlex.join(exc.argv)} failed: {exc.reason}\n")
            if exc.stderr:
                stderr.write(exc.stderr if exc.stderr.endswith("\n") else exc.stderr + "\n")
            stderr.write(r_warnings_text(_r_warnings(caught), in_addition=True))
            stderr.write("Execution halted\n")
            return 1
        except Exception as exc:  # noqa: BLE001 - every R error condition reaching the top level
            stdout.flush()
            stderr.write(_error_text(exc))
            stderr.write(r_warnings_text(_r_warnings(caught), in_addition=True))
            stderr.write("Execution halted\n")
            return 1
    stdout.flush()
    stderr.write(r_warnings_text(_r_warnings(caught)))
    if isinstance(eff, DryRunEffects):
        stderr.write(f"dry run: {len(eff.events)} mutation(s) logged, nothing was changed\n")
    return 0


# ---------------------------------------------------------------------------
# typer
# ---------------------------------------------------------------------------

app = typer.Typer(
    add_completion=False,
    rich_markup_mode=None,
    pretty_exceptions_enable=False,
    context_settings={"help_option_names": ["-h", "--help"], "terminal_width": 100, "max_content_width": 100},
)


@app.command(name="modeltests", help=DESCRIPTION, options_metavar="[OPTION]")
def modeltests_command(
    mydir: Annotated[str, typer.Option("--mydir", metavar="DIR", help=HELP_MYDIR)] = ".",
    gitdir: Annotated[str | None, typer.Option("--gitdir", metavar="DIR", help=HELP_GITDIR, show_default=False)] = None,
    model: Annotated[str | None, typer.Option("--model", metavar="NAME", help=HELP_MODEL, show_default=False)] = None,
    user: Annotated[str | None, typer.Option("--user", metavar="USER", help=HELP_USER, show_default=False)] = None,
    email: Annotated[bool, typer.Option("--email/--no-email", help=HELP_EMAIL)] = True,
    comp_scen: Annotated[bool, typer.Option("--comp-scen/--no-comp-scen", help=HELP_COMP_SCEN)] = True,
    mattermost_token_env: Annotated[
        str | None, typer.Option("--mattermost-token-env", metavar="NAME", help=HELP_TOKEN_ENV, show_default=False)
    ] = None,
    dry_run: Annotated[bool, typer.Option("--dry-run", help=HELP_DRY_RUN)] = False,
) -> None:
    opt = Options(
        mydir=mydir,
        gitdir=gitdir,
        model=model,
        user=user,
        email=email,
        comp_scen=comp_scen,
        mattermost_token_env=mattermost_token_env,
        dry_run=dry_run,
    )
    status = run_cli(opt)
    if status != 0:
        raise typer.Exit(code=status)


def _reconfigure(stream: object) -> None:
    """Write (and read) file names byte for byte, like R: undecodable bytes survive through surrogateescape."""
    reconfigure = getattr(stream, "reconfigure", None)
    if reconfigure is None:
        return
    try:
        reconfigure(errors="surrogateescape")
    except ValueError, OSError:
        pass


def main() -> None:
    """Entry point of the ``modeltests`` console script."""
    for stream in (sys.stdin, sys.stdout, sys.stderr):
        _reconfigure(stream)
    try:
        app(prog_name="modeltests")
    except SystemExit as exc:
        if exc.code == 2:
            # typer's usage error; Rscript would stop with an error, i.e. status 1 (as rs maps it, D-12)
            raise SystemExit(1) from None
        raise
