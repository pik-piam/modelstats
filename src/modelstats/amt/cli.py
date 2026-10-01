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
:class:`modelstats.env.DryRunEffects`). The two ``Rscript`` bridges (``select_scenarios.R``,
``add_to_data_changelog.R``) are NOT executed either (plan 03 section 2.3: no subprocess runs in a dry run): they
would run REMIND's ``scripts/start/*.R`` and magpie4, third-party code, unsandboxed with the caller's privileges and
outside the Effects boundary. They are logged like every other command and answered synthetically: an empty scenario
list (the dry run reports no scenario to start; the operator can run the logged ``Rscript .../select_scenarios.R ...``
command by hand to see the real list) and a successful changelog call that wrote nothing. Nothing below ``mydir``,
``gitdir`` or the state directory changes in a dry run; the one file written is the empty ``runsToStart.rds`` under
the dry run's private per-process ``tempdir()``. The directories must exist (``chdir`` is real).
"""

from __future__ import annotations

import dataclasses
import json
import os
import shlex
import subprocess
import sys
import warnings
from collections.abc import Mapping, Sequence
from typing import Annotated, TextIO

import pandas as pd
import typer

from modelstats import colors
from modelstats.amt import modeltests
from modelstats.amt.bridges import ADD_TO_DATA_CHANGELOG_SCRIPT, SELECT_SCENARIOS_SCRIPT, BridgeError
from modelstats.env import DryRunEffects, Effects, PathLike, default_effects
from modelstats.errors import RWarning
from modelstats.loop_runs import r_try_message, r_warnings_text
from modelstats.rdata_io import write_rds

__all__ = [
    "OPTION_FLAGS",
    "AmtDryRunEffects",
    "Options",
    "app",
    "bridge_dry_run_answer",
    "is_bridge_argv",
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
    "execute the reads, log every mutation as 'would ...' on stderr instead of performing it, send nothing; "
    "the two Rscript bridges (REMIND's scripts/start/*.R, magpie4) are not executed either and answer an empty "
    "scenario list"
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


_NOT_EXECUTED = "dry run: not executed"


def _bridge_option(argv: Sequence[str], name: str) -> str | None:
    """The value of ``--<name> VALUE`` in a bridge argv (``None`` when absent)."""
    for k, token in enumerate(argv):
        if token == f"--{name}" and k + 1 < len(argv):
            return argv[k + 1]
    return None


def bridge_dry_run_answer(argv: Sequence[str], cwd: str) -> str:
    """The stdout of a bridge a dry run did not execute: the JSON line of a successful bridge that found nothing.

    ``select_scenarios.R``: an empty scenario list (``nrow`` 0, no row names, no sources), and an empty data.frame
    written as RDS to the ``--out`` path (resolved against ``cwd`` as the script would), the one file a dry run
    writes; ``add_to_data_changelog.R``: ``ok`` without a row count, nothing written. ``r_version`` says
    ``dry run: not executed`` in both.
    """
    payload: dict[str, object]
    if os.path.basename(argv[1]) == SELECT_SCENARIOS_SCRIPT:
        out = _bridge_option(argv, "out")
        if out is not None:
            write_rds(os.path.join(cwd, out), pd.DataFrame())
        payload = {
            "bridge": "select_scenarios",
            "ok": True,
            "row_names": [],
            "columns": [],
            "nrow": 0,
            "out": out,
            "sources": [],
            "r_version": _NOT_EXECUTED,
        }
    else:
        payload = {
            "bridge": "add_to_data_changelog",
            "ok": True,
            "changelog": _bridge_option(argv, "changelog"),
            "version_id": _bridge_option(argv, "version-id"),
            "nrow": None,
            "magpie4_version": None,
            "r_version": _NOT_EXECUTED,
        }
    return json.dumps(payload) + "\n"


class AmtDryRunEffects(DryRunEffects):
    """``modeltests --dry-run``: :class:`~modelstats.env.DryRunEffects` with synthetic answers for the two bridges.

    No subprocess runs, the bridges included: ``select_scenarios.R`` would ``source()`` every ``scripts/start/*.R``
    of the checkout and ``add_to_data_changelog.R`` would run magpie4, third-party code outside the Effects
    boundary and outside any sandbox. A bridge command is logged as a ``run`` event like every other command and
    answered by :func:`bridge_dry_run_answer`: for ``select_scenarios.R`` an empty data.frame, also written as an
    RDS file to the bridge's ``--out`` path (the one file a dry run writes; the AMT passes a path under the dry
    run's private per-process ``tempdir()``, where ``start.select_runs_to_start`` reads it back), so the dry run
    goes on with an empty scenario list; for ``add_to_data_changelog.R`` ``ok`` with ``nrow`` null and nothing
    written. The ``report`` and ``answer`` arguments are those of the base class. File targets of the logged events
    are made absolute against the working directory at the time of the call (the AMT writes ``gRS.rds`` from inside
    ``output/`` and ``../.testsstatus`` from ``mydir``: a relative ``would write`` line would be ambiguous for the
    operator), the base class keeps them as given.
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
            self._would("run", shlex.join(argv), f"cwd {where}, answered status 0")
            answer = bridge_dry_run_answer(argv, where)
            return subprocess.CompletedProcess(list(argv), 0, answer if capture else "", "")
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
