"""The wheel's console scripts, built with ``uv build`` and run as installed programs, not source imports.

``migration/03-migration-plan.md`` section 5 ("CLI drop-in") and its phase 4 evidence ("installable with
``uv tool install`` on 3.14; the wheel's entry points tested, not source imports"),
``migration/06-user-requests.md`` section 7 phase 4. The module is in the golden tier because it reads one run
folder of the fixture snapshot (``fixtures_dir``); it also builds the wheel and installs it twice, which needs
``uv`` and the package index or a warm cache. The default tier keeps
``tests/packaging/test_console_scripts.py`` (the scripts next to the running interpreter, which in the wheel
job of ``.github/workflows/python.yml`` are the wheel's).

What is checked:

* ``uv build`` from the repository produces ``modelstats-<version>-py3-none-any.whl`` and the sdist;
* a fresh virtual environment (``uv venv --python 3.14``) holding the locked runtime set (``uv export --locked
  --no-dev``, hashes required) plus the wheel without re-resolving, as the wheel job does: its ``rs`` imports
  the package from the environment's site-packages, not from ``src/``, and ``rs <run>`` on a completed
  fixture run in local mode (this machine has no ``/p``) prints the same stdout bytes and exit status as the
  typer application run in this process over :class:`modelstats.env.ProductionEffects`, and the same stderr
  once the random ``Did you know?`` line is dropped (D-08), with and without colours (``R_CLI_NUM_COLORS``,
  the variable the goldens were made under);
* ``uv tool install <wheel> --python 3.14`` into a private tool directory installs ``rs`` and ``modeltests``
  on a 3.14 interpreter, and ``rs --help`` lists every option of ``03`` section 3.4 (D-11: the layout is
  typer's, the options are R's) plus the Python-only ``--found-in-slurm DIR``;
* the installed ``rs --found-in-slurm DIR`` prints one line on stdout and exits 0 (``03`` section 5; off the
  cluster the line is ``no``, as R's ``system(intern = TRUE)`` of a missing ``squeue`` yields no job).

``migration/harness/checks/cli_install_check.sh`` performs the same steps from the shell, for the cluster.
"""

from __future__ import annotations

import io
import os
import re
import shutil
import subprocess
import sys
from collections.abc import Mapping
from contextlib import redirect_stderr, redirect_stdout
from dataclasses import dataclass
from pathlib import Path

import pytest

from modelstats import __version__
from modelstats.cli import HINTS, Session, make_app
from modelstats.colors import Crayon
from modelstats.env import ProductionEffects

pytestmark = pytest.mark.golden

REPO = Path(__file__).resolve().parent.parent.parent
PYTHON_VERSION = "3.14"
WHEEL_NAME = f"modelstats-{__version__}-py3-none-any.whl"
SDIST_NAME = f"modelstats-{__version__}.tar.gz"
RUN_FOLDER = "p/projects/remind/modeltests/remind/output/SSP2-NPi-AMT_2026-09-28_10.30.27"
"""A completed REMIND run: its Runtime is the GAMS wall time (``R/getRunStatus.R:311``), not the clock."""
UV_TIMEOUT = 600
"""Seconds per ``uv`` call: a cold cache downloads gamsapi, scipy and pandas."""
RS_TIMEOUT = 120

R_OPTIONS: tuple[tuple[str, str, str | None], ...] = (
    ("-A", "--amt", None),
    ("-b", "--nocolor", None),
    ("-C", "--current", None),
    ("-d", "--daysback", "N"),
    ("-f", "--filter", "REGEX"),
    ("-l", "--last", None),
    ("-m", "--magpie", None),
    ("-p", "--prompt", None),
    ("-s", "--sanity", None),
    ("-t", "--time", None),
    ("-u", "--user", "USER"),
    ("-h", "--help", None),
)
"""``R/commandLineInterface.R:35-47`` as ``03`` section 3.4 lists them: short flag, long flag, metavar."""
PYTHON_OPTION = "--found-in-slurm DIR"
USAGE_LINE = "Usage: rs [OPTIONS] [PATH]"

COLOUR_VARIABLES = ("R_CLI_NUM_COLORS", "NO_COLOR")
"""The variables crayon reads off a tty (``modelstats.colors.num_ansi_colours``); the test sets them."""
COLOUR_SETTINGS: dict[str, dict[str, str]] = {"plain": {}, "colours": {"R_CLI_NUM_COLORS": "256"}}

HINT_LINE = re.compile(
    rb"^(?:\x1b\[36m)?(?:\xe2\x84\xb9|i)(?:\x1b\[39m)? Did you know\? ([^\n]*)\n", re.MULTILINE
)
"""The ``cli_alert_info("Did you know? {sample(hints, 1)}")`` line, either bullet, coloured or not (D-08)."""


@dataclass(frozen=True)
class Outcome:
    """What one ``rs`` invocation left behind: the two streams as bytes and the exit status."""

    stdout: bytes
    stderr: bytes
    status: int


@dataclass(frozen=True)
class Installation:
    """A virtual environment with the wheel installed: its ``bin`` directory and interpreter."""

    root: Path

    @property
    def bin(self) -> Path:
        return self.root / "bin"

    @property
    def python(self) -> Path:
        return self.bin / "python"

    def script(self, name: str) -> Path:
        path = self.bin / name
        assert path.is_file(), f"{name} is not installed in {self.bin}"
        return path


# ---------------------------------------------------------------------------------------------
# uv: build, the fresh environment, the tool installation
# ---------------------------------------------------------------------------------------------


def uv_environment() -> dict[str, str]:
    """The process environment for ``uv``: copied links are quiet when the cache is on another filesystem."""
    environment = dict(os.environ)
    environment.setdefault("UV_LINK_MODE", "copy")
    return environment


def run_uv(uv: str, *args: str, env: Mapping[str, str] | None = None) -> subprocess.CompletedProcess[str]:
    """``uv <args>`` from the repository root; a non-zero status fails the test with uv's stderr."""
    result = subprocess.run(
        [uv, *args],
        cwd=REPO,
        env=uv_environment() if env is None else env,
        capture_output=True,
        text=True,
        check=False,
        timeout=UV_TIMEOUT,
    )
    if result.returncode != 0:
        pytest.fail(f"uv {' '.join(args)} failed ({result.returncode}):\n{result.stderr}")
    return result


@pytest.fixture(scope="session")
def uv() -> str:
    """The ``uv`` executable (the recorded tooling decision), or a skip where it is not installed."""
    found = shutil.which("uv")
    if found is None:
        pytest.skip("uv is not installed (migration/03-migration-plan.md section 5)")
    return found


@pytest.fixture(scope="session")
def dist(uv: str, tmp_path_factory: pytest.TempPathFactory) -> Path:
    """``uv build`` into a private directory (the repository's ``dist/`` is left alone)."""
    out_dir = tmp_path_factory.mktemp("dist")
    run_uv(uv, "build", "--out-dir", str(out_dir))
    return out_dir


@pytest.fixture(scope="session")
def wheel(dist: Path) -> Path:
    return dist / WHEEL_NAME


@pytest.fixture(scope="session")
def locked_requirements(uv: str, tmp_path_factory: pytest.TempPathFactory) -> Path:
    """The locked runtime dependency set with hashes, without the project and the dev group."""
    path = tmp_path_factory.mktemp("lock") / "requirements.txt"
    run_uv(uv, "export", "--locked", "--no-emit-project", "--no-dev", "-o", str(path))
    return path


@pytest.fixture(scope="session")
def fresh_venv(
    uv: str, wheel: Path, locked_requirements: Path, tmp_path_factory: pytest.TempPathFactory
) -> Installation:
    """A fresh 3.14 environment: the locked dependencies, then the wheel with ``--no-deps`` (as CI does)."""
    root = tmp_path_factory.mktemp("venv") / "fresh"
    run_uv(uv, "venv", "--python", PYTHON_VERSION, str(root))
    python = str(root / "bin" / "python")
    run_uv(uv, "pip", "install", "--python", python, "--require-hashes", "-r", str(locked_requirements))
    run_uv(uv, "pip", "install", "--python", python, "--no-deps", str(wheel))
    return Installation(root)


@pytest.fixture(scope="session")
def tool_install(uv: str, wheel: Path, tmp_path_factory: pytest.TempPathFactory) -> Installation:
    """``uv tool install <wheel> --python 3.14`` with the tool and bin directories under a private root."""
    root = tmp_path_factory.mktemp("tool")
    env = {**uv_environment(), "UV_TOOL_DIR": str(root / "tools"), "UV_TOOL_BIN_DIR": str(root / "bin")}
    run_uv(uv, "tool", "install", "--python", PYTHON_VERSION, str(wheel), env=env)
    return Installation(root / "tools" / "modelstats")


# ---------------------------------------------------------------------------------------------
# Running rs: the installed script and the application in this process
# ---------------------------------------------------------------------------------------------


def rs_environment(colours: Mapping[str, str]) -> dict[str, str]:
    """This process's environment without the colour variables, plus the colour setting under test."""
    return {**{k: v for k, v in os.environ.items() if k not in COLOUR_VARIABLES}, **colours}


def run_script(script: Path, *args: str, env: Mapping[str, str]) -> Outcome:
    """The installed console script as a subprocess with piped streams (no tty, as a script would see it)."""
    result = subprocess.run(
        [str(script), *args], env=env, capture_output=True, check=False, timeout=RS_TIMEOUT, cwd=REPO
    )
    return Outcome(result.stdout, result.stderr, result.returncode)


def run_in_process(*args: str, env: Mapping[str, str]) -> Outcome:
    """The typer application in this process over the real environment and in-memory streams."""
    out, err = io.StringIO(), io.StringIO()
    crayon = Crayon.from_terminal(is_tty=False, environ=env)
    session = Session(
        effects=ProductionEffects(env),
        out=out,
        err=err,
        table_crayon=crayon,
        alert_crayon=crayon,
        pick_hint=lambda hints: hints[0],
    )
    with redirect_stdout(out), redirect_stderr(err):
        status = make_app(lambda: session)(args=list(args), prog_name="rs")
    return Outcome(out.getvalue().encode("utf-8", "surrogateescape"), err.getvalue().encode("utf-8"), status)


def without_hint(stderr: bytes) -> tuple[bytes, bytes]:
    """The stderr without its one ``Did you know?`` line (D-08), and the hint that line carried."""
    found = HINT_LINE.search(stderr)
    assert found is not None, f"no hint line in {stderr!r}"
    assert HINT_LINE.search(stderr, found.end()) is None, "more than one hint line"
    return stderr[: found.start()] + stderr[found.end() :], found.group(1)


def assert_help_page(outcome: Outcome) -> None:
    """``rs --help`` (D-11): exit 0, nothing on stderr, every R option with its metavar, the Python one."""
    assert outcome.status == 0, outcome.stderr
    assert outcome.stderr == b""
    page = outcome.stdout.decode("utf-8")
    assert page.startswith(USAGE_LINE), page.splitlines()[:1]
    for short, long, metavar in R_OPTIONS:
        assert f"{short}, {long}" in page, (short, long)
        if metavar is not None:
            assert f"{long} {metavar}" in page, (long, metavar)
    assert PYTHON_OPTION in page


# ---------------------------------------------------------------------------------------------
# The wheel and the fresh environment
# ---------------------------------------------------------------------------------------------


def test_uv_build_produces_the_wheel_and_the_sdist(dist: Path) -> None:
    assert sorted(path.name for path in dist.iterdir() if path.suffix in (".whl", ".gz")) == [
        WHEEL_NAME,
        SDIST_NAME,
    ]


def test_the_fresh_environment_imports_the_wheel_not_the_checkout(fresh_venv: Installation) -> None:
    """The console script is the wheel's entry point: its interpreter and package live in the environment."""
    shebang = fresh_venv.script("rs").read_text(encoding="utf-8").splitlines()[0]
    assert shebang.startswith(f"#!{fresh_venv.bin}/python"), shebang
    result = subprocess.run(
        [
            str(fresh_venv.python),
            "-c",
            "import modelstats, sys; print(modelstats.__file__); print(sys.version)",
        ],
        capture_output=True,
        text=True,
        check=True,
        timeout=RS_TIMEOUT,
        cwd=REPO,
    )
    module_file, version = result.stdout.splitlines()[:2]
    assert Path(module_file).is_relative_to(fresh_venv.root), module_file
    assert not Path(module_file).is_relative_to(REPO / "src"), module_file
    assert version.startswith(f"{PYTHON_VERSION}."), version


def test_the_fresh_environment_scripts_answer_help(fresh_venv: Installation) -> None:
    env = rs_environment({})
    assert_help_page(run_script(fresh_venv.script("rs"), "--help", env=env))
    modeltests = run_script(fresh_venv.script("modeltests"), "--help", env=env)
    assert modeltests.status == 0, modeltests.stderr
    assert modeltests.stdout.startswith(b"Usage: modeltests")


@pytest.mark.parametrize("colours", COLOUR_SETTINGS.values(), ids=COLOUR_SETTINGS.keys())
def test_installed_rs_equals_the_in_process_application(
    fresh_venv: Installation, fixtures_dir: Path, colours: Mapping[str, str]
) -> None:
    """``rs <run>`` from the wheel and from this process: same stdout and status, same stderr but the hint."""
    run_folder = fixtures_dir / RUN_FOLDER
    assert run_folder.is_dir(), run_folder
    env = rs_environment(colours)
    installed = run_script(fresh_venv.script("rs"), str(run_folder), env=env)
    in_process = run_in_process(str(run_folder), env=env)

    assert installed.status == 0, installed.stderr
    assert installed.status == in_process.status
    assert installed.stdout == in_process.stdout
    assert run_folder.name.encode() in installed.stdout  # a table was printed, not a "No runs found"
    assert b"Normal completion" in installed.stdout
    assert (b"\x1b[" in installed.stdout) == bool(colours)
    installed_stderr, installed_hint = without_hint(installed.stderr)
    in_process_stderr, in_process_hint = without_hint(in_process.stderr)
    assert installed_stderr == in_process_stderr
    assert installed_hint.decode("utf-8") in HINTS
    assert in_process_hint.decode("utf-8") == HINTS[0]


def test_installed_rs_found_in_slurm_prints_one_line(fresh_venv: Installation, fixtures_dir: Path) -> None:
    """``rs --found-in-slurm DIR``: the inSlurm string plus a newline on stdout, exit 0 (``03`` section 5)."""
    outcome = run_script(
        fresh_venv.script("rs"), "--found-in-slurm", str(fixtures_dir / RUN_FOLDER), env=rs_environment({})
    )
    assert outcome.status == 0, outcome.stderr
    assert outcome.stdout.endswith(b"\n") and outcome.stdout.count(b"\n") == 1, outcome.stdout
    if shutil.which("squeue") is None:
        assert outcome.stdout == b"no\n"


# ---------------------------------------------------------------------------------------------
# uv tool install
# ---------------------------------------------------------------------------------------------


def test_uv_tool_install_runs_on_python_3_14(tool_install: Installation) -> None:
    result = subprocess.run(
        [str(tool_install.python), "-c", "import sys; print(sys.version_info[0], sys.version_info[1])"],
        capture_output=True,
        text=True,
        check=True,
        timeout=RS_TIMEOUT,
    )
    assert result.stdout.split() == PYTHON_VERSION.split(".")


def test_uv_tool_install_rs_help_lists_every_option(tool_install: Installation) -> None:
    assert_help_page(run_script(tool_install.script("rs"), "--help", env=rs_environment({})))


def test_uv_tool_install_modeltests_help(tool_install: Installation) -> None:
    outcome = run_script(tool_install.script("modeltests"), "--help", env=rs_environment({}))
    assert outcome.status == 0, outcome.stderr
    assert outcome.stdout.startswith(b"Usage: modeltests")


def test_this_interpreter_is_3_14_too() -> None:
    """The in-process side of the comparison runs on the same minor version as the installations."""
    assert sys.version_info[:2] == tuple(int(part) for part in PYTHON_VERSION.split("."))
