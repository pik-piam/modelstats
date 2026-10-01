"""Packaging tier (``-m packaging``): the wheel builds, installs into a fresh venv and its console scripts work.

Everything here runs the *installed* ``rs`` and ``modeltests`` scripts of a venv that only holds the wheel (plan 03
phase 4: "the wheel's entry points tested, not source imports"). The worktree's own environment is used for one
thing only: the in-process ``loop_runs`` / ``run_cli`` output that the installed ``rs -b <folder>`` must reproduce.

The fixture folder is a finished REMIND run of the fixture tree (its ``Runtime`` comes from ``runstatistics.rda``,
not from the clock, so the two invocations print the same table); the comparison is skipped with a clear message
when the fixture tree is absent (CI). The ``--found-in-slurm`` contract is checked with a canned ``squeue`` on the
PATH (success path) and with an empty PATH (error path: the query cannot run), so it does not depend on the machine.
"""

from __future__ import annotations

import contextlib
import dataclasses
import io
import os
import shutil
import subprocess
import sys
import zipfile
from collections.abc import Iterator, Mapping
from pathlib import Path

import pytest

pytestmark = pytest.mark.packaging

REPO = Path(__file__).resolve().parents[2]
PYTHON_VERSION = "3.14"
BUILD_TIMEOUT = 300
INSTALL_TIMEOUT = 900  # a cold uv cache downloads gamspy_base (about 100 MB)
RUN_TIMEOUT = 120

#: The options of plan 03 section 3.4 (``R/commandLineInterface.R``) plus the help alias; this copy is deliberately
#: independent of ``modelstats.cli.OPTION_FLAGS`` so that the installed script, not the source, is what is checked.
OPTIONS: tuple[tuple[str, str], ...] = (
    ("-A", "--amt"),
    ("-b", "--nocolor"),
    ("-C", "--current"),
    ("-d", "--daysback"),
    ("-f", "--filter"),
    ("-l", "--last"),
    ("-m", "--magpie"),
    ("-p", "--prompt"),
    ("-s", "--sanity"),
    ("-t", "--time"),
    ("-u", "--user"),
    ("-h", "--help"),
)
FOUND_IN_SLURM_FLAG = "--found-in-slurm"
USAGE_LINE = "Usage: rs [OPTION] [PATH]"

#: A finished run of the fixture tree (``runstatistics.rda`` with ``timeGAMSEnd``): the same table every time.
FIXTURE_RUN = REPO / "migration/fixtures/p/projects/remind/modeltests/remind/output/testOneRegi"

#: Environment variables that switch colours on or off (crayon / cli rules); scrubbed so both sides print plain text.
COLOUR_VARIABLES = ("R_CLI_NUM_COLORS", "NO_COLOR", "COLORTERM", "FORCE_COLOR", "CLICOLOR_FORCE")


@dataclasses.dataclass(frozen=True)
class InstalledWheel:
    """The built wheel and the fresh venv it was installed into."""

    wheel: Path
    venv: Path

    @property
    def python(self) -> Path:
        return self.venv / "bin" / "python"

    @property
    def rs(self) -> Path:
        return self.venv / "bin" / "rs"

    @property
    def modeltests(self) -> Path:
        return self.venv / "bin" / "modeltests"


def _uv() -> str:
    """The uv binary: the one running this pytest (``uv run`` exports ``UV``) or the first on the PATH."""
    candidate = os.environ.get("UV") or shutil.which("uv")
    if not candidate:
        pytest.fail("uv is not available: the packaging tier needs uv to build and install the wheel")
    return candidate


def _uv_env() -> dict[str, str]:
    """The environment for uv child processes: the project's own venv must not leak into the fresh one."""
    env = dict(os.environ)
    env.pop("VIRTUAL_ENV", None)
    return env


def _run(
    argv: list[str], *, cwd: Path, timeout: int, env: Mapping[str, str] | None = None
) -> subprocess.CompletedProcess[bytes]:
    try:
        return subprocess.run(
            argv,
            cwd=cwd,
            env=dict(env) if env is not None else None,
            stdin=subprocess.DEVNULL,
            capture_output=True,
            timeout=timeout,
            check=False,
        )
    except subprocess.TimeoutExpired as exc:
        pytest.fail(f"{' '.join(argv)} exceeded {timeout} s: {exc}")


def _check(proc: subprocess.CompletedProcess[bytes], what: str) -> None:
    if proc.returncode != 0:
        pytest.fail(
            f"{what} failed with status {proc.returncode}\n"
            f"stdout:\n{proc.stdout.decode('utf-8', 'replace')}\nstderr:\n{proc.stderr.decode('utf-8', 'replace')}"
        )


def _script_env() -> dict[str, str]:
    """The environment of the installed scripts: no colour switches, nothing from the worktree."""
    env = dict(os.environ)
    for name in COLOUR_VARIABLES:
        env.pop(name, None)
    env.pop("VIRTUAL_ENV", None)
    env.pop("PYTHONPATH", None)
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    return env


@pytest.fixture(scope="session")
def installed(tmp_path_factory: pytest.TempPathFactory) -> InstalledWheel:
    """``uv build`` into a temporary directory, ``uv venv`` + ``uv pip install`` of the wheel; once per session."""
    uv = _uv()
    root = tmp_path_factory.mktemp("wheel")
    dist = root / "dist"
    _check(_run([uv, "build", "--out-dir", str(dist)], cwd=REPO, timeout=BUILD_TIMEOUT, env=_uv_env()), "uv build")
    wheels = sorted(dist.glob("modelstats-*.whl"))
    assert len(wheels) == 1, f"expected exactly one wheel in {dist}, found {wheels}"
    venv = root / "venv"
    _check(
        _run([uv, "venv", "--python", PYTHON_VERSION, str(venv)], cwd=root, timeout=INSTALL_TIMEOUT, env=_uv_env()),
        "uv venv",
    )
    _check(
        _run(
            [uv, "pip", "install", "--python", str(venv / "bin" / "python"), str(wheels[0])],
            cwd=root,
            timeout=INSTALL_TIMEOUT,
            env=_uv_env(),
        ),
        "uv pip install",
    )
    result = InstalledWheel(wheel=wheels[0], venv=venv)
    for script in (result.rs, result.modeltests):
        assert script.is_file() and os.access(script, os.X_OK), f"console script {script} is missing"
    return result


# ---------------------------------------------------------------------------
# the wheel
# ---------------------------------------------------------------------------


def test_wheel_holds_the_package_only(installed: InstalledWheel) -> None:
    """The wheel contains ``modelstats/`` (with ``py.typed``) and its dist-info; no R files, tests or fixtures."""
    with zipfile.ZipFile(installed.wheel) as archive:
        names = archive.namelist()
        entry_points = archive.read("modelstats-0.31.0.dist-info/entry_points.txt").decode("utf-8")
    assert "modelstats/py.typed" in names
    assert "modelstats/__init__.py" in names
    assert "modelstats/cli.py" in names
    stray = [n for n in names if not (n.startswith("modelstats/") or n.startswith("modelstats-0.31.0.dist-info/"))]
    assert stray == [], f"unexpected files in the wheel: {stray}"
    assert "rs = modelstats.cli:main" in entry_points
    assert "modeltests = modelstats.amt.cli:main" in entry_points


def test_installed_package_is_the_wheel_not_the_source(installed: InstalledWheel) -> None:
    """The fresh venv imports modelstats from its own site-packages, never from ``src/`` of this checkout."""
    proc = _run(
        [str(installed.python), "-c", "import modelstats; print(modelstats.__file__); print(modelstats.__version__)"],
        cwd=installed.venv,
        timeout=RUN_TIMEOUT,
        env=_script_env(),
    )
    _check(proc, "python -c 'import modelstats'")
    location, version = proc.stdout.decode("utf-8").split()
    assert Path(location).is_relative_to(installed.venv), location
    assert not Path(location).is_relative_to(REPO / "src"), location
    assert version == "0.31.0"


# ---------------------------------------------------------------------------
# the console scripts
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("flag", ["-h", "--help"])
def test_installed_rs_help_lists_every_option(installed: InstalledWheel, flag: str) -> None:
    """``rs -h`` from the wheel: exit 0, R's usage line, the eleven options of 3.4, ``-h/--help``, the contract flag."""
    proc = _run([str(installed.rs), flag], cwd=installed.venv, timeout=RUN_TIMEOUT, env=_script_env())
    _check(proc, f"rs {flag}")
    text = proc.stdout.decode("utf-8")
    assert text.splitlines()[0] == USAGE_LINE, text
    assert proc.stderr == b"", proc.stderr
    missing = [f"{short}, {long}" for short, long in OPTIONS if f"{short}, {long}" not in text]
    assert missing == [], f"rs {flag} does not list {missing}:\n{text}"
    assert FOUND_IN_SLURM_FLAG in text, text


def test_installed_modeltests_help_exits_0(installed: InstalledWheel) -> None:
    proc = _run([str(installed.modeltests), "--help"], cwd=installed.venv, timeout=RUN_TIMEOUT, env=_script_env())
    _check(proc, "modeltests --help")
    assert proc.stdout.decode("utf-8").startswith("Usage: modeltests"), proc.stdout


def _canned_squeue(tmp_path: Path, line: str) -> Path:
    """A ``squeue`` on its own PATH entry that prints one canned six-field line (``%u %Z %j %M %T %q``)."""
    bin_dir = tmp_path / "bin"
    bin_dir.mkdir()
    (bin_dir / "squeue_all.txt").write_text(line + "\n", encoding="utf-8")
    script = bin_dir / "squeue"
    script.write_text('#!/bin/sh\ncat "$(dirname "$0")/squeue_all.txt"\n', encoding="utf-8")
    script.chmod(0o755)
    return bin_dir


def test_installed_rs_found_in_slurm_success_path(installed: InstalledWheel, tmp_path: Path) -> None:
    """The contract from the wheel: exactly the ``foundInSlurm`` string plus a newline on stdout, nothing else, exit 0.

    A canned ``squeue`` reports the directory as another user's running job, so the value is that user's name
    (R's ``runuser`` branch) whoever runs the test.
    """
    run = tmp_path / "SSP2-NPi"
    run.mkdir()
    bin_dir = _canned_squeue(tmp_path, f"someone {run.resolve()} job-SSP2-NPi 12:34 RUNNING priority")
    env = _script_env()
    env["PATH"] = os.pathsep.join([str(bin_dir), "/usr/bin", "/bin"])
    proc = _run([str(installed.rs), FOUND_IN_SLURM_FLAG, str(run)], cwd=tmp_path, timeout=RUN_TIMEOUT, env=env)
    assert proc.returncode == 0, (proc.returncode, proc.stderr)
    assert proc.stdout == b"someone\n", proc.stdout
    assert proc.stderr == b"", proc.stderr


def test_installed_rs_found_in_slurm_error_path(installed: InstalledWheel, tmp_path: Path) -> None:
    """The contract's error path from the wheel: nothing on stdout, a message on stderr, exit 1.

    With an empty PATH the ``squeue`` query cannot run (R's ``error in running command``), on and off the cluster.
    """
    empty_bin = tmp_path / "empty-bin"
    empty_bin.mkdir()
    run = tmp_path / "SSP2-NPi"
    run.mkdir()
    env = _script_env()
    env["PATH"] = str(empty_bin)
    proc = _run([str(installed.rs), FOUND_IN_SLURM_FLAG, str(run)], cwd=tmp_path, timeout=RUN_TIMEOUT, env=env)
    assert proc.returncode == 1, (proc.returncode, proc.stderr)
    assert proc.stdout == b"", proc.stdout
    assert proc.stderr.strip() != b"", "the error path must explain itself on stderr"


# ---------------------------------------------------------------------------
# the installed rs against the in-process port
# ---------------------------------------------------------------------------


def _drop_hint_line(text: str) -> str:
    """D-08: the ``Did you know?`` hint is a random pick; exactly one such line is dropped."""
    lines = text.splitlines(keepends=True)
    for index, line in enumerate(lines):
        if "Did you know?" in line:
            del lines[index]
            break
    return "".join(lines)


@pytest.fixture
def plain_colours(monkeypatch: pytest.MonkeyPatch) -> Iterator[None]:
    """No colour switches in this process either; the process-wide flag is reset afterwards."""
    from modelstats import colors

    for name in COLOUR_VARIABLES:
        monkeypatch.delenv(name, raising=False)
    yield
    colors.set_enabled(False, 256)


def test_installed_rs_matches_in_process_loop_runs(
    installed: InstalledWheel, fixtures_available: bool, plain_colours: None, tmp_path: Path
) -> None:
    """``rs -b <run>`` from the wheel prints the table that ``loop_runs`` and ``run_cli`` print in this process."""
    if not fixtures_available or not FIXTURE_RUN.is_dir():
        pytest.skip(f"packaging comparison skipped: the fixture run {FIXTURE_RUN} is absent")
    from modelstats.cli import Options, run_cli
    from modelstats.loop_runs import loop_runs

    proc = _run([str(installed.rs), "-b", str(FIXTURE_RUN)], cwd=tmp_path, timeout=RUN_TIMEOUT, env=_script_env())
    _check(proc, "rs -b <fixture run>")
    script_stdout = proc.stdout.decode("utf-8", "surrogateescape")
    script_stderr = proc.stderr.decode("utf-8", "surrogateescape")
    assert "testOneRegi" in script_stdout, script_stdout
    assert "Runs found: 1" in script_stderr, script_stderr

    table = io.StringIO()
    with contextlib.redirect_stderr(io.StringIO()):
        returned = loop_runs([str(FIXTURE_RUN)], colors=False, out=table)
    assert returned is None
    assert table.getvalue() == script_stdout

    cli_stdout, cli_stderr = io.StringIO(), io.StringIO()
    with contextlib.redirect_stdout(cli_stdout), contextlib.redirect_stderr(cli_stderr):
        status = run_cli(Options(paths=str(FIXTURE_RUN), nocolor=True))
    assert status == 0
    assert cli_stdout.getvalue() == script_stdout
    assert _drop_hint_line(cli_stderr.getvalue()) == _drop_hint_line(script_stderr)
    assert sys.stdout is not cli_stdout  # the redirect is undone; nothing of this test leaks into the session
