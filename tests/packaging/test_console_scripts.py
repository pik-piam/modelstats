"""The two console scripts are installed, answer ``--help`` and run.

The scripts next to the running interpreter are used: the locked environment under ``uv run`` in
the checks job, and in the wheel job of ``.github/workflows/python.yml`` a fresh virtual environment
holding the locked dependency set plus the built wheel, where CI runs the whole default tier
(this module included) so the wheel's entry points are tested, not source imports. ``rs`` is the
drop-in of phase 4 (``migration/06-user-requests.md`` section 7): without runs below the working
directory it warns ``No runs found`` and exits 0 (``R/commandLineInterface.R:263-265``);
``modeltests`` stays the placeholder until phase 5. The wheel built by ``uv build``, a fresh
environment and ``uv tool install`` are covered by ``tests/packaging/test_wheel.py`` (golden tier).
"""

from __future__ import annotations

import subprocess
import sysconfig
from pathlib import Path

import pytest

SCRIPTS_DIR = Path(sysconfig.get_path("scripts"))


def run(script: str, *args: str, cwd: Path | None = None) -> subprocess.CompletedProcess[str]:
    executable = SCRIPTS_DIR / script
    assert executable.is_file(), f"{script} is not installed in {SCRIPTS_DIR}"
    return subprocess.run([str(executable), *args], capture_output=True, text=True, check=False, cwd=cwd)


@pytest.mark.parametrize("script", ["rs", "modeltests"])
@pytest.mark.parametrize("flag", ["-h", "--help"])
def test_help_exits_zero(script: str, flag: str) -> None:
    result = run(script, flag)
    assert result.returncode == 0, result.stderr
    assert result.stdout.startswith(f"Usage: {script} [OPTIONS]")
    assert result.stderr == ""


def test_rs_without_runs_warns_and_exits_zero(tmp_path: Path) -> None:
    """``rs`` in a directory without run folders: the announcements and the warning on stderr, exit 0."""
    result = run("rs", cwd=tmp_path)
    assert result.returncode == 0, result.stderr
    assert result.stdout == ""
    assert "Did you know? " in result.stderr
    assert result.stderr.rstrip("\n").endswith("No runs found")


def test_rs_usage_error_is_r_s_exit_one() -> None:
    """An invalid short flag: optparse's usage line and the message on stderr, exit 1 (``unknown-flag``)."""
    result = run("rs", "-z")
    assert result.returncode == 1
    assert result.stdout == ""
    assert result.stderr.startswith("Usage: \n rs [PATH] [OPTION]\n")
    assert result.stderr.endswith('NA: error: short flag "z" is invalid\n')


def test_rs_found_in_slurm_prints_one_line(tmp_path: Path) -> None:
    """``rs --found-in-slurm DIR``: exactly one line on stdout, nothing on stderr, exit 0 (``README.md``)."""
    result = run("rs", "--found-in-slurm", str(tmp_path))
    assert result.returncode == 0, result.stderr
    assert result.stdout.count("\n") == 1 and result.stdout.endswith("\n")
    assert result.stderr == ""


def test_modeltests_placeholder_names_its_phase() -> None:
    result = run("modeltests")
    assert result.returncode == 1
    assert result.stdout == ""
    assert "NotImplementedError" in result.stderr
    assert "phase 5" in result.stderr
