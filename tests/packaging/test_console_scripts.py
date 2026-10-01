"""The two console scripts are installed and answer ``--help``.

The scripts next to the running interpreter are used: the locked environment under ``uv run`` in
the checks job, and in the wheel job of ``.github/workflows/python.yml`` a fresh virtual environment
holding the locked dependency set plus the built wheel, where CI runs the whole default tier
(this module included) so the wheel's entry points are tested, not source imports.
"""

from __future__ import annotations

import subprocess
import sysconfig
from pathlib import Path

import pytest

SCRIPTS_DIR = Path(sysconfig.get_path("scripts"))


def run(script: str, *args: str) -> subprocess.CompletedProcess[str]:
    executable = SCRIPTS_DIR / script
    assert executable.is_file(), f"{script} is not installed in {SCRIPTS_DIR}"
    return subprocess.run([str(executable), *args], capture_output=True, text=True, check=False)


@pytest.mark.parametrize("script", ["rs", "modeltests"])
@pytest.mark.parametrize("flag", ["-h", "--help"])
def test_help_exits_zero(script: str, flag: str) -> None:
    result = run(script, flag)
    assert result.returncode == 0, result.stderr
    assert result.stdout.startswith(f"Usage: {script} [OPTIONS]")
    assert result.stderr == ""


@pytest.mark.parametrize(("script", "phase"), [("rs", "phase 4"), ("modeltests", "phase 5")])
def test_placeholders_name_their_phase(script: str, phase: str) -> None:
    result = run(script)
    assert result.returncode == 1
    assert result.stdout == ""
    assert "NotImplementedError" in result.stderr
    assert phase in result.stderr
