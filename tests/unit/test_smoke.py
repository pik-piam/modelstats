"""Smoke tests: the package imports, carries the R version and both console scripts exist."""

from __future__ import annotations

import importlib.metadata
import re
from pathlib import Path

import pytest
from typer.testing import CliRunner

import modelstats
from modelstats.amt.cli import app as modeltests_app
from modelstats.cli import app as rs_app
from modelstats.errors import RParityError

REPO = Path(__file__).resolve().parents[2]


def test_version_matches_description() -> None:
    assert modelstats.__version__ == "0.31.0"
    description = (REPO / "DESCRIPTION").read_text(encoding="utf-8")
    match = re.search(r"^Version:\s*(\S+)\s*$", description, flags=re.MULTILINE)
    assert match is not None
    assert match.group(1) == modelstats.__version__
    assert importlib.metadata.version("modelstats") == modelstats.__version__


def test_rparity_error_carries_the_r_message() -> None:
    err = RParityError("argument is of length zero")
    assert isinstance(err, Exception)
    assert str(err) == "argument is of length zero"


def test_console_scripts_are_declared() -> None:
    scripts = {ep.name: ep.value for ep in importlib.metadata.entry_points(group="console_scripts")}
    assert scripts["rs"] == "modelstats.cli:main"
    assert scripts["modeltests"] == "modelstats.amt.cli:main"


@pytest.mark.parametrize("app", [rs_app, modeltests_app], ids=["rs", "modeltests"])
@pytest.mark.parametrize("flag", ["--help", "-h"])
def test_help_runs(app: object, flag: str) -> None:
    result = CliRunner().invoke(app, [flag])  # type: ignore[arg-type]
    assert result.exit_code == 0, result.output
    assert "Usage:" in result.output
    assert "-h, --help" in result.output


@pytest.mark.parametrize("app", [rs_app, modeltests_app], ids=["rs", "modeltests"])
def test_placeholder_exits_2(app: object) -> None:
    result = CliRunner().invoke(app, [])  # type: ignore[arg-type]
    assert result.exit_code == 2
    assert "not implemented yet (phase 4/5)" in result.output
