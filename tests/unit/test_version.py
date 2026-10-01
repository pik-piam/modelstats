"""The package version is one number, kept in three places."""

from __future__ import annotations

import re
from importlib import metadata
from pathlib import Path

import pytest

import modelstats


def test_version_is_a_release_number() -> None:
    assert re.fullmatch(r"\d+\.\d+\.\d+", modelstats.__version__)


def test_version_matches_the_installed_distribution() -> None:
    assert metadata.version("modelstats") == modelstats.__version__


def test_version_mirrors_the_r_description(repo_root: Path) -> None:
    """``pyproject.toml`` mirrors ``DESCRIPTION`` (``migration/06-user-requests.md`` section 7, phase 0).

    Repository-only: the source distribution carries the tests but not the R package
    (``pyproject.toml`` ``[tool.hatch.build.targets.sdist]``), so from an unpacked sdist the check is
    skipped; in the git checkout, and so in CI, ``DESCRIPTION`` always exists and the mirror is enforced.
    """
    description_file = repo_root / "DESCRIPTION"
    if not description_file.is_file():
        pytest.skip(
            "DESCRIPTION is not shipped in the sdist (pyproject.toml [tool.hatch.build.targets.sdist]); "
            "the R/Python version mirror is checked from the git checkout"
        )
    description = description_file.read_text(encoding="utf-8")
    match = re.search(r"^Version:\s*(\S+)$", description, flags=re.MULTILINE)
    assert match is not None
    assert match.group(1) == modelstats.__version__
