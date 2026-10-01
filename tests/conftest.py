"""Shared pytest configuration for the unit, golden and packaging tiers.

Paths are exposed as module constants and through the session fixture
``paths``; ``fixtures_available`` says whether the fixture tree (a symlink
into the reference checkout, absent in CI) is present. When it is absent the
whole golden tier is skipped with one clear reason. A golden case that cannot
be compared while the tree is present must fail, never skip; that is the
comparator's job, not this file's.
"""

from __future__ import annotations

import dataclasses
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parent.parent
MIGRATION = REPO / "migration"
FIXTURES = MIGRATION / "fixtures"
SYNTHETIC = MIGRATION / "synthetic"
GOLDENS = MIGRATION / "goldens"
CASES = MIGRATION / "cases"
HARNESS = MIGRATION / "harness"

# The probe named by the workflow specification: the fixture tree is usable
# when this directory exists.
FIXTURES_PROBE = FIXTURES / "p" / "projects"


@dataclasses.dataclass(frozen=True)
class RepoPaths:
    """Absolute paths of the worktree and its migration material."""

    repo: Path = REPO
    migration: Path = MIGRATION
    fixtures: Path = FIXTURES
    synthetic: Path = SYNTHETIC
    goldens: Path = GOLDENS
    cases: Path = CASES
    harness: Path = HARNESS


def fixtures_present() -> bool:
    """True when the fixture tree is present (``migration/fixtures/p/projects``)."""
    return FIXTURES_PROBE.is_dir()


@pytest.fixture(scope="session")
def paths() -> RepoPaths:
    return RepoPaths()


@pytest.fixture(scope="session")
def fixtures_available() -> bool:
    return fixtures_present()


def pytest_collection_modifyitems(config: pytest.Config, items: list[pytest.Item]) -> None:
    """Skip the whole golden tier, with one reason, when the fixture tree is absent."""
    if fixtures_present():
        return
    reason = (
        f"golden tier skipped: the fixture tree {FIXTURES_PROBE} is absent "
        "(link migration/fixtures to the reference checkout or run migration/download_test_data.sh)"
    )
    skip_golden = pytest.mark.skip(reason=reason)
    for item in items:
        if item.get_closest_marker("golden") is not None:
            item.add_marker(skip_golden)
