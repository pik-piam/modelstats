"""Shared pytest configuration: the data trees and the test tiers.

Tiers (``pyproject.toml`` ``[tool.pytest.ini_options]``):

* the unit tier (default) depends on nothing outside the repository and the locked environment;
* ``-m golden`` compares against the R goldens under ``migration/goldens``; a golden test that
  reads a run also needs the fixture snapshot or the synthetic layer below ``migration/``;
* ``-m sandbox`` runs commands inside ``migration/harness/sandbox.sh``.

The paths mirror ``migration/harness/lib.sh``: ``MODELSTATS_FIXTURES`` overrides the canonical
fixture snapshot, ``MODELSTATS_SYNTHETIC`` the synthetic layer and ``MODELSTATS_GOLDENS`` the
R reference outputs; the case lists live in ``migration/cases``. A golden test is skipped only
when a tree it requests through its fixtures (``fixtures_dir``, ``synthetic_dir``) is absent:
those two trees are gitignored (``migration/01-test-data-inventory.md``), whereas the cases and
the goldens are tracked in git, so a fixture-free family (the sort family in phase 0) runs on every
checkout, CI included. A golden test that *runs* is never skipped or relaxed
(``migration/03-migration-plan.md`` section 4.6).
"""

from __future__ import annotations

import os
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parent.parent
MIGRATION = REPO / "migration"
FIXTURES = Path(os.environ.get("MODELSTATS_FIXTURES", MIGRATION / "fixtures"))
SYNTHETIC = Path(os.environ.get("MODELSTATS_SYNTHETIC", MIGRATION / "synthetic"))
GOLDENS = Path(os.environ.get("MODELSTATS_GOLDENS", MIGRATION / "goldens"))
CASES = MIGRATION / "cases"


@pytest.fixture(scope="session")
def repo_root() -> Path:
    return REPO


@pytest.fixture(scope="session")
def fixtures_dir() -> Path:
    """The canonical fixture snapshot (``<fixtures>/p`` is the cluster layout).

    Skips the test when the snapshot is absent; only tests that read a fixture run request it.
    """
    if not (FIXTURES / "p").is_dir():
        pytest.skip(f"fixture snapshot absent: {FIXTURES}/p (migration/01-test-data-inventory.md)")
    return FIXTURES


@pytest.fixture(scope="session")
def synthetic_dir() -> Path:
    """The synthetic layer; skips the test when it has not been built."""
    if not (SYNTHETIC / "p").is_dir():
        pytest.skip(f"synthetic layer not built: {SYNTHETIC} (migration/harness/build_synthetic.sh)")
    return SYNTHETIC


@pytest.fixture(scope="session")
def goldens_dir() -> Path:
    """The R reference outputs; tracked in git, so their absence is an error, never a skip."""
    return GOLDENS


@pytest.fixture(scope="session")
def cases_dir() -> Path:
    """The case lists the goldens were made from; tracked in git, so never a reason to skip."""
    return CASES
