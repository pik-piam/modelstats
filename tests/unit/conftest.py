"""Unit-tier fixtures: the R-written test files (``migration/harness/checks/write_testdata.R``)."""

from __future__ import annotations

import os
from pathlib import Path

import pytest

DEFAULT_R_TESTDATA = Path(__file__).resolve().parent.parent / "data" / "r"


@pytest.fixture(scope="session")
def r_testdata_dir() -> Path:
    """``tests/data/r``, or the directory named by ``MODELSTATS_R_TESTDATA``."""
    directory = Path(os.environ.get("MODELSTATS_R_TESTDATA", DEFAULT_R_TESTDATA))
    if not directory.is_dir():
        raise FileNotFoundError(f"R test data missing: {directory} (see migration/harness/checks/README.md)")
    return directory
