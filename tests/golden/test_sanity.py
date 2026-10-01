"""Golden family ``sanity``: ``getSanityChecks()`` bytes, or its error message, per R case id."""

from __future__ import annotations

from collections.abc import Callable
from pathlib import Path

import pytest

from golden._compare import compare_out_case
from golden.conftest import EXPECTED_PY, GOLDENS, r_golden_ids

pytestmark = pytest.mark.golden

FAMILY = "sanity"


@pytest.mark.parametrize("case_id", r_golden_ids(FAMILY))
def test_sanity(case_id: str, py_goldens: Callable[[str], Path]) -> None:
    compare_out_case(case_id, GOLDENS / FAMILY, py_goldens(FAMILY), EXPECTED_PY / FAMILY)
