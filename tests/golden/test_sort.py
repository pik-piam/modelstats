"""Golden family ``sort``: ``stri_order(numeric = TRUE)`` value and 1-based order per R case id (D-14)."""

from __future__ import annotations

from collections.abc import Callable
from pathlib import Path

import pytest

from golden._compare import compare_json_case
from golden.conftest import EXPECTED_PY, GOLDENS, r_golden_ids

pytestmark = pytest.mark.golden

FAMILY = "sort"


@pytest.mark.parametrize("case_id", r_golden_ids(FAMILY))
def test_sort(case_id: str, py_goldens: Callable[[str], Path]) -> None:
    compare_json_case(case_id, GOLDENS / FAMILY, py_goldens(FAMILY), EXPECTED_PY / FAMILY)
