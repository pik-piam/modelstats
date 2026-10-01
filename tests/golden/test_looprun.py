"""Golden family ``looprun``: ``loopRuns()`` stdout bytes (ANSI sequences included) per R case id.

One test per R golden id of ``migration/goldens/looprun/`` (``<id>.out`` and, for the BUG-005 cases, the
``<id>.error.json`` beside the partial ``.out``). The comparison is byte for byte on the ``.out`` file and by
message on the error golden, presence included; nothing is stripped or normalised (``compare_out_case``).
"""

from __future__ import annotations

from collections.abc import Callable
from pathlib import Path

import pytest

from golden._compare import compare_out_case
from golden.conftest import EXPECTED_PY, GOLDENS, r_golden_ids

pytestmark = pytest.mark.golden

FAMILY = "looprun"


@pytest.mark.parametrize("case_id", r_golden_ids(FAMILY))
def test_looprun(case_id: str, py_goldens: Callable[[str], Path]) -> None:
    compare_out_case(case_id, GOLDENS / FAMILY, py_goldens(FAMILY), EXPECTED_PY / FAMILY)
