"""Golden family ``status``: ``getRunStatus()`` records per R case id (``migration/cases/status.tsv``).

One test per (mode, detail, id): the ids carry their subdirectory (``oncluster/detailed/<id>``),
so the four sets of the family are collected from ``migration/goldens/status/**``. The comparison
is by key order, JSON type and value (a real NA is ``null``, the literal ``"NA"`` a string,
``Runtime`` and the counts integers; a brief record has no detailed columns at all), and an error
golden matches only by message.
"""

from __future__ import annotations

from collections.abc import Callable
from pathlib import Path

import pytest

from golden._compare import compare_json_case
from golden.conftest import EXPECTED_PY, GOLDENS, r_golden_ids

pytestmark = pytest.mark.golden

FAMILY = "status"


@pytest.mark.parametrize("case_id", r_golden_ids(FAMILY))
def test_status(case_id: str, py_goldens: Callable[[str], Path]) -> None:
    compare_json_case(case_id, GOLDENS / FAMILY, py_goldens(FAMILY), EXPECTED_PY / FAMILY)
