"""Golden family ``rs``: one ``rs`` process per R case (``migration/cases/rs.tsv``), compared stream by stream.

One test per R golden id of ``migration/goldens/rs/`` (the ``<id>.status`` files): ``compare_rs`` asserts the argv
lines, stdout bytes and the exit status byte for byte and stderr after dropping exactly one ``Did you know?`` line
on each side (D-08). ``migration/goldens/expected-py/rs/<id>.<ext>`` replaces the R golden for that one stream
only (D-11 help layout, D-12 usage-error wording, D-20 corrupt GDX, D-21 one stdin line per prompt); a help case
additionally has to list every option of plan 03 section 3.4.
"""

from __future__ import annotations

from collections.abc import Callable
from pathlib import Path

import pytest

from golden._compare import compare_rs
from golden.conftest import EXPECTED_PY, GOLDENS, r_golden_ids

pytestmark = pytest.mark.golden

FAMILY = "rs"


@pytest.mark.parametrize("case_id", r_golden_ids(FAMILY))
def test_rs(case_id: str, py_goldens: Callable[[str], Path]) -> None:
    compare_rs(case_id, GOLDENS / FAMILY, py_goldens(FAMILY), EXPECTED_PY / FAMILY)
