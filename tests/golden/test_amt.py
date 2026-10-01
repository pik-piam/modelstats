"""Golden family ``amt``: ``modeltests()`` run in the sandbox, compared semantically per case (plan 03 section 4.5).

One test per R golden case directory of ``migration/goldens/amt/`` (46). ``compare_amt`` asserts ``result.json``
(status and R's error message), the README bytes, ``.testsstatus``, stdout bytes, the state files by value (RDS
bytes differ between the two writers by design: the ``sha256`` fields are excluded and the value trees, read by the
same R code on both sides, must be equal), the Mattermost message text of every POST (the port sends through
``Effects.post_json``; R's payload is a string concatenation, BUG-015 / D-15, so the text is compared), the
traced commands as the ordered sequence of ``(tool, argv, cwd)`` for git, sbatch, rsync, mv, make, Rscript, squeue
and sacct (R's order is not incidental: the publication sequence of ``R/modeltests.R`` lines 435-443), the clone
diff (``effects.json``: equal path sets per category and equal sha256 for every non-RDS file), the
``unchanged.json`` flags and the MAgPIE ``data-changelog.csv``.

Documented exceptions, each named in ``tests/golden/_compare.py`` and in ``migration/06-port-findings.md``
("Deviations applied"):

- ``curl`` is absent from the compared trace: the notification is the ``post_json`` effect (payload compared);
- ``sed`` is absent from the compared trace: the port edits ``config/default.cfg`` directly (the resulting bytes are
  asserted through ``effects.json``: ``touched`` with R's sha256 for REMIND, ``modified`` for MAgPIE);
- RDS state files are compared by value, never by bytes; an RDS file R rewrote with identical bytes (``touched``)
  may be ``modified`` on the Python side;
- ``stderr.txt`` is not compared (plan 4.5): the changelog bridge names its own call (``readRDS(report)``) and a
  failed POST prints notify's reason where the real curl printed its own.

No temporary path is normalised. ``migration/goldens/expected-py/amt/<case>/<artefact>`` would replace the R
artefact for an approved deviation; none is needed.
"""

from __future__ import annotations

from collections.abc import Callable
from pathlib import Path

import pytest

from golden._compare import compare_amt
from golden.conftest import EXPECTED_PY, GOLDENS, r_golden_ids

pytestmark = pytest.mark.golden

FAMILY = "amt"


@pytest.mark.parametrize("case_id", r_golden_ids(FAMILY))
def test_amt(case_id: str, py_goldens: Callable[[str], Path]) -> None:
    compare_amt(case_id, GOLDENS / FAMILY, py_goldens(FAMILY), EXPECTED_PY / FAMILY)
