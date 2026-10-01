"""``runfolders.natural_sorted`` against every R golden under ``migration/goldens/sort`` (golden tier).

``migration/harness/make_goldens.R`` (``fam_sort``) ran ``stringi::stri_order(names, numeric = TRUE)``
inside the sandbox (``LC_ALL=C.utf8``, stringi locale ``en_US_POSIX``) on each row of
``migration/cases/sort.tsv`` and wrote the sorted names (``value``) and the 1-based permutation
(``order``). The port's ``natural_sorted`` / ``natural_order_indices`` (natsort ``ns.DEFAULT``, decision
D-14) must reproduce both. D-14 is approved with one named exception: non-ASCII names are "recorded as
they fall", so for the ``exotic`` row the exact Python order is pinned here as the expectation
(``EXOTIC_PYTHON_ORDER``) next to the immutable R golden, and the ASCII subsequence must still equal R's;
every other row is compared in full.
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from modelstats.runfolders import natural_order_indices, natural_sorted
from tests.conftest import read_case_rows

pytestmark = pytest.mark.golden

CASES = read_case_rows(Path(__file__).resolve().parents[2] / "migration" / "cases" / "sort.tsv")
DEVIATING = "exotic"

# The approved deviation (D-14): natsort puts ä after a_1 (ICU: right after Z) and ends ß, ﬁ (ICU: ﬁ, ß).
EXOTIC_PYTHON_ORDER = [
    "",
    "1.9",
    "1.10",
    "1a",
    "01a",
    "9",
    "10",
    " ",
    "Ä",
    "E",
    "Z",
    "a 1",
    "a-1",
    "a_1",
    "ä",
    "e",
    "é",
    "fi",
    "run01",
    "run1",
    "run010",
    "run10",
    "ss",
    "x1y2",
    "x1y10",
    "x10y1",
    "z",
    "ß",
    "ﬁ",
]


def case_names(cases_dir: Path, file: str) -> list[str]:
    names: list[str] = json.loads((cases_dir / "sort" / file).read_text(encoding="utf-8"))["names"]
    return names


def golden(goldens_dir: Path, case_id: str) -> tuple[list[str], list[int]]:
    data = json.loads((goldens_dir / "sort" / f"{case_id}.json").read_text(encoding="utf-8"))
    assert "error" not in data, data
    value: list[str] = data["value"]
    order: list[int] = data["order"]
    return value, order


def test_every_sort_golden_has_a_case(goldens_dir: Path) -> None:
    on_disk = {path.stem for path in (goldens_dir / "sort").glob("*.json")}
    assert on_disk == {row["id"] for row in CASES}
    assert DEVIATING in on_disk


@pytest.mark.parametrize("row", [row for row in CASES if row["id"] != DEVIATING], ids=lambda row: row["id"])
def test_sort_golden(row: dict[str, str], cases_dir: Path, goldens_dir: Path) -> None:
    names = case_names(cases_dir, row["file"])
    value, order = golden(goldens_dir, row["id"])
    assert value == [names[i - 1] for i in order]
    assert natural_sorted(names) == value
    assert [i + 1 for i in natural_order_indices(names)] == order


def test_exotic_golden_matches_on_ascii_names_and_pins_the_python_order(
    cases_dir: Path, goldens_dir: Path
) -> None:
    names = case_names(cases_dir, "exotic.json")
    value, order = golden(goldens_dir, "exotic")
    assert value == [names[i - 1] for i in order]
    ours = natural_sorted(names)
    assert ours == EXOTIC_PYTHON_ORDER
    assert [names[i] for i in natural_order_indices(names)] == EXOTIC_PYTHON_ORDER
    assert [n for n in ours if n.isascii()] == [n for n in value if n.isascii()]
    assert {n for n in names if not n.isascii()} == {"Ä", "ä", "é", "ß", "ﬁ"}
