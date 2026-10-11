"""``print_output`` reproduces every R golden of ``migration/goldens/printoutput`` byte for byte.

The cases are ``migration/cases/printoutput/<id>.json`` in the format ``make_goldens.R``
(``fam_printoutput``) reads: ``string`` is the one-row data frame (``rowname`` and ``columns``,
where a JSON ``null`` is a real ``NA``, the string ``"NA"`` stays a string and numbers stay numbers;
``{}`` is a data frame without columns), the optional ``len1stcol``, ``lenCols``, ``colSep`` and
``cols`` are passed through, and ``mode`` is the sandbox mode that decides ``file.exists("/p")``
(``on_cluster``). The golden is ``<id>.out`` (the returned string as bytes) or ``<id>.error.json``
(the R condition message); the comparison is exact (``migration/03-migration-plan.md`` 4.6).
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

import pytest

from modelstats.env import Mode
from modelstats.formatting import PrintOutputError, Row, print_output

pytestmark = pytest.mark.golden

CASES = Path(__file__).resolve().parent.parent.parent / "migration" / "cases" / "printoutput"
CASE_IDS = sorted(path.stem for path in CASES.glob("*.json"))


def load_case(cases_dir: Path, case_id: str) -> dict[str, Any]:
    data: dict[str, Any] = json.loads(
        (cases_dir / "printoutput" / f"{case_id}.json").read_text(encoding="utf-8")
    )
    return data


def call_arguments(spec: dict[str, Any]) -> dict[str, Any]:
    """The ``printOutput`` arguments of a case: only those the JSON sets (R's defaults otherwise)."""
    mapping = {"len1stcol": "len1stcol", "lenCols": "len_cols", "colSep": "col_sep", "cols": "cols"}
    arguments = {python: spec[r] for r, python in mapping.items() if spec.get(r) is not None}
    arguments["on_cluster"] = Mode(spec["mode"]).on_cluster
    return arguments


def test_every_case_has_exactly_one_golden(cases_dir: Path, goldens_dir: Path) -> None:
    assert CASE_IDS, "no printoutput cases found"
    assert sorted(path.stem for path in (cases_dir / "printoutput").glob("*.json")) == CASE_IDS
    outputs = {path.stem for path in (goldens_dir / "printoutput").glob("*.out")}
    errors = {
        path.name.removesuffix(".error.json") for path in (goldens_dir / "printoutput").glob("*.error.json")
    }
    assert outputs.isdisjoint(errors)
    assert outputs | errors == set(CASE_IDS)


@pytest.mark.parametrize("case_id", CASE_IDS)
def test_print_output_equals_the_golden(case_id: str, cases_dir: Path, goldens_dir: Path) -> None:
    spec = load_case(cases_dir, case_id)
    record = Row(spec["string"]["rowname"], spec["string"]["columns"])
    arguments = call_arguments(spec)
    error_file = goldens_dir / "printoutput" / f"{case_id}.error.json"
    if error_file.exists():
        expected_error = json.loads(error_file.read_text(encoding="utf-8"))["error"]
        with pytest.raises(PrintOutputError) as excinfo:
            print_output(record, **arguments)
        assert str(excinfo.value) == expected_error
        return
    expected = (goldens_dir / "printoutput" / f"{case_id}.out").read_bytes()
    assert print_output(record, **arguments).encode("utf-8") == expected
