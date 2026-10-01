"""``col_run_type`` against every R golden under ``migration/goldens/runtype`` (``pytest -m golden``).

``migration/harness/make_goldens.R`` (``fam_runtype``) ran ``colRunType(dir)`` inside the sandbox on
each row of ``migration/cases/runtype.tsv`` and wrote ``{"value": ...}`` or ``{"error": <R message>}``.
The Python side runs on the ``FakeEffects`` of the same case (the fixture snapshot over the synthetic
layer, as the sandbox mounted them) with ``modelstats.config.load_config`` reading the configuration
file through that overlay, and must reproduce the value or the exact error message: the no-config
rows (BUG-004, D-02 pending) and the ``config-bak`` row (BUG-003, D-01 pending) are ordinary passing
tests. Rows below ``projects/synthetic`` need the synthetic layer and skip only when it is not built.
"""

from __future__ import annotations

import json
from collections.abc import Callable
from pathlib import Path, PurePosixPath

import pytest

from modelstats.config import RunConfig, load_config
from modelstats.env import FakeEffects
from modelstats.run_type import RunTypeError, col_run_type
from tests.conftest import GoldenCase, golden_cases

pytestmark = pytest.mark.golden

SYNTHETIC_PREFIX = "projects/synthetic/"
CASES = golden_cases("runtype")


def overlay_loader(effects: FakeEffects) -> Callable[[Path], RunConfig]:
    """``load_config`` on the physical file the overlay shows for the logical path R would load."""

    def load(path: Path) -> RunConfig:
        physical = effects.paths.first_existing(PurePosixPath(path)) if effects.paths is not None else None
        return load_config(physical if physical is not None else path)

    return load


def test_every_runtype_golden_has_a_case(goldens_dir: Path) -> None:
    on_disk = {path.stem for path in (goldens_dir / "runtype").glob("*.json")}
    assert on_disk == {case.id for case in CASES}


@pytest.mark.parametrize("case", CASES, ids=lambda case: case.id)
def test_runtype_golden(
    case: GoldenCase,
    case_effects: Callable[[GoldenCase], FakeEffects],
    goldens_dir: Path,
    request: pytest.FixtureRequest,
) -> None:
    if case.row["dir"].startswith(SYNTHETIC_PREFIX):
        request.getfixturevalue("synthetic_dir")
    effects = case_effects(case)
    expected = json.loads((goldens_dir / "runtype" / f"{case.id}.json").read_text(encoding="utf-8"))
    directory = case.logical_path(case.row["dir"])
    if "error" in expected:
        with pytest.raises(RunTypeError) as excinfo:
            col_run_type(directory, effects, overlay_loader(effects))
        assert str(excinfo.value) == expected["error"]
    else:
        assert col_run_type(directory, effects, overlay_loader(effects)) == expected["value"]
