"""``get_sanity_checks`` against every R golden under ``migration/goldens/sanity`` (``pytest -m golden``).

``migration/harness/make_goldens.R`` (``fam_sanity``) ran ``getSanityChecks()`` (an empty ``dirs`` cell) or
``getSanityChecks(dirs)`` inside the sandbox for each row of ``migration/cases/sanity.tsv`` and captured
stdout as bytes into ``sanity/<id>.out``, with ``<id>.error.json`` holding R's condition message when the
call aborted (the partial ``.out`` is kept). The Python side runs on the ``FakeEffects`` of the same row
(overlay, canned scheduler, clock, user, mode) and must write the same bytes to its ``out`` stream, or raise
the exact R message after writing the same partial output (``migration/03-migration-plan.md`` 4.6). The
register bugs the family pins are ordinary passing rows: the three-space rows under the two-space header
(BUG-034, D-18 pending, ``separator--oncluster``) and the omitted MAgPIE and mif-less runs (BUG-012, D-04
pending, ``magpie-rows--oncluster``, ``amt-explicit--*``). The ``mixed--oncluster`` row reads a synthetic run
and skips only when the synthetic layer is not built.

The ``crayon`` column of the case list selects the oracle's crayon state per row (``set_crayon`` of
``make_goldens.R``): ``on`` is crayon enabled at 256 colours under the sandbox's ``R_CLI_NUM_COLORS=256``,
which the port derives with ``Crayon.from_terminal`` from the sandbox environment; ``off`` is
``options(crayon.enabled = FALSE, cli.num_colors = 1L)``, the piped case without any escape, which the
port runs under ``Crayon.disabled()`` (:func:`crayon_of`). The ``amt-default--oncluster--crayon-off``
golden was regenerated on 2026-10-01 (phase 3 Codex review, finding 2): its first recording had carried
the escapes because ``crayon.enabled = FALSE`` alone does not beat the variable, only ``cli.num_colors``
does; :func:`test_crayon_off_golden_has_no_escape_and_differs_from_its_twin` keeps that from recurring.
"""

from __future__ import annotations

import io
import json
import re
from collections.abc import Callable
from pathlib import Path

import pytest

from modelstats.colors import Crayon
from modelstats.env import FakeEffects
from modelstats.errors import ModelstatsError
from modelstats.sanity import get_sanity_checks
from tests.conftest import SANDBOX_ENVIRON, GoldenCase, golden_cases

pytestmark = pytest.mark.golden

SYNTHETIC_PREFIX = "projects/synthetic/"
CASES = golden_cases("sanity")
SANDBOX_CRAYON = Crayon.from_terminal(is_tty=False, environ={**SANDBOX_ENVIRON, "R_CLI_NUM_COLORS": "256"})


def golden_path(goldens_dir: Path, case_id: str, suffix: str) -> Path:
    return goldens_dir / "sanity" / f"{case_id}{suffix}"


def test_every_case_has_exactly_one_golden(goldens_dir: Path) -> None:
    assert CASES, "no sanity cases found"
    outputs = {path.stem for path in (goldens_dir / "sanity").glob("*.out")}
    errors = {path.name.removesuffix(".error.json") for path in (goldens_dir / "sanity").glob("*.error.json")}
    assert outputs == {case.id for case in CASES}
    assert errors <= outputs


def crayon_of(case: GoldenCase) -> Crayon:
    """``Crayon.disabled()`` for an ``off`` row, else the state the sandbox environment gives crayon."""
    return Crayon.disabled() if case.row.get("crayon") == "off" else SANDBOX_CRAYON


def test_sandbox_crayon_is_enabled_at_256_colours() -> None:
    assert Crayon(enabled=True, colours=256) == SANDBOX_CRAYON
    assert [case.id for case in CASES if crayon_of(case) == Crayon.disabled()] == [
        "amt-default--oncluster--crayon-off"
    ]


def test_crayon_off_golden_has_no_escape_and_differs_from_its_twin(goldens_dir: Path) -> None:
    """``options(cli.num_colors = 1L)``: the ``off`` golden is the ``on`` one without its escapes."""
    on = golden_path(goldens_dir, "amt-default--oncluster", ".out").read_bytes()
    off = golden_path(goldens_dir, "amt-default--oncluster--crayon-off", ".out").read_bytes()
    assert b"\x1b[36m" in on
    assert b"\x1b[" not in off
    assert re.sub(rb"\x1b\[[0-9;]*m", b"", on) == off


@pytest.mark.parametrize("case", CASES, ids=lambda case: case.id)
def test_sanity_golden(
    case: GoldenCase,
    case_effects: Callable[[GoldenCase], FakeEffects],
    goldens_dir: Path,
    request: pytest.FixtureRequest,
) -> None:
    if any(d.startswith(SYNTHETIC_PREFIX) for d in case.row["dirs"].split(",")):
        request.getfixturevalue("synthetic_dir")
    effects = case_effects(case)
    dirs = case.logical_paths("dirs") or None  # an empty cell is getSanityChecks(): the AMT lookup
    expected = golden_path(goldens_dir, case.id, ".out").read_bytes()
    error_file = golden_path(goldens_dir, case.id, ".error.json")
    out = io.StringIO()
    if error_file.exists():
        with pytest.raises(ModelstatsError) as excinfo:
            get_sanity_checks(dirs, effects, out, crayon=crayon_of(case))
        assert str(excinfo.value) == json.loads(error_file.read_text(encoding="utf-8"))["error"]
    else:
        get_sanity_checks(dirs, effects, out, crayon=crayon_of(case))
    assert out.getvalue().encode("utf-8") == expected
