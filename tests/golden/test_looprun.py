"""``loop_runs`` against every R golden under ``migration/goldens/looprun`` (``pytest -m golden``).

``migration/harness/make_goldens.R`` (``fam_looprun``) ran ``loopRuns(dirs, user, colors, sortbytime)``
inside the sandbox for each row of ``migration/cases/looprun.tsv`` and captured stdout as bytes:
``looprun/<id>.out``, plus
``<id>.error.json`` with the R condition message when the call aborted (the partial ``.out`` is kept).
The Python side runs on the ``FakeEffects`` of the same row (overlay, canned scheduler, clock, user,
mode) and must write the same bytes, escape sequences included, and raise the same error
(``migration/03-migration-plan.md`` 4.6: byte-identical, no relaxed comparison). The matrix of the
``amt-all`` rows covers colour and no-colour, sorted and unsorted, on and off the cluster; the other rows
pin the MAgPIE colour tree, the coupled runs, every synthetic REMIND and MAgPIE state with and without a
running job, the ``mtime`` order, ``startup`` with a numeric runtime, ``pending``, another user's job,
the ``skipped`` and ``skipped because of error`` lines, the live off-cluster runtime (BUG-042), repeated
basenames, the folder width at both bounds, the empty call and the BUG-005 abort. Rows below
``projects/synthetic`` need the synthetic layer and skip only when it is not built.

The ``crayon`` column of the case list selects the oracle's crayon state per row (``set_crayon`` of
``make_goldens.R``): ``on`` is ``options(crayon.enabled = TRUE, crayon.colors = 256, cli.num_colors = NULL)``
under the sandbox's ``R_CLI_NUM_COLORS=256`` (the tty case on the cluster), ``off`` is
``options(crayon.enabled = FALSE, cli.num_colors = 1L)`` (the piped case; ``cli.num_colors`` is the one
option cli's ``num_ansi_colors`` checks before the variable, ``migration/02-python-libraries.md`` 4.5).
:func:`crayon_of` mirrors that: ``Crayon.from_terminal`` on the sandbox environment for ``on``, which is
crayon enabled at 256 colours, and ``Crayon.disabled()`` for ``off``. The two ``--crayon-off`` rows are
the R goldens of the no-escape branch; they were regenerated on 2026-10-01 (phase 3 Codex review, finding
2) after their first recording had carried escapes, ``crayon.enabled = FALSE`` alone not beating the
variable, and :func:`test_crayon_off_goldens_carry_no_escape_and_differ_from_their_twins` keeps that from
recurring.

stderr: ``loopRuns`` prints base R's ``try()`` diagnostic for every failed ``getRunStatus`` call
(``R/loopRuns.R:57``, :func:`modelstats.loop_runs.try_error_message`). The looprun goldens hold stdout
only (``capture_bytes`` of ``make_goldens.R`` sinks stdout), so the ``err`` stream is compared with the
lines the ``rs`` goldens of the same synthetic runs recorded on stderr (``STDERR_BLOCKS``:
``migration/goldens/rs/synthetic-remind.err:5-11``, ``synthetic-bug005.err:5-6``), in the loop order of
the failing runs (``EXPECTED_STDERR``), and must be empty for every other case;
:func:`test_every_failing_row_has_a_pinned_stderr` ties the two tables to the goldens.
"""

from __future__ import annotations

import io
import json
import re
from collections.abc import Callable
from pathlib import Path

import pytest

from modelstats.colors import Crayon
from modelstats.env import FakeEffects, Mode
from modelstats.loop_runs import NO_RUNS_FOUND, LoopRunsError, loop_runs
from tests.conftest import SANDBOX_ENVIRON, GoldenCase, golden_cases

pytestmark = pytest.mark.golden

SANDBOX_R_ENVIRON = {**SANDBOX_ENVIRON, "R_CLI_NUM_COLORS": "256"}
"""The variables the R oracle saw (``migration/harness/sandbox.sh``): cli's colour override included."""
CRAYON_OFF_TWINS = (
    ("amt-all--oncluster--color--sorted--crayon-off", "amt-all--oncluster--color--sorted"),
    ("amt-all--oncluster--nocolor--sorted--crayon-off", "amt-all--oncluster--nocolor--sorted"),
)
"""The ``off`` rows and the ``on`` rows that differ from them in the crayon state only."""
STDERR_BLOCKS: dict[str, tuple[str, int, int]] = {
    "config-bak": ("synthetic-remind.err", 5, 5),
    "gdx-noiter": ("synthetic-remind.err", 6, 11),
    "noconfig-badstats": ("synthetic-bug005.err", 5, 6),
}
"""Synthetic run -> the ``goldens/rs`` file and the 1-based inclusive lines R's ``try()`` printed for it."""
EXPECTED_STDERR: dict[str, tuple[str, ...]] = {
    "errors--oncluster": ("config-bak",),
    "errors--local": ("config-bak",),
    "synthetic-remind--oncluster--color--unsorted": ("config-bak", "gdx-noiter"),
    "synthetic-remind--oncluster--color--unsorted--running": ("config-bak", "gdx-noiter"),
    "synthetic-remind--local--nocolor--unsorted": ("config-bak", "gdx-noiter"),
    "errors-bug005--oncluster": ("noconfig-badstats",),
    "errors-bug005--local": ("noconfig-badstats",),
}
"""Case -> its failing runs in loop order (the ``skipped because of error`` rows, then the aborting one)."""
SKIPPED_BECAUSE_OF_ERROR = b" skipped because of error\n"
ANSI = re.compile(rb"\x1b\[[0-9;]*m")

SYNTHETIC_PREFIX = "projects/synthetic/"
CASES = golden_cases("looprun")
PARAMS = [pytest.param(case, id=case.id) for case in CASES]
MATRIX_IDS = (
    "amt-all--oncluster--color--sorted",
    "amt-all--oncluster--color--unsorted",
    "amt-all--oncluster--nocolor--sorted",
    "amt-all--oncluster--nocolor--unsorted",
    "amt-all--local--color--sorted",
    "amt-all--local--color--unsorted",
    "amt-all--local--nocolor--sorted",
    "amt-all--local--nocolor--unsorted",
    "amt-all--oncluster--color--sorted--crayon-off",
    "amt-all--oncluster--nocolor--sorted--crayon-off",
)
FOCUSED_IDS = (
    "errors--oncluster",
    "errors-bug005--oncluster",
    "errors-bug005--local",
    "startup-numeric--oncluster",
    "pending--oncluster",
    "live-runtime--local",
    "dup-basenames--oncluster",
    "long-names--oncluster",
    "short-names--oncluster",
    "empty--oncluster",
)


def golden_out(goldens_dir: Path, case_id: str) -> Path:
    return goldens_dir / "looprun" / f"{case_id}.out"


def golden_error(goldens_dir: Path, case_id: str) -> Path:
    return goldens_dir / "looprun" / f"{case_id}.error.json"


def flag(case: GoldenCase, column: str) -> bool:
    """``col(row, column, "1") == "1"`` of ``make_goldens.R``: an empty cell is the R default ``TRUE``."""
    return (case.row.get(column) or "1") == "1"


def is_crayon_off(case: GoldenCase) -> bool:
    """The row's ``crayon`` column is ``off`` (``set_crayon(FALSE)`` of ``make_goldens.R``)."""
    return case.row.get("crayon") == "off"


def crayon_of(case: GoldenCase) -> Crayon:
    """The crayon state the oracle had for ``case``.

    ``Crayon.disabled()`` for an ``off`` row (``options(cli.num_colors = 1L)``: no escape at all); else what
    ``crayon::num_colors()`` returns under the sandbox variables with a piped stdout, by the port's
    reproduction of crayon's rule (``R_CLI_NUM_COLORS=256``: enabled at 256 colours).
    """
    if is_crayon_off(case):
        return Crayon.disabled()
    return Crayon.from_terminal(is_tty=False, environ=SANDBOX_R_ENVIRON)


def rs_stderr_lines(goldens_dir: Path, name: str, first: int, last: int) -> bytes:
    """Lines ``first``..``last`` (1-based, inclusive) of ``goldens/rs/<name>``, newline-terminated."""
    lines = (goldens_dir / "rs" / name).read_bytes().split(b"\n")
    return b"\n".join(lines[first - 1 : last]) + b"\n"


def expected_stderr(goldens_dir: Path, case_id: str) -> bytes:
    """What ``loopRuns`` wrote to stderr for the case: the ``try()`` blocks of its failing runs, in order."""
    return b"".join(
        rs_stderr_lines(goldens_dir, *STDERR_BLOCKS[run]) for run in EXPECTED_STDERR.get(case_id, ())
    )


def test_the_oracle_state_follows_the_crayon_column() -> None:
    off = [case for case in CASES if is_crayon_off(case)]
    assert {case.id for case in off} == {off_id for off_id, _ in CRAYON_OFF_TWINS}
    assert all(crayon_of(case) == Crayon.disabled() for case in off)
    assert all(
        crayon_of(case) == Crayon(enabled=True, colours=256) for case in CASES if not is_crayon_off(case)
    )


@pytest.mark.parametrize(("off", "on"), CRAYON_OFF_TWINS)
def test_crayon_off_goldens_carry_no_escape_and_differ_from_their_twins(
    off: str, on: str, goldens_dir: Path
) -> None:
    """The ``off`` golden is R's piped output: no escape sequence; its ``on`` twin has them."""
    off_bytes = golden_out(goldens_dir, off).read_bytes()
    on_bytes = golden_out(goldens_dir, on).read_bytes()
    assert b"\x1b[" not in off_bytes
    assert b"\x1b[" in on_bytes
    assert ANSI.sub(b"", on_bytes) == off_bytes  # the same table once the escapes are gone


def test_every_failing_row_has_a_pinned_stderr(goldens_dir: Path) -> None:
    """Every case with a ``skipped because of error`` row or an abort is in ``EXPECTED_STDERR``, in order."""
    failing = {
        case.id
        for case in CASES
        if SKIPPED_BECAUSE_OF_ERROR in golden_out(goldens_dir, case.id).read_bytes()
        or golden_error(goldens_dir, case.id).exists()
    }
    assert failing == set(EXPECTED_STDERR)
    for case_id, runs in EXPECTED_STDERR.items():
        text = ANSI.sub(b"", golden_out(goldens_dir, case_id).read_bytes())
        positions = [text.find(run.encode() + SKIPPED_BECAUSE_OF_ERROR) for run in runs]
        printed = [position for position in positions if position >= 0]
        assert printed == sorted(printed) and text.count(SKIPPED_BECAUSE_OF_ERROR) == len(printed)
        assert (len(runs) - len(printed) == 1) == golden_error(goldens_dir, case_id).exists()
        assert all(run in STDERR_BLOCKS for run in runs)


def test_every_golden_has_a_case(goldens_dir: Path) -> None:
    ids = {case.id for case in CASES}
    outputs = {path.stem for path in (goldens_dir / "looprun").glob("*.out")}
    errors = {p.name.removesuffix(".error.json") for p in (goldens_dir / "looprun").glob("*.error.json")}
    assert outputs == ids
    assert errors <= ids


def test_the_matrix_and_the_focused_cases_are_listed() -> None:
    ids = {case.id for case in CASES}
    assert set(MATRIX_IDS) <= ids
    assert set(FOCUSED_IDS) <= ids
    both_modes = {Mode.ONCLUSTER, Mode.LOCAL}
    assert {case.mode for case in CASES if case.id in MATRIX_IDS} == both_modes


@pytest.mark.parametrize("case", PARAMS)
def test_looprun_golden(
    case: GoldenCase,
    case_effects: Callable[[GoldenCase], FakeEffects],
    goldens_dir: Path,
    request: pytest.FixtureRequest,
) -> None:
    if any(d.startswith(SYNTHETIC_PREFIX) for d in case.row["dirs"].split(",")):
        request.getfixturevalue("synthetic_dir")
    effects = case_effects(case)
    dirs = case.logical_paths("dirs")
    # make_goldens.R passes user = col(row, "user", Sys.getenv("RS_USER")); an empty cell is the sandbox
    # user, which is what the R default (Sys.info()) and FakeEffects.user give here.
    user = case.row["user"] or None
    out = io.StringIO()
    err = io.StringIO()

    def call() -> str | None:
        return loop_runs(
            dirs,
            user=user,
            colors=flag(case, "colors"),
            sortbytime=flag(case, "sortbytime"),
            effects=effects,
            out=out,
            err=err,
            crayon=crayon_of(case),
        )

    expected = golden_out(goldens_dir, case.id).read_bytes()
    error_file = golden_error(goldens_dir, case.id)
    if error_file.exists():
        message = json.loads(error_file.read_text(encoding="utf-8"))["error"]
        with pytest.raises(LoopRunsError) as excinfo:
            call()
        assert str(excinfo.value) == message
    else:
        assert call() == (NO_RUNS_FOUND if not dirs else None)
    assert out.getvalue().encode("utf-8") == expected
    assert err.getvalue().encode("utf-8") == expected_stderr(goldens_dir, case.id)
