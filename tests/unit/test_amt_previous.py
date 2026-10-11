"""``modelstats.amt.previous``: the on-disk lookup of the previous run (``06`` 4.2, D-24).

The runs are synthetic (``tests/unit/test_run_status.py`` builders) in a ``tmp_path`` output directory with
an ``archive/`` below it; the effects are ``FakeEffects`` without an overlay. The R rule the lookup keeps
(converged or converged (had INFES) with a mif, ``R/modeltests.R:314-315``) and the two defects it drops
(BUG-014: no earlier sibling aborted; BUG-039: a history entry without ``runstatistics.rda`` aborted) are
each pinned by a case.
"""

from __future__ import annotations

import logging
from datetime import datetime
from pathlib import Path

import pytest

from modelstats.amt.previous import (
    ARCHIVE_DIR,
    RunName,
    candidate_runs,
    find_previous_run,
    is_usable_previous,
    run_name,
)
from modelstats.run import Convergence, MifState, Run
from tests.unit.test_facts import effects, make_run, remind_cfg, write_gdx
from tests.unit.test_run_status import remind_run

CONVERGED = {"o_iterationNumber": 26, "o_modelstat": 2, "s80_bool": 1}
NOT_CONVERGED = {"o_iterationNumber": 50, "o_modelstat": 2, "s80_bool": 0}
TITLE = "t"
CURRENT = f"{TITLE}_2026-09-28_10.30.27"


def stamped(day: str, time: str = "10.30.27", title: str = TITLE) -> str:
    return f"{title}_{day}_{time}"


# --- the run name -------------------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("name", "expected"),
    [
        ("SSP2-NPi-AMT_2026-09-28_10.30.27", RunName("SSP2-NPi-AMT", datetime(2026, 9, 28, 10, 30, 27))),
        ("a_b_2026-01-02_00.00.00", RunName("a_b", datetime(2026, 1, 2))),
        ("testOneRegi-AMT", None),
        ("archive", None),
        ("SSP2-NPi-AMT_2026-09-25_9.11.12", None),
        ("_2026-09-28_10.30.27", None),
        ("t_2026-13-45_10.30.27", None),
    ],
)
def test_run_name_parses_title_and_stamp(name: str, expected: RunName | None) -> None:
    assert run_name(name) == expected


def test_run_name_stamp_round_trips() -> None:
    parsed = run_name(CURRENT)
    assert parsed is not None and parsed.stamp == "2026-09-28_10.30.27"


# --- the candidates ------------------------------------------------------------------------------------


def test_candidates_are_the_earlier_siblings_of_both_locations_newest_first(tmp_path: Path) -> None:
    output = tmp_path / "output"
    archive = output / ARCHIVE_DIR
    for name in (CURRENT, stamped("2026-09-21"), stamped("2026-09-29"), stamped("2026-09-07")):
        make_run(output, name)
    for name in (stamped("2026-09-14"), stamped("2026-09-21", "09.00.00"), stamped("2026-09-07")):
        make_run(archive, name)
    make_run(output, "t2_2026-09-21_10.30.27")  # another title that contains this one
    make_run(output, "t-AMT")  # no stamp
    (output / stamped("2026-09-20")).write_text("a file, not a run")
    before = datetime(2026, 9, 28, 10, 30, 27)
    found = candidate_runs(effects(tmp_path), output, TITLE, before=before)
    assert [(run.name, run.path.parent.name) for run in found] == [
        (stamped("2026-09-21"), "output"),
        (stamped("2026-09-21", "09.00.00"), ARCHIVE_DIR),
        (stamped("2026-09-14"), ARCHIVE_DIR),
        (stamped("2026-09-07"), "output"),
        (stamped("2026-09-07"), ARCHIVE_DIR),
    ]


def test_candidates_without_an_archive_directory(tmp_path: Path) -> None:
    output = tmp_path / "output"
    make_run(output, stamped("2026-09-21"))
    found = candidate_runs(effects(tmp_path), output, TITLE, before=datetime(2026, 9, 28))
    assert [run.name for run in found] == [stamped("2026-09-21")]


# --- the usability rule (R/modeltests.R:314-315) -------------------------------------------------------


def test_usable_means_converged_with_a_mif(tmp_path: Path) -> None:
    env = effects(tmp_path)
    converged = Run(remind_run(tmp_path, "a", gdx=CONVERGED), env)
    assert converged.convergence is Convergence.CONVERGED and converged.mif is MifState.PRESENT
    assert is_usable_previous(converged)
    had_infes = remind_run(tmp_path, "b", gdx=CONVERGED)
    write_gdx(had_infes / "non_optimal.gdx", scalars={"o_iterationNumber": 3, "o_modelstat": 5})
    assert is_usable_previous(Run(had_infes, env))
    with_errors = remind_run(
        tmp_path, "c", gdx=CONVERGED, extra={"REMIND_generic_t_summation_errors.csv": "x"}
    )
    assert Run(with_errors, env).mif is MifState.SUMMATION_ERRORS
    assert is_usable_previous(Run(with_errors, env))
    assert not is_usable_previous(Run(remind_run(tmp_path, "d", gdx=NOT_CONVERGED), env))
    assert not is_usable_previous(Run(remind_run(tmp_path, "e", gdx=CONVERGED, mif=False), env))
    assert not is_usable_previous(Run(remind_run(tmp_path, "f"), env))  # no GDX: Conv NA
    assert not is_usable_previous(Run(make_run(tmp_path, "g"), env))  # no config at all


def test_calibration_converged_is_not_usable_as_r_listed_only_the_two_texts(tmp_path: Path) -> None:
    env = effects(tmp_path)
    calibrating = remind_run(tmp_path, "clb", cfg=remind_cfg(ces="calibrate"), gdx=CONVERGED)
    for i in range(1, 12):
        (calibrating / f"fulldata_{i:02d}.gdx").write_bytes(b"")
    run = Run(calibrating, env)
    assert run.convergence is Convergence.CALIBRATION_CONVERGED
    assert not is_usable_previous(run)


# --- find_previous_run ---------------------------------------------------------------------------------


def test_the_newest_usable_earlier_run_wins(tmp_path: Path) -> None:
    output = tmp_path / "output"
    current = remind_run(output, CURRENT, gdx=CONVERGED)
    remind_run(output, stamped("2026-09-21"), gdx=CONVERGED)
    remind_run(output, stamped("2026-09-24"), gdx=NOT_CONVERGED)  # newer, but not converged
    remind_run(output, stamped("2026-09-25"), gdx=CONVERGED, mif=False)  # newer, but no mif
    remind_run(output, stamped("2026-09-29"), gdx=CONVERGED)  # later than the current run
    remind_run(output / ARCHIVE_DIR, stamped("2026-09-14"), gdx=CONVERGED)
    env = effects(tmp_path)
    previous = find_previous_run(Run(current, env), output, env)
    assert previous is not None and previous.name == stamped("2026-09-21")


def test_the_archive_is_searched_when_output_has_nothing_usable(tmp_path: Path) -> None:
    output = tmp_path / "output"
    current = remind_run(output, CURRENT, gdx=CONVERGED)
    remind_run(output, stamped("2026-09-21"), gdx=NOT_CONVERGED)
    remind_run(output / ARCHIVE_DIR, stamped("2026-09-14"), gdx=CONVERGED)
    env = effects(tmp_path)
    previous = find_previous_run(Run(current, env), output, env)
    assert previous is not None
    assert previous.path == (output / ARCHIVE_DIR / stamped("2026-09-14")).resolve()


def test_no_earlier_sibling_is_none_not_an_abort(tmp_path: Path) -> None:
    """BUG-014 gone by design: ``max(character(0))`` made R abort here."""
    output = tmp_path / "output"
    current = remind_run(output, CURRENT, gdx=CONVERGED)
    remind_run(output, stamped("2026-09-29"), gdx=CONVERGED)
    env = effects(tmp_path)
    assert find_previous_run(Run(current, env), output, env) is None


def test_a_gutted_earlier_run_is_skipped_not_an_abort(tmp_path: Path) -> None:
    """BUG-039 gone by design: a history row whose folder lost its files made ``.readRuntime()`` abort."""
    output = tmp_path / "output"
    current = remind_run(output, CURRENT, gdx=CONVERGED)
    make_run(output / ARCHIVE_DIR, stamped("2026-09-21"), {"config.Rdata": b"", "prepareAndRun.R": ""})
    remind_run(output / ARCHIVE_DIR, stamped("2026-09-14"), gdx=CONVERGED)
    env = effects(tmp_path)
    previous = find_previous_run(Run(current, env), output, env)
    assert previous is not None and previous.name == stamped("2026-09-14")


def test_an_unreadable_candidate_is_logged_and_skipped(
    tmp_path: Path, caplog: pytest.LogCaptureFixture
) -> None:
    output = tmp_path / "output"
    current = remind_run(output, CURRENT, gdx=CONVERGED)
    broken = remind_run(output, stamped("2026-09-21"), gdx=CONVERGED)
    (broken / "fulldata.gdx").write_bytes(b"not a gdx file")
    remind_run(output, stamped("2026-09-14"), gdx=CONVERGED)
    env = effects(tmp_path)
    logger = logging.getLogger("test.previous")
    with caplog.at_level(logging.WARNING, logger=logger.name):
        previous = find_previous_run(Run(current, env), output, env, logger=logger)
    assert previous is not None and previous.name == stamped("2026-09-14")
    assert any(stamped("2026-09-21") in record.message for record in caplog.records)


def test_a_run_without_config_or_stamp_has_no_previous(tmp_path: Path) -> None:
    output = tmp_path / "output"
    env = effects(tmp_path)
    remind_run(output, stamped("2026-09-21"), gdx=CONVERGED)
    no_config = make_run(output, stamped("2026-09-28"), {"full.log": "*** Status: Normal completion\n"})
    assert find_previous_run(Run(no_config, env), output, env) is None
    unstamped = remind_run(output, "t-AMT", gdx=CONVERGED)
    assert find_previous_run(Run(unstamped, env), output, env) is None


def test_only_the_exact_title_counts(tmp_path: Path) -> None:
    """R's ``grepl(cfg$title, rownames)`` also matched a longer title; the lookup matches the name exactly."""
    output = tmp_path / "output"
    current = remind_run(output, CURRENT, gdx=CONVERGED)
    longer = remind_run(output, stamped("2026-09-21", title="t-extra"), gdx=CONVERGED, mif=False)
    (longer / "REMIND_generic_t-extra.mif").write_text("mif")
    env = effects(tmp_path)
    assert find_previous_run(Run(current, env), output, env) is None
