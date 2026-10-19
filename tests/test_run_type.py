from __future__ import annotations

from pathlib import Path

import pytest

from modelstats.run_type import run_type
from tests.conftest import make_run


def yaml_config(model: str, **gms: object) -> str:
    """A config.yml; strings are quoted so that ``on``/``yes`` stay strings as in a config.Rdata."""
    lines = [f"model_name: {model}", "title: x", "gms:"]
    lines += [
        f"  {key}: {value!r}" if isinstance(value, str) else f"  {key}: {value}" for key, value in gms.items()
    ]
    return "\n".join(lines) + "\n"


def test_remind_config_rdata(tmp_path: Path) -> None:
    run = make_run(tmp_path, "nash-run", config="remind")
    assert run_type(run) == "nash"
    assert run_type(run, {"model_name": "REMIND", "gms": {"optimization": "negishi"}}) == "negishi"


@pytest.mark.parametrize(
    ("gms", "expected"),
    [
        ({"optimization": "nash"}, "nash"),
        ({"optimization": "nash", "cm_nash_mode": "debug"}, "nash debug"),
        ({"optimization": "nash", "cm_nash_mode": 1}, "nash debug"),
        ({"optimization": "nash", "cm_nash_mode": 2}, "nash"),
        ({"optimization": "nash", "cm_nash_mode": "1"}, "nash debug"),
        ({"optimization": "nash", "cm_nash_mode": "1.0"}, "nash"),
        ({"optimization": "nash", "cm_nash_mode": True}, "nash debug"),
        ({"optimization": "nash", "cm_MAgPIE_Nash": "1"}, "nash + mag"),
        ({"optimization": "nash", "cm_MAgPIE_Nash": True}, "nash + mag"),
        ({"optimization": 7, "cm_nash_mode": "debug"}, "7 debug"),
        ({"optimization": "testOneRegi", "c_testOneRegi_region": 12.0}, "testOneRegi 12"),
        ({"optimization": "nash", "CES_parameters": "calibrate"}, "Calib_nash"),
        (
            {"optimization": "negishi", "CES_parameters": "calibrate", "cm_nash_mode": "debug"},
            "Calib_negishi debug",
        ),
        ({"optimization": "nash", "cm_MAgPIE_coupling": "on"}, "nash + mag"),
        ({"optimization": "nash", "cm_MAgPIE_Nash": 1}, "nash + mag"),
        ({"optimization": "nash", "cm_MAgPIE_Nash": 0, "cm_MAgPIE_coupling": "off"}, "nash"),
        ({"optimization": "testOneRegi", "c_testOneRegi_region": "EUR"}, "testOneRegi EUR"),
        (
            {"optimization": "testOneRegi", "c_testOneRegi_region": "EUR", "cm_nash_mode": "debug"},
            "debug EUR",
        ),
        ({"optimization": "testOneRegi", "c_testOneRegi_region": "USA", "cm_quick_mode": "on"}, "quick USA"),
        ({"optimization": "testOneRegi", "cm_quick_mode": "on", "cm_nash_mode": 1}, "debug"),
        ({"optimization": "testOneRegi", "CES_parameters": "calibrate"}, "testOneRegi"),
        ({"optimization": "nash", "c_empty_model": "on"}, "empty model"),
        (
            {"optimization": "testOneRegi", "c_testOneRegi_region": "EUR", "c_empty_model": "on"},
            "empty model",
        ),
    ],
)
def test_remind_switches(tmp_path: Path, gms: dict[str, object], expected: str) -> None:
    run = make_run(tmp_path, "run", {"config.yml": yaml_config("REMIND", **gms)})
    assert run_type(run) == expected


def test_magpie_returns_the_optimization_as_is(tmp_path: Path) -> None:
    run = make_run(tmp_path, "mag", config="magpie")
    assert run_type(run) == "nlp_apr17"
    run = make_run(
        tmp_path, "mag2", {"config.yml": yaml_config("MAgPIE", optimization="nlp_par", c_empty_model="on")}
    )
    assert run_type(run) == "nlp_par"


def test_missing_optimization_is_na(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run", {"config.yml": "model_name: REMIND\ngms:\n  cm_nash_mode: debug\n"})
    assert run_type(run) == "NA"
    run = make_run(tmp_path, "mag", {"config.yml": "model_name: MAgPIE\ngms:\n  c_timesteps: 5\n"})
    assert run_type(run) == "NA"
    run = make_run(
        tmp_path, "lst", {"config.yml": "model_name: REMIND\ngms:\n  optimization: [nash, negishi]\n"}
    )
    assert run_type(run) == "NA"
    run = make_run(
        tmp_path, "na", {"config.yml": "model_name: REMIND\ngms:\n  optimization: ~\n  cm_nash_mode: debug\n"}
    )
    assert run_type(run) == "NA"


def test_missing_model_name_follows_the_remind_rules(tmp_path: Path) -> None:
    run = make_run(
        tmp_path, "run", {"config.yml": "gms:\n  optimization: nash\n  CES_parameters: calibrate\n"}
    )
    assert run_type(run) == "Calib_nash"


def test_region_list_is_dropped(tmp_path: Path) -> None:
    run = make_run(
        tmp_path,
        "run",
        {
            "config.yml": "model_name: REMIND\ngms:\n  optimization: testOneRegi\n  c_testOneRegi_region: [EUR, USA]\n"
        },
    )
    assert run_type(run) == "testOneRegi"
    run = make_run(
        tmp_path,
        "one",
        {
            "config.yml": "model_name: REMIND\ngms:\n  optimization: testOneRegi\n  c_testOneRegi_region: !character [EUR]\n"
        },
    )
    assert run_type(run) == "testOneRegi EUR"


def test_config_rdata_wins_over_yml(tmp_path: Path) -> None:
    run = make_run(
        tmp_path, "run", {"config.yml": yaml_config("REMIND", optimization="negishi")}, config="remind"
    )
    assert run_type(run) == "nash"


FULL_LST = (
    "   10  $setGlobal c_expname  default\n"
    "   11  $setGlobal optimization  nash         !! def = nash\n"
    "   12  $setglobal CES_parameters  {ces}       !! def = load\n"
    "   13  $setGlobal optimization  negishi         !! def = nash\n"
)


def test_full_lst_without_config(tmp_path: Path) -> None:
    run = make_run(tmp_path, "run", {"full.lst": FULL_LST.format(ces="load")})
    assert run_type(run) == "nash"
    run = make_run(tmp_path, "calib", {"full.lst": FULL_LST.format(ces="calibrate")})
    assert run_type(run) == "Calib_nash"


def test_full_lst_without_the_lines(tmp_path: Path) -> None:
    run = make_run(
        tmp_path, "run", {"full.lst": "   11  $setGlobal optimization  nash         !! def = nash\n"}
    )
    assert run_type(run) == "nash"
    run = make_run(tmp_path, "empty", {"full.lst": "nothing here\n"})
    assert run_type(run) == "NA"


def test_nothing_known_is_na(tmp_path: Path) -> None:
    assert run_type(make_run(tmp_path, "bare")) == "NA"
    assert run_type(tmp_path / "missing") == "NA"


def test_one_item_yaml_sequences_are_scalars(tmp_path: Path) -> None:
    cfg = "model_name: REMIND\ngms:\n  optimization: [nash]\n  c_testOneRegi_region: [EUR]\n  cm_nash_mode: debug\n"
    assert run_type(make_run(tmp_path, "plain", {"config.yml": cfg})) == "nash debug"
