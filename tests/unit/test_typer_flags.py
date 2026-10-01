"""typer parses the ``rs`` option set the way R's optparse did.

Converted from ``migration/harness/smoke/typer_test.py`` and ``typer_rs.py``. The port's ``rs``
(phase 4) relies on typer 0.27 (own parser, no click) for bundled short flags, attached values and
one optional positional (``migration/02-python-libraries.md`` section 4.6). This module pins that
behaviour on a stand-in app with the twelve options of ``R/commandLineInterface.R``; the real app is
tested in its own phase.
"""

from __future__ import annotations

import json
from typing import Annotated

import pytest
import typer
from typer.testing import CliRunner

app = typer.Typer(
    add_completion=False,
    rich_markup_mode=None,
    pretty_exceptions_enable=False,
    context_settings={"help_option_names": ["-h", "--help"]},
)


@app.command()
def rs(
    paths: Annotated[str | None, typer.Argument()] = None,
    amt: Annotated[bool, typer.Option("-A", "--amt")] = False,
    nocolor: Annotated[bool, typer.Option("-b", "--nocolor")] = False,
    current: Annotated[bool, typer.Option("-C", "--current")] = False,
    daysback: Annotated[int, typer.Option("-d", "--daysback", metavar="N")] = 0,
    filter_: Annotated[str, typer.Option("-f", "--filter", metavar="REGEX")] = ".*",
    last: Annotated[bool, typer.Option("-l", "--last")] = False,
    magpie: Annotated[bool, typer.Option("-m", "--magpie")] = False,
    prompt: Annotated[bool, typer.Option("-p", "--prompt")] = False,
    sanity: Annotated[bool, typer.Option("-s", "--sanity")] = False,
    time: Annotated[bool, typer.Option("-t", "--time")] = False,
    user: Annotated[str, typer.Option("-u", "--user", metavar="USER")] = "you",
) -> None:
    """Stand-in for the rs command: print the parsed options as JSON."""
    parsed = {
        "paths": paths,
        "A": amt,
        "b": nocolor,
        "C": current,
        "d": daysback,
        "f": filter_,
        "l": last,
        "m": magpie,
        "p": prompt,
        "s": sanity,
        "t": time,
        "u": user,
    }
    print(json.dumps(parsed))


DEFAULTS: dict[str, object] = {
    "paths": None,
    "A": False,
    "b": False,
    "C": False,
    "d": 0,
    "f": ".*",
    "l": False,
    "m": False,
    "p": False,
    "s": False,
    "t": False,
    "u": "you",
}


def parse(args: list[str]) -> dict[str, object]:
    result = CliRunner().invoke(app, args)
    assert result.exit_code == 0, result.output
    parsed: dict[str, object] = json.loads(result.stdout.strip().splitlines()[-1])
    return parsed


@pytest.mark.parametrize(
    ("args", "expected"),
    [
        ([], {}),
        (["-mltpbf", "PkBudg"], {"m": True, "l": True, "t": True, "p": True, "b": True, "f": "PkBudg"}),
        (
            ["/p/projects/remind/runs/X/remind/", "-mltpbf", "PkBudg"],
            {
                "paths": "/p/projects/remind/runs/X/remind/",
                "m": True,
                "l": True,
                "t": True,
                "p": True,
                "b": True,
                "f": "PkBudg",
            },
        ),
        (["-As"], {"A": True, "s": True}),
        (["-C", "-u", "alf", "-d", "5"], {"C": True, "u": "alf", "d": 5}),
        (["-A", "-f", "SSP2EU-Base"], {"A": True, "f": "SSP2EU-Base"}),
        (["folder1,folder2", "-f", "PkBudg500,EU21"], {"paths": "folder1,folder2", "f": "PkBudg500,EU21"}),
        (["-fPkBudg"], {"f": "PkBudg"}),
        (["-bt", "."], {"b": True, "t": True, "paths": "."}),
    ],
    ids=lambda value: " ".join(value) if isinstance(value, list) else "",
)
def test_documented_invocations_parse_like_optparse(args: list[str], expected: dict[str, object]) -> None:
    assert parse(args) == {**DEFAULTS, **expected}


@pytest.mark.parametrize("args", [["-x"], ["a", "b"]], ids=["unknown flag", "second positional"])
def test_usage_errors_exit_2_with_an_error_line(args: list[str]) -> None:
    result = CliRunner().invoke(app, args)
    assert result.exit_code == 2
    assert "Error" in result.output
    assert "Usage" in result.output


@pytest.mark.parametrize("flag", ["-h", "--help"])
def test_help_is_reachable_through_both_aliases(flag: str) -> None:
    result = CliRunner().invoke(app, [flag])
    assert result.exit_code == 0
    assert result.output.startswith("Usage: rs [OPTIONS] [paths]")
    assert "-f, --filter REGEX" in result.output
