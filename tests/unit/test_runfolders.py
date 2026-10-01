"""``runfolders`` reproduces the path logic of ``R/commandLineInterface.R`` on temporary trees.

The R behaviours pinned here were verified with R 4.6.1 under ``LC_ALL=C.utf8`` (``list.dirs(recursive
= FALSE)`` includes hidden directories and links, composes ``"<dir>/<name>"`` verbatim, gives nothing
for a missing path and orders names by the ICU root collation: ``_x .hid archive Archive b run-1
SSP2``; ``strsplit`` drops one trailing empty piece; ``normalizePath`` of a missing path is the input;
``runfolders[max(which(...))]`` keeps the last list position). The register bugs BUG-025 and BUG-026 are
reproduced, not fixed. ``ProductionEffects`` runs the real filesystem calls; ``FakeEffects`` stands in
where the working directory matters.
"""

from __future__ import annotations

import os
from datetime import datetime
from pathlib import Path
from zoneinfo import ZoneInfo

import pytest

from modelstats.env import Effects, FakeEffects, ProductionEffects
from modelstats.runfolders import (
    expand_paths,
    filter_coupled,
    is_main_folder,
    is_run_folder,
    last_iterations,
    natural_order,
    natural_order_indices,
    natural_sorted,
    r_basename,
    split_comma_list,
)

FROZEN = datetime(2026, 9, 30, 12, 0, 0, tzinfo=ZoneInfo("Europe/Berlin"))


def touch(directory: Path, *names: str) -> Path:
    directory.mkdir(parents=True, exist_ok=True)
    for name in names:
        (directory / name).write_text("", encoding="utf-8")
    return directory


def effects_in(cwd: Path) -> FakeEffects:
    """A fake environment whose working directory is ``cwd`` (no overlay: paths are taken as they are)."""
    return FakeEffects(now=FROZEN, user="tester", on_cluster=False, cwd=cwd)


@pytest.fixture(scope="module")
def env() -> Effects:
    return ProductionEffects()


# --- is.runfolder / is.mainfolder: R/commandLineInterface.R:22-31 -------------------------------


def test_remind_run_folder_needs_four_of_five_markers(tmp_path: Path, env: Effects) -> None:
    run = touch(tmp_path / "run", "full.gms", "log.txt", "config.Rdata", "prepareAndRun.R")
    assert is_run_folder(str(run), env)
    (run / "prepareAndRun.R").unlink()
    assert not is_run_folder(str(run), env)


def test_magpie_run_folder_needs_all_four_markers(tmp_path: Path, env: Effects) -> None:
    run = touch(tmp_path / "run", "full.gms", "submit.R", "config.yml", "magpie_y1995.gdx")
    assert is_run_folder(str(run), env)
    (run / "magpie_y1995.gdx").unlink()
    assert not is_run_folder(str(run), env)


def test_a_directory_named_like_a_marker_counts(tmp_path: Path, env: Effects) -> None:
    run = touch(tmp_path / "run", "log.txt", "config.Rdata", "prepare_and_run.R")
    (run / "full.gms").mkdir()
    assert is_run_folder(str(run), env)


def test_main_folder(tmp_path: Path, env: Effects) -> None:
    main = touch(tmp_path / "main", "output.R", "start.R", "main.gms")
    assert not is_main_folder(str(main), env)
    (main / "output").mkdir()
    assert is_main_folder(str(main), env)
    assert not is_main_folder(str(tmp_path / "nope"), env)


# --- strsplit: R/commandLineInterface.R:115-116 -------------------------------------------------


@pytest.mark.parametrize(
    ("text", "expected"),
    [
        ("a,b", ["a", "b"]),
        ("a,b,", ["a", "b"]),
        ("a,,", ["a", ""]),
        (",a", ["", "a"]),
        (",", [""]),
        ("", []),
        ("single", ["single"]),
    ],
)
def test_split_comma_list_like_strsplit(text: str, expected: list[str]) -> None:
    assert split_comma_list(text) == expected


# --- the expansion: R/commandLineInterface.R:112-115,192-202 -------------------------------------


def test_expand_paths_defaults_to_the_working_directory(tmp_path: Path) -> None:
    (tmp_path / "run2").mkdir()
    (tmp_path / "run1").mkdir()
    (tmp_path / ".hidden").mkdir()
    (tmp_path / "file").write_text("", encoding="utf-8")
    assert expand_paths(None, effects_in(tmp_path)) == ["./.hidden", "./run1", "./run2"]


def test_expand_paths_keeps_a_run_folder_as_given(tmp_path: Path) -> None:
    touch(tmp_path / "run", "full.gms", "log.txt", "config.Rdata", "prepareAndRun.R")
    env = effects_in(tmp_path)
    assert expand_paths("run", env) == ["run"]
    assert expand_paths("run/", env) == ["run/"]
    assert expand_paths(str(tmp_path / "run"), env) == [str(tmp_path / "run")]


def test_expand_paths_lists_the_output_of_a_main_folder(tmp_path: Path, env: Effects) -> None:
    main = touch(tmp_path / "main", "output.R", "start.R", "main.gms")
    (main / "output" / "x").mkdir(parents=True)
    (main / "output" / "y").mkdir()
    (main / "other").mkdir()
    assert expand_paths(str(main), env) == [f"{main}/output/x", f"{main}/output/y"]
    assert expand_paths(f"{main}/", env) == [f"{main}//output/x", f"{main}//output/y"]


def test_expand_paths_lists_any_other_directory_in_r_order_and_skips_missing_ones(
    tmp_path: Path, env: Effects
) -> None:
    out = tmp_path / "out"
    for name in ("SSP2", "archive", "Archive", "_x", "b"):
        (out / name).mkdir(parents=True)
    (out / "file").write_text("", encoding="utf-8")
    (tmp_path / "elsewhere").mkdir()
    os.symlink(tmp_path / "elsewhere", out / "link", target_is_directory=True)
    assert expand_paths(f"{out},{tmp_path}/missing,{tmp_path}/out/", env) == [
        f"{out}/_x",
        f"{out}/archive",
        f"{out}/Archive",
        f"{out}/b",
        f"{out}/link",
        f"{out}/SSP2",
        f"{out}//_x",
        f"{out}//archive",
        f"{out}//Archive",
        f"{out}//b",
        f"{out}//link",
        f"{out}//SSP2",
    ]
    assert expand_paths("", env) == []


# --- -m and -l: R/commandLineInterface.R:207-222 ------------------------------------------------


def test_filter_coupled_decides_by_basename(tmp_path: Path) -> None:
    folders = [
        "a/C_SSP2-rem-1",
        "a/C_SSP2-mag-1",
        "a/SSP2-rem-2",
        "a/SSP2-mag-10",
        "a/SSP2-rem-x",
        "a/plain",
        "C_dir/plain",
        "a/C_SSP2-rem-3/",
    ]
    assert filter_coupled(folders, effects_in(tmp_path)) == [
        "a/C_SSP2-rem-1",
        "a/C_SSP2-mag-1",
        "a/SSP2-rem-2",
        "a/SSP2-mag-10",
        "a/C_SSP2-rem-3/",
    ]


def test_filter_coupled_never_adds_magpie_output(tmp_path: Path) -> None:
    (tmp_path / "magpie" / "output" / "C_x-mag-1").mkdir(parents=True)
    env = effects_in(tmp_path)
    assert filter_coupled(["C_x-rem-1"], env) == ["C_x-rem-1"]  # BUG-026
    assert filter_coupled([], env) == []
    assert [call.args for call in env.calls if call.name == "is_dir"] == [
        ("magpie/output",),
        ("magpie/output",),
    ]


def test_last_iterations_keeps_the_last_list_position() -> None:
    folders = ["x-rem-1", "x-rem-10", "x-rem-2"]
    assert last_iterations(folders) == ["x-rem-2"]  # BUG-025: listing order, not iteration number


def test_last_iterations_groups_by_path_without_suffix() -> None:
    folders = [
        "out/C_a-rem-1",
        "out/C_b-rem-1",
        "out/C_a-mag-1",
        "out/C_a-rem-2",
        "other/C_a-rem-1",
        "out/plain",
    ]
    assert last_iterations(folders) == ["out/C_a-rem-2", "out/C_b-rem-1", "other/C_a-rem-1", "out/plain"]
    assert last_iterations([]) == []


# --- ordering: R/commandLineInterface.R:238, decision D-14 ---------------------------------------


def test_natural_order_sorts_by_the_basename_of_the_resolved_path(tmp_path: Path) -> None:
    for name in ("run-10", "run-9", "run-1"):
        (tmp_path / "out" / name).mkdir(parents=True)
    os.symlink(tmp_path / "out" / "run-9", tmp_path / "alias", target_is_directory=True)
    env = effects_in(tmp_path / "out")
    folders = ["./run-10", f"{tmp_path}/alias", "run-1/", f"{tmp_path}/out/zzz-5", "."]
    assert natural_order(folders, env) == [
        ".",  # normalizePath(".") is <tmp>/out, basename "out"
        "run-1/",
        f"{tmp_path}/alias",  # resolves to run-9
        "./run-10",
        f"{tmp_path}/out/zzz-5",  # missing: its own basename
    ]


def test_natural_order_is_stable_for_equal_names(tmp_path: Path) -> None:
    folders = ["a/x", "b/x", "c/x"]
    assert natural_order(folders, effects_in(tmp_path)) == folders


def test_natural_sorted_is_the_coupled_order_of_the_sort_goldens() -> None:
    names = [
        "C_SSP2-rem-1",
        "C_SSP2-rem-10",
        "C_SSP2-rem-2",
        "C_SSP2-mag-1",
        "C_SSP2-mag-2",
        "C_SSP2EU-rem-3",
        "C_SSP1-rem-1",
    ]
    assert natural_sorted(names) == [
        "C_SSP1-rem-1",
        "C_SSP2-mag-1",
        "C_SSP2-mag-2",
        "C_SSP2-rem-1",
        "C_SSP2-rem-2",
        "C_SSP2-rem-10",
        "C_SSP2EU-rem-3",
    ]
    assert natural_order_indices(names) == [6, 3, 4, 0, 2, 1, 5]
    assert natural_sorted([]) == []


def test_natural_sort_is_case_sensitive_like_the_cluster_locale() -> None:
    assert natural_sorted(["archive", "SSP2-EcBudg500", "SSP2-EU21", "default"]) == [
        "SSP2-EU21",
        "SSP2-EcBudg500",
        "archive",
        "default",
    ]


@pytest.mark.parametrize(
    ("path", "expected"),
    [("a/b", "b"), ("a/b/", "b"), ("b", "b"), ("/", ""), ("", ""), ("./x", "x"), ("a//b//", "b")],
)
def test_r_basename(path: str, expected: str) -> None:
    assert r_basename(path) == expected
