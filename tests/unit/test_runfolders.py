"""``modelstats.runfolders`` against ``R/commandLineInterface.R`` and the sort goldens.

The nine ``sort`` cases and their R results (``stringi::stri_order(numeric = TRUE)`` in
the sandbox) are embedded here so that the test runs without ``migration/``; a second
test checks the embedded copies against ``migration/goldens/sort`` when that tree exists.
"""

from __future__ import annotations

import json
import os
from pathlib import Path
from typing import Any

import pytest

from modelstats.env import Effects, FileStat
from modelstats.runfolders import (
    expand_paths,
    filter_coupled,
    is_main_folder,
    is_run_folder,
    last_iterations,
    natural_order,
    natural_order_indices,
    r_basename,
    split_comma,
)

REPO = Path(__file__).resolve().parents[2]
SORT_CASES = REPO / "migration" / "cases" / "sort"
SORT_GOLDENS = REPO / "migration" / "goldens" / "sort"


class FsEffects(Effects):
    """A read-only Effects over the real filesystem (what the tests need of it)."""

    def listdir_like_r(self, directory: str | os.PathLike[str]) -> list[str]:
        try:
            names = os.listdir(directory)
        except FileNotFoundError, NotADirectoryError:
            return []
        return sorted(name for name in names if not name.startswith("."))

    def exists(self, path: str | os.PathLike[str]) -> bool:
        return os.path.exists(path)

    def is_dir(self, path: str | os.PathLike[str]) -> bool:
        return os.path.isdir(path)

    def stat(self, path: str | os.PathLike[str]) -> FileStat:
        st = os.stat(path)
        return FileStat(mtime=st.st_mtime, ctime=st.st_ctime, size=st.st_size)

    def read_text(self, path: str | os.PathLike[str]) -> str:
        return Path(path).read_text(encoding="utf-8")

    def read_bytes(self, path: str | os.PathLike[str]) -> bytes:
        return Path(path).read_bytes()

    def glob(self, pattern: str | os.PathLike[str]) -> list[str]:
        raise NotImplementedError

    def run(self, argv: Any, cwd: Any = None, env: Any = None, input: Any = None) -> Any:  # noqa: A002
        raise NotImplementedError

    def now(self) -> Any:
        raise NotImplementedError

    def today(self) -> Any:
        raise NotImplementedError

    @property
    def user(self) -> str:
        return "tester"

    @property
    def on_cluster(self) -> bool:
        return False

    def getenv(self, name: str, default: str = "") -> str:
        return default


@pytest.fixture
def effects() -> FsEffects:
    return FsEffects()


def touch(folder: Path, *names: str) -> None:
    folder.mkdir(parents=True, exist_ok=True)
    for name in names:
        if name.endswith("/"):
            (folder / name).mkdir()
        else:
            (folder / name).write_text("", encoding="utf-8")


# --- is.runfolder / is.mainfolder ---------------------------------------------------------------


@pytest.mark.parametrize(
    ("names", "expected"),
    [
        (("full.gms", "log.txt", "config.Rdata", "prepare_and_run.R", "prepareAndRun.R"), True),
        (("full.gms", "log.txt", "config.Rdata", "prepare_and_run.R"), True),
        (("full.gms", "log.txt", "config.Rdata", "prepareAndRun.R"), True),
        (("full.gms", "log.txt", "config.Rdata"), False),
        (("config.Rdata", "log.txt"), False),  # BUG-027: a run that failed before full.gms
        (("full.gms", "submit.R", "config.yml", "magpie_y1995.gdx"), True),
        (("full.gms", "submit.R", "config.yml"), False),
        (("full.gms", "log.txt", "config.Rdata", "submit.R", "config.yml", "magpie_y1995.gdx"), True),
        ((), False),
    ],
)
def test_is_run_folder(tmp_path: Path, effects: FsEffects, names: tuple[str, ...], expected: bool) -> None:
    touch(tmp_path, *names)
    assert is_run_folder(str(tmp_path), effects) is expected


def test_is_run_folder_counts_directories_too(tmp_path: Path, effects: FsEffects) -> None:
    # file.exists() is true for directories as well
    touch(tmp_path, "full.gms/", "log.txt/", "config.Rdata/", "prepare_and_run.R/")
    assert is_run_folder(str(tmp_path), effects) is True


def test_is_run_folder_nonexistent(tmp_path: Path, effects: FsEffects) -> None:
    assert is_run_folder(str(tmp_path / "nowhere"), effects) is False


@pytest.mark.parametrize(
    ("names", "expected"),
    [
        (("output/", "output.R", "start.R", "main.gms"), True),
        (("output/", "output.R", "start.R"), False),
        (("output.R", "start.R", "main.gms"), False),
        ((), False),
    ],
)
def test_is_main_folder(tmp_path: Path, effects: FsEffects, names: tuple[str, ...], expected: bool) -> None:
    touch(tmp_path, *names)
    assert is_main_folder(str(tmp_path), effects) is expected


def test_default_effects(tmp_path: Path) -> None:
    touch(tmp_path, "output/", "output.R", "start.R", "main.gms")
    assert is_main_folder(str(tmp_path)) is True
    assert is_run_folder(str(tmp_path)) is False


# --- paths ---------------------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("arg", "expected"),
    [
        (None, ["."]),
        (".", ["."]),
        ("a,b", ["a", "b"]),
        ("a", ["a"]),
        ("", []),
        ("a,", ["a"]),
        (",a", ["", "a"]),
        ("a,,b", ["a", "", "b"]),
        (",", [""]),
        (",,", ["", ""]),
        ("/p/x/output,/p/y/output/", ["/p/x/output", "/p/y/output/"]),
    ],
)
def test_expand_paths(arg: str | None, expected: list[str]) -> None:
    assert expand_paths(arg) == expected


def test_split_comma_is_r_strsplit() -> None:
    assert split_comma("PkBudg500,EU21") == ["PkBudg500", "EU21"]
    assert split_comma("a,b,") == ["a", "b"]
    assert split_comma("") == []


@pytest.mark.parametrize(
    ("path", "expected"),
    [
        ("a/b/", "b"),
        ("a//b//", "b"),
        ("/", ""),
        ("", ""),
        (".", "."),
        ("x", "x"),
        ("/x", "x"),
        ("./output/C_SSP2-rem-1", "C_SSP2-rem-1"),
        ("magpie/output", "output"),
    ],
)
def test_r_basename(path: str, expected: str) -> None:
    assert r_basename(path) == expected


# --- the -m and -l blocks ------------------------------------------------------------------------

COUPLED = [
    "./output/C_SSP2EU-Base-rem-1",
    "./output/C_SSP2EU-Base-rem-10",
    "./output/C_SSP2EU-Base-rem-2",
    "./output/SSP2-NPi-AMT_2026-09-28_10.30.27",
    "./output/C_SSP2-NDC-LTS_2026-07-08_05.05.25",
    "../magpie/output/C_SSP2EU-Base-mag-1",
    "../magpie/output/C_SSP2EU-Base-mag-10",
    "/p/rem/x/output/default-AMT_2026-09-28_13.23.51",
    "/p/mag-1/output/testOneRegi",
    "./output/C_x-rem-1/",
]


def test_filter_coupled_by_basename(tmp_path: Path, effects: FsEffects) -> None:
    assert filter_coupled(COUPLED, str(tmp_path), effects) == [
        "./output/C_SSP2EU-Base-rem-1",
        "./output/C_SSP2EU-Base-rem-10",
        "./output/C_SSP2EU-Base-rem-2",
        "./output/C_SSP2-NDC-LTS_2026-07-08_05.05.25",
        "../magpie/output/C_SSP2EU-Base-mag-1",
        "../magpie/output/C_SSP2EU-Base-mag-10",
        "./output/C_x-rem-1/",
    ]


def test_filter_coupled_magpie_output_is_added_and_removed_again(tmp_path: Path, effects: FsEffects) -> None:
    # BUG-026: dir.exists("magpie/output") relative to the cwd appends "magpie/output", whose
    # basename "output" never matches the coupled pattern
    (tmp_path / "magpie" / "output").mkdir(parents=True)
    assert filter_coupled(["./output/C_a-rem-1", "./output/b"], str(tmp_path), effects) == ["./output/C_a-rem-1"]
    assert filter_coupled([], str(tmp_path), effects) == []


def test_filter_coupled_does_not_touch_the_input(tmp_path: Path, effects: FsEffects) -> None:
    folders = ["./output/C_a-rem-1"]
    filter_coupled(folders, str(tmp_path), effects)
    assert folders == ["./output/C_a-rem-1"]


def test_last_iterations_keeps_the_largest_list_position() -> None:
    # BUG-025: listing order, not iteration number: rem-2 survives, rem-10 does not
    folders = ["o/C_x-rem-1", "o/C_x-rem-10", "o/C_x-rem-2"]
    assert last_iterations(folders) == ["o/C_x-rem-2"]


def test_last_iterations_per_prefix_in_order_of_first_appearance() -> None:
    folders = [
        "o/C_b-rem-3",
        "o/C_a-rem-1",
        "o/C_a-rem-2",
        "m/C_a-mag-1",
        "m/C_a-mag-2",
        "o/C_b-rem-1",
        "o/C_new_2026-07-08_05.05.25",
    ]
    assert last_iterations(folders) == ["o/C_b-rem-1", "o/C_a-rem-2", "m/C_a-mag-2", "o/C_new_2026-07-08_05.05.25"]


def test_last_iterations_prefix_is_the_whole_path() -> None:
    # the same run name below two roots is two prefixes
    folders = ["a/C_x-rem-1", "b/C_x-rem-1"]
    assert last_iterations(folders) == ["a/C_x-rem-1", "b/C_x-rem-1"]


def test_last_iterations_empty() -> None:
    assert last_iterations([]) == []


# --- natural order (stri_order(numeric = TRUE) as the goldens pin it) ----------------------------

SORT_CASE_DATA: dict[str, tuple[list[str], list[int]]] = {
    "amt-names": (
        [
            "SSP2-NPi-AMT_2026-09-28_10.30.27",
            "default-AMT_2026-09-28_13.23.51",
            "SSP2-EU21-PkBudg650-AMT_2026-09-28_13.54.59",
            "SSP2-NPi2025-calibrate-AMT_2026-09-28_10.27.04",
            "testOneRegi-AMT",
            "testOneRegi",
            "SSP2-EcBudg500-AMT_2026-09-19_01.25.14",
            "SSP3-NPi2025-AMT_2026-09-28_17.12.58",
            "SSP2-NPi-AMT_2026-08-28_22.06.57",
            "SSP2-EU21-NPi2025-AMT_2026-09-18_22.11.36",
            "export",
            "gamscompile",
            "archive/SSP2-EU21-NPi-AMT_2025-07-25_22.18.56",
            "archive/SSP2-NPi2025-calibrate-AMT_2026-06-26_22.16.54",
            "archive/default-AMT_2026-06-27_00.53.26",
        ],
        [10, 3, 7, 4, 9, 1, 8, 13, 14, 15, 2, 11, 12, 6, 5],
    ),
    "cli-list": (
        [
            "run-rem-10",
            "run-rem-2",
            "C_SSP2-rem-1",
            "SSP2-NPi-AMT_2026-09-25_10.11.12",
            "SSP2-NPi-AMT_2026-09-25_9.11.12",
            "b",
            "B",
            "a10",
            "a2",
            "A1",
            "_x",
            "x-1",
            "x_1",
            "x.1",
            "Run1",
        ],
        [10, 7, 3, 15, 5, 4, 11, 9, 8, 6, 2, 1, 12, 14, 13],
    ),
    "coupled-9-10-11": (
        [
            "C_SSP2EU-Base-rem-11",
            "C_SSP2EU-Base-rem-9",
            "C_SSP2EU-Base-rem-10",
            "C_SSP2EU-Base-rem-1",
            "C_SSP2EU-Base-rem-2",
            "C_SSP2EU-Base-mag-10",
            "C_SSP2EU-Base-mag-9",
            "C_SSP2EU-Base-mag-1",
        ],
        [8, 7, 6, 4, 5, 2, 3, 1],
    ),
    "coupled": (
        [
            "C_SSP2-rem-1",
            "C_SSP2-rem-10",
            "C_SSP2-rem-2",
            "C_SSP2-mag-1",
            "C_SSP2-mag-2",
            "C_SSP2EU-rem-3",
            "C_SSP1-rem-1",
        ],
        [7, 4, 5, 1, 3, 2, 6],
    ),
    "duplicates": (["a", "a", "A", "a1", "a1", "a01"], [3, 1, 2, 4, 5, 6]),
    "empty": ([], []),
    "exotic": (
        [
            "run01",
            "run1",
            "run010",
            "run10",
            "Ä",
            "ä",
            "z",
            "Z",
            "a-1",
            "a_1",
            "a 1",
            "1a",
            "01a",
            "10",
            "9",
            "1.10",
            "1.9",
            "é",
            "e",
            "E",
            "ß",
            "ss",
            "ﬁ",
            "fi",
            "x1y2",
            "x1y10",
            "x10y1",
            "",
            " ",
        ],
        [28, 17, 16, 12, 13, 15, 14, 29, 5, 20, 8, 6, 11, 9, 10, 19, 18, 24, 1, 2, 3, 4, 22, 25, 26, 27, 7, 23, 21],
    ),
    "single": (["only"], [1]),
    "sortroot": (
        [
            "run-rem-10",
            "run-rem-2",
            "run-rem-1",
            "C_SSP2-rem-1",
            "SSP2-NPi-AMT_2026-09-25_10.11.12",
            "SSP2-NPi-AMT_2026-09-25_9.11.12",
            "b",
            "B",
            "a10",
            "a2",
            "A1",
            "_x",
            "x-1",
            "x_1",
            "x.1",
            "Run1",
        ],
        [11, 8, 4, 16, 6, 5, 12, 10, 9, 7, 3, 2, 1, 13, 15, 14],
    ),
}


@pytest.mark.parametrize("case", sorted(SORT_CASE_DATA))
def test_natural_order_matches_the_sort_goldens(case: str) -> None:
    names, order = SORT_CASE_DATA[case]
    assert natural_order_indices(names) == order
    assert natural_order(names) == [names[i - 1] for i in order]


@pytest.mark.skipif(not SORT_GOLDENS.is_dir(), reason="migration/goldens is absent")
@pytest.mark.parametrize("case", sorted(SORT_CASE_DATA))
def test_embedded_sort_cases_equal_the_migration_files(case: str) -> None:
    with open(SORT_CASES / f"{case}.json", encoding="utf-8") as handle:
        names = json.load(handle)["names"]
    with open(SORT_GOLDENS / f"{case}.json", encoding="utf-8") as handle:
        golden = json.load(handle)
    assert (names, golden["order"]) == SORT_CASE_DATA[case]
    assert natural_order(names) == golden["value"]


@pytest.mark.skipif(not SORT_GOLDENS.is_dir(), reason="migration/goldens is absent")
def test_every_sort_golden_is_embedded() -> None:
    assert {path.stem for path in SORT_GOLDENS.glob("*.json")} == set(SORT_CASE_DATA)


def test_natural_order_sample_of_02_section_4_4() -> None:
    names = [
        "C_SSP2-rem-1",
        "C_SSP2-rem-10",
        "C_SSP2-rem-2",
        "C_SSP2-mag-1",
        "C_SSP2-mag-2",
        "C_SSP2EU-rem-3",
        "C_SSP1-rem-1",
    ]
    assert natural_order(names) == [
        "C_SSP1-rem-1",
        "C_SSP2-mag-1",
        "C_SSP2-mag-2",
        "C_SSP2-rem-1",
        "C_SSP2-rem-2",
        "C_SSP2-rem-10",
        "C_SSP2EU-rem-3",
    ]


def test_natural_order_is_stable_and_one_based() -> None:
    assert natural_order_indices(["b", "a", "a", "A"]) == [4, 2, 3, 1]
    assert natural_order_indices(["x"]) == [1]
    assert natural_order_indices([]) == []


def test_natural_order_numbers_before_letters_and_case_blocks() -> None:
    # ICU en_US_POSIX as in the sandbox: digits < uppercase < "_" < lowercase, numeric runs
    assert natural_order(["b", "_x", "A", "10", "9", "a", "B"]) == ["9", "10", "A", "B", "_x", "a", "b"]
    assert natural_order(["e", "é", "ê", "E"]) == ["E", "e", "é", "ê"]
    assert natural_order(["ä1", "a2", "ä10", "a 1"]) == ["ä1", "a2", "ä10", "a 1"]


# stringi::stri_order(x, numeric = TRUE) under LC_ALL=C.utf8 (R 4.6.1, stringi, 2026-10-01): the
# sandbox collation, ICU en_US_POSIX. Numbers weigh less than every tailored ASCII character
# (even space and punctuation), uppercase precedes lowercase, accents and expansions follow root.
R_ICU_ORDERS = [
    ("ä1 | a2 | ä10 | a 1", ["ä1", "a2", "ä10", "a 1"]),
    ("a | a1 | a 1 | a-1 | a.1 | a_1 | aa", ["a-1", "a1", "a 1", "a_1", "aa", "a.1", "a"]),
    ("x | x1 | x1y | x  | x- | x-y", ["x1", "x-", "x ", "x", "x1y", "x-y"]),
    ("E | e | é | ê", ["e", "é", "ê", "E"]),
    ("9 | 10 | A | B | _x | a | b", ["b", "_x", "A", "10", "9", "a", "B"]),
    ("RUN1 | Run1 | rUn1 | run1 | run-rem-2", ["Run1", "run-rem-2", "RUN1", "run1", "rUn1"]),
    (
        "C_SSP1-rem-1 | C_SSP2-rem-1 | C_SSP2-rem-10 | C_SSP2EU-rem-3 | c_ssp2-rem-2",
        ["C_SSP2-rem-1", "C_SSP2-rem-10", "c_ssp2-rem-2", "C_SSP2EU-rem-3", "C_SSP1-rem-1"],
    ),
    ("O | Ö | P | o | ö | oe | p", ["ö", "o", "Ö", "O", "oe", "p", "P"]),
    (" | 1 | 1a |   | - | _ | a | a1", ["1a", "a1", "1", "a", "", " ", "-", "_"]),
    (
        "default-AMT_2026-06-27_00.53.26 | default-AMT_2026-6-27_00.53.26 | default-AMT_2026-06-27_0.53.26"
        " | default-AMT_2026-06-27_00.53.26 x | default-AMT_2026-06-27_00.53.26-x",
        [
            "default-AMT_2026-06-27_00.53.26",
            "default-AMT_2026-6-27_00.53.26",
            "default-AMT_2026-06-27_0.53.26",
            "default-AMT_2026-06-27_00.53.26-x",
            "default-AMT_2026-06-27_00.53.26 x",
        ],
    ),
]


@pytest.mark.parametrize(("r_output", "names"), R_ICU_ORDERS, ids=range(len(R_ICU_ORDERS)))
def test_natural_order_matches_r_in_the_sandbox_locale(r_output: str, names: list[str]) -> None:
    assert " | ".join(natural_order(names)) == r_output
