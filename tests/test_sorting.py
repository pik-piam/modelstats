from __future__ import annotations

from pathlib import Path

from modelstats._sorting import (
    RunName,
    existing_dirs,
    is_coupled,
    last_iterations,
    last_timestamps,
    natural_key,
    parse_run_name,
    run_sort_key,
)


def test_parse_iteration_name() -> None:
    assert parse_run_name("C_SSP2-NPi-rem-10") == RunName("C_SSP2-NPi-rem-10", "C_SSP2-NPi", "rem", 10, None)
    assert parse_run_name("C_SSP2-NPi-mag-3") == RunName("C_SSP2-NPi-mag-3", "C_SSP2-NPi", "mag", 3, None)


def test_parse_timestamp_name() -> None:
    run = parse_run_name("SSP2-NPi-AMT_2026-10-18_10.00.00")
    assert run == RunName(
        "SSP2-NPi-AMT_2026-10-18_10.00.00", "SSP2-NPi-AMT", None, None, "2026-10-18_10.00.00"
    )


def test_parse_plain_name() -> None:
    assert parse_run_name("default") == RunName("default", "default", None, None, None)


def test_natural_key_numbers_and_case() -> None:
    assert natural_key("PkBudg500") < natural_key("PkBudg1000")
    assert natural_key("Abc") < natural_key("abd")
    assert natural_key("b") < natural_key("b-rem-9")


def test_run_sort_key_natural_order() -> None:
    # like stringi::stri_order(x, numeric = TRUE) (R 4.6.1): numbers numeric, letters case-insensitive;
    # unlike it, iterations of one stem are ordered by iteration before kind (C_x-rem-1 < C_x-mag-2)
    names = [
        "b-rem-10",
        "b-rem-9",
        "a_2026-01-02_10.00.00",
        "PkBudg1000",
        "PkBudg500",
        "C_x-mag-2",
        "C_x-rem-1",
        "Abc",
        "abd",
        "B",
    ]
    expected = [
        "a_2026-01-02_10.00.00",
        "Abc",
        "abd",
        "B",
        "b-rem-9",
        "b-rem-10",
        "C_x-rem-1",
        "C_x-mag-2",
        "PkBudg500",
        "PkBudg1000",
    ]
    assert [p.name for p in sorted((Path("x") / n for n in names), key=run_sort_key)] == expected


def test_run_sort_key_timestamps_then_iterations() -> None:
    paths = [Path("r_2026-02-01_00.00.00"), Path("r_2026-01-01_00.00.00"), Path("r-rem-2"), Path("r-rem-1")]
    assert [p.name for p in sorted(paths, key=run_sort_key)] == [
        "r-rem-1",
        "r-rem-2",
        "r_2026-01-01_00.00.00",
        "r_2026-02-01_00.00.00",
    ]


def test_is_coupled() -> None:
    assert is_coupled("C_SSP2-rem-1")
    assert is_coupled("C_SSP2-mag-12")
    assert not is_coupled("C_SSP2")
    assert not is_coupled("rem-1-notes")


def test_last_iterations_keeps_highest_per_parent_stem_and_kind() -> None:
    paths = [
        Path(p)
        for p in [
            "out/C_A-rem-10",
            "out/C_A-rem-9",
            "out/C_A-rem-2",
            "mag/C_A-mag-3",
            "mag/C_A-mag-10",
            "out/C_B",
            "out/C_A-mag-1",
        ]
    ]
    assert [str(p) for p in last_iterations(paths)] == [
        "out/C_A-rem-10",
        "mag/C_A-mag-10",
        "out/C_B",
        "out/C_A-mag-1",
    ]


def test_last_timestamps_keeps_latest_per_parent_and_stem() -> None:
    paths = [
        Path(p)
        for p in [
            "o/X_2026-01-01_10.00.00",
            "o/X_2026-03-01_10.00.00",
            "o/X_2026-02-01_10.00.00",
            "o/Y",
            "p/X_2025-01-01_10.00.00",
        ]
    ]
    assert [str(p) for p in last_timestamps(paths)] == [
        "o/X_2026-03-01_10.00.00",
        "o/Y",
        "p/X_2025-01-01_10.00.00",
    ]


def test_last_iterations_keeps_timestamped_stems_apart() -> None:
    paths = [
        Path("o/X_2026-01-01_10.00.00-rem-1"),
        Path("o/X_2026-02-01_10.00.00-rem-2"),
        Path("o/X_2026-01-01_10.00.00-rem-3"),
    ]
    assert [p.name for p in last_iterations(paths)] == [
        "X_2026-01-01_10.00.00-rem-3",
        "X_2026-02-01_10.00.00-rem-2",
    ]


def test_plain_names_yield_to_suffixed_siblings_in_either_order() -> None:
    # R grouped by the name without suffix: the timestamped/iterated folder wins over the plain one
    paths = [Path("o/X"), Path("o/X_2026-01-01_10.00.00"), Path("o/Y")]
    assert [p.name for p in last_timestamps(paths)] == ["X_2026-01-01_10.00.00", "Y"]
    assert [p.name for p in last_timestamps(paths[::-1])] == ["Y", "X_2026-01-01_10.00.00"]
    assert [p.name for p in last_iterations([Path("o/C_X"), Path("o/C_X-rem-1")])] == ["C_X-rem-1"]
    assert [p.name for p in last_iterations([Path("o/C_X-rem-1"), Path("o/C_X")])] == ["C_X-rem-1"]


def test_existing_dirs(tmp_path: Path) -> None:
    import os

    old = tmp_path / "old"
    new = tmp_path / "new"
    old.mkdir()
    new.mkdir()
    (tmp_path / "file").write_text("")
    os.utime(old, (1_000_000, 1_000_000))
    os.utime(new, (2_000_000, 2_000_000))
    paths = [old, new, tmp_path / "file", tmp_path / "missing", old]
    assert existing_dirs(paths, newest_first=False) == [old, new, old]
    assert existing_dirs(paths, newest_first=True) == [new, old, old]
    os.utime(new, (1_000_000, 1_000_000))
    assert existing_dirs([old, new], newest_first=True) == [old, new]  # equal times keep the order


def test_subdirs(tmp_path: Path) -> None:
    from modelstats._sorting import subdirs

    for name in ("b", ".hidden", "A", "a10", "a9"):
        (tmp_path / name).mkdir()
    (tmp_path / "file").write_text("")
    assert [p.name for p in subdirs(tmp_path)] == [".hidden", "A", "a10", "a9", "b"]  # code points
    assert subdirs(tmp_path / "missing") == []
    assert subdirs(tmp_path / "file") == []


def test_coupled_names_with_newlines_like_grepl() -> None:
    from modelstats._sorting import is_coupled, parse_run_name

    assert is_coupled("a\nb-rem-3")  # "." matches a newline in R's regex
    assert not is_coupled("x-rem-3\n")  # "$" is the end of the string in R
    assert parse_run_name("x_2026-01-01_10.00.00\n").timestamp is None
