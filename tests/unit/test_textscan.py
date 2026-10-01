"""textscan: the shell pipelines of R/getRunStatus.R reproduced in Python.

Expected values marked "R" come from R 4.6.1 ``system(cmd, intern = TRUE)`` under
``LC_ALL=C.utf8`` with GNU grep 3.12, coreutils 9.11 (tac, tail) and gawk 5.4.1 on
2026-10-01; the full sweep over every fixture and synthetic log lives in
``migration/_scratch/p1-tables-textscan-env`` (5083 checks, 0 mismatches).
"""

from __future__ import annotations

import random
import re
import shutil
import subprocess
from pathlib import Path

import pytest

from modelstats import textscan as ts

CHUNKS = [1, 2, 3, 5, 7, 64, ts.CHUNK_SIZE]


def write(tmp_path: Path, data: bytes, name: str = "f.txt") -> Path:
    path = tmp_path / name
    path.write_bytes(data)
    return path


# ---------------------------------------------------------------------------
# tac | grep -m 1
# ---------------------------------------------------------------------------


def tac_reference(data: bytes) -> list[bytes]:
    """tac's output lines (newline is a record suffix; a final partial record is glued in front)."""
    if not data:
        return []
    parts = data.split(b"\n")
    partial, lines = parts[-1], parts[:-1]
    if partial:
        return [partial + lines[-1], *reversed(lines[:-1])] if lines else [partial]
    return list(reversed(lines))


@pytest.mark.parametrize(
    ("data", "expected"),
    [
        (b"a\nb\nc", ["cb", "a"]),  # tac: "cb\na\n"
        (b"a\nb\n", ["b", "a"]),
        (b"ax\nb", ["bax"]),  # tac | grep -m 1 x -> "bax"
        (b"abc", ["abc"]),
        (b"", []),
        (b"\n", [""]),
        (b"a\n\nc", ["c", "a"]),  # tac: "c\na\n"
        (b"a\n\n", ["", "a"]),
        (b"\n\nb", ["b", ""]),
    ],
)
@pytest.mark.parametrize("chunk", CHUNKS)
def test_tac_lines(tmp_path: Path, data: bytes, expected: list[str], chunk: int) -> None:
    assert list(ts.tac_lines(write(tmp_path, data), chunk)) == expected
    assert [line.encode() for line in ts.tac_lines(write(tmp_path, data), chunk)] == tac_reference(data)


@pytest.mark.parametrize("chunk", CHUNKS)
def test_last_match_basic(tmp_path: Path, chunk: int) -> None:
    path = write(tmp_path, b"cm_iteration_max = 50;\nx\ncm_iteration_max = 73;\ncm_iteration_max = 0;\nlast\n")
    assert ts.last_match(path, "cm_iteration_max = [1-9].*.;[ ]*$", chunk) == "cm_iteration_max = 73;"
    assert ts.last_match(path, "^x$", chunk) == "x"
    assert ts.last_match(path, "nothing", chunk) is None
    assert ts.last_match(path, re.compile("^last"), chunk) == "last"


@pytest.mark.parametrize("chunk", CHUNKS)
def test_last_match_glues_a_final_partial_line_like_tac(tmp_path: Path, chunk: int) -> None:
    assert ts.last_match(write(tmp_path, b"ax\nb"), "x", chunk) == "bax"
    assert ts.last_match(write(tmp_path, b"ax\nb"), "^b", chunk) == "bax"
    assert ts.last_match(write(tmp_path, b"ax\nb"), "^a", chunk) is None
    assert ts.last_match(write(tmp_path, b"only"), "on", chunk) == "only"
    assert ts.last_match(write(tmp_path, b""), ".", chunk) is None


def test_last_match_never_matches_across_lines(tmp_path: Path) -> None:
    path = write(tmp_path, b"a\nb\n")
    assert ts.last_match(path, "a\nb", 1) is None
    assert ts.last_match(path, "a\nb", ts.CHUNK_SIZE) is None
    assert ts.last_match(path, "[^x]+;", 3) is None
    assert ts.last_match(write(tmp_path, b"ab\nb\nbb\n"), "^b$", 2) == "b"


@pytest.mark.parametrize("seed", range(40))
def test_last_match_agrees_with_full_read_reference(tmp_path: Path, seed: int) -> None:
    rng = random.Random(seed)
    alphabet = b"ab\n;x "
    data = bytes(rng.choice(alphabet) for _ in range(rng.randint(0, 60)))
    path = write(tmp_path, data)
    for pattern in ["a", "^b", "x$", "a.*b", "[^;]+;", "^$", "b b"]:
        expected = next((ln.decode() for ln in tac_reference(data) if re.search(pattern, ln.decode())), None)
        for chunk in (1, 2, 3, 7, 64):
            assert ts.last_match(path, pattern, chunk) == expected, (data, pattern, chunk)
            assert list(ts.tac_lines(path, chunk)) == [ln.decode() for ln in tac_reference(data)]


@pytest.mark.parametrize("chunk", [3, ts.CHUNK_SIZE])
def test_last_match_large_block_prefilter(tmp_path: Path, chunk: int) -> None:
    path = write(tmp_path, b"\n".join(b"line %d" % i for i in range(5000)) + b"\n")
    assert ts.last_match(path, "line 4321$", chunk) == "line 4321"
    assert ts.last_match(path, "line 7$", chunk) == "line 7"
    assert ts.last_match(path, "line 9999", chunk) is None


# ---------------------------------------------------------------------------
# forward grep, awk, tail
# ---------------------------------------------------------------------------


def test_iter_lines_and_all_matches(tmp_path: Path) -> None:
    path = write(tmp_path, b"LOOPS = 1\r\nx\n*** Status: Normal completion\nLOOPS = 2")
    assert list(ts.iter_lines(path)) == ["LOOPS = 1\r", "x", "*** Status: Normal completion", "LOOPS = 2"]
    assert ts.all_matches(path, "LOOPS") == ["LOOPS = 1\r", "LOOPS = 2"]
    assert ts.last_match_forward(path, "LOOPS") == "LOOPS = 2"
    assert ts.last_match_forward(path, "nothing") is None
    assert ts.all_matches(path, r"\*\*\* Status: ") == ["*** Status: Normal completion"]
    assert ts.count_lines_matching(path, "LOOPS") == 2
    assert ts.count_lines_matching(path, re.compile("^x$")) == 1
    assert ts.all_matches(write(tmp_path, b""), ".") == []


@pytest.mark.parametrize(
    ("data", "expected"),
    [
        (b"a\n \t\n\r\n", "\r"),  # awk: "\r" is a field
        (b"a\nb  \n   \n", "b  "),
        (b"", ""),
        (b"  \n\t\n", ""),
        (b"a\nb", "b"),
        (b"a\n  ", "a"),
        (b"Starting MAgPIE...\n\n", "Starting MAgPIE..."),
    ],
)
@pytest.mark.parametrize("chunk", CHUNKS)
def test_last_nonempty_line(tmp_path: Path, data: bytes, expected: str, chunk: int) -> None:
    assert ts.last_nonempty_line(write(tmp_path, data), chunk) == expected


@pytest.mark.parametrize(
    ("data", "expected"),
    [
        (b"", None),
        (b"a\n", "a"),
        (b"a\n\n", ""),
        (b"a\nb", "b"),
        (b"\n", ""),
        (b"try to acquire model lock\n", "try to acquire model lock"),
    ],
)
@pytest.mark.parametrize("chunk", CHUNKS)
def test_last_line(tmp_path: Path, data: bytes, expected: str | None, chunk: int) -> None:
    assert ts.last_line(write(tmp_path, data), chunk) == expected


# ---------------------------------------------------------------------------
# system(intern = TRUE), tail -1, grep -zoP
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("stream", "expected"),
    [
        (b"ab\0cd\nef\0gh\n", ["ab", "ef"]),  # R
        (b"ab\n\0", ["ab", ""]),  # R
        (b"ab\ncd", ["ab", "cd"]),  # R
        (b"ab\n\n", ["ab", ""]),  # R
        (b"\n", [""]),  # R
        (b"", []),  # R
        (b"ab\r\ncd\r", ["ab\r", "cd\r"]),  # R
        (b"a" * 20000 + b"\ntail\n", ["a" * 20000, "tail"]),  # R: lines are never split by length
        ("str\ninput", ["str", "input"]),
    ],
)
def test_r_intern_lines(stream: bytes | str, expected: list[str]) -> None:
    assert ts.r_intern_lines(stream) == expected


@pytest.mark.parametrize(
    ("stream", "expected"),
    [(b"", None), (b"a\nb\n", b"b\n"), (b"a\nb", b"b"), (b"\n", b"\n"), (b"x\n3\0", b"3\0"), (b"only", b"only")],
)
def test_tail_1(stream: bytes, expected: bytes | None) -> None:
    assert ts.tail_1(stream) == expected


W1 = b"x\nWarning messages:\n1: a\n2: b\n3: c\nother\nWarning messages:\n1: d\n2: e\n"
W3 = b"Warning messages:\n1: a\n  extra\n2: b\nother\n"
W4 = b"Warning messages:\n1: a\n2: b"
W5 = b"Warning messages:\n1: a\n\nfoo\n2: b\n9"
W6 = b"Warning messages:\r\n1: a\r\n2: b\r\n3"
W7 = b"x\0Warning messages:\n1: a\n2\0Warning messages:\n1: b\n2: c\n3\n"
W8 = b"Warning messages:\n1: \xff\xfe bad utf8\n2: b\n3\n"


@pytest.mark.parametrize(
    ("data", "magpie", "remind_block"),
    [
        # exact bytes of GNU grep -zoP under LC_ALL=C.utf8
        (
            W1,
            b"Warning messages:\n1: a\n2: b\n3\0Warning messages:\n1: d\n2\0",
            b"Warning messages:\n1: a\n2: b\n3: c\nother\n\0Warning messages:\n1: d\n2: e\n\0",
        ),
        (W5, b"Warning messages:\n1\0", b"Warning messages:\n1: a\n\n\0"),
        (W6, b"", b""),
        (
            W7,
            b"Warning messages:\n1: a\n2\0Warning messages:\n1: b\n2: c\n3\0",
            b"Warning messages:\n1: a\n\0Warning messages:\n1: b\n2: c\n\0",
        ),
        (W8, b"Warning messages:\n1\0", b"Warning messages:\n\0"),  # `.` never matches an invalid byte
    ],
)
def test_grep_z_only_matching(tmp_path: Path, data: bytes, magpie: bytes, remind_block: bytes) -> None:
    path = write(tmp_path, data)
    assert ts.grep_z_only_matching(path, ts.MAGPIE_WARNINGS_PATTERN) == magpie
    assert ts.grep_z_only_matching(path, ts.REMIND_WARNINGS_BLOCK_PATTERN) == remind_block


S1 = b"Warning messages:\n1: first\n  explanation\n2: second\n"
S2 = (
    b"x\nWarning messages:\n1: Infeasible solutions found (2040)!\n2: In interpolate(x = a,  ... :\n"
    b"  Sum over all land pools is not constant\n3: In interpolate(x = b,  ... :\n  Cluster level differences are\n"
    b"          greater than 5% of the total\nSaving runstatistics.rda\n"
)
S3 = b"Warning message:\nsomething\n"
S4 = b"no warnings here\n"
S5 = b"There were 50 or more warnings (use warnings() to see the first 50)\nWarning messages:\n1: first\n2: second\n"
S6 = b"Warning messages:\n1: a\n2: b\n3: c\n"
S7 = (
    b"Warning messages:\n1: In run() :\n  loaded renv must be equal.\n2: In .dropRegi(mifdata, dropRegi) :\n"
    b"  Because of dropRegi\n3: In .dropRegi(mifdata, dropRegi) :\n  Because of dropRegi\n"
    b"  Project-related issues found\n"
    b"There were 50 or more warnings (use warnings() to see the first 50)\n"
)
W2 = b"There were 5 warnings (use warnings())\nfoo\nThere were 7 warnings\n"


@pytest.mark.parametrize(
    ("data", "expected"),
    [
        (W1, "2"),
        (W3, "2"),
        (W4, "2"),
        (W5, "1"),
        (W6, "0"),
        (W7, "3"),
        (W8, "1"),
        (S1, "2"),
        (S2, "3"),
        (S3, "1"),
        (S4, "0"),
        (S5, "2"),
        (S6, "3"),
        (S7, "3"),
    ],
)
def test_magpie_warnings(tmp_path: Path, data: bytes, expected: str) -> None:
    assert ts.magpie_warnings(write(tmp_path, data)) == expected  # R


def test_magpie_warnings_block_wins_over_single_warning(tmp_path: Path) -> None:
    assert ts.magpie_warnings(write(tmp_path, S3 + S6)) == "3"
    assert ts.magpie_warnings(write(tmp_path, b"Warning messages:\nno numbered entries\n")) == "0"


@pytest.mark.parametrize(
    ("data", "there_were", "block_count", "expected"),
    [
        (W2, ["There were 5 warnings"], 0, "5"),  # R: the first match only (BUG-011)
        (W1, [], 5, "5"),
        (W4, [], 1, "1"),
        (W5, [], 1, "1"),
        (S1, [], 2, "2"),
        (S2, [], 2, "2"),
        (S3, [], 0, "0"),
        (S4, [], 0, "0"),
        (S5, [], 2, "2"),  # "50 or more" does not match (BUG-029 parity)
        (S7, [], 3, "3"),
        (W7, [], 3, "3"),
        (b"There were 12 warnings\nWarning messages:\n1: a\n", ["There were 12 warnings"], 1, "12"),
    ],
)
def test_remind_warnings(tmp_path: Path, data: bytes, there_were: list[str], block_count: int, expected: str) -> None:
    path = write(tmp_path, data)
    assert ts.remind_there_were_warnings(path) == there_were  # R
    assert ts.remind_warnings_block_count(path) == block_count  # R
    assert ts.remind_warnings(path) == expected


def test_remind_block_lines_as_r_sees_them(tmp_path: Path) -> None:
    lines = ts.r_intern_lines(ts.grep_z_only_matching(write(tmp_path, W1), ts.REMIND_WARNINGS_BLOCK_PATTERN))
    assert lines == ["Warning messages:", "1: a", "2: b", "3: c", "other", "", "1: d", "2: e", ""]  # R
    lines = ts.r_intern_lines(ts.grep_z_only_matching(write(tmp_path, W4), ts.REMIND_WARNINGS_BLOCK_PATTERN))
    assert lines == ["Warning messages:", "1: a", ""]  # R


# ---------------------------------------------------------------------------
# cross-check against the real tools when they are available
# ---------------------------------------------------------------------------


def gnu_tools_available() -> bool:
    if not all(shutil.which(t) for t in ("tac", "grep", "awk", "tail", "sh")):
        return False
    version = subprocess.run(["grep", "--version"], capture_output=True, text=True, check=False).stdout
    return "GNU grep" in version


@pytest.mark.skipif(not gnu_tools_available(), reason="GNU grep, tac, awk and tail are needed for the cross-check")
@pytest.mark.parametrize("seed", range(12))
def test_cross_check_with_the_real_pipelines(tmp_path: Path, seed: int) -> None:
    rng = random.Random(seed)
    pieces = [
        b"Warning messages:\n",
        b"Warning message:\n",
        b"1: a\n",
        b"2: b\n",
        b"3\n",
        b"  extra\n",
        b"\n",
        b"x\n",
        b"There were 7 warnings\n",
        b"9",
        b" \t\n",
    ]
    data = b"".join(rng.choice(pieces) for _ in range(rng.randint(0, 14)))
    path = write(tmp_path, data)
    env = {"LC_ALL": "C.utf8", "PATH": "/usr/bin:/bin"}

    def sh(cmd: str) -> bytes:
        return subprocess.run(["sh", "-c", cmd], capture_output=True, env=env, check=False).stdout

    mag = ts.r_intern_lines(sh(f'grep -zoP "Warning messages:\\n([0-9]+:(.*\\n)?.*\\n)*([0-9]+)" {path} | tail -1'))
    assert ts.magpie_warnings(path) == (
        mag[0] if mag else ("1" if sh(f'grep "Warning message:" {path} | tail -1') else "0")
    )
    there = ts.r_intern_lines(sh(f'grep -zoP "There were ([0-9]+) warnings" {path}'))
    assert ts.remind_there_were_warnings(path) == there
    block = ts.r_intern_lines(sh(f'grep -zoP "Warning messages:\\n([0-9]+:(.*\\n)?.*\\n)*" {path}'))
    assert ts.remind_warnings_block_count(path) == sum(1 for ln in block if re.match("[0-9]+:", ln))
    assert ts.last_nonempty_line(path) == ts.r_intern_lines(sh(f"awk 'NF{{s=$0}}END{{print s}}' {path}"))[0]
    tail = ts.r_intern_lines(sh(f"tail -1 {path}"))
    assert ts.last_line(path) == (tail[0] if tail else None)
    for pattern in ("a", "^[0-9]", "x$", "Warning"):
        got = ts.r_intern_lines(sh(f"tac '{path}' | grep -m 1 '{pattern}'"))
        assert ts.last_match(path, pattern, 5) == (got[0] if got else None), (data, pattern)
