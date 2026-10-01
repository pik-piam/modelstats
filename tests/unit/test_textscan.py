r"""``textscan`` reproduces the shell pipelines of ``R/getRunStatus.R`` byte for byte.

The pipeline semantics pinned here were probed on this machine with GNU coreutils 9.11 and GNU grep
3.12 (``/usr/bin/grep``; the semantics of ``tac``'s joined partial record, the BRE ``'*** Status: '``
and ``grep -zoP ... | tail -1`` are documented in the module). The reverse scanners are exercised with
tiny chunk sizes so that every line boundary falls on a chunk boundary at least once, and compared
with a naive whole-file reference. The golden-marked tests at the end read the fixture and synthetic
runs (``migration/01-test-data-inventory.md``) and check that the ``Warnings`` column R computed from
``grep -zoP`` equals what the scanners find; they also cross-check every ``slurm.log`` against GNU
grep itself when it is installed. ``MODELSTATS_TEXTSCAN_BIGFILE_MB`` enables the memory and timing
test on a generated ``full.gms`` of that size (the phase 1 evidence runs it with 500).
"""

from __future__ import annotations

import json
import os
import re
import subprocess
import sys
import time
from collections.abc import Iterator
from pathlib import Path
from typing import Any

import pytest

from modelstats import textscan as ts

GNU_GREP = "/usr/bin/grep"


def write(path: Path, data: bytes) -> Path:
    path.write_bytes(data)
    return path


def naive_tac_records(data: bytes) -> list[bytes]:
    """What ``tac`` prints, record by record: the partial last line joins the previous one."""
    if not data:
        return []
    lines = data.split(b"\n")
    if data.endswith(b"\n"):
        lines.pop()
        return lines[::-1]
    partial = lines.pop()
    if not lines:
        return [partial]
    lines[-1] = partial + lines[-1]
    return lines[::-1]


# --- intern_lines: system(intern = TRUE) ---------------------------------------------------------


@pytest.mark.parametrize(
    ("output", "expected"),
    [
        (b"", []),
        (b"a\nb\n", ["a", "b"]),
        (b"a\nb", ["a", "b"]),
        (b"\n", [""]),
        (b"2\0", ["2"]),
        (b"x\0y\nz\n", ["x", "z"]),
        (b"\0Warning messages:\n1: a\n", ["", "1: a"]),
        (b"a\r\n", ["a\r"]),
    ],
)
def test_intern_lines_cuts_at_nul_and_chops_one_newline(output: bytes, expected: list[str]) -> None:
    assert ts.intern_lines(output) == expected


def test_intern_lines_keeps_a_line_longer_than_the_fgets_buffer_whole() -> None:
    """R >= 3.3.0 reads with getline(): R 4.6.1 returns one 9000-character element (BUG-053, not a split)."""
    assert ts.intern_lines(b"x" * 9000 + b"\nnext\n") == ["x" * 9000, "next"]
    assert ts.intern_lines(b"y" * 8095 + b"z") == ["y" * 8095 + "z"]
    assert ts.intern_lines(b"y" * 8095 + b"\n") == ["y" * 8095]
    assert ts.intern_lines(b"x" * 70000 + b"\n") == ["x" * 70000]


def test_intern_lines_keeps_undecodable_bytes() -> None:
    assert ts.intern_lines(b"caf\xe9\n")[0].encode("utf-8", "surrogateescape") == b"caf\xe9"


# --- tail_last_line ------------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("output", "expected"),
    [(b"", b""), (b"a\nb\n", b"b\n"), (b"a\nb", b"b"), (b"abc", b"abc"), (b"\n", b"\n"), (b"a\n\n", b"\n")],
)
def test_tail_last_line(output: bytes, expected: bytes) -> None:
    assert ts.tail_last_line(output) == expected


# --- iter_lines / tac_records ---------------------------------------------------------------------


def test_iter_lines_keeps_a_partial_last_line_and_carriage_returns(tmp_path: Path) -> None:
    path = write(tmp_path / "f", b"a\r\nb\n\nc")
    assert list(ts.iter_lines(path)) == [b"a\r", b"b", b"", b"c"]
    assert list(ts.iter_lines(write(tmp_path / "e", b""))) == []


SAMPLES = [
    b"",
    b"\n",
    b"\n\n",
    b"a",
    b"a\n",
    b"a\nb",
    b"a\nb\n",
    b"first\nsecond\nthird\n",
    b"first\nsecond\nthird",
    b"\nlead\n\nblank\n\n",
    b"x" * 50 + b"\n" + b"y" * 3 + b"\n" + b"z" * 20,
    b"".join(b"line %d\n" % i for i in range(40)),
    b"".join(b"line %d\n" % i for i in range(40)) + b"tail without newline",
]


@pytest.mark.parametrize("data", SAMPLES, ids=range(len(SAMPLES)))
@pytest.mark.parametrize("chunk_size", [1, 2, 3, 7, 16, 1 << 20])
def test_tac_records_match_the_reference_for_every_chunk_size(
    tmp_path: Path, data: bytes, chunk_size: int
) -> None:
    path = write(tmp_path / "f", data)
    assert list(ts.tac_records(path, chunk_size=chunk_size)) == naive_tac_records(data)


def test_tac_records_joins_an_unterminated_last_line_with_the_previous_one(tmp_path: Path) -> None:
    assert list(ts.tac_records(write(tmp_path / "f", b"a\nb"))) == [b"ba"]
    assert list(ts.tac_records(write(tmp_path / "g", b"only"))) == [b"only"]


def test_tac_records_accepts_an_open_stream(tmp_path: Path) -> None:
    path = write(tmp_path / "f", b"a\nb\n")
    with open(path, "rb") as stream:
        assert list(ts.tac_records(stream, chunk_size=1)) == [b"b", b"a"]


# --- last_match: tac | grep -m 1 ------------------------------------------------------------------


def naive_last_match(data: bytes, pattern: re.Pattern[bytes]) -> str | None:
    for record in naive_tac_records(data):
        if pattern.search(record):
            return ts.intern_lines(record + b"\n")[0]
    return None


LAST_MATCH_PATTERNS = [
    ts.ITERATION_MAX_LINE,
    re.compile(rb"^line 1"),
    re.compile(rb"3$"),
    re.compile(rb"second"),
    re.compile(rb"nowhere"),
]


@pytest.mark.parametrize(
    "pattern", LAST_MATCH_PATTERNS, ids=[p.pattern.decode() for p in LAST_MATCH_PATTERNS]
)
@pytest.mark.parametrize("chunk_size", [1, 3, 8, 1 << 20])
def test_last_match_equals_the_reference(tmp_path: Path, pattern: re.Pattern[bytes], chunk_size: int) -> None:
    body = b"".join(b"line %d\n" % i for i in range(30))
    for name, data in {
        "gms": b"cm_iteration_max = 100;\n" + body + b"cm_iteration_max = 50;   \n" + body,
        "partial": body + b"cm_iteration_max = 7;",
        "join": b"first\nsecond\nthird",
        "anchored": b"line 13\nline 23 \nline 3\n",
    }.items():
        path = write(tmp_path / name, data)
        assert ts.last_match(path, pattern, chunk_size=chunk_size) == naive_last_match(data, pattern), name


def test_last_match_takes_the_last_occurrence_and_returns_the_whole_line(tmp_path: Path) -> None:
    path = write(tmp_path / "full.gms", b"cm_iteration_max = 100;\nfiller\ncm_iteration_max = 50;\nmore\n")
    assert ts.last_match(path, ts.ITERATION_MAX_LINE) == "cm_iteration_max = 50;"
    assert ts.scan_last_match(path, ts.ITERATION_MAX_LINE) == ["cm_iteration_max = 50;"]
    assert ts.last_match(path, b"absent") is None
    assert ts.scan_last_match(path, b"absent") == []


def test_last_match_with_a_fixed_string_pattern_is_literal(tmp_path: Path) -> None:
    path = write(tmp_path / "log.txt", b"Starting output generation for X\nStarting out.ut\n")
    assert ts.last_match(path, b"Starting out.ut") == "Starting out.ut"
    assert ts.last_match(path, b"Starting output generation for") == "Starting output generation for X"


def test_last_match_through_the_joined_partial_record(tmp_path: Path) -> None:
    # tac prints "cm_iteration_max = 100;prev" for a file whose last line lacks a newline: no match for
    # the anchored pattern, exactly as R sees it (register note in the module docstring).
    path = write(tmp_path / "full.gms", b"prev\ncm_iteration_max = 100;")
    assert ts.last_match(path, ts.ITERATION_MAX_LINE) is None
    assert ts.last_match(path, b"100;prev") == "cm_iteration_max = 100;prev"


def test_last_match_crossing_many_chunks_with_a_long_line(tmp_path: Path) -> None:
    long_line = b"L" * 5000
    path = write(tmp_path / "f", b"hit early\n" + long_line + b"\n" + b"z\n" * 100)
    assert ts.last_match(path, b"hit", chunk_size=64) == "hit early"
    assert ts.last_match(path, b"LLL", chunk_size=64) == "L" * 5000


# --- all_matches / last_matching_line / count_matches -------------------------------------------


def test_all_matches_in_file_order_including_a_partial_last_line(tmp_path: Path) -> None:
    path = write(tmp_path / "f", b"x1\ny\nx2\nx3")
    assert ts.all_matches(path, re.compile(rb"^x")) == ["x1", "x2", "x3"]
    assert ts.count_matches(path, re.compile(rb"^x")) == 3
    assert ts.last_matching_line(path, re.compile(rb"^x")) == "x3"
    assert ts.scan_last_matching_line(path, re.compile(rb"^x")) == ["x3"]
    assert ts.last_matching_line(path, b"none") is None
    assert ts.all_matches(path, b"none") == []


def test_last_matching_line_does_not_join_like_tac(tmp_path: Path) -> None:
    path = write(tmp_path / "f", b"prev\nlast")
    assert ts.last_matching_line(path, b"last") == "last"
    assert ts.last_match(path, b"last") == "lastprev"


# --- tail -1 / awk -------------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("data", "expected"),
    [(b"", None), (b"a\nb\n", "b"), (b"a\nb", "b"), (b"a\n\n", ""), (b"\n", ""), (b"solo", "solo")],
)
def test_last_line(tmp_path: Path, data: bytes, expected: str | None) -> None:
    path = write(tmp_path / "f", data)
    for chunk_size in (1, 2, 1 << 20):
        assert ts.last_line(path, chunk_size=chunk_size) == expected
    assert ts.scan_last_line(path) == ([] if expected is None else [expected])


@pytest.mark.parametrize(
    ("data", "expected"),
    [
        (b"", ""),
        (b"\n\n  \n\t\n", ""),
        (b"line1\n\n  \nlast\n\n", "last"),
        (b"a\n  b  \n", "  b  "),
        (b"a\nb", "b"),
        (b"Starting MAgPIE...\n\n", "Starting MAgPIE..."),
        (b"a\n\r\n", "\r"),
    ],
)
def test_last_nonempty_line(tmp_path: Path, data: bytes, expected: str) -> None:
    path = write(tmp_path / "f", data)
    for chunk_size in (1, 3, 1 << 20):
        assert ts.last_nonempty_line(path, chunk_size=chunk_size) == expected
    assert ts.scan_last_nonempty_line(path) == [expected]


# --- grep -zoP blocks ----------------------------------------------------------------------------

BLOCK = b"Warning messages:\n1: first\n  explanation\n2: second\n"


def test_grep_z_only_matching_prints_each_match_followed_by_a_nul(tmp_path: Path) -> None:
    path = write(tmp_path / "slurm.log", b"head\n" + BLOCK + b"tail\n" + BLOCK)
    output = ts.grep_z_only_matching(path, ts.MAGPIE_WARNING_BLOCK)
    assert (
        output
        == b"Warning messages:\n1: first\n  explanation\n2\0Warning messages:\n1: first\n  explanation\n2\0"
    )
    assert ts.grep_z_only_matching(path, re.compile(rb"absent")) == b""


def test_grep_z_matches_each_nul_delimited_record_on_its_own(tmp_path: Path) -> None:
    """GNU grep 3.12 ``-zoP`` output (``od -c``) for a NUL inside a block and for a NUL ending the file."""
    nul_inside = write(tmp_path / "inside.log", b"Warning messages:\n1: a\0\n2: b\n")
    assert ts.grep_z_only_matching(nul_inside, ts.MAGPIE_WARNING_BLOCK) == b"Warning messages:\n1\0"
    assert ts.grep_z_only_matching(nul_inside, ts.REMIND_WARNING_BLOCK) == b"Warning messages:\n\0"
    nul_at_end = write(tmp_path / "end.log", b"Warning messages:\n1: a\n2: b\n\0")
    assert ts.grep_z_only_matching(nul_at_end, ts.MAGPIE_WARNING_BLOCK) == b"Warning messages:\n1: a\n2\0"
    assert (
        ts.grep_z_only_matching(nul_at_end, ts.REMIND_WARNING_BLOCK) == b"Warning messages:\n1: a\n2: b\n\0"
    )
    assert ts.grep_z_only_matching(write(tmp_path / "nul.log", b"\0"), ts.MAGPIE_WARNING_BLOCK) == b""


@pytest.mark.parametrize(
    ("text", "expected"),
    [
        (b"nothing here\n", []),
        (BLOCK, ["2"]),
        (b"x\n" + BLOCK + b"y\n" + b"Warning messages:\n1: a\n2: b\n3: c\nend\n", ["3"]),
        # the greedy group swallows "2: second" and its follower, so the final digits come from "3 more"
        (BLOCK + b"3 more lines\n", ["3"]),
        (b"Warning messages:\n12: twelve\n", ["12"]),
        (b"Warning messages:\n7 starts with a digit\n", ["7"]),
        # the NUL ends the grep -z record after "1: a"; GNU grep 3.12 -zoP | tail -1 prints "1"
        (b"Warning messages:\n1: a\0\n2: b\n", ["1"]),
        (b"Warning messages:\nno number\n", []),
        (b"Warning message:\nsingular\n", []),
    ],
)
def test_scan_magpie_warnings(tmp_path: Path, text: bytes, expected: list[str]) -> None:
    path = write(tmp_path / "slurm.log", text)
    assert ts.scan_magpie_warnings(path) == expected
    assert ts.last_block_match(path, ts.MAGPIE_WARNING_BLOCK) == expected


def test_has_warning_message_is_the_singular_form_only(tmp_path: Path) -> None:
    assert ts.has_warning_message(write(tmp_path / "a", b"Warning message:\nIn f()\n"))
    assert not ts.has_warning_message(write(tmp_path / "b", BLOCK))
    assert not ts.has_warning_message(write(tmp_path / "c", b""))


@pytest.mark.parametrize(
    ("text", "expected"),
    [
        (b"There were 27 warnings (use warnings() to see them)\n", ["27"]),
        (b"There were 27 warnings\nThere were 3 warnings\n", ["27"]),
        (b"There were 50 or more warnings (use warnings() to see the first 50)\n", []),
        (b"", []),
    ],
)
def test_scan_warning_summary_keeps_only_the_first_summary(
    tmp_path: Path, text: bytes, expected: list[str]
) -> None:
    assert ts.scan_warning_summary(write(tmp_path / "log.txt", text)) == expected


@pytest.mark.parametrize(
    ("text", "expected"),
    [
        (b"", 0),
        (b"Warning messages:\n", 0),
        (BLOCK, 2),
        (BLOCK + b"x\nWarning messages:\n1: a\n2: b\n3: c\n", 5),
        (b"Warning messages:\n1: a\n2: b\n3: c\nnot numbered\n", 3),
        (b"Warning message:\n1: singular is not a block\n", 0),
        # the NUL cuts the record after "1: a": grep -zoP prints "Warning messages:\n\0", no numbered line
        (b"Warning messages:\n1: a\0\n2: b\n", 0),
    ],
)
def test_count_numbered_warning_lines(tmp_path: Path, text: bytes, expected: int) -> None:
    assert ts.count_numbered_warning_lines(write(tmp_path / "log.txt", text)) == expected


# --- the R lines ---------------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("text", "expected"),
    [
        (b"cm_iteration_max = 100;\n", ["100"]),
        (b"$setglobal x\ncm_iteration_max = 1;\n cm_iteration_max = 200;   \nmore\n", ["200"]),
        (b"cm_iteration_max = 0;\n", []),
        (b"cm_iteration_max = 100; !! comment\n", []),
        (b"", []),
        (b"\n", []),
    ],
)
def test_scan_iteration_max(tmp_path: Path, text: bytes, expected: list[str]) -> None:
    assert ts.scan_iteration_max(write(tmp_path / "full.gms", text)) == expected


@pytest.mark.parametrize(
    ("text", "expected"),
    [
        (b"--- LOOPS iteration = 1\n--- LOOPS iteration = 26\n", ["26"]),
        (b"--- LOOPS t = y1995\n", ["y1995"]),
        (b"no loops\n", []),
        (b"", []),
    ],
)
def test_scan_last_loops(tmp_path: Path, text: bytes, expected: list[str]) -> None:
    assert ts.scan_last_loops(write(tmp_path / "full.log", text)) == expected


@pytest.mark.parametrize(
    ("text", "expected"),
    [
        (b"*** Status: Normal completion\n", ["Normal completion"]),
        (b"*** Status: Execution error(s)\n", ["Execution error"]),
        (b"*** Status: Compilation error(s)\n", ["Compilation error"]),
        (b"*** Status: Terminated due to disk I/O Error(s)\n", ["Terminated due to"]),
        (b"*** Status: Terminated by user\n", ["Terminated by use"]),
        (b"*** Status: Normal completion\nx\n*** Status: Normal completion\n", ["Normal completion"] * 2),
        # GNU grep reads the BRE '*** Status: ' as "zero or more stars", so this line matches too
        (b"a Status: x\n", ["a Status: x"]),
        (b"nothing\n", []),
    ],
)
def test_scan_status_lines(tmp_path: Path, text: bytes, expected: list[str]) -> None:
    assert ts.scan_status_lines(write(tmp_path / "full.log", text)) == expected


def test_scan_slurmstepd_errors(tmp_path: Path) -> None:
    text = (
        b"ok\nslurmstepd: error: *** JOB 1 ON cs-x CANCELLED AT 2026-09-19T01:00:00 DUE TO TIME LIMIT ***\n"
    )
    assert ts.scan_slurmstepd_errors(write(tmp_path / "log.txt", text)) == [text.splitlines()[1].decode()]
    assert ts.scan_slurmstepd_errors(write(tmp_path / "empty", b"")) == []


@pytest.mark.parametrize(
    ("text", "expected"),
    [
        (b"CES calibration iteration:  1 \nCES calibration iteration:  10 \n", ["10"]),
        (b"CES calibration iteration 123\n", ["3"]),
        (b"CES calibration iteration none\n", []),
        (b"iteration 5\n", []),
        (b"", []),
    ],
)
def test_scan_calibration_iteration(tmp_path: Path, text: bytes, expected: list[str]) -> None:
    assert ts.scan_calibration_iteration(write(tmp_path / "log.txt", text)) == expected


def test_count_files_named(tmp_path: Path) -> None:
    run = tmp_path / "run"
    (run / "sub" / "deep").mkdir(parents=True)
    for i in range(1, 12):
        write(run / f"fulldata_{i}.gdx", b"")
    write(run / "sub" / "deep" / "fulldata_x.gdx", b"")
    (run / "fulldata_dir.gdx").mkdir()
    write(run / "fulldata.gdx", b"")
    write(run / ".fulldata_hidden.gdx", b"")
    assert ts.count_files_named(run, "fulldata_*.gdx") == 13
    assert ts.count_files_named(run, "input_*.gdx") == 0
    assert ts.count_files_named(tmp_path / "missing", "fulldata_*.gdx") == 0
    assert ts.count_files_named(run / "fulldata_1.gdx", "fulldata_*.gdx") == 1
    link = tmp_path / "linked"
    link.mkdir()
    os.symlink(run, link / "fulldata_link.gdx")
    assert ts.count_files_named(link, "fulldata_*.gdx") == 1


# --- goldens: the Warnings column on the fixture and synthetic runs --------------------------------


def run_dirs(
    goldens_dir: Path, fixtures_dir: Path, synthetic: Path
) -> Iterator[tuple[str, Path, dict[str, Any]]]:
    """Every single-directory status golden with its run directory (fixtures first, then synthetic)."""
    for golden in sorted((goldens_dir / "status" / "oncluster" / "detailed").glob("*.json")):
        if golden.stem.startswith("focused__"):
            continue
        data = json.loads(golden.read_text(encoding="utf-8"))
        if "error" in data:
            continue
        relative = golden.stem.split("--", 1)[0].replace("__", "/")
        for root in (fixtures_dir, synthetic):
            directory = root / "p" / "projects" / relative
            if directory.is_dir():
                yield golden.stem, directory, data["rows"][0]
                break


def model_is_magpie(directory: Path) -> bool:
    """``isTRUE(stats$config$model_name == "MAgPIE")`` from ``runstatistics.rda`` (``R/getRunStatus.R:257``).

    The golden tier reads through the package's own reader (``rdata_io``); the unit tier never imports it.
    """
    from modelstats.rdata_io import read_rda

    rda = directory / "runstatistics.rda"
    if not rda.is_file():
        return False
    stats = read_rda(rda)["stats"]
    config = stats.get("config") if isinstance(stats, dict) else None
    name = config.get("model_name") if isinstance(config, dict) else None
    return name == ["MAgPIE"] or name == "MAgPIE"


def warnings_like_r(directory: Path, magpie: bool) -> str:
    """``R/getRunStatus.R:256-275`` on top of the scanners."""
    slurm_log, log_txt = directory / "slurm.log", directory / "log.txt"
    if slurm_log.exists() and magpie:
        block = ts.scan_magpie_warnings(slurm_log)
        if block:
            return block[0]
        return "1" if ts.has_warning_message(slurm_log) else "0"
    if log_txt.exists() and not magpie:
        summary = ts.scan_warning_summary(log_txt)
        return summary[0] if summary else str(ts.count_numbered_warning_lines(log_txt))
    return "NA"


@pytest.mark.golden
def test_warnings_column_of_every_status_golden(goldens_dir: Path, fixtures_dir: Path) -> None:
    synthetic = Path(os.environ.get("MODELSTATS_SYNTHETIC", fixtures_dir.parent / "synthetic"))
    checked = 0
    for case_id, directory, row in run_dirs(goldens_dir, fixtures_dir, synthetic):
        if "Warnings" not in row:
            continue
        assert warnings_like_r(directory, model_is_magpie(directory)) == row["Warnings"], case_id
        checked += 1
    assert checked >= 100


@pytest.mark.golden
def test_magpie_blocks_equal_gnu_grep_on_every_slurm_log(fixtures_dir: Path) -> None:
    version = subprocess.run([GNU_GREP, "--version"], capture_output=True, text=True, check=False).stdout
    if "GNU grep" not in version:
        pytest.skip(f"{GNU_GREP} is not GNU grep; the goldens cover the R values")
    synthetic = Path(os.environ.get("MODELSTATS_SYNTHETIC", fixtures_dir.parent / "synthetic"))
    logs = sorted(fixtures_dir.glob("p/projects/**/slurm.log")) + sorted(
        synthetic.glob("p/projects/**/slurm.log")
    )
    assert logs
    pipeline = GNU_GREP + r' -zoP "Warning messages:\n([0-9]+:(.*\n)?.*\n)*([0-9]+)" "$0" | tail -1'
    for log in logs:
        shell = subprocess.run(["sh", "-c", pipeline, str(log)], capture_output=True, check=False).stdout
        assert ts.scan_magpie_warnings(log) == ts.intern_lines(shell), log


# --- memory and wall time on a generated full.gms -------------------------------------------------

MEASURE = """
import os, resource, sys, time
from modelstats.textscan import scan_iteration_max
start = time.perf_counter()
value = scan_iteration_max(sys.argv[1])
elapsed = time.perf_counter() - start
# ru_maxrss carries the peak of the process that spawned this one (Linux folds it in at exec), so the
# address space's own high-water mark VmHWM is reported where /proc exists.
peak_kb = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
if os.path.exists('/proc/self/status'):
    with open('/proc/self/status') as status:
        peak_kb = next(int(line.split()[1]) for line in status if line.startswith('VmHWM'))
print(value, peak_kb, elapsed)
"""


def generate_full_gms(path: Path, megabytes: int) -> None:
    """A GAMS-like listing: the only ``cm_iteration_max`` line near the start, filler to the size."""
    head = b"$title REMIND\n$setglobal c_expname  default\ncm_iteration_max = 100;\n$offlisting\n"
    filler = b"".join(
        b'  vm_cesIO.lo(t,regi,"%06d") = 0.001;  !! lower bound of the CES input\n' % i for i in range(16000)
    )
    with open(path, "wb") as handle:
        handle.write(head)
        written = len(head)
        target = megabytes << 20
        while written < target:
            handle.write(filler)
            written += len(filler)


@pytest.mark.skipif(
    not os.environ.get("MODELSTATS_TEXTSCAN_BIGFILE_MB"), reason="set MODELSTATS_TEXTSCAN_BIGFILE_MB"
)
def test_last_match_on_a_big_full_gms_stays_below_50_mb(tmp_path: Path) -> None:
    megabytes = int(os.environ["MODELSTATS_TEXTSCAN_BIGFILE_MB"])
    path = tmp_path / "full.gms"
    generate_full_gms(path, megabytes)
    size_mb = path.stat().st_size / (1 << 20)
    result = subprocess.run(
        [sys.executable, "-c", MEASURE, str(path)], capture_output=True, text=True, check=True
    )
    value, maxrss_kb, elapsed = result.stdout.split()
    peak_mb = int(maxrss_kb) / 1024
    print(
        f"\nlast_match on {size_mb:.0f} MB: value={value} "
        f"peak RSS={peak_mb:.1f} MB wall={float(elapsed):.2f} s"
    )
    assert value == "['100']"
    assert peak_mb < 50
    version = subprocess.run([GNU_GREP, "--version"], capture_output=True, text=True, check=False).stdout
    if "GNU grep" in version:
        pipeline = f"tac \"$0\" | {GNU_GREP} -m 1 'cm_iteration_max = [1-9].*.;[ ]*$'"
        start = time.perf_counter()
        shell = subprocess.run(["sh", "-c", pipeline, str(path)], capture_output=True, check=False).stdout
        shell_elapsed = time.perf_counter() - start
        print(f"tac | grep -m 1 on the same file: {shell.decode().strip()!r} wall={shell_elapsed:.2f} s")
        assert ts.intern_lines(shell) == ["cm_iteration_max = 100;"]
