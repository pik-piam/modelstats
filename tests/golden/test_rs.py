"""``rs`` against every R golden under ``migration/goldens/rs``, in process and end to end.

``migration/harness/make_goldens.R`` (``fam_rs``) ran ``commandLineInterface(argv)`` in its own ``Rscript``
process inside the sandbox for each row of ``migration/cases/rs.tsv`` (argv, cwd, mode, user, squeue case,
clock, scripted stdin) and recorded ``<id>.out`` (stdout bytes), ``<id>.err`` (stderr bytes) and
``<id>.status``. The Python command line is run two ways against the same three files:

* ``pytest -m golden``: in process, through :func:`modelstats.cli.make_app` over the ``FakeEffects`` of the
  row (overlay, canned scheduler, clock, user, cwd), in-memory streams, the scripted stdin and crayon's
  state under the sandbox variables (``R_CLI_NUM_COLORS=256``, ``tests/golden/test_sanity.py``);
* ``pytest -m sandbox``: end to end, the console script ``.venv/bin/rs`` as a subprocess inside
  ``migration/harness/sandbox.sh`` through ``migration/harness/run_rs_case.sh`` (fake ``squeue``/``sacct``
  on ``PATH``, stopped clock, fake passwd user, fixture overlay), exactly as the R oracle ran.

Comparison (``migration/03-migration-plan.md`` 4.6): stdout byte for byte, the exit status, and stderr after
dropping exactly the ``Did you know?`` line from both sides (decision D-08, approved: the hint is random).
Register bugs are ordinary passing rows (BUG-005 ``synthetic-bug005``, BUG-008 ``current--regex-*``, BUG-009,
BUG-025, BUG-026, BUG-027, BUG-028, BUG-040 ``prompt-invalid-then-ok``). Three decisions name a span that
differs from the R golden; everything else in those cases must still match:

* D-11 (approved): the help page layout is typer's; ``help`` and ``help-long`` compare the exit status and
  stderr and assert every R option in the Python page instead of the stdout bytes.
* D-20 (pending, unavoidable): the GDX library killed R (exit 134, ``free(): invalid pointer``) on the
  synthetic ``gdx-broken`` run; the port prints the ``skipped because of error`` row, ``try()``'s error line
  with the library's message on stderr and exits 0 (``synthetic-broken``, ``synthetic-broken--local``).
* D-21 (pending, unavoidable; BUG-040): with a piped stdin R's ``getLine()`` loses every line after the
  first. ``prompt-pattern`` and ``prompt-fixed``: ``chooseFromList`` recurses to a stack overflow (exit 1)
  where the port reads the pattern from the second line, lists the matches and, the input ending at the
  confirmation prompt, exits 1 with an error line; ``prompt-invalid-then-ok``: R selects nothing after the
  rejected line where the port reads ``3`` and prints that run. :data:`PROMPT_EXPECTATIONS` pins the Python
  side from the last common prompt on; the pinned table's cells are the ``prompt-all`` golden's row for the
  same run (:func:`test_prompt_selection_expectation_comes_from_the_r_goldens`).
"""

from __future__ import annotations

import io
import re
import shlex
import shutil
import subprocess
from collections.abc import Callable, Iterator
from contextlib import redirect_stderr, redirect_stdout
from dataclasses import dataclass
from pathlib import Path

import pytest

from modelstats.cli import DID_YOU_KNOW, HINTS, Session, make_app
from modelstats.colors import Crayon
from modelstats.env import CannedSlurm, FakeEffects
from tests.conftest import (
    MIGRATION,
    REPO,
    SANDBOX_ENVIRON,
    SANDBOX_FROZEN,
    SANDBOX_TZ,
    SYNTHETIC,
    GoldenCase,
    golden_cases,
    resolve_frozen,
)

CASES = golden_cases("rs")
PARAMS = [pytest.param(case, id=case.id) for case in CASES]
SANDBOX_CRAYON = Crayon.from_terminal(is_tty=False, environ={**SANDBOX_ENVIRON, "R_CLI_NUM_COLORS": "256"})
RUNNER = MIGRATION / "harness" / "run_rs_case.sh"
FAKETIME = MIGRATION / "harness" / "vendor" / "libfaketime" / "src" / "libfaketime.so.1"
RS_BIN = REPO / ".venv" / "bin" / "rs"
SYNTHETIC_PREFIX = "projects/synthetic/"
FROZEN_FORMAT = "%Y-%m-%d %H:%M:%S"
SANDBOX_TIMEOUT = 600

HELP_IDS = ("help", "help-long")
"""D-11: the help page is not compared byte for byte."""
R_OPTIONS = (
    ("-A", "--amt"),
    ("-b", "--nocolor"),
    ("-C", "--current"),
    ("-d", "--daysback"),
    ("-f", "--filter"),
    ("-l", "--last"),
    ("-m", "--magpie"),
    ("-p", "--prompt"),
    ("-s", "--sanity"),
    ("-t", "--time"),
    ("-u", "--user"),
    ("-h", "--help"),
)
"""The option list of ``R/commandLineInterface.R:35-47`` that the Python help page must show (D-11)."""

GDX_ABORT_IDS = ("synthetic-broken", "synthetic-broken--local")
"""D-20: R died in the GDX library; the port prints the error row."""
GDX_ABORT_LINE = b"free(): invalid pointer\n"
GDX_ABORT_ROW = b"gdx-broken skipped because of error\n"
GDX_ABORT_MESSAGE = "File not recognized as a GDX file"
BROKEN_RUN = "projects/synthetic/broken/output/gdx-broken"


@dataclass(frozen=True)
class PromptExpectation:
    """D-21: the Python side of a ``-p`` case from the last prompt both sides print alike.

    ``marker`` is that prompt (its first occurrence in R's stderr, or the last when ``last`` is set),
    ``r_evidence`` a text R's stderr must carry after it (the overflow, the empty selection); ``stderr``,
    ``stdout`` and ``status`` are the port's from the marker on.
    """

    marker: bytes
    r_evidence: bytes
    stderr: bytes
    stdout: bytes
    status: int
    last: bool = False


CONFIRMATION_EOF = (
    b"\nAre you sure these are the right folders? (y/n): \n"
    b"Error: end of input at the confirmation prompt\n"
    b"Execution halted\n"
)
PKBUDG650 = b"SSP2-EU21-PkBudg650-AMT_2026-09-28_13.54.59"
PKBUDG650_TABLE = (
    b"# Color code: \x1b[33mpending\x1b[39m/\x1b[33mstartup\x1b[39m, \x1b[36mrunning\x1b[39m, "
    b"\x1b[32mconverged\x1b[39m/\x1b[32mfinished\x1b[39m, \x1b[38;5;214mno mif\x1b[39m, "
    b"\x1b[35mconopt stalled?\x1b[39m, \x1b[38;5;202merror\x1b[39m.\n"
    b"\n"
    b"\x1b[4mFolder                                       Runtime      inSlurm   RunType      RunStatus"
    b"          Warnings   Iter              Conv                   modelstat              Mif     "
    b"AppResults\x1b[24m \n"
    b"\x1b[38;5;202m" + PKBUDG650 + b"  21.6 hours   no        nash         Normal completion  27"
    b"         100/100           not_converged          2: Locally Optimal     sumErr  yes\n"
    b"\x1b[39m"
)
"""``loopRuns`` of the one run entry ``3`` selects: the folder column is as wide as its name (43)."""

PROMPT_EXPECTATIONS: dict[str, PromptExpectation] = {
    "prompt-pattern": PromptExpectation(
        marker=b"\nInsert the regular expression: \n",
        r_evidence=b"node stack overflow",
        stderr=b"\n\nThe search pattern matches the following folders:\n1: ./"
        + PKBUDG650
        + b"\n"
        + CONFIRMATION_EOF,
        stdout=b"",
        status=1,
    ),
    "prompt-fixed": PromptExpectation(
        marker=b"\nInsert the search pattern with fixed=TRUE: \n",
        r_evidence=b"node stack overflow",
        stderr=b"\n\nThe search pattern matches the following folders:\n"
        b"1: ./testOneRegi\n2: ./testOneRegi-AMT\n" + CONFIRMATION_EOF,
        stdout=b"",
        status=1,
    ),
    "prompt-invalid-then-ok": PromptExpectation(
        marker=b"\nNumbers entered as 2,4:6,9 or leave empty:\n",
        r_evidence=b"Selected: \n",
        stderr=b"Selected: ./" + PKBUDG650 + b"\n\x1b[36m\xe2\x84\xb9\x1b[39m Runs found: 1\n",
        stdout=PKBUDG650_TABLE,
        status=0,
        last=True,
    ),
}
"""D-21 (BUG-040): the three ``-p`` rows whose second stdin line R lost."""

HINT_LINE = re.compile(rb"^(?:\x1b\[36m)?\xe2\x84\xb9(?:\x1b\[39m)? Did you know\? ([^\n]*)\n", re.MULTILINE)
"""The ``cli_alert_info("Did you know? {sample(hints, 1)}")`` line, coloured or not (D-08)."""
ANSI = re.compile(rb"\x1b\[[0-9;]*m")


@dataclass(frozen=True)
class Outcome:
    """What one ``rs`` process left behind: the two streams as bytes and the exit status."""

    stdout: bytes
    stderr: bytes
    status: int


def r_outcome(goldens_dir: Path, case_id: str) -> Outcome:
    """The R golden of a case: ``<id>.out``, ``<id>.err`` and ``<id>.status``."""
    base = goldens_dir / "rs" / case_id
    return Outcome(
        base.with_suffix(".out").read_bytes(),
        base.with_suffix(".err").read_bytes(),
        int(base.with_suffix(".status").read_text(encoding="utf-8").strip()),
    )


def argv_of(case: GoldenCase) -> list[str]:
    """The row's ``argv`` split as ``scan(text = , what = "character")`` did (equal to ``<id>.argv``)."""
    return shlex.split(case.row["argv"]) if case.row["argv"] else []


def stdin_lines(case: GoldenCase) -> list[str]:
    r"""The row's scripted stdin: ``\n``-separated in the case list, one line per element."""
    text = case.row.get("stdin", "")
    return text.replace("\\n", "\n").split("\n") if text else []


def needs_synthetic(case: GoldenCase) -> bool:
    return case.row.get("cwd", "").startswith(SYNTHETIC_PREFIX)


def without_hint(stderr: bytes) -> tuple[bytes, bytes | None]:
    """The stderr without its ``Did you know?`` line (D-08) and the hint it carried (``None`` without one)."""
    found = HINT_LINE.search(stderr)
    if found is None:
        return stderr, None
    assert HINT_LINE.search(stderr, found.end()) is None, "more than one hint line"
    return stderr[: found.start()] + stderr[found.end() :], found.group(1)


def expected_python(case: GoldenCase, golden: Outcome, gdx_path: str) -> Outcome:
    """The expectation for the port: the R golden, except in the named spans of D-20 and D-21."""
    if case.id in GDX_ABORT_IDS:
        assert golden.status == 134 and golden.stderr.endswith(GDX_ABORT_LINE)
        error_line = f"Error : {gdx_path}: {GDX_ABORT_MESSAGE}\n".encode()
        stderr = golden.stderr[: -len(GDX_ABORT_LINE)] + error_line
        return Outcome(golden.stdout + GDX_ABORT_ROW, stderr, 0)
    if case.id in PROMPT_EXPECTATIONS:
        pin = PROMPT_EXPECTATIONS[case.id]
        found = golden.stderr.rindex(pin.marker) if pin.last else golden.stderr.index(pin.marker)
        cut = found + len(pin.marker)
        assert pin.r_evidence in golden.stderr[cut:]
        return Outcome(pin.stdout, golden.stderr[:cut] + pin.stderr, pin.status)
    return golden


def assert_same_outcome(case: GoldenCase, actual: Outcome, expected: Outcome) -> None:
    """The comparison of 03 section 4.6 with the D-08 hint line dropped and the D-11 help page asserted."""
    actual_stderr, actual_hint = without_hint(actual.stderr)
    expected_stderr, expected_hint = without_hint(expected.stderr)
    assert (actual_hint is None) == (expected_hint is None)
    if actual_hint is not None:
        assert actual_hint.decode("utf-8") in HINTS
    assert actual.status == expected.status
    assert actual_stderr == expected_stderr
    if case.id in HELP_IDS:
        page = actual.stdout.decode("utf-8")
        for short, long in R_OPTIONS:
            assert short in page and long in page
        return
    assert actual.stdout == expected.stdout


# ---------------------------------------------------------------------------------------------
# The golden files and the case list
# ---------------------------------------------------------------------------------------------


@pytest.mark.golden
def test_every_case_has_its_goldens_and_nothing_else(goldens_dir: Path) -> None:
    assert CASES, "no rs cases found"
    ids = {case.id for case in CASES}
    for suffix in (".argv", ".out", ".err", ".status"):
        on_disk = {path.name.removesuffix(suffix) for path in (goldens_dir / "rs").glob(f"*{suffix}")}
        assert on_disk == ids, suffix


@pytest.mark.golden
def test_the_named_cases_exist() -> None:
    ids = {case.id for case in CASES}
    assert set(HELP_IDS) <= ids
    assert set(GDX_ABORT_IDS) <= ids
    assert set(PROMPT_EXPECTATIONS) <= ids


@pytest.mark.golden
def test_prompt_selection_expectation_comes_from_the_r_goldens(goldens_dir: Path) -> None:
    """The pinned table of ``prompt-invalid-then-ok``: ``prompt-all``'s legend, titles and row for the run."""
    prompt_all = r_outcome(goldens_dir, "prompt-all").stdout.split(b"\n")
    pinned = PKBUDG650_TABLE.split(b"\n")
    assert pinned[0] == prompt_all[0]
    r_row = next(line for line in prompt_all if PKBUDG650 in line)
    py_row = next(line for line in pinned if PKBUDG650 in line)
    assert ANSI.sub(b"", py_row).split() == ANSI.sub(b"", r_row).split()
    assert ANSI.sub(b"", pinned[2]).split() == ANSI.sub(b"", prompt_all[2]).split()  # the header titles


@pytest.mark.golden
@pytest.mark.parametrize("case", PARAMS)
def test_r_hint_lines_are_from_the_list(case: GoldenCase, goldens_dir: Path) -> None:
    """Every R golden with the announcement carries one hint from ``HINTS`` (the generator's ``set.seed``)."""
    stripped, hint = without_hint(r_outcome(goldens_dir, case.id).stderr)
    assert DID_YOU_KNOW.encode() not in stripped
    if hint is not None:
        assert hint.decode("utf-8") in HINTS


# ---------------------------------------------------------------------------------------------
# In process: FakeEffects and in-memory streams
# ---------------------------------------------------------------------------------------------


def scripted(lines: list[str]) -> Callable[[], str | None]:
    """A ``read_line`` over the scripted stdin: one line per prompt, ``None`` at the end (D-21)."""
    pending: Iterator[str] = iter(lines)
    return lambda: next(pending, None)


def run_in_process(case: GoldenCase, effects: FakeEffects) -> Outcome:
    """``rs argv`` in this process: the typer application over a session on ``effects``."""
    out, err = io.StringIO(), io.StringIO()
    session = Session(
        effects=effects,
        out=out,
        err=err,
        table_crayon=SANDBOX_CRAYON,
        alert_crayon=SANDBOX_CRAYON,
        read_line=scripted(stdin_lines(case)),
        pick_hint=lambda hints: hints[0],
    )
    with redirect_stdout(out), redirect_stderr(err):  # typer's help page and usage errors
        status = make_app(lambda: session)(args=argv_of(case), prog_name="rs")
    return Outcome(
        out.getvalue().encode("utf-8", "surrogateescape"),
        err.getvalue().encode("utf-8", "surrogateescape"),
        status,
    )


@pytest.mark.golden
@pytest.mark.parametrize("case", PARAMS)
def test_rs_golden_in_process(
    case: GoldenCase,
    case_effects: Callable[[GoldenCase], FakeEffects],
    goldens_dir: Path,
    request: pytest.FixtureRequest,
) -> None:
    if needs_synthetic(case):
        request.getfixturevalue("synthetic_dir")
    effects = case_effects(case)
    gdx_path = str(SYNTHETIC / "p" / BROKEN_RUN / "fulldata.gdx")  # the physical file the library names
    expected = expected_python(case, r_outcome(goldens_dir, case.id), gdx_path)
    assert_same_outcome(case, run_in_process(case, effects), expected)


@pytest.mark.golden
def test_found_in_slurm_contract(tmp_path: Path) -> None:
    """``rs --found-in-slurm DIR``: the ``inSlurm`` string plus a newline, nothing else, exit 0; error: 1."""
    run = tmp_path / "output" / "myrun"
    run.mkdir(parents=True)
    now = resolve_frozen(SANDBOX_FROZEN).astimezone(SANDBOX_TZ)
    line = f"pascalfu {run} myrun 1:02:03 RUNNING standby\n"

    def outcome(user: str, files: dict[str, str]) -> Outcome:
        effects = FakeEffects(now=now, user=user, on_cluster=True, slurm=CannedSlurm(files), cwd=tmp_path)
        out, err = io.StringIO(), io.StringIO()
        session = Session(effects=effects, out=out, err=err, table_crayon=Crayon(), alert_crayon=Crayon())
        with redirect_stdout(out), redirect_stderr(err):
            status = make_app(lambda: session)(args=["--found-in-slurm", str(run)], prog_name="rs")
        return Outcome(out.getvalue().encode(), err.getvalue().encode(), status)

    none = outcome("pascalfu", {"squeue_all.txt": ""})
    assert none == Outcome(b"no\n", b"", 0)
    mine = outcome("pascalfu", {"squeue_all.txt": line})
    assert mine == Outcome(b"standby\n", b"", 0)
    failed = outcome("pascal[", {"squeue_all.txt": line})  # the user name is a regex in R (BUG-008 family)
    assert failed.status == 1 and failed.stdout == b"" and failed.stderr.startswith(b"rs --found-in-slurm: ")
    assert failed.stderr.count(b"\n") == 1


# ---------------------------------------------------------------------------------------------
# End to end: the console script inside sandbox.sh
# ---------------------------------------------------------------------------------------------


@pytest.fixture(scope="session")
def sandbox_runner(fixtures_dir: Path) -> Path:
    """``run_rs_case.sh`` with its prerequisites (bwrap, libfaketime, the venv's ``rs``), or a skip."""
    if shutil.which("bwrap") is None:
        pytest.skip("bubblewrap (bwrap) is not installed")
    if not FAKETIME.is_file() and shutil.which("faketime") is None:
        pytest.skip(f"libfaketime not built: {FAKETIME} (migration/harness/README.md)")
    if not RS_BIN.is_file():
        pytest.skip(f"console script absent: {RS_BIN} (uv sync)")
    assert RUNNER.is_file(), RUNNER
    return RUNNER


def run_in_sandbox(case: GoldenCase, runner: Path, case_dir: Path) -> Outcome:
    """``rs argv`` as a subprocess inside ``sandbox.sh`` with the row's settings, as ``fam_rs`` ran R."""
    case_dir.mkdir(parents=True, exist_ok=True)
    stdin = case_dir / "stdin.txt"
    lines = stdin_lines(case)
    stdin.write_text("".join(f"{line}\n" for line in lines), encoding="utf-8")
    command = [
        str(runner),
        str(case_dir),
        "--mode",
        case.mode.value,
        "--user",
        case.user,
        "--slurm-case",
        case.slurm_case,
        "--frozen",
        case.frozen.strftime(FROZEN_FORMAT),
        "--cwd",
        case.cwd,
        "--stdin",
        str(stdin),
        "--",
        *argv_of(case),
    ]
    subprocess.run(command, check=False, timeout=SANDBOX_TIMEOUT, capture_output=True)
    return Outcome(
        (case_dir / "rs.out").read_bytes(),
        (case_dir / "rs.err").read_bytes(),
        int((case_dir / "rs.status").read_text(encoding="utf-8").strip()),
    )


@pytest.mark.sandbox
@pytest.mark.parametrize("case", PARAMS)
def test_rs_golden_in_sandbox(
    case: GoldenCase,
    sandbox_runner: Path,
    goldens_dir: Path,
    tmp_path: Path,
    request: pytest.FixtureRequest,
) -> None:
    if needs_synthetic(case):
        request.getfixturevalue("synthetic_dir")
    gdx_path = f"{case.fixture_root}/{BROKEN_RUN}/fulldata.gdx"  # the logical path inside the sandbox
    expected = expected_python(case, r_outcome(goldens_dir, case.id), gdx_path)
    assert_same_outcome(case, run_in_sandbox(case, sandbox_runner, tmp_path / case.id), expected)
