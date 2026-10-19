from __future__ import annotations

import os
from pathlib import Path

import pytest
from rich.text import Text

from modelstats import loop_runs, run_status
from modelstats.errors import ModelstatsError
from modelstats.loop_runs import print_run_table, runtime_cell
from modelstats.run_status import RunStatus
from tests.conftest import CapturedConsole, FakeShell, capture_console, make_run

FULL_LOG_OK = "*** LOOPS = 12\n*** Status: Normal completion\n"
LEGEND = "# Color code: pending/startup, running, converged/finished, no mif, conopt stalled?, error."
CLUSTER_HEADER = (
    "Folder           Runtime      inSlurm   RunType      RunStatus          Warnings   Iter              "
    "Conv                   modelstat              Mif     AppResults "
)
LOCAL_HEADER = (
    "Folder           Runtime      RunType      RunStatus          Warnings   Iter              "
    "Conv                   modelstat            Mif    "
)


def row(**values: object) -> RunStatus:
    base = RunStatus(name="run", path=Path("run"), warnings="0", conv="NA")
    for key, value in values.items():
        setattr(base, key, value)
    return base


@pytest.fixture
def styles(monkeypatch: pytest.MonkeyPatch) -> None:
    """Make the colour of a printed line visible as a ``[style]`` prefix."""
    monkeypatch.setattr(loop_runs, "styled", lambda text, style: Text(f"[{style}]{text}" if style else text))


@pytest.fixture
def statuses(monkeypatch: pytest.MonkeyPatch) -> dict[str, RunStatus]:
    """Replace get_run_status by a lookup on the folder name (a bare RunStatus by default)."""
    table: dict[str, RunStatus] = {}

    def fake(dirs: Path, *, user: str | None = None, **_: object) -> list[RunStatus]:
        default = RunStatus(name=dirs.name, path=dirs, job_in_slurm="no", warnings="NA", conv="NA")
        status = table.get(dirs.name, default)
        status.name, status.path = dirs.name, dirs
        return [status]

    monkeypatch.setattr(loop_runs, "get_run_status", fake)
    return table


# --- runtime_cell -------------------------------------------------------------------------------


def test_runtime_cell() -> None:
    assert runtime_cell(row(job_in_slurm="standby pending", runtime=100.0)) == "pending"
    assert runtime_cell(row(job_in_slurm="no", runtime=12345.0)) == "3.4 hours"
    assert runtime_cell(row(job_in_slurm="standby", runtime=12345.0)) == "> 3.4 hours"
    assert runtime_cell(row(job_in_slurm="standby", runtime=62730.5)) == ">17.4 hours"
    assert runtime_cell(row(job_in_slurm="NA", runtime=63.0)) == "> 1 mins"
    assert runtime_cell(row(job_in_slurm="short startup", runtime=None)) == "startup"
    assert runtime_cell(row(job_in_slurm="no", runtime=None)) == "NA"
    assert runtime_cell(row(job_in_slurm="NA", runtime=None)) == "NA"


# --- table layout -------------------------------------------------------------------------------


def test_header_legend_and_rows_on_cluster(
    tmp_path: Path, shell: FakeShell, statuses: dict[str, RunStatus], styles: None
) -> None:
    run = make_run(tmp_path, "SSP2-NPi-AMT", {"config.yml": "x: 1\n"})
    statuses["SSP2-NPi-AMT"] = RunStatus(
        name="",
        path=run,
        job_in_slurm="no",
        run_type="nash",
        modelstat="2: Locally Optimal",
        run_in_app_results="yes",
        mif="yes",
        iter="12/50",
        run_status="Normal completion",
        warnings="0",
        conv="converged",
        runtime=12345.0,
    )
    out = capture_console()
    print_run_table([run], out=out.console)
    assert out.lines[0].startswith("# Color code: ")
    assert out.lines[1] == ""
    assert out.lines[2] == CLUSTER_HEADER
    assert out.lines[3] == (
        "[green]SSP2-NPi-AMT     3.4 hours    no        nash         Normal completion  0          12/50             "
        "converged              2: Locally Optimal     yes     yes"
    )


def test_legend_text(tmp_path: Path, shell: FakeShell, statuses: dict[str, RunStatus]) -> None:
    run = make_run(tmp_path, "r", {"log.txt": "x\n"})
    out = capture_console()
    print_run_table([run], out=out.console)
    assert out.lines[0] == LEGEND
    coloured = capture_console(color=True)
    print_run_table([run], out=coloured.console)
    assert "\x1b[33mpending" in coloured.text and "\x1b[38;5;202merror" in coloured.text


def test_header_and_row_off_cluster_without_colour(
    tmp_path: Path, shell: FakeShell, statuses: dict[str, RunStatus], styles: None
) -> None:
    shell.cluster = False
    run = make_run(tmp_path, "r", {"log.txt": "x\n"})
    statuses["r"] = RunStatus(
        name="",
        path=run,
        job_in_slurm="NA",
        run_type="testOneRegi EUR",
        modelstat="NA",
        mif="no",
        iter="NA",
        run_status="full.log missing",
        warnings="NA",
        conv="NA",
    )
    out = capture_console()
    print_run_table([run], color=False, out=out.console)
    assert out.lines == [
        LOCAL_HEADER,
        "r                NA           1Regi EUR    full.log missing   NA         NA                NA                     NA                   no",
    ]


def test_first_column_width_follows_the_longest_name(
    tmp_path: Path, shell: FakeShell, statuses: dict[str, RunStatus], styles: None
) -> None:
    long_name = "C_" + "x" * 80
    runs = [
        make_run(tmp_path, "short", {"log.txt": "x\n"}),
        make_run(tmp_path, long_name, {"log.txt": "x\n"}),
    ]
    out = capture_console()
    print_run_table(runs, color=False, sort_by_time=False, out=out.console)
    assert out.lines[0].startswith("Folder" + " " * 61 + "  Runtime")
    assert out.lines[2].startswith(long_name[:67] + "  ")


def test_order_by_time_or_as_given(
    tmp_path: Path, shell: FakeShell, statuses: dict[str, RunStatus], styles: None
) -> None:
    old = make_run(tmp_path, "old", {"log.txt": "x\n"})
    new = make_run(tmp_path, "new", {"log.txt": "x\n"})
    os.utime(old, (1_000_000, 1_000_000))
    os.utime(new, (2_000_000, 2_000_000))
    out = capture_console()
    print_run_table(
        [old, new, tmp_path / "missing", tmp_path / "old" / "log.txt"], color=False, out=out.console
    )
    assert [line.split()[0] for line in out.lines[1:]] == ["new", "old"]
    out = capture_console()
    print_run_table([old, new], color=False, sort_by_time=False, out=out.console)
    assert [line.split()[0] for line in out.lines[1:]] == ["old", "new"]


def test_no_folders_prints_nothing(tmp_path: Path, shell: FakeShell, statuses: dict[str, RunStatus]) -> None:
    out = capture_console()
    print_run_table([], out=out.console)
    assert out.text == ""


def test_list_without_directories_prints_the_header(
    tmp_path: Path, shell: FakeShell, statuses: dict[str, RunStatus]
) -> None:
    out = capture_console()
    print_run_table([tmp_path / "missing"], out=out.console)
    assert out.lines == [LEGEND, "", CLUSTER_HEADER]


def test_user_defaults_to_the_current_user(
    tmp_path: Path, shell: FakeShell, monkeypatch: pytest.MonkeyPatch
) -> None:
    seen: list[str | None] = []

    def fake(dirs: Path, *, user: str | None = None, **_: object) -> list[RunStatus]:
        seen.append(user)
        return [RunStatus(name=dirs.name, path=dirs, warnings="NA", conv="NA")]

    monkeypatch.setattr(loop_runs, "get_run_status", fake)
    run = make_run(tmp_path, "r", {"log.txt": "x\n"})
    print_run_table([run], color=False, out=capture_console().console)
    print_run_table([run], user="bob", color=False, out=capture_console().console)
    assert seen == ["alice", "bob"]


# --- skipped lines ------------------------------------------------------------------------------


def test_folders_without_config_or_log_are_skipped_when_not_in_slurm(
    tmp_path: Path, shell: FakeShell, statuses: dict[str, RunStatus], styles: None
) -> None:
    bare = make_run(tmp_path, "archive")
    hidden = make_run(tmp_path, "hidden", {".config": "x\n"})
    with_config = make_run(tmp_path, "cfg", {"config.csv": "x\n"})
    running = make_run(tmp_path, "running")
    statuses["running"] = RunStatus(name="", path=running, job_in_slurm="standby", warnings="NA", conv="NA")
    almost = make_run(tmp_path, "almost", {"logXtxt": "x\n", "NA": "x\n"})  # BUG-057: neither counts
    broken = make_run(tmp_path, "broken", {"log.txt": "x\n"})
    (broken / "config.yml").symlink_to(broken / "nowhere")  # the first match is a broken link: skipped
    dotted = Path("./" + os.path.relpath(bare))  # BUG-058: printed without the ./
    out = capture_console()
    print_run_table(
        [bare, hidden, with_config, running, almost, broken, dotted],
        color=False,
        sort_by_time=False,
        out=out.console,
    )
    assert out.lines[1] == f"{bare} skipped."
    assert out.lines[2] == f"{hidden} skipped."
    assert out.lines[3].startswith("cfg ")
    assert out.lines[4].startswith("running ")
    assert out.lines[5] == f"{almost} skipped."
    assert out.lines[6] == f"{broken} skipped."
    assert out.lines[7] == f"{os.path.relpath(bare)} skipped."


def test_off_cluster_na_status_is_not_skipped(
    tmp_path: Path, shell: FakeShell, statuses: dict[str, RunStatus]
) -> None:
    shell.cluster = False
    bare = make_run(tmp_path, "bare")
    statuses["bare"] = RunStatus(name="", path=bare, job_in_slurm="NA", warnings="NA", conv="NA")
    out = capture_console()
    print_run_table([bare], color=False, out=out.console)
    assert out.lines[1].startswith("bare ")


def test_error_in_status_is_reported(
    tmp_path: Path, shell: FakeShell, err: CapturedConsole, monkeypatch: pytest.MonkeyPatch
) -> None:

    def failing(dirs: Path, *, user: str | None = None, **_: object) -> list[RunStatus]:
        raise ModelstatsError("boom")

    monkeypatch.setattr(loop_runs, "get_run_status", failing)
    run = make_run(tmp_path, "broken")  # no config, not in slurm: the error check comes first (BUG-040)
    out = capture_console()
    print_run_table([run], color=False, out=out.console)
    assert out.lines[1] == "broken skipped because of error"
    assert err.lines[-1] == f"Error in {run}: boom"
    monkeypatch.chdir(run)
    print_run_table([Path(".")], color=False, out=out.console)
    assert out.lines[-1] == ". skipped because of error"


def test_os_errors_are_reported_too(
    tmp_path: Path, shell: FakeShell, err: CapturedConsole, monkeypatch: pytest.MonkeyPatch
) -> None:
    def failing(dirs: Path, *, user: str | None = None, **_: object) -> list[RunStatus]:
        raise PermissionError("denied")

    monkeypatch.setattr(loop_runs, "get_run_status", failing)
    run = make_run(tmp_path, "locked", {"log.txt": "x\n"})
    out = capture_console()
    print_run_table([run], color=False, out=out.console)
    assert out.lines[1] == "locked skipped because of error"
    assert "denied" in err.lines[-1]


# --- colours ------------------------------------------------------------------------------------


def colour_of(status: RunStatus, tmp_path: Path, statuses: dict[str, RunStatus]) -> str:
    run = make_run(tmp_path, "r", {"log.txt": "x\n"})
    statuses["r"] = status
    out = capture_console()
    print_run_table([run], out=out.console)
    line = out.lines[3]
    return line[1 : line.index("]")] if line.startswith("[") else "none"


@pytest.mark.parametrize(
    ("status", "expected"),
    [
        (row(job_in_slurm="standby pending"), "yellow"),
        (row(job_in_slurm="short startup"), "yellow"),
        (row(job_in_slurm="standby", run_status="conoptspy >2h"), "magenta"),
        (row(job_in_slurm="standby", run_status="Run in progress"), "cyan"),
        (row(job_in_slurm="bob", run_status="Run in progress"), "cyan"),
        (row(job_in_slurm="no", conv="converged (had INFES)", mif="yes"), "green"),
        (
            row(job_in_slurm="no", conv="converged (had INFES)", mif="no", run_status="Normal completion"),
            "orange",
        ),
        (row(job_in_slurm="no", run_status="Execution error"), "orangered"),
        (row(job_in_slurm="no", run_status="Compilation error"), "orangered"),
        (row(job_in_slurm="no", run_status="Run interrupted"), "orangered"),
        (row(job_in_slurm="no", conv="converged", mif="yes"), "green"),
        (row(job_in_slurm="no", conv="Clb_converged", mif="sumErr"), "green"),
        (row(job_in_slurm="no", modelstat="2: Locally Optimal", run_type="testOneRegi EUR"), "green"),
        (row(job_in_slurm="no", modelstat="2: Locally Optimal", run_type="nash", conv="NA"), "orangered"),
        (row(job_in_slurm="no", conv="converged", mif="no"), "orange"),
        (row(job_in_slurm="no", conv="NA", mif="NA", run_status="Normal completion"), "orangered"),
        (row(job_in_slurm="NA", conv="NA", mif="NA", run_status="Normal completion"), "cyan"),
        (
            row(job_in_slurm="no", conv="converged (had INFES)", mif="yes", run_status="Execution error"),
            "green",
        ),
        (row(job_in_slurm="no", run_status="Intermed Infes"), "orangered"),
        (row(job_in_slurm="no", run_status="not_converged", conv="converged", mif="yes"), "orangered"),
        (row(job_in_slurm="short startup", runtime=30.0), "cyan"),
    ],
)
def test_remind_colours(
    tmp_path: Path,
    shell: FakeShell,
    statuses: dict[str, RunStatus],
    styles: None,
    status: RunStatus,
    expected: str,
) -> None:
    assert colour_of(status, tmp_path, statuses) == expected


@pytest.mark.parametrize(
    ("status", "expected"),
    [
        (row(run_type="nlp_apr17", job_in_slurm="standby pending"), "yellow"),
        (row(run_type="nlp_apr17", run_status="Execution error"), "orangered"),
        (row(run_type="nlp_apr17", run_status="full.log missing"), "orangered"),
        (row(run_type="nlp_apr17", run_status="Abort EUR 4*Infes"), "orangered"),
        (row(run_type="nlp_apr17", run_status="converged"), "green"),
        (row(run_type="nlp_apr17", modelstat="222"), "green"),
        (row(run_type="nlp_apr17", modelstat="2.2"), "none"),
        (row(run_type="nlp_apr17", modelstat="2: Locally Optimal"), "green"),
        (row(run_type="nlp_apr17", run_status="conoptspy >2h"), "magenta"),
        (row(run_type="nlp_apr17", run_status="Run in progress", job_in_slurm="standby"), "cyan"),
        (row(iter="y2050", run_status="Run in progress", job_in_slurm="standby"), "cyan"),
        (row(run_type="nlp_apr17", modelstat="NA", mif="FALSE"), "orangered"),
        (row(run_type="nlp_apr17", modelstat="227"), "none"),
        (row(run_type="nlp_apr17", run_status="not_converged"), "orangered"),
        (row(run_type="nlp_apr17", modelstat="222.2"), "none"),
        (row(run_type="nlp_apr17", job_in_slurm="short startup"), "none"),
    ],
)
def test_magpie_colours(
    tmp_path: Path,
    shell: FakeShell,
    statuses: dict[str, RunStatus],
    styles: None,
    status: RunStatus,
    expected: str,
) -> None:
    assert colour_of(status, tmp_path, statuses) == expected


def test_rows_are_right_trimmed_and_cells_overridden(
    tmp_path: Path, shell: FakeShell, statuses: dict[str, RunStatus]
) -> None:
    run = make_run(tmp_path, "r", {"log.txt": "x\n"})
    statuses["r"] = row(
        job_in_slurm="standby startup", run_type="testOneRegi EUR", runtime=None, run_in_app_results=None
    )
    out = capture_console()
    print_run_table([run], color=False, out=out.console)
    line = out.lines[1]
    assert line == line.rstrip(" ")
    assert "  startup      standby   1Regi EUR    " in line


def test_real_status_end_to_end(
    tmp_path: Path, shell: FakeShell, clock: object, monkeypatch: pytest.MonkeyPatch
) -> None:
    shell.cluster = False
    run = make_run(tmp_path, "SSP2-NPi-AMT", {"full.log": FULL_LOG_OK}, config="remind", stats="remind")
    out = capture_console()
    print_run_table([run], color=False, out=out.console)
    assert out.lines[0] == LOCAL_HEADER
    # off the cluster the slurm state is "NA" (not "no"), so R prefixed the runtime with ">" (BUG-043)
    assert out.lines[1] == (
        "SSP2-NPi-AMT     > 1 days     nash         Normal completion  NA         12/50             "
        "NA                     2: Locally Optimal   no"
    )
    assert run_status.get_run_status(run)[0].runtime == 90030


def test_header_is_underlined_even_without_colour(
    tmp_path: Path, shell: FakeShell, statuses: dict[str, RunStatus]
) -> None:
    run = make_run(tmp_path, "r", {"log.txt": "x\n"})
    coloured = capture_console(color=True)
    print_run_table([run], color=False, out=coloured.console)
    assert "\x1b[4m" in coloured.text.splitlines()[0]
    assert "# Color code" not in coloured.text
    assert coloured.text.splitlines()[1].startswith("r ") and "\x1b[3" not in coloured.text.splitlines()[1]


def test_legend_colours(tmp_path: Path, shell: FakeShell, statuses: dict[str, RunStatus]) -> None:
    run = make_run(tmp_path, "r", {"log.txt": "x\n"})
    coloured = capture_console(color=True)
    print_run_table([run], out=coloured.console)
    legend = coloured.text.splitlines()[0]
    assert "\x1b[38;5;214mno mif" in legend and "\x1b[35mconopt stalled?" in legend


def test_equal_mtimes_keep_the_input_order(
    tmp_path: Path, shell: FakeShell, statuses: dict[str, RunStatus]
) -> None:
    runs = [make_run(tmp_path, name, {"log.txt": "x\n"}) for name in ("b", "a", "c")]
    for run in runs:
        os.utime(run, (3_000_000, 3_000_000))
    out = capture_console()
    print_run_table(runs, color=False, out=out.console)
    assert [line.split()[0] for line in out.lines[1:]] == ["b", "a", "c"]
