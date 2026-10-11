"""``modelstats.amt.notify``: the REMIND status message, the failure message and the webhook transport.

The unit tier pins the composition rules of ``R/modeltests.R:453-466`` on synthetic inputs (the fences,
the ``capture.output`` line semantics, the optional "did not start" block), the new failure message of
``06`` section 3.2, the URL lookup and the three outcomes of :func:`modelstats.amt.notify.send` (sent, no
variable, a failed exchange that is logged and swallowed, including the recorded ``curl`` answer of the
``remind-evaluate-curlfail`` case through ``RecordingEffects``).
:func:`test_status_message_equals_the_r_golden` is the reference comparison of ``06`` section 4.4
(``pytest -m golden``, the fixture snapshot required): the
message composed from the ``remind-evaluate`` case's inputs equals the text inside the recorded
``mattermost.json``, with the sandbox's crayon state injected (the recording ran under
``R_CLI_NUM_COLORS=256``, which put escape sequences into the tables; see the module docstring of
``notify``).
"""

from __future__ import annotations

import json
import logging
import re
from collections.abc import Iterator
from dataclasses import dataclass, field
from datetime import date, datetime
from pathlib import Path
from typing import Any
from zoneinfo import ZoneInfo

import pytest

from modelstats.amt.notify import (
    MATTERMOST_URL_VARIABLE,
    NEXT_STEPS,
    RS_A_HEADER,
    SANITY_HEADER,
    CapturedTables,
    Delivery,
    FailureReport,
    StatusReport,
    capture_tables,
    captured_lines,
    failure_message,
    failure_reason,
    payload,
    payload_json,
    send,
    send_to,
    status_message,
    webhook_url,
)
from modelstats.amt.readme import CODE_FENCE, NOT_STARTED_TITLE
from modelstats.colors import Crayon
from modelstats.env import (
    CannedAmt,
    CannedSlurm,
    ClusterPaths,
    FakeEffects,
    HttpResponse,
    Mode,
    OverlayPaths,
    PostRecord,
    ProductionEffects,
    RecordingEffects,
)
from modelstats.sanity import amt_runcode
from tests.conftest import SANDBOX_ENVIRON, SYNTHETIC, resolve_frozen, slurm_case_dir
from tests.unit.test_facts import effects
from tests.unit.test_run_status import remind_run

BERLIN = ZoneInfo("Europe/Berlin")
TODAY = date(2026, 9, 30)
AT = datetime(2026, 9, 30, 12, 0, 0, tzinfo=BERLIN)
RUNS_PATH = "/p/projects/remind/modeltests/remind"
URL = "https://mattermost.example.org/hooks/fake-amt-token"
GIT_INFO = (
    "Tested commit: abc",
    "The test of 2026-09-30 contains these merges:",
    "abc Merge pull request #1",
)
TABLES = CapturedTables(rs="header \nrow one\nrow two\n", sanity="Results from x \n\nheader \n\nrow\n")
# R/modeltests.R:384: the testthat sentence of a cycle without test-full.log (the fixture snapshot has none).
TESTTHAT_MISSING = "Could not check for the results of `make test-full`, test-full.log not found"
REMIND_EVALUATE = "remind-evaluate"
CURLFAIL = "remind-evaluate-curlfail"
ESCAPE = re.compile(r"\x1b\[[0-9;]*m")
FAILED_LINE = (
    "The REMIND automated model tests (AMT) failed: `modeltests evaluate` stopped with an error at "
    "2026-09-30 12:00:00. Runs are here: `/p/projects/remind/modeltests/remind`."
)


def report(**overrides: Any) -> StatusReport:
    values: dict[str, Any] = {
        "today": TODAY,
        "runs_path": RUNS_PATH,
        "summary": "Summary: AMT runs look good.",
        "testthat_result": TESTTHAT_MISSING,
        "git_info": GIT_INFO,
        "tables": TABLES,
    }
    values.update(overrides)
    return StatusReport(**values)


# --- capture.output semantics ----------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("text", "lines"),
    [
        ("", []),
        ("a\n", ["a"]),
        ("a", ["a"]),
        ("a\nb\n", ["a", "b"]),
        ("a\n\nb\n", ["a", "", "b"]),
        ("\n", [""]),
        ("a\n\n", ["a", ""]),
    ],
)
def test_captured_lines_splits_like_capture_output(text: str, lines: list[str]) -> None:
    assert captured_lines(text) == lines


# --- the status message (R/modeltests.R:453-466) --------------------------------------------------------


def test_status_message_follows_the_r_order_and_fences() -> None:
    expected = "\n".join(
        [
            "Please find below the status of the REMIND automated model tests (AMT) of 2026-09-30. "
            "Runs are here: `/p/projects/remind/modeltests/remind`.",
            "Summary: AMT runs look good.",
            TESTTHAT_MISSING,
            CODE_FENCE,
            *GIT_INFO,
            CODE_FENCE,
            RS_A_HEADER,
            CODE_FENCE,
            "header ",
            "row one",
            "row two",
            CODE_FENCE,
            SANITY_HEADER,
            CODE_FENCE,
            "Results from x ",
            "",
            "header ",
            "",
            "row",
            CODE_FENCE,
        ]
    )
    assert status_message(report()) == expected


def test_not_started_block_is_appended_only_when_there_are_names() -> None:
    without = status_message(report())
    assert NOT_STARTED_TITLE not in without
    assert not without.endswith("\n")
    with_names = status_message(report(runs_not_started=["a-AMT", "b-AMT"]))
    assert with_names == f"{without}\n{NOT_STARTED_TITLE}\na-AMT\nb-AMT"
    assert status_message(report(runs_not_started=[])) == without


def test_empty_tables_give_adjacent_fences() -> None:
    message = status_message(report(tables=CapturedTables(rs="", sanity=""), git_info=[]))
    assert f"\n{CODE_FENCE}\n{CODE_FENCE}\n" in message
    assert message.count(CODE_FENCE) == 6


# --- the tables (R/modeltests.R:453-454) ----------------------------------------------------------------


def test_capture_tables_runs_the_two_commands_without_colour(tmp_path: Path) -> None:
    """The sanity table is the cycle's runs in the given ``output/``; the cluster paths play no part."""
    output = tmp_path / "output"
    run = remind_run(output, "SSP2-NPi-AMT_2026-09-28_10.30.27")
    env = effects(output, on_cluster=True, cluster_paths=ClusterPaths())  # the production default
    tables = capture_tables([run], env, output_dir=output)
    assert tables.rs.startswith("Folder")
    assert "SSP2-NPi-AMT_2026-09-28_10.30.27" in tables.rs
    assert "\x1b[" not in tables.rs
    assert tables.sanity.startswith(f"Results from {output}/ \n")
    lines = tables.sanity.splitlines()
    assert lines[1] == "" and lines[2].startswith("For column explanations see: ")
    assert lines[3].startswith("Folder") and lines[5].startswith("SSP2-NPi-AMT_2026-09-28_10.30.27")
    assert "\x1b[" not in tables.sanity
    assert not any(a.startswith(ClusterPaths().amt_dir) for c in env.calls for a in c.args)
    coloured = capture_tables([run], env, output_dir=output, crayon=Crayon())
    assert coloured.rs.startswith("\x1b[4mFolder")
    assert ESCAPE.sub("", coloured.rs) == tables.rs
    assert ESCAPE.sub("", coloured.sanity) == tables.sanity


# --- the failure message (06 section 3.2) ---------------------------------------------------------------


def test_failure_message_names_the_command_the_error_and_the_log() -> None:
    message = failure_message(
        FailureReport(
            command="evaluate",
            message="squeue had exit status > 0 more than 3 times in a row.",
            runs_path=RUNS_PATH,
            at=AT,
            log=Path("/p/projects/remind/modeltests/remind/logs/amt-2026-09-30.log"),
        )
    )
    assert message == "\n".join(
        [
            FAILED_LINE,
            CODE_FENCE,
            "squeue had exit status > 0 more than 3 times in a row.",
            CODE_FENCE,
            "Log: `/p/projects/remind/modeltests/remind/logs/amt-2026-09-30.log`",
            NEXT_STEPS,
        ]
    )


def test_failure_message_without_time_and_log_keeps_the_error_lines() -> None:
    message = failure_message(
        FailureReport(
            command="start", message="git exited with status 128\nfatal: not a git repository", runs_path="/x"
        )
    )
    assert message == "\n".join(
        [
            "The REMIND automated model tests (AMT) failed: `modeltests start` stopped with an error. "
            "Runs are here: `/x`.",
            CODE_FENCE,
            "git exited with status 128",
            "fatal: not a git repository",
            CODE_FENCE,
            NEXT_STEPS,
        ]
    )


# --- the transport --------------------------------------------------------------------------------------


@dataclass
class Poster:
    """``getenv`` and ``post_json`` only: the two doors ``send`` uses, recording the posts."""

    environ: dict[str, str] = field(default_factory=dict)
    status: int = 200
    body: str = "ok"
    posts: list[tuple[str, object]] = field(default_factory=list)

    def getenv(self, name: str) -> str:
        return self.environ.get(name, "")

    def post_json(self, url: str, payload: object) -> HttpResponse:
        self.posts.append((url, payload))
        return HttpResponse(url, self.status, self.body)


@pytest.fixture
def caplog_notify(caplog: pytest.LogCaptureFixture) -> Iterator[pytest.LogCaptureFixture]:
    with caplog.at_level(logging.INFO, logger="modelstats.amt.notify"):
        yield caplog


@pytest.mark.parametrize("value", ["", "   ", "\n"])
def test_webhook_url_is_none_for_an_unset_or_blank_variable(tmp_path: Path, value: str) -> None:
    unset = effects(tmp_path)
    assert webhook_url(unset) is None
    blank = effects(tmp_path, environ={MATTERMOST_URL_VARIABLE: value})
    assert webhook_url(blank) is None


def test_webhook_url_reads_the_named_variable_and_strips_it(tmp_path: Path) -> None:
    env = effects(tmp_path, environ={"HOOK": f" {URL}\n"})
    assert webhook_url(env, "HOOK") == URL
    assert webhook_url(env) is None


def test_payload_is_the_text_object_and_its_json_escapes_the_message() -> None:
    message = 'line "one"\nline two \x1b[4m'
    assert payload(message) == {"text": message}
    body = payload_json(message)
    assert json.loads(body) == {"text": message}
    assert "\n" not in body
    assert PostRecord(URL, payload(message)).raw == body


def test_send_posts_the_payload_to_the_url_of_the_variable(caplog_notify: pytest.LogCaptureFixture) -> None:
    poster = Poster({MATTERMOST_URL_VARIABLE: URL})
    assert send(poster, "hello\nworld") is Delivery.SENT
    assert poster.posts == [(URL, {"text": "hello\nworld"})]
    assert "Mattermost notification sent (11 characters, HTTP 200)" in caplog_notify.text
    assert URL not in caplog_notify.text


def test_send_to_posts_to_the_given_url_and_classifies_like_send(
    caplog_notify: pytest.LogCaptureFixture,
) -> None:
    poster = Poster()
    assert send_to(poster, URL, "hello") is Delivery.SENT
    assert poster.posts == [(URL, {"text": "hello"})]
    assert "Mattermost notification sent (5 characters, HTTP 200)" in caplog_notify.text
    failing = Poster(status=500, body="boom")
    assert send_to(failing, URL, "hello", variable="HOOK") is Delivery.FAILED
    assert failing.posts == [(URL, {"text": "hello"})]
    assert "Mattermost notification failed (webhook from $HOOK): HTTP 500: boom" in caplog_notify.text
    assert URL not in caplog_notify.text


def test_send_with_a_malformed_url_fails_without_raising_or_a_network_call(
    caplog_notify: pytest.LogCaptureFixture,
) -> None:
    """``AMT_MATTERMOST_URL=not-a-url``: a failed delivery, as R's failing ``curl`` was (``:61``)."""
    production = ProductionEffects(environ={MATTERMOST_URL_VARIABLE: "not-a-url"}, http_timeout=1.0)
    assert send(production, "hello") is Delivery.FAILED
    assert "no HTTP exchange (webhook URL is not an http(s) URL)" in caplog_notify.text
    assert "not-a-url" not in caplog_notify.text


def test_send_without_the_variable_makes_no_call(caplog_notify: pytest.LogCaptureFixture) -> None:
    poster = Poster({"OTHER": URL})
    assert send(poster, "hello") is Delivery.NOT_CONFIGURED
    assert poster.posts == []
    assert "No Mattermost webhook URL in $AMT_MATTERMOST_URL" in caplog_notify.text
    assert send(poster, "hello", variable="OTHER") is Delivery.SENT
    assert len(poster.posts) == 1


@pytest.mark.parametrize(
    ("status", "body", "reason"),
    [
        (500, '{"message":"boom"}', 'HTTP 500: {"message":"boom"}'),
        (404, "", "HTTP 404"),
        (
            0,
            "[Errno -2] Name or service not known",
            "no HTTP exchange ([Errno -2] Name or service not known)",
        ),
    ],
)
def test_send_logs_a_failed_exchange_and_does_not_raise(
    caplog_notify: pytest.LogCaptureFixture, status: int, body: str, reason: str
) -> None:
    poster = Poster({MATTERMOST_URL_VARIABLE: URL}, status=status, body=body)
    assert send(poster, "hello") is Delivery.FAILED
    assert poster.posts == [(URL, {"text": "hello"})]
    failure = [r for r in caplog_notify.records if r.levelno == logging.ERROR]
    assert len(failure) == 1
    assert (
        failure[0].getMessage()
        == f"Mattermost notification failed (webhook from $AMT_MATTERMOST_URL): {reason}"
    )
    assert URL not in caplog_notify.text


def test_failure_reason_flattens_and_cuts_the_body() -> None:
    assert failure_reason(HttpResponse(URL, 502, "<html>\n<body>Bad Gateway</body>\n</html>\n")) == (
        "HTTP 502: <html> <body>Bad Gateway</body> </html>"
    )
    long = failure_reason(HttpResponse(URL, 500, "x" * 300))
    assert long == "HTTP 500: " + "x" * 200 + "..."
    assert failure_reason(HttpResponse(URL, 0, "")) == "no HTTP exchange ()"


def test_send_with_the_recorded_curl_answers(
    cases_dir: Path, caplog_notify: pytest.LogCaptureFixture
) -> None:
    """The fake ``curl`` of ``remind-evaluate-curlfail`` answers ``HTTP/1.1 500`` (exit 22); R went on."""
    failing = RecordingEffects(
        now=AT,
        user="pascalfu",
        on_cluster=False,
        amt=CannedAmt.from_case_dir(cases_dir / "amt" / CURLFAIL),
        environ={MATTERMOST_URL_VARIABLE: URL},
    )
    assert send(failing, "hello") is Delivery.FAILED
    assert failing.posts == [PostRecord(URL, {"text": "hello"})]
    assert 'HTTP 500: {"message":"boom"}' in caplog_notify.text
    default = RecordingEffects(
        now=AT, user="pascalfu", on_cluster=False, environ={MATTERMOST_URL_VARIABLE: URL}
    )
    assert send(default, "hello") is Delivery.SENT
    assert [post.raw for post in default.posts] == [payload_json("hello")]


class ListHandler(logging.Handler):
    def __init__(self) -> None:
        super().__init__()
        self.records: list[logging.LogRecord] = []

    def emit(self, record: logging.LogRecord) -> None:
        self.records.append(record)


def test_send_uses_the_given_logger() -> None:
    logger = logging.getLogger("tests.notify.custom")
    handler = ListHandler()
    logger.addHandler(handler)
    logger.setLevel(logging.INFO)
    try:
        assert send(Poster(), "hello", log=logger) is Delivery.NOT_CONFIGURED
    finally:
        logger.removeHandler(handler)
    assert [r.name for r in handler.records] == ["tests.notify.custom"]


# --- the R golden of remind-evaluate (06 section 4.4, item 3) -------------------------------------------


def _readme_sections(readme: str) -> tuple[list[str], str, list[str]]:
    """The git lines, the summary and the not-started names of a recorded README (R/modeltests.R:219-429)."""
    lines = readme.splitlines()
    first_git = next(i for i, line in enumerate(lines) if line.startswith("Tested commit: "))
    header = next(i for i, line in enumerate(lines) if line.startswith("Run "))
    summary = next(line for line in lines if line.startswith("Summary: "))
    not_started: list[str] = []
    if NOT_STARTED_TITLE in lines:
        start = lines.index(NOT_STARTED_TITLE) + 1
        not_started = lines[start : lines.index(" ", start)]
    return lines[first_git:header], summary, not_started


def _recorded_message(mattermost_json: Path) -> str:
    """The text R pasted into ``'{"text": "<message>"}'`` (``R/modeltests.R:58-59``, BUG-015)."""
    record = json.loads(mattermost_json.read_text(encoding="utf-8"))
    body: str = record["payload"]
    prefix, suffix = '{"text": "', '"}'
    assert body.startswith(prefix) and body.endswith(suffix)
    assert "payload_json_error" in record, "the R body is not JSON (BUG-015)"
    return body[len(prefix) : -len(suffix)]


@pytest.mark.golden
def test_status_message_equals_the_r_golden(fixtures_dir: Path, goldens_dir: Path, cases_dir: Path) -> None:
    case = json.loads((cases_dir / "amt" / REMIND_EVALUATE / "case.json").read_text(encoding="utf-8"))
    checkout = case["mydir"].rstrip("/")
    mode = Mode(case["mode"])
    env = FakeEffects(
        now=resolve_frozen(case["frozen"] or "2026-09-30 12:00:00"),
        user=case["user"],
        on_cluster=mode.on_cluster,
        paths=OverlayPaths.sandbox(fixtures_dir / "p", SYNTHETIC / "p", mode),
        slurm=CannedSlurm.from_case_dir(slurm_case_dir(case["slurm_case"])),
        cwd=f"{checkout}/output",
        environ={**SANDBOX_ENVIRON, "USER": case["user"]},
    )
    # R/modeltests.R:238-239: the started runs are the output folders matching the cycle's runcode.
    runcode = re.compile(amt_runcode(env))
    folders = env.list_dirs_like_r(f"{checkout}/output", full_names=False)
    runs_started = [folder for folder in folders if runcode.search(folder)]
    assert len(runs_started) == 5
    golden = goldens_dir / "amt" / REMIND_EVALUATE
    git_info, summary, not_started = _readme_sections((golden / "README.md").read_text(encoding="utf-8"))
    assert len(not_started) == 20
    sandbox_crayon = Crayon.from_terminal(is_tty=False, environ=SANDBOX_ENVIRON)
    composed = status_message(
        StatusReport(
            today=env.now().date(),
            runs_path=checkout,
            summary=summary,
            testthat_result=TESTTHAT_MISSING,
            git_info=git_info,
            tables=capture_tables(runs_started, env, output_dir=f"{checkout}/output", crayon=sandbox_crayon),
            runs_not_started=not_started,
        )
    )
    recorded = _recorded_message(golden / "mattermost.json")
    assert composed == recorded
    assert json.loads(payload_json(composed)) == {"text": recorded}
    assert PostRecord(URL, payload(composed)).to_json()["payload_json"] == {"text": recorded}
