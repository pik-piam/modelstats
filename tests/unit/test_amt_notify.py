"""amt.notify: the Mattermost notification of evaluateRuns against R/modeltests.R lines 57-62 and 447-486.

Self-contained part (no ``migration/``): the ``capture.output`` line semantics and the visible return value
of ``loopRuns`` (R 4.6.1 facts pinned 2026-10-01 with ``migration/_scratch/p5-notify/probe_capture.R``),
the exact line list of the REMIND and MAgPIE messages (the MAgPIE texts are the goldens' payloads verbatim),
the payload frame and its inverse for both writers (BUG-015 / D-15), the absent-token path and the failing
transport (R: ``system(cmd, intern = TRUE)`` warns ``running command '<cmd>' had status <n>`` and continues).

Golden part (skipped without ``migration/goldens/amt``): for every AMT case that recorded a ``mattermost.json``
the message inputs are rebuilt from the case's README.md (summary, git info, the not-started block), result.json
(the frozen date) and, for the parts that exist nowhere else, the payload itself (the ``rs -A`` and sanity
blocks, the testthat line); the composed text must equal the recorded payload's text. The four cases named by
the specification (remind-evaluate, remind-evaluate-email, magpie-evaluate-recent, magpie-evaluate-old) are
asserted to be among them.
"""

from __future__ import annotations

import json
import socket
import ssl
import urllib.error
import warnings
from collections.abc import Callable, Sequence
from pathlib import Path
from typing import IO

import pytest
from _fake_effects import FakeEffects

from modelstats.amt import notify
from modelstats.amt.notify import (
    MAGPIE_SUCCESS,
    MAGPIE_WARNINGS,
    NOT_STARTED_HEADER,
    SYSTEM_CALL,
    capture_loop_runs,
    capture_sanity_checks,
    curl_command,
    curl_exit_status,
    magpie_message,
    mattermost_bot_message,
    payload_for,
    payload_text,
    r_capture_lines,
    r_print_character,
    remind_message,
    send_notification,
)
from modelstats.env import Effects, ProductionEffects
from modelstats.errors import RWarning

GOLDENS_AMT = Path(__file__).resolve().parents[2] / "migration" / "goldens" / "amt"
TOKEN = "https://mattermost.example.org/hooks/fake-amt-token"
# migration/goldens/amt/magpie-evaluate-recent/mattermost.json and magpie-evaluate-old: the recorded payload
MAGPIE_WARNINGS_PAYLOAD = (
    '{"text": "Some MAgPIE tests produce warnings. Please check https://gitlab.pik-potsdam.de/landuse/testing_suite"}'
)
ESC = "\x1b"


# --------------------------------------------------------------------------- doubles


class RaisingEffects(FakeEffects):
    """A double whose ``post_json`` records the call and then raises (a caller that propagates exceptions)."""

    def __init__(self, failure: BaseException) -> None:
        super().__init__(on_cluster=False)
        self.failure = failure

    def post_json(self, url: str, payload: str | bytes) -> tuple[int, str]:
        super().post_json(url, payload)
        raise self.failure


def _fake_loop_runs(printed: str, value: str | None, calls: list[dict[str, object]]) -> Callable[..., str | None]:
    def fake(
        mydir: Sequence[str], user: str | None, colors: bool, sortbytime: bool, effects: Effects, out: IO[str]
    ) -> str | None:
        calls.append(
            {"mydir": list(mydir), "user": user, "colors": colors, "sortbytime": sortbytime, "effects": effects}
        )
        out.write(printed)
        return value

    return fake


def _fake_sanity(printed: str, calls: list[dict[str, object]]) -> Callable[..., None]:
    def fake(dirs: object, effects: Effects, out: IO[str]) -> None:
        calls.append({"dirs": dirs, "effects": effects})
        out.write(printed)

    return fake


# --------------------------------------------------------------------------- capture.output


@pytest.mark.parametrize(
    ("printed", "lines"),
    [
        ("a\nb\n", ["a", "b"]),
        ("a\nb", ["a", "b"]),
        ("a\n\nb\n", ["a", "", "b"]),
        ("", []),
        ("\n", [""]),
        ("1 \n2 \n", ["1 ", "2 "]),
        (f"{ESC}[4mhead{ESC}[24m \nr1\n", [f"{ESC}[4mhead{ESC}[24m ", "r1"]),
    ],
)
def test_r_capture_lines_pins_capture_output(printed: str, lines: list[str]) -> None:
    """capture.output: trailing newline closes the line, an incomplete last line is kept, nothing gives character(0)."""
    assert r_capture_lines(printed) == lines


def test_r_print_character_is_rs_print_of_one_string() -> None:
    assert r_print_character("No runs found") == '[1] "No runs found"\n'
    assert r_print_character('a "q" \\ b\n') == '[1] "a \\"q\\" \\\\ b\\n"\n'


def test_capture_loop_runs_prints_the_visible_return_value_for_no_runs() -> None:
    """loopRuns(character(0)) returns "No runs found" visibly: capture.output ends with [1] "No runs found"."""
    assert capture_loop_runs([], FakeEffects()) == ['[1] "No runs found"']


def test_capture_loop_runs_passes_rs_arguments_and_splits_lines(monkeypatch: pytest.MonkeyPatch) -> None:
    calls: list[dict[str, object]] = []
    eff = FakeEffects()
    monkeypatch.setattr(
        notify, "loop_runs", _fake_loop_runs(f"{ESC}[4mFolder{ESC}[24m \nrow one\nrow two\n", None, calls)
    )
    assert capture_loop_runs(["a-AMT", "b-AMT"], eff) == [f"{ESC}[4mFolder{ESC}[24m ", "row one", "row two"]
    assert calls == [{"mydir": ["a-AMT", "b-AMT"], "user": None, "colors": False, "sortbytime": False, "effects": eff}]


def test_capture_loop_runs_merges_an_incomplete_line_with_the_printed_value(monkeypatch: pytest.MonkeyPatch) -> None:
    """R: cat("x") followed by a visible "No runs found" captures as one line x[1] "No runs found"."""
    monkeypatch.setattr(notify, "loop_runs", _fake_loop_runs("x", "No runs found", []))
    assert capture_loop_runs(["whatever"], FakeEffects()) == ['x[1] "No runs found"']


def test_capture_sanity_checks_uses_the_amt_lookup(monkeypatch: pytest.MonkeyPatch) -> None:
    calls: list[dict[str, object]] = []
    eff = FakeEffects()
    monkeypatch.setattr(notify, "get_sanity_checks", _fake_sanity("Results from /p/x/ \n\nhead \n\n", calls))
    assert capture_sanity_checks(eff) == ["Results from /p/x/ ", "", "head ", ""]
    assert calls == [{"dirs": None, "effects": eff}]


# --------------------------------------------------------------------------- composition


RS2 = [f"{ESC}[4mFolder  Runtime{ESC}[24m ", "a-AMT_2026-09-28  3.4 hours"]
RSS = ["Results from /p/projects/remind/modeltests/remind/output/ ", "", f"{ESC}[36mFor column{ESC}[39m", "a-AMT  0"]
GIT_INFO = [
    "Tested commit: 3f5e2a1b",
    "The test of 2026-09-30 contains these merges:",
    "3f5e2a1 Merge pull request #999",
]


def test_remind_message_is_the_exact_line_list() -> None:
    text = remind_message(
        today="2026-09-30",
        summary="Summary: AMT runs look good.",
        testthat_result="All tests pass in `make test-full`: [ FAIL 0 | WARN 0 | SKIP 3 | PASS 120 ]",
        git_info=GIT_INFO,
        rs2=RS2,
        rss=RSS,
        runs_not_started=["SSP2-NDC-AMT", "SSP1-NPi2025-AMT"],
    )
    assert text == "\n".join(
        [
            "Please find below the status of the REMIND automated model tests (AMT) of 2026-09-30. "
            "Runs are here: `/p/projects/remind/modeltests/remind`.",
            "Summary: AMT runs look good.",
            "All tests pass in `make test-full`: [ FAIL 0 | WARN 0 | SKIP 3 | PASS 120 ]",
            "```",
            *GIT_INFO,
            "```",
            "`rs -A` returns:",
            "```",
            *RS2,
            "```",
            "Sanity checks (`rs -s`) return:",
            "```",
            *RSS,
            "```",
            "These scenarios did not start at all:",
            "SSP2-NDC-AMT",
            "SSP1-NPi2025-AMT",
        ]
    )


def test_remind_message_not_started_block_is_tri_state() -> None:
    """exists("runsNotStarted"): absent -> no block; character(0) -> header only; names -> header and names."""
    common = dict(today="d", summary="s", testthat_result="t", git_info=["g"], rs2=["r"], rss=["q"])
    absent = remind_message(**common)
    empty = remind_message(**common, runs_not_started=[])
    named = remind_message(**common, runs_not_started=["x-AMT"])
    assert absent.endswith("\n```")
    assert empty == absent + "\n" + NOT_STARTED_HEADER
    assert named == empty + "\nx-AMT"


def test_remind_message_zero_length_vectors_add_no_line() -> None:
    """paste0(c("```", character(0), "```"), collapse = "\\n") puts the fences on consecutive lines."""
    text = remind_message(today="d", summary="s", testthat_result="t", git_info=[], rs2=[], rss=[])
    assert text.split("\n")[3:] == [
        "```",
        "```",
        "`rs -A` returns:",
        "```",
        "```",
        "Sanity checks (`rs -s`) return:",
        "```",
        "```",
    ]


def test_magpie_message_depends_on_a_non_null_error_list() -> None:
    assert magpie_message(["Some run(s) did not converge"]) == MAGPIE_WARNINGS
    assert magpie_message(None) == MAGPIE_SUCCESS
    assert magpie_message([]) == MAGPIE_SUCCESS


# --------------------------------------------------------------------------- payload


def test_payload_for_plain_message_equals_the_r_concatenation() -> None:
    """magpie-evaluate-recent / magpie-evaluate-old: the recorded R payload, byte for byte."""
    assert payload_for(MAGPIE_WARNINGS) == MAGPIE_WARNINGS_PAYLOAD
    assert payload_text(MAGPIE_WARNINGS_PAYLOAD) == MAGPIE_WARNINGS


def test_payload_for_escapes_what_r_does_not() -> None:
    message = f'line 1\n{ESC}[4mhead{ESC}[24m \nsay "hi" \\ ü'
    payload = payload_for(message)
    assert "\n" not in payload and ESC not in payload and "ü" in payload
    assert json.loads(payload) == {"text": message}
    assert payload_text(payload) == message


def test_payload_text_strips_rs_frame_from_an_unescaped_payload() -> None:
    """R's payload is {"text": "<message>"} with raw newlines and quotes inside (BUG-015)."""
    message = 'Please find below\n```\n[1] "No runs found"\n```\nend'
    r_payload = '{"text": "' + message + '"}'
    with pytest.raises(json.JSONDecodeError):
        json.loads(r_payload)
    assert payload_text(r_payload) == message


@pytest.mark.parametrize("payload", ["", "{}", '{"text": 1}', '{"text": "a", "b": 1}', '["text"]', "not json at all"])
def test_payload_text_rejects_other_shapes(payload: str) -> None:
    with pytest.raises(ValueError):
        payload_text(payload)


def test_curl_command_is_rs_string_concatenation() -> None:
    """Rscript: cat(.mattermostBotMessage("hello\\nworld", "https://x.example/hooks/t"))."""
    assert (
        curl_command("hello\nworld", "https://x.example/hooks/t")
        == "curl -i -X POST -H 'Content-Type: application/json' -d '{\"text\": \"hello\nworld\"}' https://x.example/hooks/t"
    )


# --------------------------------------------------------------------------- transport


def test_mattermost_bot_message_posts_the_json_payload() -> None:
    eff = FakeEffects(post_json_response=(200, "ok"))
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        result = mattermost_bot_message("hello\nworld", TOKEN, eff)
    assert result == (200, "ok")
    assert eff.posts == [(TOKEN, '{"text": "hello\\nworld"}')]
    assert [c for c in caught if isinstance(c.message, RWarning)] == []


def test_http_error_response_is_not_a_failure() -> None:
    """curl runs without --fail: a 500 answer exits 0, R sees nothing to warn about."""
    error = urllib.error.HTTPError(TOKEN, 500, "Internal Server Error", {}, None)  # type: ignore[arg-type]
    eff = RaisingEffects(error)
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        assert mattermost_bot_message("m", TOKEN, eff) == (500, "Internal Server Error")
    assert caught == []


def test_transport_failure_warns_like_system_intern_and_continues(capsys: pytest.CaptureFixture[str]) -> None:
    """remind-evaluate-curlfail: the RWarning carries R's call and text, the reason goes to stderr, no exception."""
    eff = RaisingEffects(urllib.error.URLError(ConnectionRefusedError(111, "Connection refused")))
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        assert mattermost_bot_message("hello\nworld", TOKEN, eff) is None
    assert eff.posts == [(TOKEN, '{"text": "hello\\nworld"}')]
    [warning] = [c.message for c in caught if isinstance(c.message, RWarning)]
    assert warning.call == SYSTEM_CALL == "system(cmd, intern = TRUE)"
    assert warning.text == (
        "running command 'curl -i -X POST -H 'Content-Type: application/json' -d "
        '\'{"text": "hello\nworld"}\' https://mattermost.example.org/hooks/fake-amt-token\' had status 7'
    )
    captured = capsys.readouterr()
    assert captured.out == ""
    assert captured.err == "Mattermost notification failed: URLError: <urlopen error [Errno 111] Connection refused>\n"


def test_status_zero_is_the_transport_failure(capsys: pytest.CaptureFixture[str]) -> None:
    """Effects.post_json never raises for a network condition: status 0 and the error text as body."""
    body = "URLError: <urlopen error [Errno -2] Name or service not known>"
    eff = FakeEffects(post_json_response=(0, body))
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        assert mattermost_bot_message("m", TOKEN, eff) is None
    [warning] = [c.message for c in caught if isinstance(c.message, RWarning)]
    assert warning.call == SYSTEM_CALL
    assert warning.text == f"running command '{curl_command('m', TOKEN)}' had status 6"
    assert capsys.readouterr().err == f"Mattermost notification failed: {body}\n"


def test_real_post_json_with_an_unknown_scheme_fails_offline(capsys: pytest.CaptureFixture[str]) -> None:
    """ProductionEffects.post_json on an unknown URL scheme: urllib rejects it before any socket; curl would exit 3."""
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        assert mattermost_bot_message("m", "nope://hooks/fake", ProductionEffects()) is None
    [warning] = [c.message for c in caught if isinstance(c.message, RWarning)]
    assert warning.text == f"running command '{curl_command('m', 'nope://hooks/fake')}' had status 3"
    assert (
        capsys.readouterr().err == "Mattermost notification failed: URLError: <urlopen error unknown url type: nope>\n"
    )


@pytest.mark.parametrize(
    ("exc", "status"),
    [
        (urllib.error.URLError(ConnectionRefusedError(111, "Connection refused")), 7),
        (ConnectionRefusedError(111, "Connection refused"), 7),
        (urllib.error.URLError(socket.gaierror(-2, "Name or service not known")), 6),
        (urllib.error.URLError(TimeoutError("timed out")), 28),
        (TimeoutError("timed out"), 28),
        (urllib.error.URLError(ssl.SSLError(1, "certificate verify failed")), 35),
        (ValueError("unknown url type: 'nope'"), 3),
        (urllib.error.URLError("no host given"), 3),
        (urllib.error.URLError("unknown url type: nope"), 3),
        ("URLError: <urlopen error [Errno 111] Connection refused>", 7),
        ("URLError: <urlopen error [Errno -2] Name or service not known>", 6),
        ("URLError: <urlopen error timed out>", 28),
        ("TimeoutError: timed out", 28),
        ("URLError: <urlopen error [SSL: CERTIFICATE_VERIFY_FAILED] certificate verify failed>", 35),
        ("ValueError: unknown url type: 'nope'", 3),
        ("something else entirely", 7),
    ],
)
def test_curl_exit_status_classes(exc: BaseException | str, status: int) -> None:
    assert curl_exit_status(exc) == status


# --------------------------------------------------------------------------- the evaluateRuns step


def _guard(name: str) -> Callable[..., None]:
    def fail(*args: object, **kwargs: object) -> None:
        raise AssertionError(f"{name} must not be called")

    return fail


def test_send_notification_without_token_does_nothing(monkeypatch: pytest.MonkeyPatch) -> None:
    """remind-evaluate-notoken / magpie-evaluate-notoken: no curl call, no listing, no sanity checks."""
    monkeypatch.setattr(notify, "loop_runs", _guard("loop_runs"))
    monkeypatch.setattr(notify, "get_sanity_checks", _guard("get_sanity_checks"))
    eff = FakeEffects()
    for model in ("REMIND", "MAgPIE"):
        assert (
            send_notification(
                model,
                None,
                today="d",
                summary="s",
                testthat_result="t",
                git_info=["g"],
                runs_started=["a-AMT"],
                effects=eff,
            )
            is None
        )
    assert eff.posts == []


def test_send_notification_remind_captures_then_posts(monkeypatch: pytest.MonkeyPatch) -> None:
    order: list[str] = []
    loop_calls: list[dict[str, object]] = []
    sanity_calls: list[dict[str, object]] = []
    fake_loop = _fake_loop_runs("", "No runs found", loop_calls)
    fake_sanity = _fake_sanity("Results from /p/x/ \n", sanity_calls)

    def loop_then(*args: object, **kwargs: object) -> str | None:
        order.append("rs2")
        return fake_loop(*args, **kwargs)

    def sanity_then(*args: object, **kwargs: object) -> None:
        order.append("rss")
        fake_sanity(*args, **kwargs)

    monkeypatch.setattr(notify, "loop_runs", loop_then)
    monkeypatch.setattr(notify, "get_sanity_checks", sanity_then)
    eff = FakeEffects()
    message = send_notification(
        "REMIND",
        TOKEN,
        today="2026-09-30",
        summary="Summary: No runs started",
        testthat_result="Could not check for the results of `make test-full`, test-full.log not found",
        git_info=GIT_INFO,
        runs_started=[],
        runs_not_started=["default-AMT"],
        effects=eff,
    )
    assert order == ["rs2", "rss"]
    assert loop_calls[0]["mydir"] == [] and loop_calls[0]["effects"] is eff
    assert sanity_calls[0]["effects"] is eff
    assert message == remind_message(
        today="2026-09-30",
        summary="Summary: No runs started",
        testthat_result="Could not check for the results of `make test-full`, test-full.log not found",
        git_info=GIT_INFO,
        rs2=['[1] "No runs found"'],
        rss=["Results from /p/x/ "],
        runs_not_started=["default-AMT"],
    )
    assert eff.posts == [(TOKEN, payload_for(message))]
    assert payload_text(eff.posts[0][1]) == message


def test_send_notification_magpie_posts_one_line(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(notify, "loop_runs", _guard("loop_runs"))
    monkeypatch.setattr(notify, "get_sanity_checks", _guard("get_sanity_checks"))
    eff = FakeEffects()
    common = dict(today="d", summary="s", testthat_result=None, git_info=["g"], runs_started=["default_x"], effects=eff)
    assert send_notification("MAgPIE", TOKEN, error_list=["Some run(s) did not converge"], **common) == MAGPIE_WARNINGS
    assert send_notification("MAgPIE", TOKEN, error_list=None, **common) == MAGPIE_SUCCESS
    assert eff.posts == [(TOKEN, MAGPIE_WARNINGS_PAYLOAD), (TOKEN, payload_for(MAGPIE_SUCCESS))]


def test_send_notification_other_model_composes_nothing(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(notify, "loop_runs", _guard("loop_runs"))
    monkeypatch.setattr(notify, "get_sanity_checks", _guard("get_sanity_checks"))
    eff = FakeEffects()
    assert (
        send_notification(
            "OTHER", TOKEN, today="d", summary="s", testthat_result="t", git_info=[], runs_started=[], effects=eff
        )
        is None
    )
    assert eff.posts == []


def test_send_notification_remind_requires_the_testthat_line(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(notify, "loop_runs", _fake_loop_runs("", "No runs found", []))
    monkeypatch.setattr(notify, "get_sanity_checks", _fake_sanity("", []))
    with pytest.raises(ValueError, match="testthatResult"):
        send_notification(
            "REMIND",
            TOKEN,
            today="d",
            summary="s",
            testthat_result=None,
            git_info=[],
            runs_started=[],
            effects=FakeEffects(),
        )


def test_send_notification_survives_a_failing_transport(
    monkeypatch: pytest.MonkeyPatch, capsys: pytest.CaptureFixture[str]
) -> None:
    """magpie-evaluate-curlfail: the message is composed and the step returns it; the caller saves lastcommit.rds."""
    eff = RaisingEffects(urllib.error.URLError(ConnectionRefusedError(111, "Connection refused")))
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        message = send_notification(
            "MAgPIE",
            TOKEN,
            today="d",
            summary="s",
            testthat_result=None,
            git_info=[],
            runs_started=["default_x"],
            error_list=["e"],
            effects=eff,
        )
    assert message == MAGPIE_WARNINGS
    assert eff.posts == [(TOKEN, MAGPIE_WARNINGS_PAYLOAD)]
    [warning] = [c.message for c in caught if isinstance(c.message, RWarning)]
    assert warning.text == f"running command '{curl_command(MAGPIE_WARNINGS, TOKEN)}' had status 7"
    assert capsys.readouterr().err.startswith("Mattermost notification failed: ")


# --------------------------------------------------------------------------- the recorded payloads


def _golden_cases() -> list[str]:
    if not GOLDENS_AMT.is_dir():
        return []
    return sorted(p.parent.name for p in GOLDENS_AMT.glob("*/mattermost.json"))


GOLDEN_CASES = _golden_cases()
NAMED_CASES = ["remind-evaluate", "remind-evaluate-email", "magpie-evaluate-recent", "magpie-evaluate-old"]
needs_goldens = pytest.mark.skipif(not GOLDENS_AMT.is_dir(), reason="migration/goldens/amt is not present (gitignored)")


def _golden_payload_text(case: str) -> str:
    record = json.loads((GOLDENS_AMT / case / "mattermost.json").read_text(encoding="utf-8"))
    assert record["url"] == TOKEN
    return payload_text(record["payload"])


def _readme_lines(case: str) -> list[str]:
    lines = (GOLDENS_AMT / case / "README.md").read_text(encoding="utf-8").split("\n")
    assert lines[0] == "```" and lines[-2:] == ["```", ""], "README.md is the fenced block written line by line"
    return lines[1:-2]


def _expected_message(case: str, golden_text: str) -> str:
    """Rebuild the message from README.md, result.json and the parts that exist only in the payload."""
    readme = _readme_lines(case)
    result = json.loads((GOLDENS_AMT / case / "result.json").read_text(encoding="utf-8"))
    assert result["status"] == "ok"
    summary = readme[-1]
    assert summary.startswith("Summary: ")
    if result["model"] == "MAgPIE":
        return magpie_message(None if summary == "Summary: AMT runs look good." else [summary])
    today = result["frozen"][:10]
    email_line = next(i for i, line in enumerate(readme) if line.startswith("If you are currently viewing the email"))
    titles = next(i for i, line in enumerate(readme) if line.startswith("Run  "))
    git_info = readme[email_line + 1 : titles]
    runs_not_started: list[str] | None = None
    if NOT_STARTED_HEADER in readme:
        start = readme.index(NOT_STARTED_HEADER)
        runs_not_started = readme[start + 1 : readme.index(" ", start)]
    # only in the payload: the testthat line and the two captured blocks
    payload_lines = golden_text.split("\n")
    fences = [i for i, line in enumerate(payload_lines) if line == "```"]
    assert len(fences) == 6, case
    return remind_message(
        today=today,
        summary=summary,
        testthat_result=payload_lines[2],
        git_info=git_info,
        rs2=payload_lines[fences[2] + 1 : fences[3]],
        rss=payload_lines[fences[4] + 1 : fences[5]],
        runs_not_started=runs_not_started,
    )


@needs_goldens
def test_named_notification_cases_recorded_a_payload() -> None:
    assert set(NAMED_CASES) <= set(GOLDEN_CASES)
    for case in ("remind-evaluate-notoken", "magpie-evaluate-notoken"):
        assert (GOLDENS_AMT / case).is_dir() and not (GOLDENS_AMT / case / "mattermost.json").exists()


@pytest.mark.parametrize("case", GOLDEN_CASES or [pytest.param("none", marks=needs_goldens)])
def test_message_text_equals_the_recorded_payload(case: str) -> None:
    golden_text = _golden_payload_text(case)
    assert _expected_message(case, golden_text) == golden_text
    assert payload_text(payload_for(golden_text)) == golden_text


@needs_goldens
@pytest.mark.parametrize("case", ["remind-evaluate-curlfail", "magpie-evaluate-curlfail"])
def test_curlfail_cases_sent_the_same_text_and_finished(case: str) -> None:
    base = case.removesuffix("-curlfail") if case.startswith("remind") else "magpie-evaluate-recent"
    assert _golden_payload_text(case) == _golden_payload_text(base)
    result = json.loads((GOLDENS_AMT / case / "result.json").read_text(encoding="utf-8"))
    assert result["status"] == "ok"
    assert (GOLDENS_AMT / case / "testsstatus").read_text(encoding="utf-8") == "next:start\n"
