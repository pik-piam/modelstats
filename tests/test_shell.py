from __future__ import annotations

import json
import subprocess
import sys
from datetime import date
from pathlib import Path
from typing import Any

import pytest

from modelstats import _shell

pytestmark = pytest.mark.real_shell


def test_run_captures_text_output() -> None:
    completed = _shell.run([sys.executable, "-c", "print('hi')"])
    assert completed.returncode == 0
    assert completed.stdout == "hi\n"


def test_run_missing_executable_is_status_127() -> None:
    completed = _shell.run(["/nonexistent/program-xyz"])
    assert completed.returncode == 127
    with pytest.raises(subprocess.CalledProcessError):
        _shell.run(["/nonexistent/program-xyz"], check=True)


def test_run_missing_cwd_raises(tmp_path: Path) -> None:
    with pytest.raises(FileNotFoundError):
        _shell.run([sys.executable, "-c", "pass"], cwd=tmp_path / "missing")


def test_run_replaces_undecodable_output() -> None:
    code = "import sys; sys.stdout.buffer.write(b'caf\\xe9 ok\\n')"
    assert _shell.lines([sys.executable, "-c", code]) == ["caf\ufffd ok"]


def test_run_passes_cwd_and_env(tmp_path: Path) -> None:
    code = "import os; print(os.getcwd()); print(os.environ.get('X'))"
    completed = _shell.run([sys.executable, "-c", code], cwd=tmp_path, env={"X": "v", "PATH": ""})
    assert completed.stdout.splitlines() == [str(tmp_path), "v"]


def test_lines_returns_stdout_lines_or_empty_on_failure() -> None:
    assert _shell.lines([sys.executable, "-c", "print('a'); print('b')"]) == ["a", "b"]
    assert _shell.lines([sys.executable, "-c", "import sys; print('x'); sys.exit(1)"]) == []
    assert _shell.lines(["/nonexistent/program-xyz"]) == []


def test_squeue_and_sacct_argument_lists(monkeypatch: pytest.MonkeyPatch) -> None:
    seen: list[list[str]] = []
    monkeypatch.setattr(_shell, "lines", lambda argv, cwd=None: seen.append(list(argv)) or ["line"])
    assert _shell.squeue("%Z") == ["line"]
    assert _shell.squeue("%j", user="alice,bob") == ["line"]
    assert _shell.sacct("alice", date(2026, 10, 15), "WorkDir") == ["line"]
    assert _shell.squeue_argv("%Z") == seen[0]
    assert _shell.sacct_argv("alice", date(2026, 10, 15), "WorkDir") == seen[2]
    assert seen == [
        ["squeue", "-h", "-o", "%Z"],
        ["squeue", "-h", "-o", "%j", "-u", "alice,bob"],
        [
            "sacct",
            "-u",
            "alice",
            "-s",
            "cd,f,cancelled,timeout,oom",
            "-S",
            "2026-10-15",
            "-E",
            "now",
            "-P",
            "-n",
            "--format",
            "WorkDir",
        ],
    ]


def test_lines_splits_on_newlines_only(monkeypatch: pytest.MonkeyPatch) -> None:
    def fake_run(argv: list[str], **kwargs: Any) -> subprocess.CompletedProcess[str]:
        return subprocess.CompletedProcess(argv, 0, "a\fb\x85c\rd\n\n", "")

    monkeypatch.setattr(_shell, "run", fake_run)
    assert _shell.lines(["x"]) == ["a\fb\x85c\rd", ""]  # system(intern = TRUE) splits on "\n" only


def test_lines_of_empty_output(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(_shell, "run", lambda argv, **kw: subprocess.CompletedProcess(argv, 0, "", ""))
    assert _shell.lines(["x"]) == []  # character(0)


def test_post_json_rejects_a_non_url(caplog: pytest.LogCaptureFixture) -> None:
    with caplog.at_level("WARNING", logger="modelstats._shell"):
        _shell.post_json("hooks/secret", {"text": "hello"})
    assert "not a URL" in caplog.text and "secret" not in caplog.text


def test_post_json_sends_one_json_post(monkeypatch: pytest.MonkeyPatch) -> None:
    captured: dict[str, Any] = {}

    class FakeResponse:
        def __enter__(self) -> FakeResponse:
            return self

        def __exit__(self, *args: object) -> None:
            return None

    def fake_urlopen(request: Any, timeout: float) -> FakeResponse:
        captured["url"] = request.full_url
        captured["body"] = json.loads(request.data)
        captured["type"] = request.get_header("Content-type")
        captured["method"] = request.get_method()
        return FakeResponse()

    monkeypatch.setattr(_shell.urllib.request, "urlopen", fake_urlopen)
    _shell.post_json("https://chat.example/hooks/x", {"text": "hello"})
    assert captured == {
        "url": "https://chat.example/hooks/x",
        "body": {"text": "hello"},
        "type": "application/json",
        "method": "POST",
    }


def test_post_json_failure_is_logged_not_raised(
    monkeypatch: pytest.MonkeyPatch, caplog: pytest.LogCaptureFixture
) -> None:
    def failing_urlopen(request: Any, timeout: float) -> Any:
        raise OSError("connection refused")

    monkeypatch.setattr(_shell.urllib.request, "urlopen", failing_urlopen)
    with caplog.at_level("WARNING", logger="modelstats._shell"):
        _shell.post_json("https://chat.example/hooks/secret", {"text": "hello"})
    assert "connection refused" in caplog.text
    assert "secret" not in caplog.text


def test_post_json_hides_the_url_of_an_invalid_url_error(
    monkeypatch: pytest.MonkeyPatch, caplog: pytest.LogCaptureFixture
) -> None:
    import http.client

    def failing_urlopen(request: Any, timeout: float) -> Any:
        raise http.client.InvalidURL("URL can't contain control characters. '/hooks/sec ret'")

    monkeypatch.setattr(_shell.urllib.request, "urlopen", failing_urlopen)
    with caplog.at_level("WARNING", logger="modelstats._shell"):
        _shell.post_json("https://chat.example/hooks/sec ret", {"text": "hello"})
    assert "not a URL" in caplog.text and "sec ret" not in caplog.text


def test_rscript_argv() -> None:
    assert _shell.rscript_argv("cat(1)", "a", "b c") == ["Rscript", "-e", "cat(1)", "a", "b c"]


def test_on_cluster_checks_for_p(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(_shell.Path, "exists", lambda self: str(self) == "/p")
    assert _shell.on_cluster()
    monkeypatch.setattr(_shell.Path, "exists", lambda self: False)
    assert not _shell.on_cluster()


def test_current_user_comes_from_the_password_database(monkeypatch: pytest.MonkeyPatch) -> None:
    import os
    import pwd

    monkeypatch.setenv("USER", "somebody-else")
    monkeypatch.setenv("LOGNAME", "somebody-else")
    assert _shell.current_user() == pwd.getpwuid(os.getuid()).pw_name
    assert _shell.current_user() != "somebody-else"


def test_cluster_constants() -> None:
    assert str(_shell.AMT_OUTPUT) == "/p/projects/remind/modeltests/remind/output"
    assert str(_shell.AMT_RUNCODE) == "/p/projects/remind/modeltests/remind/runcode.rds"
    assert str(_shell.REMIND_RESULTS_ARCHIVE) == "/p/projects/rd3mod/models/results/remind"
    assert _shell.MAGPIE_RESULTS_ARCHIVE_ENV == "MAGPIE_RESULTS_ARCHIVE_PATH"
