"""Shared test helpers: synthetic run folders, GDX fixtures, captured consoles, a fixed clock.

Tests never touch the cluster, the network or R: ``_shell`` is monkeypatched, GDX files are
written with ``gams.transfer`` and the R data files come from ``tests/data/`` (made by
``tests/data/make_data.R``).
"""

from __future__ import annotations

import errno
import io
import shutil
import subprocess
import sys
from collections import deque
from collections.abc import Callable, Iterator, Mapping, Sequence
from dataclasses import dataclass, field
from datetime import date, datetime
from pathlib import Path
from typing import Any
from zoneinfo import ZoneInfo

import pytest
from rich.console import Console

from modelstats import _shell, _timefmt

DATA = Path(__file__).parent / "data"
BERLIN = ZoneInfo("Europe/Berlin")
FIXED_NOW = datetime(2026, 10, 18, 12, 0, 0, tzinfo=BERLIN)


@pytest.fixture
def data_dir() -> Path:
    return DATA


def make_run(
    root: Path,
    name: str,
    files: Mapping[str, str | bytes] | None = None,
    *,
    config: str | None = None,
    stats: str | None = None,
) -> Path:
    """Create ``root/name`` with the given files.

    ``config="remind"`` copies ``config_remind.Rdata`` as ``config.Rdata``, ``config="magpie"``
    copies ``config_magpie.yml`` as ``config.yml``; ``stats="remind"``/``"magpie"`` copy the matching
    ``runstatistics.rda``. Text files get a trailing newline when they do not end with one.
    """
    run_dir = root / name
    run_dir.mkdir(parents=True, exist_ok=True)
    if config == "remind":
        shutil.copy(DATA / "config_remind.Rdata", run_dir / "config.Rdata")
    elif config == "magpie":
        shutil.copy(DATA / "config_magpie.yml", run_dir / "config.yml")
    if stats is not None:
        shutil.copy(DATA / f"runstatistics_{stats}.rda", run_dir / "runstatistics.rda")
    for relative, content in (files or {}).items():
        target = run_dir / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        if isinstance(content, bytes):
            target.write_bytes(content)
        else:
            text = content if content.endswith("\n") or not content else content + "\n"
            target.write_text(text, encoding="utf-8")
    return run_dir


def make_main_folder(
    root: Path, names: Sequence[str], files: Mapping[str, str | bytes] | None = None
) -> Path:
    """A REMIND main folder (``output``, ``output.R``, ``start.R``, ``main.gms``) with runs in ``output/``."""
    (root / "output").mkdir(parents=True, exist_ok=True)
    for name in ("output.R", "start.R", "main.gms"):
        (root / name).write_text("")
    for name in names:
        make_run(root / "output", name, files)
    return root


def _gdx_na() -> float:
    import gams.transfer as gt

    return float(gt.SpecialValues.NA)


GDX_NA = _gdx_na()  # the GDX special value NA (a NaN)


def write_gdx(
    path: Path,
    scalars: Mapping[str, float] | None = None,
    parameters: Mapping[str, tuple[list[str], list[tuple[Any, ...]]]] | None = None,
    sets: Mapping[str, list[str]] | None = None,
) -> Path:
    """Write a GDX file: scalars by name, sets by name, parameters as (domain names, records)."""
    import gams.transfer as gt
    import gamspy_base
    import pandas as pd

    container = gt.Container(system_directory=gamspy_base.directory)
    set_objects = {name: gt.Set(container, name, records=elements) for name, elements in (sets or {}).items()}
    for name, value in (scalars or {}).items():
        gt.Parameter(container, name, records=value)
    for name, (domains, records) in (parameters or {}).items():
        domain = [set_objects.get(d, d) for d in domains]
        frame = pd.DataFrame(records, columns=[*domains, "value"])
        gt.Parameter(container, name, domain, records=frame)
    container.write(str(path))
    return path


@dataclass
class CapturedConsole:
    """A rich console writing into a buffer, and the text written so far."""

    console: Console
    buffer: io.StringIO

    @property
    def text(self) -> str:
        return self.buffer.getvalue()

    @property
    def lines(self) -> list[str]:
        return self.text.splitlines()


def capture_console(*, color: bool = False, width: int = 400) -> CapturedConsole:
    buffer = io.StringIO()
    console = Console(
        file=buffer,
        width=width,
        color_system="256" if color else None,  # not TERM's: the tests pin the 256-colour codes
        no_color=not color,
        force_terminal=color,
        highlight=False,
        soft_wrap=True,
        legacy_windows=False,
    )
    return CapturedConsole(console, buffer)


@pytest.fixture
def out() -> CapturedConsole:
    return capture_console()


@pytest.fixture
def err(monkeypatch: pytest.MonkeyPatch) -> CapturedConsole:
    """The stderr console of the package, captured."""
    from modelstats import _console

    captured = capture_console()
    monkeypatch.setattr(_console, "err", captured.console)
    return captured


@dataclass(frozen=True)
class Call:
    """One recorded call of a ``_shell`` function."""

    argv: tuple[str, ...]
    cwd: Path | None = None
    env: Mapping[str, str] | None = None
    input: str | None = None
    check: bool = False


@dataclass
class FakeShell:
    """Replacement answers for the ``_shell`` functions, and a record of what was called.

    ``squeue_answers`` are consumed one per call before ``squeue_lines`` is used; ``outputs`` and
    ``returncodes`` are keyed by the argument tuple of ``lines``/``run``; ``run_answers`` (consumed
    one per call before ``outputs``; an item is the lines, or a tuple of exit status and lines) and
    ``errors`` (stderr text) apply to ``run`` only. A missing ``cwd`` raises like ``subprocess``,
    except under ``/p/`` (cluster paths that only exist there).
    """

    squeue_lines: list[str] = field(default_factory=list)
    squeue_answers: deque[list[str]] = field(default_factory=deque)
    sacct_lines: dict[str, list[str]] = field(default_factory=dict)
    outputs: dict[tuple[str, ...], list[str]] = field(default_factory=dict)
    run_answers: dict[tuple[str, ...], deque[list[str] | tuple[int, list[str]]]] = field(default_factory=dict)
    errors: dict[tuple[str, ...], str] = field(default_factory=dict)
    returncodes: dict[tuple[str, ...], int] = field(default_factory=dict)
    calls: list[Call] = field(default_factory=list)
    posts: list[tuple[str, dict[str, Any]]] = field(default_factory=list)
    sleeps: list[float] = field(default_factory=list)
    user: str = "alice"
    cluster: bool = True

    @property
    def argvs(self) -> list[list[str]]:
        return [list(call.argv) for call in self.calls]

    def squeue(self, fmt: str, *, user: str | None = None) -> list[str]:
        self.calls.append(Call(tuple(_shell.squeue_argv(fmt, user=user))))
        return list(self.squeue_answers.popleft()) if self.squeue_answers else list(self.squeue_lines)

    def sacct(self, user: str, since: date, fmt: str) -> list[str]:
        self.calls.append(Call(tuple(_shell.sacct_argv(user, since, fmt))))
        return list(self.sacct_lines.get(fmt, []))

    def lines(self, argv: Sequence[str], *, cwd: Path | None = None) -> list[str]:
        key = tuple(argv)
        self.calls.append(Call(key, cwd=cwd))
        self._check_cwd(cwd)
        if self.returncodes.get(key, 0):
            return []
        return list(self.outputs.get(key, []))

    @staticmethod
    def _check_cwd(cwd: Path | None) -> None:
        """A missing working directory raises as ``subprocess`` does; cluster paths (``/p/...``) pass."""
        if cwd is not None and not Path(cwd).is_dir() and not str(cwd).startswith("/p/"):
            raise FileNotFoundError(errno.ENOENT, "No such file or directory", str(cwd))

    def run(
        self,
        argv: Sequence[str],
        *,
        cwd: Path | None = None,
        check: bool = False,
        input: str | None = None,
        env: Mapping[str, str] | None = None,
    ) -> subprocess.CompletedProcess[str]:
        key = tuple(argv)
        self.calls.append(Call(key, cwd=cwd, env=env, input=input, check=check))
        self._check_cwd(cwd)
        code = self.returncodes.get(key, 0)
        answers = self.run_answers.get(key)
        answer = answers.popleft() if answers else self.outputs.get(key, [])
        if isinstance(answer, tuple):  # (exit status, lines)
            code, output = answer
        else:
            output = answer
        stdout = "".join(f"{line}\n" for line in output)
        stderr = self.errors.get(key, "")
        if check and code != 0:
            raise subprocess.CalledProcessError(code, list(argv), stdout, stderr)
        return subprocess.CompletedProcess(list(argv), code, stdout, stderr)

    def sleep(self, seconds: float) -> None:
        self.sleeps.append(seconds)

    def post_json(self, url: str, payload: Mapping[str, Any]) -> None:
        self.posts.append((url, dict(payload)))


@pytest.fixture(autouse=True)
def _no_real_programs(request: pytest.FixtureRequest, monkeypatch: pytest.MonkeyPatch) -> None:
    """Tests never run programs, call the cluster or the network unless marked ``real_shell``."""
    if request.node.get_closest_marker("real_shell"):
        return

    def refuse(*args: object, **kwargs: object) -> None:
        raise RuntimeError(
            f"a test tried to run a program through _shell: {args} {kwargs}; use the shell fixture"
        )

    for name in ("run", "lines", "squeue", "sacct", "post_json", "sleep"):
        monkeypatch.setattr(_shell, name, refuse)
    monkeypatch.setattr(_shell, "on_cluster", lambda: False)
    monkeypatch.setattr(_shell, "current_user", lambda: "alice")


@pytest.fixture(autouse=True)
def _restore_stream_errors() -> Iterator[None]:
    """``tolerate_undecodable_names()`` reconfigures the process streams; put them back after each test."""
    before = [
        (stream, stream.errors) for stream in (sys.stdout, sys.stderr) if isinstance(stream, io.TextIOWrapper)
    ]
    yield
    for stream, errors in before:
        stream.reconfigure(errors=errors)


@pytest.fixture
def shell(monkeypatch: pytest.MonkeyPatch) -> FakeShell:
    fake = FakeShell()
    monkeypatch.setattr(_shell, "squeue", fake.squeue)
    monkeypatch.setattr(_shell, "sacct", fake.sacct)
    monkeypatch.setattr(_shell, "lines", fake.lines)
    monkeypatch.setattr(_shell, "run", fake.run)
    monkeypatch.setattr(_shell, "sleep", fake.sleep)
    monkeypatch.setattr(_shell, "post_json", fake.post_json)
    monkeypatch.setattr(_shell, "on_cluster", lambda: fake.cluster)
    monkeypatch.setattr(_shell, "current_user", lambda: fake.user)
    return fake


@pytest.fixture
def clock(monkeypatch: pytest.MonkeyPatch) -> Callable[[datetime], None]:
    """Freeze ``_timefmt.now``/``today``; call the returned function to move the clock."""

    def set_time(moment: datetime) -> None:
        monkeypatch.setattr(_timefmt, "now", lambda: moment)
        monkeypatch.setattr(_timefmt, "today", lambda: moment.date())

    set_time(FIXED_NOW)
    return set_time
