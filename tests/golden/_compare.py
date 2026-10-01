"""Comparators of the golden tier (plan 03 section 4.6).

Parity is exact: JSON goldens are compared by key order, JSON type and value
(a real NA is ``null`` on both sides, the string ``"NA"`` is a string, ints are
ints and never equal to a float of the same value); text goldens byte for byte;
``rs`` cases by stdout bytes, exit status and stderr after dropping exactly one
line containing ``Did you know?`` on each side (D-08); ``amt`` cases semantically
(:func:`compare_amt`, plan 4.5). An approved deviation
puts a Python expectation under ``migration/goldens/expected-py/<same layout>``,
which replaces the R golden for that one stream of that one case; nothing else
is normalised. Every mismatch is an ``AssertionError`` with the location of the
first difference; nothing here skips.
"""

from __future__ import annotations

import json
from collections import Counter
from pathlib import Path
from typing import Any

HINT_MARKER = b"Did you know?"


# ---------------------------------------------------------------------------
# JSON
# ---------------------------------------------------------------------------


def load_json(path: Path) -> Any:
    """The parsed golden; a missing or unparsable file is an assertion failure."""
    if not path.is_file():
        raise AssertionError(f"missing golden file {path}")
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeDecodeError, ValueError) as exc:
        raise AssertionError(f"{path}: not readable as JSON: {exc}") from None


def json_type(value: object) -> str:
    """The JSON type of a parsed value (``bool`` before ``int``: Python's bool is an int)."""
    if value is None:
        return "null"
    if isinstance(value, bool):
        return "boolean"
    if isinstance(value, int):
        return "integer"
    if isinstance(value, float):
        return "number"
    if isinstance(value, str):
        return "string"
    if isinstance(value, list):
        return "array"
    if isinstance(value, dict):
        return "object"
    raise AssertionError(f"not a JSON value: {value!r}")


def _as_json(value: object) -> Any:
    return load_json(value) if isinstance(value, Path) else value


def compare_json(r: object, py: object, *, label: str = "") -> None:
    """Assert that two parsed JSON values (or files) are equal in keys and key order, JSON types and values."""
    _compare_json(_as_json(r), _as_json(py), label or "$")


def _compare_json(r: Any, py: Any, where: str) -> None:
    r_type, py_type = json_type(r), json_type(py)
    if r_type != py_type:
        raise AssertionError(f"{where}: JSON type differs: R {r_type} {r!r} vs Python {py_type} {py!r}")
    if r_type == "object":
        r_keys, py_keys = list(r), list(py)
        if r_keys != py_keys:
            raise AssertionError(f"{where}: keys differ (order matters): R {r_keys} vs Python {py_keys}")
        for key in r_keys:
            _compare_json(r[key], py[key], f"{where}.{key}")
    elif r_type == "array":
        if len(r) != len(py):
            raise AssertionError(f"{where}: array length differs: R {len(r)} vs Python {len(py)}")
        for index, (a, b) in enumerate(zip(r, py, strict=True)):
            _compare_json(a, b, f"{where}[{index}]")
    elif r != py:
        raise AssertionError(f"{where}: value differs: R {r!r} vs Python {py!r}")


# ---------------------------------------------------------------------------
# bytes
# ---------------------------------------------------------------------------


def _as_bytes(value: bytes | Path) -> bytes:
    if isinstance(value, Path):
        if not value.is_file():
            raise AssertionError(f"missing golden file {value}")
        return value.read_bytes()
    return value


def _line_at(data: bytes, offset: int) -> bytes:
    """The line of ``data`` that contains ``offset`` (without its newline)."""
    if offset >= len(data):
        return b"<end of data>"
    start = data.rfind(b"\n", 0, offset) + 1
    end = data.find(b"\n", offset)
    return data[start : end if end >= 0 else len(data)]


def compare_bytes(r: bytes | Path, py: bytes | Path, *, label: str = "") -> None:
    """Assert byte equality; the message shows the first differing offset and both lines there with ``repr``."""
    r_bytes, py_bytes = _as_bytes(r), _as_bytes(py)
    if r_bytes == py_bytes:
        return
    common = min(len(r_bytes), len(py_bytes))
    offset = next((i for i in range(common) if r_bytes[i] != py_bytes[i]), common)
    line_no = r_bytes.count(b"\n", 0, offset) + 1
    raise AssertionError(
        f"{label or 'bytes'}: first difference at offset {offset} (line {line_no}; "
        f"R {len(r_bytes)} bytes, Python {len(py_bytes)} bytes)\n"
        f"  R:      {_line_at(r_bytes, offset)!r}\n"
        f"  Python: {_line_at(py_bytes, offset)!r}"
    )


# ---------------------------------------------------------------------------
# per-family case comparisons
# ---------------------------------------------------------------------------


def _same_presence(r_file: Path, py_file: Path, what: str) -> None:
    if r_file.exists() and not py_file.exists():
        raise AssertionError(f"{what}: R wrote {r_file.name} but the Python generator did not ({py_file})")
    if py_file.exists() and not r_file.exists():
        raise AssertionError(f"{what}: the Python generator wrote {py_file.name} but R did not ({r_file})")


def _has_override(expected_py_dir: Path | None, names: tuple[str, ...]) -> bool:
    return expected_py_dir is not None and any((expected_py_dir / name).is_file() for name in names)


def compare_json_case(case_id: str, r_dir: Path, py_dir: Path, expected_py_dir: Path | None = None) -> None:
    """One JSON-family case (``status``, ``runtype``, ``slurm``, ``sort``): ``<id>.json`` on both sides.

    ``expected_py_dir/<id>.json``, when it exists, is the reference instead of the R golden (an approved deviation
    of plan 3.5, listed in ``migration/06-port-findings.md``).
    """
    label = f"{case_id}.json"
    if _has_override(expected_py_dir, (label,)):
        assert expected_py_dir is not None
        r_dir, label = expected_py_dir, f"{label} (expected-py)"
    r_file, py_file = r_dir / f"{case_id}.json", py_dir / f"{case_id}.json"
    if not r_file.is_file():
        raise AssertionError(f"{case_id}: no R golden {r_file}")
    if not py_file.is_file():
        raise AssertionError(f"{case_id}: the Python generator wrote no {py_file}")
    compare_json(r_file, py_file, label=label)


def compare_out_case(case_id: str, r_dir: Path, py_dir: Path, expected_py_dir: Path | None = None) -> None:
    """One byte-family case (``printoutput``, ``looprun``, ``sanity``): ``<id>.out`` and/or ``<id>.error.json``.

    The presence of each file must agree (an error golden on one side only is a failure), the ``.out`` bytes must
    be equal (for ``looprun`` a partial output beside an error golden included) and the error messages equal.
    When ``expected_py_dir`` holds ``<id>.out`` or ``<id>.error.json`` the whole case is compared against that
    directory instead of the R golden (presence included: an expected-py case without ``.error.json`` says that
    the Python side must not write one, which a per-file override could not express).
    """
    if _has_override(expected_py_dir, (f"{case_id}.out", f"{case_id}.error.json")):
        assert expected_py_dir is not None
        r_dir = expected_py_dir
    r_out, py_out = r_dir / f"{case_id}.out", py_dir / f"{case_id}.out"
    r_err, py_err = r_dir / f"{case_id}.error.json", py_dir / f"{case_id}.error.json"
    if not r_out.exists() and not r_err.exists():
        raise AssertionError(f"{case_id}: no R golden ({r_out} or {r_err})")
    _same_presence(r_out, py_out, f"{case_id} output")
    _same_presence(r_err, py_err, f"{case_id} error golden")
    if r_err.exists():
        compare_json(r_err, py_err, label=f"{case_id}.error.json")
    if r_out.exists():
        compare_bytes(r_out, py_out, label=f"{case_id}.out")


def drop_hint_line(data: bytes) -> bytes:
    """``data`` without the first line containing ``Did you know?`` (exactly one line; unchanged when absent)."""
    lines = data.splitlines(keepends=True)
    for index, line in enumerate(lines):
        if HINT_MARKER in line:
            del lines[index]
            break
    return b"".join(lines)


#: The ``rs`` options of plan 03 section 3.4 plus the help alias: every pair must appear in ``rs -h`` (D-11).
HELP_OPTIONS: tuple[tuple[str, str], ...] = (
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
HELP_ARGV = ({"-h"}, {"--help"})


def is_help_case(argv_file: Path) -> bool:
    """Whether the case's argv is exactly ``-h`` or ``--help`` (the only cases whose stdout is a help page)."""
    lines = argv_file.read_text(encoding="utf-8").splitlines()
    return set(lines) in HELP_ARGV and len(lines) == 1


def assert_help_lists_options(data: bytes, *, label: str = "help") -> None:
    """D-11: the help page is not compared with optparse's layout, but it must name every option of 3.4."""
    text = data.decode("utf-8", "replace")
    missing = [f"{short}, {long}" for short, long in HELP_OPTIONS if short not in text or long not in text]
    if missing:
        raise AssertionError(f"{label}: the help output does not list {missing}:\n{text}")


def compare_rs(case_id: str, r_dir: Path, py_dir: Path, expected_py_dir: Path | None = None) -> None:
    """One ``rs`` case: argv lines, stdout bytes, the ``.status`` file and stderr (hint line dropped on each side).

    Every stream but stderr is compared byte for byte, the exit status included (both generators write
    ``<digits>\\n``); nothing is stripped.

    When ``expected_py_dir/<id>.<ext>`` exists for ``out``, ``err`` or ``status`` that file replaces the R golden for
    that stream only (an approved deviation of plan 3.5); the other streams of the case still compare with R.
    A help case (argv exactly ``-h`` or ``--help``) must in addition list every option of plan 03 section 3.4
    (D-11: the layout is typer's, pinned by the expected-py ``.out`` file, the option list is asserted).
    """
    if is_help_case(r_dir / f"{case_id}.argv"):
        py_out = py_dir / f"{case_id}.out"
        if not py_out.is_file():
            raise AssertionError(f"{case_id}: the Python generator wrote no {py_out}")
        assert_help_lists_options(py_out.read_bytes(), label=f"{case_id}.out")
    for ext in ("argv", "out", "status", "err"):
        r_file = r_dir / f"{case_id}.{ext}"
        py_file = py_dir / f"{case_id}.{ext}"
        if expected_py_dir is not None and ext != "argv":
            override = expected_py_dir / f"{case_id}.{ext}"
            if override.is_file():
                r_file = override
        if not r_file.is_file():
            raise AssertionError(f"{case_id}: no R golden {r_file}")
        if not py_file.is_file():
            raise AssertionError(f"{case_id}: the Python generator wrote no {py_file}")
        label = f"{case_id}.{ext}" + (" (expected-py)" if r_file.parent != r_dir else "")
        if ext == "err":
            compare_bytes(drop_hint_line(r_file.read_bytes()), drop_hint_line(py_file.read_bytes()), label=label)
        else:
            compare_bytes(r_file, py_file, label=label)


# ---------------------------------------------------------------------------
# amt: the semantic comparison of plan 03 section 4.5
# ---------------------------------------------------------------------------

#: The traced tools whose (tool, argv, cwd) multiset must agree. ``curl`` is excluded: the port sends the
#: notification through ``Effects.post_json`` and the payload is compared through ``mattermost.json`` instead.
#: ``sed`` is excluded: the port edits ``config/default.cfg`` in place of ``sed -i`` (same bytes, asserted through
#: ``effects.json``). The port's ``Rscript`` bridges are not traced on either side (the fake Rscript intercepts
#: ``start.R`` only; R ran that code in-process), so they never appear here.
AMT_TRACE_TOOLS: frozenset[str] = frozenset({"git", "sbatch", "rsync", "mv", "make", "Rscript", "squeue", "sacct"})
#: The amt artefacts an ``expected-py/amt/<case>/<name>`` file may replace (whole file, one artefact each).
AMT_OVERRIDABLE: tuple[str, ...] = (
    "result.json",
    "README.md",
    "testsstatus",
    "stdout.txt",
    "mattermost.json",
    "trace.jsonl",
    "effects.json",
    "unchanged.json",
    "data-changelog.csv",
)


def _payload_text(payload: str) -> str:
    """The message inside a Mattermost payload (the port's ``notify.payload_text``: JSON or R's raw frame)."""
    from modelstats.amt.notify import payload_text

    return payload_text(payload)


def mattermost_records(path: Path) -> list[tuple[str | None, str]]:
    """The ``(url, message text)`` of every POST recorded in a ``mattermost.json`` (JSON lines; ``[]`` when absent)."""
    if not path.is_file():
        return []
    records: list[tuple[str | None, str]] = []
    for line in path.read_text(encoding="utf-8").splitlines():
        if not line.strip():
            continue
        record = json.loads(line)
        payload = record.get("payload")
        if not isinstance(payload, str):
            raise AssertionError(f"{path}: a record without a payload string: {line[:200]!r}")
        records.append((record.get("url"), _payload_text(payload)))
    return records


def trace_multiset(path: Path) -> Counter[tuple[str, tuple[str, ...], str]]:
    """The ``(tool, argv, cwd)`` multiset of the traced calls of :data:`AMT_TRACE_TOOLS` (``trace.jsonl``)."""
    entries: Counter[tuple[str, tuple[str, ...], str]] = Counter()
    if not path.is_file():
        return entries
    for line in path.read_text(encoding="utf-8").splitlines():
        if not line.strip():
            continue
        record = json.loads(line)
        tool = str(record.get("tool"))
        if tool not in AMT_TRACE_TOOLS:
            continue
        entries[(tool, tuple(str(a) for a in record["argv"]), str(record.get("cwd")))] += 1
    return entries


def _format_counter(counter: Counter[tuple[str, tuple[str, ...], str]]) -> str:
    return "\n".join(f"    {n} x {tool} {list(argv)} (cwd {cwd})" for (tool, argv, cwd), n in sorted(counter.items()))


def compare_trace(r_file: Path, py_file: Path, *, label: str) -> None:
    """Assert equal multisets of traced ``(tool, argv, cwd)`` for the tools of :data:`AMT_TRACE_TOOLS`."""
    r_calls, py_calls = trace_multiset(r_file), trace_multiset(py_file)
    if r_calls == py_calls:
        return
    only_r = r_calls - py_calls
    only_py = py_calls - r_calls
    raise AssertionError(
        f"{label}: the traced commands differ\n  only in R ({sum(only_r.values())}):\n{_format_counter(only_r)}\n"
        f"  only in Python ({sum(only_py.values())}):\n{_format_counter(only_py)}"
    )


def _without_sha256(value: Any) -> Any:
    """A state record without its top-level ``sha256`` (RDS bytes differ between the two writers by design)."""
    if isinstance(value, dict):
        return {key: item for key, item in value.items() if key != "sha256"}
    return value


def compare_state_files(r_dir: Path, py_dir: Path, *, label: str) -> None:
    """``state/<file>.json`` by value: the same set of files, each equal in keys, types and values except ``sha256``.

    Both sides serialise through the same R code (``dump_state.R`` is ``make_goldens.R``'s ``value_of_state``), so
    an RDS file written by the port compares as R reads it: class, row names, column order, types and values.
    """
    r_names = sorted(p.name for p in r_dir.glob("*.json"))
    py_names = sorted(p.name for p in py_dir.glob("*.json")) if py_dir.is_dir() else []
    if r_names != py_names:
        raise AssertionError(f"{label}: state files differ: R {r_names} vs Python {py_names}")
    for name in r_names:
        compare_json(
            _without_sha256(load_json(r_dir / name)), _without_sha256(load_json(py_dir / name)), label=f"{label}/{name}"
        )


def _is_rds(path: str) -> bool:
    return path.lower().endswith(".rds")


def compare_effects(r_file: Path, py_file: Path, *, label: str) -> None:
    """``effects.json`` (the clone diff): equal path sets per category and equal sha256 for every non-RDS file.

    The one documented equivalence: an RDS state file that R rewrote with identical bytes is ``touched`` on the
    R side and ``modified`` on the Python side (the two writers' bytes differ by design, the value is compared
    through ``state/``), so RDS paths are compared over ``modified`` and ``touched`` together. ``status`` must agree.
    """
    r_diff, py_diff = load_json(r_file), load_json(py_file)
    if r_diff.get("status") != py_diff.get("status"):
        raise AssertionError(f"{label}: status differs: R {r_diff.get('status')!r} vs Python {py_diff.get('status')!r}")
    if r_diff.get("fixture_root") != py_diff.get("fixture_root"):
        raise AssertionError(f"{label}: fixture_root differs")

    def paths(diff: Any, category: str) -> set[str]:
        return {str(entry["path"]) for entry in diff.get(category, [])}

    for category in ("created", "deleted"):
        if paths(r_diff, category) != paths(py_diff, category):
            raise AssertionError(
                f"{label}: {category} paths differ: R {sorted(paths(r_diff, category))} vs "
                f"Python {sorted(paths(py_diff, category))}"
            )
    for category in ("modified", "touched"):
        r_paths = {p for p in paths(r_diff, category) if not _is_rds(p)}
        py_paths = {p for p in paths(py_diff, category) if not _is_rds(p)}
        if r_paths != py_paths:
            raise AssertionError(f"{label}: {category} paths differ: R {sorted(r_paths)} vs Python {sorted(py_paths)}")
    r_rds = {p for c in ("modified", "touched") for p in paths(r_diff, c) if _is_rds(p)}
    py_rds = {p for c in ("modified", "touched") for p in paths(py_diff, c) if _is_rds(p)}
    if r_rds != py_rds:
        raise AssertionError(f"{label}: rewritten RDS paths differ: R {sorted(r_rds)} vs Python {sorted(py_rds)}")

    def hashes(diff: Any) -> dict[str, str | None]:
        return {
            str(entry["path"]): entry.get("sha256")
            for category in ("created", "modified", "touched")
            for entry in diff.get(category, [])
            if entry.get("kind") == "file" and not _is_rds(str(entry["path"]))
        }

    r_hashes, py_hashes = hashes(r_diff), hashes(py_diff)
    for path in sorted(r_hashes):
        if r_hashes[path] != py_hashes.get(path):
            raise AssertionError(
                f"{label}: sha256 of {path} differs: R {r_hashes[path]} vs Python {py_hashes.get(path)}"
            )


def compare_unchanged(r_file: Path, py_file: Path, *, label: str) -> None:
    """``unchanged.json``: the same watched files, the same starting hashes and the same ``same`` flags."""
    r_rec, py_rec = load_json(r_file), load_json(py_file)
    if list(r_rec) != list(py_rec):
        raise AssertionError(f"{label}: watched files differ: R {list(r_rec)} vs Python {list(py_rec)}")
    for name, entry in r_rec.items():
        if entry.get("before") != py_rec[name].get("before"):
            raise AssertionError(
                f"{label}: {name} started from different content: R {entry.get('before')} vs "
                f"Python {py_rec[name].get('before')} (prepare scripts diverged?)"
            )
        if entry.get("same") != py_rec[name].get("same"):
            raise AssertionError(
                f"{label}: {name} 'same' differs: R {entry.get('same')} vs Python {py_rec[name].get('same')}"
            )


def compare_amt(case_id: str, r_root: Path, py_root: Path, expected_py_root: Path | None = None) -> None:
    """One ``amt`` case directory: the semantic comparison of plan 03 section 4.5.

    Compared, in this order: ``result.json`` (status and R's error message, plus the case fields); ``README.md``
    (presence and bytes); ``testsstatus`` (presence and bytes); ``stdout.txt`` (bytes: the ``cs2com`` line and the
    ``print(oldRuns)`` block live there); ``state/*.json`` by value (``sha256`` excluded, see
    :func:`compare_state_files`); ``mattermost.json`` (the url and message text of every POST, in order; the
    Python payload is ``json.dumps``, R's a string concatenation, BUG-015 / D-15, so the TEXT is compared);
    ``trace.jsonl`` as the multiset of :data:`AMT_TRACE_TOOLS` calls (curl -> ``post_json``, sed -> file edit:
    the two documented exclusions); ``effects.json`` (see :func:`compare_effects`); ``unchanged.json``;
    ``data-changelog.csv`` (presence and bytes). ``stderr.txt`` is not compared (plan 4.5: R messages; the two
    documented textual differences are the changelog bridge's ``readRDS(report)`` call and notify's failure line).
    No temporary path is normalised: every compared text names paths below the fixture root only.

    ``expected_py_root/<case>/<artefact>`` replaces the R file for that one artefact (plan 3.5; listed in
    ``migration/06-port-findings.md``).
    """
    r_dir, py_dir = r_root / case_id, py_root / case_id
    expected_dir = None if expected_py_root is None else expected_py_root / case_id
    if not (r_dir / "result.json").is_file():
        raise AssertionError(f"{case_id}: no R golden {r_dir / 'result.json'}")
    if not (py_dir / "result.json").is_file():
        raise AssertionError(f"{case_id}: the Python generator wrote no {py_dir / 'result.json'}")

    def reference(name: str) -> tuple[Path, str]:
        if expected_dir is not None and name in AMT_OVERRIDABLE and (expected_dir / name).is_file():
            return expected_dir / name, f"{case_id}/{name} (expected-py)"
        return r_dir / name, f"{case_id}/{name}"

    r_result, label = reference("result.json")
    compare_json(r_result, py_dir / "result.json", label=label)
    for name in ("README.md", "testsstatus", "data-changelog.csv"):
        r_file, label = reference(name)
        _same_presence(r_file, py_dir / name, f"{case_id} {name}")
        if r_file.exists():
            compare_bytes(r_file, py_dir / name, label=label)
    r_file, label = reference("stdout.txt")
    compare_bytes(r_file, py_dir / "stdout.txt", label=label)
    compare_state_files(r_dir / "state", py_dir / "state", label=f"{case_id}/state")
    r_file, label = reference("mattermost.json")
    _same_presence(r_file, py_dir / "mattermost.json", f"{case_id} mattermost.json")
    r_posts, py_posts = mattermost_records(r_file), mattermost_records(py_dir / "mattermost.json")
    if len(r_posts) != len(py_posts):
        raise AssertionError(f"{label}: R recorded {len(r_posts)} POST(s), Python {len(py_posts)}")
    for index, ((r_url, r_text), (py_url, py_text)) in enumerate(zip(r_posts, py_posts, strict=True)):
        if r_url != py_url:
            raise AssertionError(f"{label}[{index}]: url differs: R {r_url!r} vs Python {py_url!r}")
        compare_bytes(
            r_text.encode("utf-8", "surrogateescape"),
            py_text.encode("utf-8", "surrogateescape"),
            label=f"{label}[{index}] message text",
        )
    r_file, label = reference("trace.jsonl")
    compare_trace(r_file, py_dir / "trace.jsonl", label=label)
    r_file, label = reference("effects.json")
    compare_effects(r_file, py_dir / "effects.json", label=label)
    r_file, label = reference("unchanged.json")
    compare_unchanged(r_file, py_dir / "unchanged.json", label=label)
