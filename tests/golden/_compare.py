"""Comparators of the golden tier (plan 03 section 4.6).

Parity is exact: JSON goldens are compared by key order, JSON type and value
(a real NA is ``null`` on both sides, the string ``"NA"`` is a string, ints are
ints and never equal to a float of the same value); text goldens byte for byte;
``rs`` cases by stdout bytes, exit status and stderr after dropping exactly one
line containing ``Did you know?`` on each side (D-08). An approved deviation
puts a Python expectation under ``migration/goldens/expected-py/<same layout>``,
which replaces the R golden for that one stream of that one case; nothing else
is normalised. Every mismatch is an ``AssertionError`` with the location of the
first difference; nothing here skips.
"""

from __future__ import annotations

import json
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


def compare_rs(case_id: str, r_dir: Path, py_dir: Path, expected_py_dir: Path | None = None) -> None:
    """One ``rs`` case: argv lines, stdout bytes, exit status and stderr (hint line dropped on each side).

    When ``expected_py_dir/<id>.<ext>`` exists for ``out``, ``err`` or ``status`` that file replaces the R golden for
    that stream only (an approved deviation of plan 3.5); the other streams of the case still compare with R.
    """
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
        if ext == "status":
            r_status, py_status = r_file.read_text().strip(), py_file.read_text().strip()
            if r_status != py_status:
                raise AssertionError(f"{label}: exit status differs: R {r_status} vs Python {py_status}")
        elif ext == "err":
            compare_bytes(drop_hint_line(r_file.read_bytes()), drop_hint_line(py_file.read_bytes()), label=label)
        else:
            compare_bytes(r_file, py_file, label=label)
