"""Run-folder discovery and ordering helpers of ``R/commandLineInterface.R``.

Each function mirrors one block of the R command line interface (section 3.4 of
``migration/03-migration-plan.md``):

* :func:`is_run_folder` / :func:`is_main_folder`: the marker-file tests ``is.runfolder``
  and ``is.mainfolder`` (BUG-027: a run that failed before ``full.gms`` existed is not a
  run folder);
* :func:`expand_paths`: the positional ``[PATH]`` argument, comma separated, ``.`` default;
* :func:`filter_coupled`: the ``-m`` block (BUG-026: ``magpie/output`` relative to the
  current working directory is appended and removed again by the basename filter);
* :func:`last_iterations`: the ``-l`` block (BUG-025: the entry at the largest list position
  per prefix, in listing order, not the highest iteration number);
* :func:`natural_order` / :func:`natural_order_indices`: ``stringi::stri_order(numeric =
  TRUE)`` as the goldens pin it (D-14, see the note on the collation below).

Collation (D-14). The R goldens were produced in the sandbox with ``LC_ALL=C.utf8``, where
stringi collates with ICU's ``en_US_POSIX`` tailoring: ASCII characters in code-point order
at the primary level (digits, then ``A-Z``, then ``_``, then ``a-z``; ``-`` < ``.`` < ``_``),
embedded numbers compared numerically, canonically decomposable letters (``Ä``, ``é``) by
their base letter first and their accent second, and letters that only have a root-table
expansion (``ß``, ``ﬁ``) after the ASCII block. natsort (``ns.DEFAULT``: NFD normalisation
and integer splitting, no locale, no case folding) reproduces all of that except the accent
and expansion rules, which :func:`_text_key` adds as a deterministic two-level key. No
environment-dependent backend (PyICU, ``locale``) is consulted.
"""

from __future__ import annotations

import os
import re
import unicodedata
from typing import TYPE_CHECKING

from natsort import natsort_keygen, ns

if TYPE_CHECKING:
    from collections.abc import Iterable, Sequence

    from modelstats.env import Effects

__all__ = [
    "expand_paths",
    "filter_coupled",
    "is_main_folder",
    "is_run_folder",
    "last_iterations",
    "natural_order",
    "natural_order_indices",
    "r_basename",
    "split_comma",
]

RUN_MARKERS_REMIND = ("full.gms", "log.txt", "config.Rdata", "prepare_and_run.R", "prepareAndRun.R")
RUN_MARKERS_MAGPIE = ("full.gms", "submit.R", "config.yml", "magpie_y1995.gdx")
MAIN_MARKERS = ("output", "output.R", "start.R", "main.gms")

COUPLED_BASENAME_RE = re.compile(r"(^C_)|(-(rem|mag)-[0-9]+$)")
ITERATION_SUFFIX_RE = re.compile(r"-(rem|mag)-[0-9]+$")


def _effects(effects: Effects | None) -> Effects:
    if effects is not None:
        return effects
    from modelstats.env import default_effects

    return default_effects()


# --- R string helpers ----------------------------------------------------------------------------


def r_basename(path: str) -> str:
    """R's ``basename()``: tilde expansion, trailing slashes dropped, the last component.

    ``basename("a/b/")`` is ``"b"``, ``basename("/")`` and ``basename("")`` are ``""``.
    """
    expanded = os.path.expanduser(path) if path.startswith("~") else path
    stripped = expanded.rstrip("/")
    return stripped.rsplit("/", 1)[-1]


def split_comma(value: str) -> list[str]:
    """``strsplit(value, ",")[[1]]``: a trailing empty piece is dropped, others are kept.

    ``""`` gives ``[]``, ``"a,"`` gives ``["a"]``, ``",a"`` gives ``["", "a"]`` and
    ``"a,,b"`` gives ``["a", "", "b"]``.
    """
    pieces = value.split(",")
    if pieces and pieces[-1] == "":
        pieces.pop()
    return pieces


# --- folder classification -----------------------------------------------------------------------


def is_run_folder(dir: str, effects: Effects | None = None) -> bool:
    """``is.runfolder``: at least 4 of the 5 REMIND markers, or all 4 MAgPIE markers, exist."""
    eff = _effects(effects)
    remind = sum(1 for name in RUN_MARKERS_REMIND if eff.exists(f"{dir}/{name}"))
    if remind >= 4:
        return True
    magpie = sum(1 for name in RUN_MARKERS_MAGPIE if eff.exists(f"{dir}/{name}"))
    return magpie == 4


def is_main_folder(dir: str, effects: Effects | None = None) -> bool:
    """``is.mainfolder``: ``output``, ``output.R``, ``start.R`` and ``main.gms`` all exist."""
    eff = _effects(effects)
    return sum(1 for name in MAIN_MARKERS if eff.exists(f"{dir}/{name}")) == 4


def expand_paths(paths_arg: str | None) -> list[str]:
    """The ``[PATH]`` positional: ``"."`` when absent, split at commas like ``strsplit``.

    ``None`` (no positional argument) gives ``["."]``; an empty string gives ``[]`` (R's
    ``strsplit("", ",")[[1]]`` is ``character(0)``).
    """
    if paths_arg is None:
        return ["."]
    return split_comma(paths_arg)


# --- the -m and -l blocks ------------------------------------------------------------------------


def filter_coupled(runfolders: Sequence[str], cwd: str, effects: Effects | None = None) -> list[str]:
    """The ``-m`` block: keep coupling iterations by their basename.

    R first appends the relative path ``magpie/output`` when that directory exists below
    ``cwd`` and then keeps the entries whose basename matches ``(^C_)|(-(rem|mag)-[0-9]+$)``,
    which removes ``magpie/output`` again (BUG-026 parity). R runs the block only when
    ``runfolders`` is not NULL; the caller keeps that distinction (a NULL list skips the
    block, an empty character vector runs it).
    """
    eff = _effects(effects)
    folders = list(runfolders)
    if eff.is_dir(os.path.join(cwd, "magpie", "output")):
        folders.append("magpie/output")
    return [folder for folder in folders if COUPLED_BASENAME_RE.search(r_basename(folder))]


def last_iterations(runfolders: Sequence[str]) -> list[str]:
    """The ``-l`` block: per prefix (the path without ``-rem-N`` / ``-mag-N``) keep the entry at
    the largest list position, prefixes in order of first appearance (BUG-025 parity: with
    ``x-rem-1, x-rem-10, x-rem-2`` in listing order ``x-rem-2`` survives).

    R's ``lastdirs`` stays NULL for an empty input, which makes the CLI print
    ``No coupled runs found``; here that is the empty list.
    """
    prefixes = [ITERATION_SUFFIX_RE.sub("", folder) for folder in runfolders]
    out: list[str] = []
    for prefix in _unique(prefixes):
        last = max(i for i, candidate in enumerate(prefixes) if candidate == prefix)
        out.append(runfolders[last])
    return out


def _unique(values: Iterable[str]) -> list[str]:
    """R's ``unique()``: first occurrences in order."""
    seen: set[str] = set()
    out: list[str] = []
    for value in values:
        if value not in seen:
            seen.add(value)
            out.append(value)
    return out


# --- natural order (stri_order(numeric = TRUE) as the goldens pin it) ----------------------------

_NATSORT_KEY = natsort_keygen(alg=ns.DEFAULT)

_Primary = tuple[tuple[int, int | str], ...]
_Secondary = tuple[str, ...]


def _text_key(chunk: str) -> tuple[_Primary, _Secondary]:
    """Two-level key of a (NFD-normalised) text chunk, see the module docstring.

    Primary weights: ``(0, code point)`` for ASCII, ``(1, NFKD form)`` for other base
    characters (so ``ﬁ`` and ``ß`` land after the ASCII block, ``ﬁ`` before ``ß``).
    Secondary weights: the combining marks that follow each base character (``""`` for none),
    so ``e`` < ``é`` only when the primaries tie.
    """
    primary: list[tuple[int, int | str]] = []
    secondary: list[str] = []
    for ch in chunk:
        if unicodedata.combining(ch) and secondary:
            secondary[-1] += ch
            continue
        if ord(ch) < 128:
            primary.append((0, ord(ch)))
        else:
            primary.append((1, unicodedata.normalize("NFKD", ch)))
        secondary.append("")
    return tuple(primary), tuple(secondary)


def _natural_key(name: str) -> tuple[tuple[object, ...], tuple[_Secondary, ...]]:
    parts = _NATSORT_KEY(name)
    primary = tuple(_text_key(part)[0] if isinstance(part, str) else part for part in parts)
    secondary = tuple(_text_key(part)[1] for part in parts if isinstance(part, str))
    return primary, secondary


def natural_order_indices(names: Sequence[str]) -> list[int]:
    """``stringi::stri_order(names, numeric = TRUE)``: 1-based positions, ties in input order."""
    order = sorted(range(len(names)), key=lambda i: _natural_key(names[i]))
    return [i + 1 for i in order]


def natural_order(names: Sequence[str]) -> list[str]:
    """``names[stringi::stri_order(names, numeric = TRUE)]``."""
    return [names[i - 1] for i in natural_order_indices(names)]
