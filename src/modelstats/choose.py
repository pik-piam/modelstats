"""``choose_from_list``: the ``rs -p`` menu, a port of ``gms::chooseFromList(theList, type = "folders")``.

Only the configuration the command line interface uses is ported: ``multiple = TRUE``,
``addAllPattern = TRUE``, an unnamed list (no groups), ``returnBoolean = FALSE``. The menu
text, the input syntax and the three ``Try again`` messages follow the gms source (gms
0.3x, ``chooseFromList`` and ``choosePatternFromList``) byte for byte for every input the
goldens exercise; R's own messages are reproduced as far as the small expression grammar
allows (see :func:`_r_eval_c`).

Input syntax (after R's ``gsub`` clean-up: ``-`` becomes ``:``, spaces vanish, ``,,``
collapses to ``,``): comma separated numbers and ranges (``2,4:6``, descending ``6:4``), the
letters ``a`` (all), ``p`` (regular expression on the next line) and ``f`` (fixed pattern on
the next line), which R evaluates as the local variables ``a = 1``, ``p = n - 1`` and
``f = n``; anything else re-prompts.

Everything R prints with ``message()`` goes to the ``stdout`` stream parameter; R writes
it to stderr, and the ``rs`` goldens show it there, so the command line interface passes
``sys.stderr``. One line is read per prompt (D-21). On an exhausted stdin the main prompt
behaves like R's ``character(0)`` input (empty selection); the pattern prompt raises
:class:`EOFError` (R re-prompts forever there) and the confirmation prompt raises
``RParityError("argument is of length zero")`` exactly like R's ``if (!character(0) %in% ...)``.
"""

from __future__ import annotations

import math
import re
from dataclasses import dataclass
from typing import TYPE_CHECKING

from modelstats.errors import RParityError

if TYPE_CHECKING:
    from collections.abc import Iterable, Sequence
    from typing import TextIO

__all__ = ["choose_from_list"]

ALL_ENTRY = "all"
PATTERN_ENTRY = "Search pattern by regular expression..."
FIXED_ENTRY = "Search by fixed pattern..."
PROMPT = "\nNumbers entered as 2,4:6,9 or leave empty:"
SELECTED_LIMIT = 666  # getOption("chooseFromListLimit", 666)
ALLOWED_INPUT = re.compile(r"[afp0-9,: -]*")
RANGE_CAP = 1000  # longer ranges can never fit the list; only their first 240 pasted characters matter
R_XLEN_T_MAX = 4503599627370496  # 2^52: beyond it R's `:` refuses to build the vector

EVAL_CALL = 'eval(parse(text = paste("c(", userinput, ")")))'
PARSE_CALL = 'parse(text = paste("c(", userinput, ")"))'


# --- public entry point --------------------------------------------------------------------------


def choose_from_list(
    items: Iterable[str],
    *,
    type: str = "folders",
    stdin: TextIO,
    stdout: TextIO,
) -> list[str]:
    """Let the user pick entries of ``items``; returns them in selection order.

    ``stdout`` receives every line R's ``message()`` prints (the menu, the ``Try again``
    texts, the pattern dialogue and ``Selected: ...``); ``stdin`` is read one line per
    prompt.
    """
    original = list(items)
    write = stdout.write
    if not original:
        write(f"No {type} found that might be selected, returning the empty list.\n")
        return []
    the_list = [ALL_ENTRY, *original, PATTERN_ENTRY, FIXED_ENTRY]
    n = len(the_list)
    env = {"a": 1.0, "p": float(n - 1), "f": float(n)}
    menu = _menu(the_list)
    errormessage = ""
    while True:
        write(f"\n\nPlease choose {type}:\n\n{menu}\n{errormessage}{PROMPT}\n")
        line = _get_line(stdin)
        userinput = "" if line is None else line
        userinput = userinput.replace("-", ":").replace(" ", "").replace(",,", ",")
        try:
            values, truncated = _r_eval_c(userinput, env)
            condition = ""
            failed = False
        except _REvalError as exc:
            condition = exc.text
            failed = True
            values, truncated = [], False
        if ALLOWED_INPUT.fullmatch(userinput) is None or failed:
            errormessage = f"Try again, you have to choose some numbers. {condition}\n"
            continue
        if truncated or not all(_in_list(v, n) for v in values):
            pasted = ", ".join(_r_num_str(v) for v in values)[:240]
            errormessage = f"Try again, not all in list: {pasted}...\n"
            continue
        break
    identifier = [int(v) for v in values]
    if 1 in identifier:
        selected = list(range(1, len(original) + 1))
    else:
        ids = _unique(identifier)
        if n - 1 in ids:
            matches = _choose_pattern(original, type, fixed=False, stdin=stdin, stdout=stdout)
            ids = _unique([*ids, *(i + 1 for i in matches)])
        if n in ids:
            matches = _choose_pattern(original, type, fixed=True, stdin=stdin, stdout=stdout)
            ids = _unique([*ids, *(i + 1 for i in matches)])
        selected = [i - 1 for i in ids if i < len(original) + 2]
    chosen = [original[i - 1] for i in selected]
    stopafter = len(chosen)
    total = 0
    for k, entry in enumerate(chosen, 1):
        total += len(entry)
        if total > SELECTED_LIMIT:
            stopafter = k
            break
    tail = ", ..." if stopafter < len(chosen) else ""
    write(f"Selected: {', '.join(chosen[:stopafter])}{tail}\n")
    return chosen


def _menu(the_list: Sequence[str]) -> str:
    n = len(the_list)
    width = len(str(n))
    suffixes = [",a", *([""] * (n - 3)), ",p", ",f"]
    rows = zip(suffixes, the_list, strict=True)
    return "\n".join(f"{str(i).rjust(width)}{suffix}: {entry}" for i, (suffix, entry) in enumerate(rows, 1))


def _get_line(stdin: TextIO) -> str | None:
    """One input line without its line ending; ``None`` when stdin is exhausted."""
    line = stdin.readline()
    if line == "":
        return None
    return line.removesuffix("\n").removesuffix("\r")


def _in_list(v: float, n: int) -> bool:
    """``v %in% seq_along(theList)`` for one number (NA, Inf and fractions are never in the list)."""
    return math.isfinite(v) and v == int(v) and 1 <= v <= n


def _unique(values: Iterable[int]) -> list[int]:
    seen: set[int] = set()
    out: list[int] = []
    for value in values:
        if value not in seen:
            seen.add(value)
            out.append(value)
    return out


# --- choosePatternFromList -----------------------------------------------------------------------


def _choose_pattern(items: Sequence[str], type: str, *, fixed: bool, stdin: TextIO, stdout: TextIO) -> list[int]:
    """``gms:::choosePatternFromList``: ask for a pattern, show the matches, ask to confirm.

    Returns the 1-based positions of the matching items. R's ``grep`` uses TRE regular
    expressions; the port uses :mod:`re`, which agrees for the patterns a run name needs.
    """
    write = stdout.write
    while True:
        write("\nInsert the " + ("search pattern with fixed=TRUE: " if fixed else "regular expression: ") + "\n")
        pattern = _get_line(stdin)
        if pattern is None:
            raise EOFError("stdin exhausted while answering the search pattern prompt of chooseFromList")
        try:
            if fixed:
                ids = [i for i, item in enumerate(items, 1) if pattern in item]
            else:
                regex = re.compile(pattern)
                ids = [i for i, item in enumerate(items, 1) if regex.search(item)]
        except re.error as exc:
            write(
                "Error in grep(pattern = pattern, theList, fixed = fixed) : \n"
                f"  invalid regular expression '{pattern}', reason '{exc.msg}'\n"
            )
            write("\n\nMatching created an error. Try again!\n")
            continue
        if ids:
            write(f"\n\nThe search pattern matches the following {type}:\n")
            write("\n".join(f"{k}: {items[i - 1]}" for k, i in enumerate(ids, 1)) + "\n")
        else:
            write("Oops. You didn't select anything.\n")
        write(f"\nAre you sure these are the right {type}? (y/n): \n")
        answer = _get_line(stdin)
        if answer is None:
            # if (!getLine() %in% c("y", "Y")) on character(0)
            raise RParityError("argument is of length zero")
        if answer in ("y", "Y"):
            return ids


# --- the R expression `c( <userinput> )` -----------------------------------------------------------


class _REvalError(Exception):
    """An R parse or evaluation error; ``text`` is ``as.character(condition)`` (ends with a newline)."""

    def __init__(self, text: str) -> None:
        super().__init__(text)
        self.text = text


@dataclass(frozen=True)
class _Token:
    kind: str  # "num", "sym", ",", ":", "::", ":::", "other"
    text: str
    start: int

    @property
    def end(self) -> int:
        return self.start + len(self.text)


_NUMBER = re.compile(r"(?:0[xX][0-9a-fA-F]+|[0-9]+\.?[0-9]*(?:[eE][+-]?[0-9]+)?|\.[0-9]+(?:[eE][+-]?[0-9]+)?)[Li]?")
_SYMBOL = re.compile(r"[A-Za-z.][A-Za-z0-9._]*")
_R_CONSTANTS: dict[str, float] = {
    "T": 1.0,
    "TRUE": 1.0,
    "F": 0.0,
    "FALSE": 0.0,
    "pi": math.pi,
    "NA": math.nan,
    "NaN": math.nan,
    "Inf": math.inf,
    "c": math.nan,  # the function c(): no error by itself, NA/NaN argument inside a range
}


def _tokenize(text: str) -> list[_Token]:
    tokens: list[_Token] = []
    i = 0
    while i < len(text):
        if text.startswith(":::", i):
            tokens.append(_Token(":::", ":::", i))
        elif text.startswith("::", i):
            tokens.append(_Token("::", "::", i))
        elif text[i] in ":,":
            tokens.append(_Token(text[i], text[i], i))
        elif (m := _NUMBER.match(text, i)) is not None:
            tokens.append(_Token("num", m.group(0), i))
        elif (m := _SYMBOL.match(text, i)) is not None:
            tokens.append(_Token("sym", m.group(0), i))
        else:
            tokens.append(_Token("other", text[i], i))
        i = tokens[-1].end
    return tokens


def _parse_error(text: str, col: int, what: str, source: str) -> _REvalError:
    """R's parse error text: ``<text>:1:COL: unexpected WHAT`` plus the source line and caret."""
    caret = " " * (3 + col) + "^"
    return _REvalError(f"Error in {PARSE_CALL}: <text>:1:{col}: unexpected {what}\n1: c( {source}\n{caret}\n")


def _unexpected(text: str, token: _Token) -> _REvalError:
    what = {"num": "numeric constant", "sym": "symbol"}.get(token.kind, f"'{token.text}'")
    return _parse_error(text, 4 + token.start, what, text[: token.end])


def _parse_args(text: str) -> list[list[_Token] | None]:
    """Split ``c( text )`` into arguments: a token chain per argument, ``None`` for an empty one."""
    tokens = _tokenize(text)
    args: list[list[_Token] | None] = []
    chain: list[_Token] = []
    state = "start"  # start | atom | colon | ns
    for token in tokens:
        if state in ("start", "colon", "ns"):
            if token.kind in ("num", "sym"):
                if state == "ns" and token.kind == "num":
                    raise _unexpected(text, token)
                chain.append(token)
                state = "atom"
            elif token.kind == "," and state == "start":
                args.append(None)
            else:
                raise _unexpected(text, token)
        else:  # after an atom
            if token.kind == ",":
                args.append(chain)
                chain = []
                state = "start"
            elif token.kind == ":":
                chain.append(token)
                state = "colon"
            elif token.kind in ("::", ":::"):
                if chain[-1].kind != "sym":
                    raise _unexpected(text, token)
                chain.append(token)
                state = "ns"
            else:
                raise _unexpected(text, token)
    if state in ("colon", "ns"):
        raise _parse_error(text, len(text) + 5, "')'", f"{text} )")
    if state == "atom":
        args.append(chain)
    elif args:  # a comma right before the closing parenthesis: a trailing empty argument
        args.append(None)
    return args


def _deparse_chain(chain: list[_Token]) -> str:
    out = ""
    for token in chain:
        out += _r_num_str(_number(token.text)) if token.kind == "num" else token.text
    return out


def _number(text: str) -> float:
    body = text.rstrip("Li")
    if body[:2].lower() == "0x":
        return float(int(body, 16))
    return float(body)


def _atom_value(token: _Token, env: dict[str, float]) -> list[float]:
    if token.kind == "num":
        return [_number(token.text)]
    if token.text in env:
        return [env[token.text]]
    if token.text == "NULL":
        return []
    if token.text in _R_CONSTANTS:
        return [_R_CONSTANTS[token.text]]
    raise _REvalError(f"Error in {EVAL_CALL}: object '{token.text}' not found\n")


def _eval_chain(chain: list[_Token], env: dict[str, float]) -> tuple[list[float], bool]:
    """Evaluate ``atom(:atom)*`` like R's ``:`` (first element of each operand, descending allowed)."""
    ns_pos = next((i for i, token in enumerate(chain) if token.kind in ("::", ":::")), None)
    if ns_pos is not None:
        # sym::sym is loadNamespace(sym), which fails; the atoms before it are evaluated first
        for token in chain[: ns_pos - 1]:
            if token.kind in ("num", "sym"):
                _atom_value(token, env)
        package = chain[ns_pos - 1].text
        raise _REvalError(f"Error in loadNamespace(x): there is no package called ‘{package}’\n")
    atoms = [token for token in chain if token.kind != ":"]
    value = _atom_value(atoms[0], env)
    truncated = False
    for k, atom in enumerate(atoms[1:], 1):
        right = _atom_value(atom, env)
        call = _deparse_chain(chain[: 2 * k + 1])
        if not value or not right:
            raise _REvalError(f"Error in {call}: argument of length 0\n")
        start, stop = value[0], right[0]
        if math.isnan(start) or math.isnan(stop):
            raise _REvalError(f"Error in {call}: NA/NaN argument\n")
        length = math.floor(abs(stop - start) + 1e-10) + 1
        if length >= R_XLEN_T_MAX:
            raise _REvalError(f"Error in {call}: result would be too long a vector\n")
        step = 1.0 if stop >= start else -1.0
        count = min(length, RANGE_CAP)
        value = [start + step * j for j in range(count)]
        truncated = truncated or length > RANGE_CAP
    return value, truncated


def _r_eval_c(text: str, env: dict[str, float]) -> tuple[list[float], bool]:
    """``eval(parse(text = paste("c(", text, ")")))`` for the chooser's grammar.

    Returns the numeric vector and whether a range was cut at :data:`RANGE_CAP` (such a
    vector can never fit the list). Raises :class:`_REvalError` with R's error text for
    parse errors, empty arguments, unknown symbols and namespace lookups.
    """
    args = _parse_args(text)
    values: list[float] = []
    truncated = False
    for k, chain in enumerate(args, 1):
        if chain is None:
            call = "c(" + ", ".join("" if c is None else _deparse_chain(c) for c in args) + ")"
            raise _REvalError(f"Error in {call}: argument {k} is empty\n")
        part, cut = _eval_chain(chain, env)
        values.extend(part)
        truncated = truncated or cut
    return values, truncated


def _r_num_str(v: float) -> str:
    """``as.character()`` / ``paste()`` of an R number (15 significant digits, fixed or scientific)."""
    if math.isnan(v):
        return "NA"
    if math.isinf(v):
        return "Inf" if v > 0 else "-Inf"
    if v == int(v) and abs(v) < 1e15:
        fixed = str(int(v))
    else:
        fixed = f"{v:.15g}" if "e" not in f"{v:.15g}" else ""
    mantissa, exponent = f"{v:.14e}".split("e")
    mantissa = mantissa.rstrip("0").rstrip(".")
    exp = int(exponent)
    sci = f"{mantissa}e{'+' if exp >= 0 else '-'}{abs(exp):02d}"
    if fixed and len(fixed) <= len(sci):
        return fixed
    return sci
