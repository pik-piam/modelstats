"""Mattermost notification of the automated model tests (``R/modeltests.R`` lines 57-62 and 447-486).

R composes one character vector per model (one element per line of the message), joins it
with newlines and hands it to ``.mattermostBotMessage``, which builds the webhook body by
string concatenation::

    curl -i -X POST -H 'Content-Type: application/json' -d '{"text": "<message>"}' <token>

and runs it with ``system(cmd, intern = TRUE)``. Nothing of the message is escaped (BUG-015,
decision D-15 pending): the recorded R payloads contain raw newlines and escape bytes and
are therefore not valid JSON (``migration/goldens/amt/remind-evaluate/mattermost.json`` has
``payload_json_error``; ``remind-evaluate-runcode-nomatch`` even carries the unescaped
``[1] "No runs found"`` that ``capture.output`` printed for ``loopRuns``' return value).
The port keeps the message TEXT byte for byte and sends ``json.dumps({"text": message})``
through :meth:`Effects.post_json`; :func:`payload_text` is the inverse for both writers, so
the goldens are compared on the text (plan 03 section 2.3 and 4.5).

Transport failures follow R: ``system(intern = TRUE)`` on a ``curl`` with a non-zero exit
status warns ``running command '<cmd>' had status <n>`` (call ``system(cmd, intern = TRUE)``)
and ``evaluateRuns`` carries on and saves ``lastcommit.rds`` (case ``remind-evaluate-curlfail``,
result ``ok``). ``curl`` is called without ``--fail``, so an HTTP error response is not a
failure: only an exception from ``post_json`` (no connection, no resolution, timeout, a
malformed URL) becomes the warning, with curl's exit code for that failure class. Without a
token nothing is called (``remind-evaluate-notoken`` has no ``mattermost.json``).

R facts pinned with R 4.6.1 on 2026-10-01 (``migration/_scratch/p5-notify/probe_capture.out``):

- ``utils::capture.output(f())`` returns the printed text split into lines: a trailing
  newline adds no element, an incomplete last line is kept, ``cat("a\\n\\nb\\n")`` gives
  ``"a" "" "b"``, nothing printed gives ``character(0)``, and a *visible* return value is
  printed as well: ``loopRuns(character(0))`` returns ``"No runs found"`` visibly, so the
  captured lines end with ``[1] "No runs found"`` (``cat("x")`` before it would merge into
  ``x[1] "No runs found"``); the normal path ends in a ``for`` loop (invisible ``NULL``).
- ``exists("runsNotStarted")`` is ``TRUE`` only when the REMIND branch assigned it (an empty
  ``character(0)`` counts), so the trailing block is a tri-state: absent, header only, or
  header plus names.
- ``paste0(c("a", "```", character(0), "```", "b"), collapse = "\\n")`` is ``"a\\n```\\n```\\nb"``:
  a zero-length vector contributes no line.
- The deparsed call of the warning is ``system(cmd, intern = TRUE)``; ``curl`` to a closed
  port gives status 7 and prints its own reason on stderr (``curl: (7) Failed to connect``),
  which ``intern = TRUE`` does not capture.
"""

from __future__ import annotations

import io
import json
import socket
import ssl
import sys
import urllib.error
import warnings
from collections.abc import Sequence

from modelstats.env import Effects, default_effects
from modelstats.errors import RWarning
from modelstats.loop_runs import loop_runs
from modelstats.sanity import get_sanity_checks

__all__ = [
    "MAGPIE_SUCCESS",
    "MAGPIE_WARNINGS",
    "NOT_STARTED_HEADER",
    "PAYLOAD_PREFIX",
    "PAYLOAD_SUFFIX",
    "REMIND_INTRO",
    "REMIND_RS_HEADER",
    "REMIND_SANITY_HEADER",
    "SYSTEM_CALL",
    "capture_loop_runs",
    "capture_sanity_checks",
    "curl_command",
    "curl_exit_status",
    "magpie_message",
    "mattermost_bot_message",
    "payload_for",
    "payload_text",
    "r_capture_lines",
    "r_print_character",
    "remind_message",
    "send_notification",
]

#: The deparsed R call that owns the warning of a failing ``curl`` (``.mattermostBotMessage``).
SYSTEM_CALL = "system(cmd, intern = TRUE)"
#: R's payload frame: ``{"text": "`` + message + ``"}`` (lines 58-59).
PAYLOAD_PREFIX = '{"text": "'
PAYLOAD_SUFFIX = '"}'

REMIND_INTRO = (
    "Please find below the status of the REMIND automated model tests (AMT) of {today}. "
    "Runs are here: `/p/projects/remind/modeltests/remind`."
)
REMIND_RS_HEADER = "`rs -A` returns:"
REMIND_SANITY_HEADER = "Sanity checks (`rs -s`) return:"
NOT_STARTED_HEADER = "These scenarios did not start at all:"
FENCE = "```"
# Lines 470-478: inside `if (model == "MAgPIE")` the ifelse(model == "MAgPIE", "landuse", model) is always "landuse".
MAGPIE_WARNINGS = "Some MAgPIE tests produce warnings. Please check https://gitlab.pik-potsdam.de/landuse/testing_suite"
MAGPIE_SUCCESS = (
    "MAgPIE tests completed successfully. Find the results at https://gitlab.pik-potsdam.de/landuse/testing_suite."
)


def _effects(effects: Effects | None) -> Effects:
    return default_effects() if effects is None else effects


# ---------------------------------------------------------------------------
# capture.output emulation
# ---------------------------------------------------------------------------


def r_capture_lines(text: str) -> list[str]:
    """The lines ``utils::capture.output`` returns for printed ``text``.

    A trailing newline closes the last line without adding an element, an incomplete last
    line is kept, empty lines stay, and nothing printed gives ``[]`` (``character(0)``).
    """
    if text == "":
        return []
    lines = text.split("\n")
    if lines[-1] == "":
        lines.pop()
    return lines


def r_print_character(value: str) -> str:
    """What ``print()`` writes for a length-one character vector: ``[1] "<value>"`` and a newline.

    The value is quoted as ``encodeString`` does for the characters that can occur here
    (backslash, double quote, newline, carriage return, tab); the only value that reaches this
    function is ``loopRuns``' visible ``"No runs found"``.
    """
    escaped = (
        value.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t")
    )
    return f'[1] "{escaped}"\n'


def capture_loop_runs(runs_started: Sequence[str], effects: Effects | None = None) -> list[str]:
    """Line 453: ``utils::capture.output(loopRuns(runsStarted, user = NULL, colors = FALSE, sortbytime = FALSE))``.

    The listing is printed into a buffer; a visible return value (``"No runs found"`` for an
    empty ``runsStarted``) is printed after it as ``capture.output`` does. ``user = NULL`` is
    the process user (``effects.user``). The header underline still carries its escape bytes
    when colours are enabled, exactly as crayon does with ``colors = FALSE`` (the goldens were
    recorded with ``R_CLI_NUM_COLORS=256``).
    """
    buffer = io.StringIO()
    value = loop_runs(
        list(runs_started), user=None, colors=False, sortbytime=False, effects=_effects(effects), out=buffer
    )
    text = buffer.getvalue()
    if value is not None:
        text += r_print_character(value)
    return r_capture_lines(text)


def capture_sanity_checks(effects: Effects | None = None) -> list[str]:
    """Line 454: ``utils::capture.output(getSanityChecks())`` (the AMT lookup through ``runcode.rds``)."""
    buffer = io.StringIO()
    get_sanity_checks(None, effects=_effects(effects), out=buffer)
    return r_capture_lines(buffer.getvalue())


# ---------------------------------------------------------------------------
# message composition (lines 451-483)
# ---------------------------------------------------------------------------


def remind_message(
    *,
    today: str,
    summary: str,
    testthat_result: str,
    git_info: Sequence[str],
    rs2: Sequence[str],
    rss: Sequence[str],
    runs_not_started: Sequence[str] | None = None,
) -> str:
    """Lines 455-466 joined with newlines (line 483): the REMIND status message.

    ``git_info`` is the vector written to the README (``Tested commit: ...``, ``The test of
    ... contains these merges:``, the merge lines), ``rs2`` and ``rss`` the captured lines of
    :func:`capture_loop_runs` and :func:`capture_sanity_checks`. ``runs_not_started`` is
    ``None`` when ``runsNotStarted`` was never assigned (no block), otherwise the header line
    is added followed by the names (none for an empty vector), as ``exists("runsNotStarted")``
    decides in R.
    """
    lines: list[str] = [
        REMIND_INTRO.format(today=today),
        summary,
        testthat_result,
        FENCE,
        *git_info,
        FENCE,
        REMIND_RS_HEADER,
        FENCE,
        *rs2,
        FENCE,
        REMIND_SANITY_HEADER,
        FENCE,
        *rss,
        FENCE,
    ]
    if runs_not_started is not None:
        lines.append(NOT_STARTED_HEADER)
        lines.extend(runs_not_started)
    return "\n".join(lines)


def magpie_message(error_list: Sequence[str] | None) -> str:
    """Lines 469-479: the MAgPIE message, one line, chosen by ``!is.null(errorList)``.

    R's ``errorList`` is ``NULL`` until the first error text is appended (``c(NULL, x)``), so
    ``None`` and an empty sequence both mean "no errors".
    """
    return MAGPIE_WARNINGS if error_list else MAGPIE_SUCCESS


# ---------------------------------------------------------------------------
# payload and transport (lines 57-62)
# ---------------------------------------------------------------------------


def curl_command(message: str, token: str) -> str:
    """The shell command R builds at lines 58-60, byte for byte (it appears in the warning text only)."""
    return (
        "curl -i -X POST -H 'Content-Type: application/json' -d '"
        + PAYLOAD_PREFIX
        + message
        + PAYLOAD_SUFFIX
        + "' "
        + token
    )


def payload_for(message: str) -> str:
    """The webhook body: ``json.dumps({"text": message})`` (plan 03 section 2.3, D-15).

    Equal to R's ``{"text": "<message>"}`` whenever the message contains no character that
    JSON must escape (quotes, backslashes, control characters); non-ASCII text is kept as
    UTF-8 like R's bytes.
    """
    return json.dumps({"text": message}, ensure_ascii=False)


def payload_text(payload: str) -> str:
    """The message text inside a recorded payload, for the R writer and for :func:`payload_for`.

    A payload that parses as JSON (every Python payload, R's single-line messages) gives its
    ``text`` member. R's concatenated payloads with raw newlines or quotes in the message do
    not parse; for them the frame ``{"text": "`` ... ``"}`` is stripped, which is R's exact
    inverse because R never escapes anything. Anything else raises ``ValueError``.
    """
    try:
        parsed = json.loads(payload)
    except json.JSONDecodeError:
        if (
            payload.startswith(PAYLOAD_PREFIX)
            and payload.endswith(PAYLOAD_SUFFIX)
            and len(payload) >= len(PAYLOAD_PREFIX) + len(PAYLOAD_SUFFIX)
        ):
            return payload[len(PAYLOAD_PREFIX) : -len(PAYLOAD_SUFFIX)]
        raise ValueError('not a Mattermost payload: neither JSON nor R\'s {"text": "..."} frame') from None
    if not isinstance(parsed, dict) or list(parsed) != ["text"] or not isinstance(parsed["text"], str):
        raise ValueError("not a Mattermost payload: expected exactly one string member 'text'")
    return parsed["text"]


_RESOLVE_HINTS = ("gaierror", "Name or service not known", "nodename nor servname", "name resolution")
_TIMEOUT_HINTS = ("TimeoutError", "timed out")
_TLS_HINTS = ("SSLError", "SSL:", "CERTIFICATE")
_URL_HINTS = ("ValueError", "unknown url type", "no host given")


def curl_exit_status(failure: BaseException | str) -> int:
    """curl's exit code for the failure class of ``failure`` (the number in R's warning text).

    6 when the host could not be resolved, 28 for a timeout, 35 for a TLS failure, 3 for a
    malformed URL, 7 (could not connect) otherwise. An exception is classified by its type
    (``URLError`` through its ``reason``); the body text :meth:`Effects.post_json` returns
    with status 0 (``<ExceptionType>: <message>``) by the names and messages it carries.
    """
    if isinstance(failure, str):
        if any(hint in failure for hint in _URL_HINTS):
            return 3
        if any(hint in failure for hint in _RESOLVE_HINTS):
            return 6
        if any(hint in failure for hint in _TIMEOUT_HINTS):
            return 28
        if any(hint in failure for hint in _TLS_HINTS):
            return 35
        return 7
    if isinstance(failure, ValueError):
        return 3
    reason: object = failure.reason if isinstance(failure, urllib.error.URLError) else failure
    if isinstance(reason, str):  # urllib's own diagnoses (unknown url type, no host given) carry a text reason
        return curl_exit_status(f"{type(failure).__name__}: {failure}")
    if isinstance(reason, socket.gaierror):
        return 6
    if isinstance(reason, TimeoutError):
        return 28
    if isinstance(reason, ssl.SSLError):
        return 35
    return 7


def mattermost_bot_message(message: str, token: str, effects: Effects | None = None) -> tuple[int, str] | None:
    """``.mattermostBotMessage(message, token)``: post the message to the webhook ``token``.

    Returns the ``(status, body)`` of the HTTP response (an error response included: ``curl``
    runs without ``--fail``, so R sees no failure there) or ``None`` when no response arrived.
    :meth:`Effects.post_json` reports that as status ``0`` with the error text as body (a test
    double may raise instead; both are handled). That failure is R's: the reason goes to
    stderr as curl's would, an :class:`RWarning` ``running command '<curl command>' had status
    <n>`` is emitted for the end-of-run ``Warning message`` block, and the caller continues.
    """
    eff = _effects(effects)
    payload = payload_for(message)
    failure: BaseException | str
    try:
        status, body = eff.post_json(token, payload)
    except urllib.error.HTTPError as exc:
        return exc.code, str(exc.reason)  # the server answered: not a failure for curl
    except (OSError, ValueError) as exc:
        failure, body = exc, f"{type(exc).__name__}: {exc}"
    else:
        if status != 0:
            return status, body
        failure = body
    sys.stderr.write(f"Mattermost notification failed: {body}\n")
    sys.stderr.flush()
    text = f"running command '{curl_command(message, token)}' had status {curl_exit_status(failure)}"
    warnings.warn(RWarning(SYSTEM_CALL, text), stacklevel=2)
    return None


# ---------------------------------------------------------------------------
# the notification step of evaluateRuns (lines 449-486)
# ---------------------------------------------------------------------------


def send_notification(
    model: str,
    token: str | None,
    *,
    today: str,
    summary: str,
    testthat_result: str | None,
    git_info: Sequence[str],
    runs_started: Sequence[str],
    runs_not_started: Sequence[str] | None = None,
    error_list: Sequence[str] | None = None,
    effects: Effects | None = None,
) -> str | None:
    """Lines 449-486 of ``evaluateRuns``: compose the message for ``model`` and send it.

    Nothing happens without a ``token``. For REMIND the listing (``rs -A`` of the started
    runs, in the current directory ``output/``) and the sanity checks are captured first, in
    that order and only now, then the message is composed; for MAgPIE the one-line message
    depends on ``error_list``; any other model composes nothing. The message is sent through
    :func:`mattermost_bot_message` and returned (``None`` when nothing was sent). The preceding
    ``message("Composing message and sending it to mattermost channel")`` of line 447 is the
    caller's, it is printed with or without a token.
    """
    if token is None:
        return None
    eff = _effects(effects)
    message: str | None = None
    if model == "REMIND":
        rs2 = capture_loop_runs(runs_started, eff)
        rss = capture_sanity_checks(eff)
        if testthat_result is None:
            raise ValueError("testthat_result is required for the REMIND message (object 'testthatResult' not found)")
        message = remind_message(
            today=today,
            summary=summary,
            testthat_result=testthat_result,
            git_info=git_info,
            rs2=rs2,
            rss=rss,
            runs_not_started=runs_not_started,
        )
    if model == "MAgPIE":
        message = magpie_message(error_list)
    if message is not None:
        mattermost_bot_message(message, token, eff)
    return message
