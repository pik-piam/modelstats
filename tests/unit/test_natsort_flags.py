"""natsort with fixed flags reproduces the three R sort orders in use.

Converted from ``migration/harness/smoke/py_misc.py`` and ``yaml_test.py``. R sorts run names three
different ways (``stringi::stri_order(numeric = TRUE)``, ``gtools::mixedsort`` and
``gtools::mixedorder(gsub("-", "", x))``) and they disagree with each other; natsort 8.4 reproduces
each with the flags recorded in ``migration/02-python-libraries.md`` section 4.4 (decision D-14: no
PyICU auto-detection). ``migration/harness/checks/runtime_format.R`` asserts the same orders on the
R side.

``stri_order`` collates with stringi's default ICU locale, and that locale follows the process
locale: under the cluster's ``LC_ALL=C.utf8`` (set by the ``piam`` module and pinned by
``migration/harness/sandbox.sh``) it is ``en_US_POSIX``, plain code-point order with uppercase
before lowercase, so ``SSP2-EU21-* < SSP2-EcBudg500-*`` and ``SSP* < archive/* < default-*``. The
stand-in is therefore the case-sensitive ``ns.DEFAULT``, not ``ns.IGNORECASE``, which agrees only on
samples where case never decides (Codex review of phase 0, finding 3). The pairs below come from
``migration/goldens/sort/amt-names.json``; ``tests/golden/test_sort_goldens.py`` checks
``ns.DEFAULT`` against every ``migration/goldens/sort/*.json``.
"""

from __future__ import annotations

from natsort import natsorted, ns

COUPLED = [
    "C_SSP2-rem-1",
    "C_SSP2-rem-10",
    "C_SSP2-rem-2",
    "C_SSP2-mag-1",
    "C_SSP2-mag-2",
    "C_SSP2EU-rem-3",
    "C_SSP1-rem-1",
]

# Real AMT names in the order goldens/sort/amt-names.json records them (R under LC_ALL=C.utf8).
AMT_NAMES_IN_R_ORDER = [
    "SSP2-EU21-NPi2025-AMT_2026-09-18_22.11.36",
    "SSP2-EcBudg500-AMT_2026-09-19_01.25.14",
    "archive/SSP2-EU21-NPi-AMT_2025-07-25_22.18.56",
    "default-AMT_2026-09-28_13.23.51",
    "export",
]


def test_stri_order_numeric_is_case_sensitive_default() -> None:
    assert natsorted(COUPLED, alg=ns.DEFAULT) == [
        "C_SSP1-rem-1",
        "C_SSP2-mag-1",
        "C_SSP2-mag-2",
        "C_SSP2-rem-1",
        "C_SSP2-rem-2",
        "C_SSP2-rem-10",
        "C_SSP2EU-rem-3",
    ]


def test_stri_order_under_c_utf8_puts_uppercase_before_lowercase() -> None:
    shuffled = list(reversed(AMT_NAMES_IN_R_ORDER))
    assert natsorted(shuffled, alg=ns.DEFAULT) == AMT_NAMES_IN_R_ORDER
    # ns.IGNORECASE is the en_US order, not the cluster's: it is what the first conversion asserted.
    assert natsorted(shuffled, alg=ns.IGNORECASE) != AMT_NAMES_IN_R_ORDER
    assert natsorted(shuffled, alg=ns.IGNORECASE)[0] == "archive/SSP2-EU21-NPi-AMT_2025-07-25_22.18.56"


def test_gtools_mixedsort_is_signed_ignorecase() -> None:
    # gtools reads "-10" as a negative number, so the coupled runs sort in reverse (a register-worthy quirk).
    assert natsorted(COUPLED, alg=ns.SIGNED | ns.IGNORECASE) == [
        "C_SSP1-rem-1",
        "C_SSP2-mag-2",
        "C_SSP2-mag-1",
        "C_SSP2-rem-10",
        "C_SSP2-rem-2",
        "C_SSP2-rem-1",
        "C_SSP2EU-rem-3",
    ]


def test_gtools_mixedorder_without_hyphens_is_ignorecase_on_a_stripped_key() -> None:
    assert natsorted(COUPLED, key=lambda s: s.replace("-", ""), alg=ns.IGNORECASE) == [
        "C_SSP1-rem-1",
        "C_SSP2EU-rem-3",
        "C_SSP2-mag-1",
        "C_SSP2-mag-2",
        "C_SSP2-rem-1",
        "C_SSP2-rem-2",
        "C_SSP2-rem-10",
    ]


def test_amt_timestamps_sort_numerically_by_hour() -> None:
    names = ["SSP2-NPi-AMT_2026-09-25_10.11.12", "SSP2-NPi-AMT_2026-09-25_9.11.12"]
    assert natsorted(names, alg=ns.DEFAULT) == list(reversed(names))
