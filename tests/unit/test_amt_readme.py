"""amt.readme: the README builder of ``evaluateRuns`` against ``R/modeltests.R`` (R 4.6.1 facts pinned 2026-10-01).

The five README goldens named by the workflow (``migration/goldens/amt/<case>/README.md`` of remind-evaluate,
remind-evaluate-nocompscen, remind-evaluate-git-merges-empty, magpie-evaluate-recent and magpie-evaluate-old) are
embedded here byte for byte together with the inputs reconstructed from the goldens, so that the test runs
without ``migration/``:

- ``model``, ``mydir`` and ``compScen`` from ``migration/cases/amt/<case>/case.json``, ``today`` from the frozen
  clock in ``result.json``;
- the tested commit and the merge lines from the fake ``git`` (``migration/harness/fakebin/git``: ``git log -1``
  prints ``commit 3f5e2a1b...``, ``git log --merges`` two ``Merge pull request`` lines; the git-merges-empty case
  cans an empty output);
- the status records of the started runs from ``state/output_gRS_rds.json`` (REMIND; the ``try(rbind(...))`` of
  line 230 failed in those cases and ``gRS`` is a fresh ``getRunStatus(dir())``, so its rows are the per-run
  records) and from the status goldens of the landuse runs (MAgPIE, where ``gRS.rds`` is never saved), in the
  ``runsStarted`` order of ``stderr.txt``;
- the scenario names of ``state/runsToStart_rds.json``;
- the error list as R accumulates it over the runs (lines 279-298): REMIND: SSP2-EU21-PkBudg650 ``not_converged``
  and ``Mif = sumErr``, SSP2-NPi2025-calibrate ``Calib_nash`` without ``Clb_converged``, SSP3-NPi2025 ``Conv`` of
  digits, ``runInAppResults = no``; MAgPIE recent: v39k_FSECc_BAU ``modelstat = NA`` and ``runInAppResults = no``,
  weeklyTests_SSP1-Ref ``222222222000000000``; MAgPIE old: no run started.

A second group of tests, skipped without ``migration/``, checks the embedded copies against those files, and that
the partial READMEs of the failure cases (grs-corrupt, grs-stale, duplicate, runstostart-missing) are what
:class:`Readme` holds after the corresponding steps.

R facts verified with ``LC_ALL=C.utf8 Rscript`` on 2026-10-01 (see the module docstring of ``amt/readme.py``):
``write(c("a", "b"), f)`` writes ``a\\nb\\n``, ``write(character(0), f)`` a single ``\\n``,
``write(NA_character_, f)`` ``NA\\n``; ``paste0("x", NULL, "y")`` is ``xy``; ``if (NULL == "REMIND" && TRUE)``
stops with ``missing value where TRUE/FALSE needed`` while ``NULL == "REMIND" && FALSE`` is ``FALSE``;
``sub("\\n$", "", "abc\\n\\n")`` is ``abc\\n``; ``setdiff(c("default-AMT", "a", "a", "b"), "a")`` is
``default-AMT b``; ``is.numeric(NA)`` is ``FALSE``, ``is.numeric(NA_real_)`` is ``TRUE`` and
``format(round(make_difftime(second = NA_real_), 1))`` is ``NA days``.
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest
from _fake_effects import FakeEffects

from modelstats.amt import readme as rm
from modelstats.errors import RParityError
from modelstats.run_status import RunStatus

REPO = Path(__file__).resolve().parents[2]
GOLDENS = REPO / "migration" / "goldens"
AMT_GOLDENS = GOLDENS / "amt"
STATUS_GOLDENS = GOLDENS / "status" / "oncluster" / "detailed"
AMT_CASES = REPO / "migration" / "cases" / "amt"

COMMIT = "3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f"
MERGES = [
    "3f5e2a1 Merge pull request #999 from example/feature",
    "1a2b3c4 Merge pull request #998 from example/bugfix",
]
REMIND_MYDIR = "/p/projects/remind/modeltests/remind/"
MAGPIE_MYDIR = "/p/projects/landuse/tests/magpie/"
NOT_CONVERGED = "Some run(s) did not converge"
SUM_ERR = "Summation checks for some run(s) revealed some gaps"
NOT_REPORTED = "Some run(s) did not report correctly"
# lines 279-298 over the five REMIND runs, in run order (duplicates as R accumulates them)
REMIND_ERRORS = [NOT_CONVERGED, SUM_ERR, NOT_CONVERGED, NOT_CONVERGED, NOT_REPORTED]
# line 294 and 298 over the five landuse runs
MAGPIE_RECENT_ERRORS = [NOT_CONVERGED, NOT_REPORTED, NOT_CONVERGED]


# --------------------------------------------------------------------------- the goldens, embedded


README_REMIND_EVALUATE = (
    "```\n"
    "This is the result of the automated model tests for REMIND on 2026-09-30.\n"
    "Path to runs: /p/projects/remind/modeltests/remind/output/\n"
    "Direct and interactive access to plots: open shinyResults::appResults, then use 'AMT' as keyword"
    " in the title search\n"
    "Each run folder below should contain a compareScenarios PDF comparing the output of the current "
    "and the last successful tests (comp_with_RUN-DATE.pdf)\n"
    "Note: 'Mif' = 'no' indicates a possible error in output generation, please check!\n"
    "If you are currently viewing the email: Overview of the last test is in red, and of the current "
    "test in green\n"
    "Tested commit: 3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f\n"
    "The test of 2026-09-30 contains these merges:\n"
    "3f5e2a1 Merge pull request #999 from example/feature\n"
    "1a2b3c4 Merge pull request #998 from example/bugfix\n"
    "Run                                             Runtime        RunType      RunStatus           "
    "Warnings   Iter              Conv                   modelstat            Mif     AppResults\n"
    "default-AMT_2026-09-28_13.23.51                 3.4 hours      nash         Normal completion   "
    "21         41/100            converged (had INFES)  2: Locally Optimal   yes     yes\n"
    "SSP2-EU21-PkBudg650-AMT_2026-09-28_13.54.59     21.6 hours     nash         Normal completion   "
    "27         100/100           not_converged          2: Locally Optimal   sumErr  yes\n"
    "SSP2-NPi-AMT_2026-09-28_10.30.27                2.3 hours      nash         Normal completion   "
    "27         26/100            converged              2: Locally Optimal   yes     yes\n"
    "SSP2-NPi2025-calibrate-AMT_2026-09-28_10.27.04  14.6 hours     Calib_nash   Normal completion   "
    "3          26/100 Clb: 10    converged              2: Locally Optimal   yes     yes\n"
    "SSP3-NPi2025-AMT_2026-09-28_17.12.58            20.4 mins      nash         Execution error     "
    "0          3/100             722552252275           5: Locally Infes     no      no \n"
    " \n"
    "These scenarios did not start at all:\n"
    "SSP2-NDC-LTS-pf-AMT\n"
    "SSP2-NDC-LTS-my-AMT\n"
    "SSP2-NDC-AMT\n"
    "SSP2-NPi2025-AMT\n"
    "SSP2-PkBudg650-AMT\n"
    "SSP2-PkBudg750-AMT\n"
    "SSP2-PkBudg750_wo100EJBiobound-AMT\n"
    "SSP2-PkBudg1000-AMT\n"
    "SSP2-EcBudg500-AMT\n"
    "SSP2-rollBack-AMT\n"
    "SSP2-EU21-NPi2025-AMT\n"
    "SSP2-EU21-PkBudg750-AMT\n"
    "SSP2-EU21-PkBudg1000-AMT\n"
    "SSP2-EU21-EU-Ger-NZ-AMT\n"
    "SSP3-rollBack-AMT\n"
    "SSP1-EU21-NPi2025-AMT\n"
    "SSP1-EU21-PkBudg750-AMT\n"
    "SSP1-NPi2025-AMT\n"
    "SSP1-PkBudg750-AMT\n"
    "SSP1-PkBudg1000-AMT\n"
    " \n"
    "Summary: Some run(s) did not converge. Summation checks for some run(s) revealed some gaps. Some"
    " run(s) did not report correctly\n"
    "```\n"
)

README_REMIND_EVALUATE_NOCOMPSCEN = (
    "```\n"
    "This is the result of the automated model tests for REMIND on 2026-09-30.\n"
    "Path to runs: /p/projects/remind/modeltests/remind/output/\n"
    "Direct and interactive access to plots: open shinyResults::appResults, then use 'AMT' as keyword"
    " in the title search\n"
    "Note: 'Mif' = 'no' indicates a possible error in output generation, please check!\n"
    "If you are currently viewing the email: Overview of the last test is in red, and of the current "
    "test in green\n"
    "Tested commit: 3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f\n"
    "The test of 2026-09-30 contains these merges:\n"
    "3f5e2a1 Merge pull request #999 from example/feature\n"
    "1a2b3c4 Merge pull request #998 from example/bugfix\n"
    "Run                                             Runtime        RunType      RunStatus           "
    "Warnings   Iter              Conv                   modelstat            Mif     AppResults\n"
    "default-AMT_2026-09-28_13.23.51                 3.4 hours      nash         Normal completion   "
    "21         41/100            converged (had INFES)  2: Locally Optimal   yes     yes\n"
    "SSP2-EU21-PkBudg650-AMT_2026-09-28_13.54.59     21.6 hours     nash         Normal completion   "
    "27         100/100           not_converged          2: Locally Optimal   sumErr  yes\n"
    "SSP2-NPi-AMT_2026-09-28_10.30.27                2.3 hours      nash         Normal completion   "
    "27         26/100            converged              2: Locally Optimal   yes     yes\n"
    "SSP2-NPi2025-calibrate-AMT_2026-09-28_10.27.04  14.6 hours     Calib_nash   Normal completion   "
    "3          26/100 Clb: 10    converged              2: Locally Optimal   yes     yes\n"
    "SSP3-NPi2025-AMT_2026-09-28_17.12.58            20.4 mins      nash         Execution error     "
    "0          3/100             722552252275           5: Locally Infes     no      no \n"
    " \n"
    "These scenarios did not start at all:\n"
    "SSP2-NDC-LTS-pf-AMT\n"
    "SSP2-NDC-LTS-my-AMT\n"
    "SSP2-NDC-AMT\n"
    "SSP2-NPi2025-AMT\n"
    "SSP2-PkBudg650-AMT\n"
    "SSP2-PkBudg750-AMT\n"
    "SSP2-PkBudg750_wo100EJBiobound-AMT\n"
    "SSP2-PkBudg1000-AMT\n"
    "SSP2-EcBudg500-AMT\n"
    "SSP2-rollBack-AMT\n"
    "SSP2-EU21-NPi2025-AMT\n"
    "SSP2-EU21-PkBudg750-AMT\n"
    "SSP2-EU21-PkBudg1000-AMT\n"
    "SSP2-EU21-EU-Ger-NZ-AMT\n"
    "SSP3-rollBack-AMT\n"
    "SSP1-EU21-NPi2025-AMT\n"
    "SSP1-EU21-PkBudg750-AMT\n"
    "SSP1-NPi2025-AMT\n"
    "SSP1-PkBudg750-AMT\n"
    "SSP1-PkBudg1000-AMT\n"
    " \n"
    "Summary: Some run(s) did not converge. Summation checks for some run(s) revealed some gaps. Some"
    " run(s) did not report correctly\n"
    "```\n"
)

README_REMIND_EVALUATE_GIT_MERGES_EMPTY = (
    "```\n"
    "This is the result of the automated model tests for REMIND on 2026-09-30.\n"
    "Path to runs: /p/projects/remind/modeltests/remind/output/\n"
    "Direct and interactive access to plots: open shinyResults::appResults, then use 'AMT' as keyword"
    " in the title search\n"
    "Each run folder below should contain a compareScenarios PDF comparing the output of the current "
    "and the last successful tests (comp_with_RUN-DATE.pdf)\n"
    "Note: 'Mif' = 'no' indicates a possible error in output generation, please check!\n"
    "If you are currently viewing the email: Overview of the last test is in red, and of the current "
    "test in green\n"
    "Tested commit: 3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f\n"
    "The test of 2026-09-30 contains these merges:\n"
    "Run                                             Runtime        RunType      RunStatus           "
    "Warnings   Iter              Conv                   modelstat            Mif     AppResults\n"
    "default-AMT_2026-09-28_13.23.51                 3.4 hours      nash         Normal completion   "
    "21         41/100            converged (had INFES)  2: Locally Optimal   yes     yes\n"
    "SSP2-EU21-PkBudg650-AMT_2026-09-28_13.54.59     21.6 hours     nash         Normal completion   "
    "27         100/100           not_converged          2: Locally Optimal   sumErr  yes\n"
    "SSP2-NPi-AMT_2026-09-28_10.30.27                2.3 hours      nash         Normal completion   "
    "27         26/100            converged              2: Locally Optimal   yes     yes\n"
    "SSP2-NPi2025-calibrate-AMT_2026-09-28_10.27.04  14.6 hours     Calib_nash   Normal completion   "
    "3          26/100 Clb: 10    converged              2: Locally Optimal   yes     yes\n"
    "SSP3-NPi2025-AMT_2026-09-28_17.12.58            20.4 mins      nash         Execution error     "
    "0          3/100             722552252275           5: Locally Infes     no      no \n"
    " \n"
    "These scenarios did not start at all:\n"
    "SSP2-NDC-LTS-pf-AMT\n"
    "SSP2-NDC-LTS-my-AMT\n"
    "SSP2-NDC-AMT\n"
    "SSP2-NPi2025-AMT\n"
    "SSP2-PkBudg650-AMT\n"
    "SSP2-PkBudg750-AMT\n"
    "SSP2-PkBudg750_wo100EJBiobound-AMT\n"
    "SSP2-PkBudg1000-AMT\n"
    "SSP2-EcBudg500-AMT\n"
    "SSP2-rollBack-AMT\n"
    "SSP2-EU21-NPi2025-AMT\n"
    "SSP2-EU21-PkBudg750-AMT\n"
    "SSP2-EU21-PkBudg1000-AMT\n"
    "SSP2-EU21-EU-Ger-NZ-AMT\n"
    "SSP3-rollBack-AMT\n"
    "SSP1-EU21-NPi2025-AMT\n"
    "SSP1-EU21-PkBudg750-AMT\n"
    "SSP1-NPi2025-AMT\n"
    "SSP1-PkBudg750-AMT\n"
    "SSP1-PkBudg1000-AMT\n"
    " \n"
    "Summary: Some run(s) did not converge. Summation checks for some run(s) revealed some gaps. Some"
    " run(s) did not report correctly\n"
    "```\n"
)

README_MAGPIE_EVALUATE_RECENT = (
    "```\n"
    "This is the result of the automated model tests for MAgPIE on 2026-10-01.\n"
    "Path to runs: /p/projects/landuse/tests/magpie/output/\n"
    "Direct and interactive access to plots: open shinyResults::appResults, then use 'weeklyTests' as"
    " keyword in the title search\n"
    "Note: 'Mif' = 'no' indicates a possible error in output generation, please check!\n"
    "If you are currently viewing the email: Overview of the last test is in red, and of the current "
    "test in green\n"
    "Tested commit: 3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f\n"
    "The test of 2026-10-01 contains these merges:\n"
    "3f5e2a1 Merge pull request #999 from example/feature\n"
    "1a2b3c4 Merge pull request #998 from example/bugfix\n"
    "Run                                             Runtime        RunType      RunStatus           "
    "Warnings   Iter              Conv                   modelstat            Mif     AppResults\n"
    "default_2026-09-19_04.05.47                     33.5 mins      nlp_apr17    Normal completion   "
    "0          y2100             NA                     222222222222222222   yes     yes\n"
    "v39k_FSECc_BAU                                                 nlp_apr17    full.log missing    "
    "NA         NA                NA                     NA                   no      no \n"
    "weeklyTests_singleTimeStep                      4.7 mins       nlp_apr17    Normal completion   "
    "2          y1995             NA                     2: Locally Optimal   yes     yes\n"
    "weeklyTests_SSP1-PkBudg1000                     37.7 mins      nlp_apr17    Normal completion   "
    "2          y2100             NA                     222222222222222222   yes     yes\n"
    "weeklyTests_SSP1-Ref                            14.2 mins      nlp_apr17    Terminated due to   "
    "3          y2035             NA                     222222222000000000   yes     yes\n"
    "Summary: Some run(s) did not converge. Some run(s) did not report correctly\n"
    "```\n"
)

README_MAGPIE_EVALUATE_OLD = (
    "```\n"
    "This is the result of the automated model tests for MAgPIE on 2026-10-04.\n"
    "Path to runs: /p/projects/landuse/tests/magpie/output/\n"
    "Direct and interactive access to plots: open shinyResults::appResults, then use 'weeklyTests' as"
    " keyword in the title search\n"
    "Note: 'Mif' = 'no' indicates a possible error in output generation, please check!\n"
    "If you are currently viewing the email: Overview of the last test is in red, and of the current "
    "test in green\n"
    "Tested commit: 3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f\n"
    "The test of 2026-10-04 contains these merges:\n"
    "3f5e2a1 Merge pull request #999 from example/feature\n"
    "1a2b3c4 Merge pull request #998 from example/bugfix\n"
    "Run                                             Runtime        RunType      RunStatus           "
    "Warnings   Iter              Conv                   modelstat            Mif     AppResults\n"
    "Summary: No runs started\n"
    "```\n"
)

REMIND_ROWS: list[tuple[str, dict[str, object]]] = [
    (
        "default-AMT_2026-09-28_13.23.51",
        {
            "jobInSLURM": "no",
            "RunType": "nash",
            "modelstat": "2: Locally Optimal",
            "runInAppResults": "yes",
            "Mif": "yes",
            "Iter": "41/100",
            "RunStatus": "Normal completion",
            "Warnings": "21",
            "Conv": "converged (had INFES)",
            "Runtime": 12213,
        },
    ),
    (
        "SSP2-EU21-PkBudg650-AMT_2026-09-28_13.54.59",
        {
            "jobInSLURM": "no",
            "RunType": "nash",
            "modelstat": "2: Locally Optimal",
            "runInAppResults": "yes",
            "Mif": "sumErr",
            "Iter": "100/100",
            "RunStatus": "Normal completion",
            "Warnings": "27",
            "Conv": "not_converged",
            "Runtime": 77679,
        },
    ),
    (
        "SSP2-NPi-AMT_2026-09-28_10.30.27",
        {
            "jobInSLURM": "no",
            "RunType": "nash",
            "modelstat": "2: Locally Optimal",
            "runInAppResults": "yes",
            "Mif": "yes",
            "Iter": "26/100",
            "RunStatus": "Normal completion",
            "Warnings": "27",
            "Conv": "converged",
            "Runtime": 8304,
        },
    ),
    (
        "SSP2-NPi2025-calibrate-AMT_2026-09-28_10.27.04",
        {
            "jobInSLURM": "no",
            "RunType": "Calib_nash",
            "modelstat": "2: Locally Optimal",
            "runInAppResults": "yes",
            "Mif": "yes",
            "Iter": "26/100 Clb: 10",
            "RunStatus": "Normal completion",
            "Warnings": "3",
            "Conv": "converged",
            "Runtime": 52473,
        },
    ),
    (
        "SSP3-NPi2025-AMT_2026-09-28_17.12.58",
        {
            "jobInSLURM": "no",
            "RunType": "nash",
            "modelstat": "5: Locally Infes",
            "runInAppResults": "no",
            "Mif": "no",
            "Iter": "3/100",
            "RunStatus": "Execution error",
            "Warnings": "0",
            "Conv": "722552252275",
            "Runtime": 1223,
        },
    ),
]

MAGPIE_ROWS: list[tuple[str, dict[str, object]]] = [
    (
        "default_2026-09-19_04.05.47",
        {
            "jobInSLURM": "no",
            "RunType": "nlp_apr17",
            "modelstat": "222222222222222222",
            "runInAppResults": "yes",
            "Mif": "yes",
            "Iter": "y2100",
            "RunStatus": "Normal completion",
            "Warnings": "0",
            "Conv": "NA",
            "Runtime": 2013,
        },
    ),
    (
        "v39k_FSECc_BAU",
        {
            "jobInSLURM": "no",
            "RunType": "nlp_apr17",
            "modelstat": "NA",
            "runInAppResults": "no",
            "Mif": "no",
            "Iter": "NA",
            "RunStatus": "full.log missing",
            "Warnings": "NA",
            "Conv": "NA",
            "Runtime": None,
        },
    ),
    (
        "weeklyTests_singleTimeStep",
        {
            "jobInSLURM": "no",
            "RunType": "nlp_apr17",
            "modelstat": "2: Locally Optimal",
            "runInAppResults": "yes",
            "Mif": "yes",
            "Iter": "y1995",
            "RunStatus": "Normal completion",
            "Warnings": "2",
            "Conv": "NA",
            "Runtime": 279,
        },
    ),
    (
        "weeklyTests_SSP1-PkBudg1000",
        {
            "jobInSLURM": "no",
            "RunType": "nlp_apr17",
            "modelstat": "222222222222222222",
            "runInAppResults": "yes",
            "Mif": "yes",
            "Iter": "y2100",
            "RunStatus": "Normal completion",
            "Warnings": "2",
            "Conv": "NA",
            "Runtime": 2260,
        },
    ),
    (
        "weeklyTests_SSP1-Ref",
        {
            "jobInSLURM": "no",
            "RunType": "nlp_apr17",
            "modelstat": "222222222000000000",
            "runInAppResults": "yes",
            "Mif": "yes",
            "Iter": "y2035",
            "RunStatus": "Terminated due to",
            "Warnings": "3",
            "Conv": "NA",
            "Runtime": 853,
        },
    ),
]

RUNS_TO_START = [
    "SSP2-NPi2025-calibrate-AMT",
    "SSP2-NDC-LTS-pf-AMT",
    "SSP2-NDC-LTS-my-AMT",
    "SSP2-NDC-AMT",
    "SSP2-NPi-AMT",
    "SSP2-NPi2025-AMT",
    "SSP2-PkBudg650-AMT",
    "SSP2-PkBudg750-AMT",
    "SSP2-PkBudg750_wo100EJBiobound-AMT",
    "SSP2-PkBudg1000-AMT",
    "SSP2-EcBudg500-AMT",
    "SSP2-rollBack-AMT",
    "SSP2-EU21-NPi2025-AMT",
    "SSP2-EU21-PkBudg650-AMT",
    "SSP2-EU21-PkBudg750-AMT",
    "SSP2-EU21-PkBudg1000-AMT",
    "SSP2-EU21-EU-Ger-NZ-AMT",
    "SSP3-NPi2025-AMT",
    "SSP3-rollBack-AMT",
    "SSP1-EU21-NPi2025-AMT",
    "SSP1-EU21-PkBudg750-AMT",
    "SSP1-NPi2025-AMT",
    "SSP1-PkBudg750-AMT",
    "SSP1-PkBudg1000-AMT",
]


# --------------------------------------------------------------------------- the five cases

REMIND_STARTED = [rowname for rowname, _ in REMIND_ROWS]
REMIND_NOT_STARTED = rm.runs_not_started(REMIND_STARTED, RUNS_TO_START)

CASES: dict[str, tuple[dict[str, object], str]] = {
    "remind-evaluate": (
        {
            "model": "REMIND",
            "today": "2026-09-30",
            "mydir": REMIND_MYDIR,
            "comp_scen": True,
            "git_info_lines": rm.git_info(COMMIT, "2026-09-30", MERGES),
            "runs": REMIND_ROWS,
            "runs_not_started_names": REMIND_NOT_STARTED,
            "error_list": REMIND_ERRORS,
        },
        README_REMIND_EVALUATE,
    ),
    "remind-evaluate-nocompscen": (
        {
            "model": "REMIND",
            "today": "2026-09-30",
            "mydir": REMIND_MYDIR,
            "comp_scen": False,
            "git_info_lines": rm.git_info(COMMIT, "2026-09-30", MERGES),
            "runs": REMIND_ROWS,
            "runs_not_started_names": REMIND_NOT_STARTED,
            "error_list": REMIND_ERRORS,
        },
        README_REMIND_EVALUATE_NOCOMPSCEN,
    ),
    "remind-evaluate-git-merges-empty": (
        {
            "model": "REMIND",
            "today": "2026-09-30",
            "mydir": REMIND_MYDIR,
            "comp_scen": True,
            "git_info_lines": rm.git_info(COMMIT, "2026-09-30", []),
            "runs": REMIND_ROWS,
            "runs_not_started_names": REMIND_NOT_STARTED,
            "error_list": REMIND_ERRORS,
        },
        README_REMIND_EVALUATE_GIT_MERGES_EMPTY,
    ),
    "magpie-evaluate-recent": (
        {
            "model": "MAgPIE",
            "today": "2026-10-01",
            "mydir": MAGPIE_MYDIR,
            "comp_scen": False,
            "git_info_lines": rm.git_info(COMMIT, "2026-10-01", MERGES),
            "runs": MAGPIE_ROWS,
            "runs_not_started_names": None,
            "error_list": MAGPIE_RECENT_ERRORS,
        },
        README_MAGPIE_EVALUATE_RECENT,
    ),
    "magpie-evaluate-old": (
        {
            "model": "MAgPIE",
            "today": "2026-10-04",
            "mydir": MAGPIE_MYDIR,
            "comp_scen": False,
            "git_info_lines": rm.git_info(COMMIT, "2026-10-04", MERGES),
            "runs": [],
            "runs_not_started_names": None,
            "error_list": ["No runs started"],
        },
        README_MAGPIE_EVALUATE_OLD,
    ),
}

# The partial READMEs of the REMIND failure cases are prefixes of remind-evaluate's (same inputs, the run aborted
# later): line counts of the goldens, checked against the files below when migration/ is present.
PARTIAL_LINES = {
    "remind-evaluate-grs-corrupt": 11,  # header and git info; readRDS("gRS.rds") failed (unknown input format)
    "remind-evaluate-grs-stale": 13,  # + titles + one run; .readRuntime() of a run that is not in the snapshot
    "remind-evaluate-duplicate": 15,  # + three runs
    "remind-evaluate-runstostart-missing": 17,  # + five runs; readRDS(runsToStart.rds) failed
}


@pytest.fixture
def eff() -> FakeEffects:
    """The sandbox regime: /p exists, so printOutput uses its ten cluster columns."""
    return FakeEffects(on_cluster=True)


def prefix(text: str, n_lines: int) -> str:
    return "".join(line + "\n" for line in text.split("\n")[:n_lines])


# --------------------------------------------------------------------------- README bytes


@pytest.mark.parametrize("case", sorted(CASES))
def test_readme_bytes_equal_the_golden(case: str, eff: FakeEffects) -> None:
    kwargs, expected = CASES[case]
    text = rm.build_readme(effects=eff, **kwargs)  # type: ignore[arg-type]
    assert text.encode("utf-8") == expected.encode("utf-8")


def test_incremental_readme_equals_build_and_keeps_partial_text(eff: FakeEffects) -> None:
    """The Readme object holds exactly what R has written to tempdir()/README.md after each step."""
    kwargs, expected = CASES["remind-evaluate"]
    readme = rm.Readme()
    assert readme.text == "" and readme.lines == []
    readme.begin("REMIND", "2026-09-30", REMIND_MYDIR, True)
    assert readme.text == prefix(expected, 7)
    readme.add_git_info(rm.git_info(COMMIT, "2026-09-30", MERGES))
    assert readme.text == prefix(expected, PARTIAL_LINES["remind-evaluate-grs-corrupt"])
    readme.add_column_titles()
    assert readme.lines[-1] == rm.COLUMN_TITLE_LINE
    for n, (rowname, row) in enumerate(REMIND_ROWS, start=1):
        line = readme.add_run(row, rowname=rowname, effects=eff)
        assert line == readme.lines[-1]
        assert readme.text == prefix(expected, 12 + n)
    assert readme.text == prefix(expected, PARTIAL_LINES["remind-evaluate-runstostart-missing"])
    readme.add_not_started(REMIND_NOT_STARTED)
    summary = readme.finish(REMIND_ERRORS)
    assert summary == "Summary: " + NOT_CONVERGED + ". " + SUM_ERR + ". " + NOT_REPORTED
    assert readme.text == expected
    assert readme.text == rm.build_readme(effects=eff, **kwargs)  # type: ignore[arg-type]


def test_begin_starts_over() -> None:
    """``write("```", readme)`` without append truncates the file."""
    readme = rm.Readme()
    readme.write("stale")
    readme.begin("MAgPIE", "2026-10-04", MAGPIE_MYDIR, False)
    assert readme.lines[0] == "```" and "stale" not in readme.text


# --------------------------------------------------------------------------- write()


@pytest.mark.parametrize(
    ("value", "expected"),
    [
        ("```", "```\n"),
        (["a", "b"], "a\nb\n"),
        ([], "\n"),
        (None, "\n"),
        ((), "\n"),
        (" ", " \n"),
        ("", "\n"),
        ([None], "NA\n"),
        (["a", None], "a\nNA\n"),
        ("a\nb", "a\nb\n"),
        ("two\n", "two\n\n"),
    ],
)
def test_r_write(value: object, expected: str) -> None:
    assert rm.r_write(value) == expected  # type: ignore[arg-type]
    readme = rm.Readme()
    readme.write(value)  # type: ignore[arg-type]
    assert readme.text == expected


def test_lines_property() -> None:
    readme = rm.Readme()
    readme.write(["a", "b"])
    readme.write([])
    readme.write(" ")
    assert readme.lines == ["a", "b", "", " "]
    assert readme.text == "a\nb\n\n \n"


# --------------------------------------------------------------------------- header (lines 203-217)


def test_header_remind_with_and_without_comp_scen() -> None:
    with_pdf = rm.header_lines("REMIND", "2026-09-30", REMIND_MYDIR, True)
    without = rm.header_lines("REMIND", "2026-09-30", REMIND_MYDIR, False)
    assert with_pdf == README_REMIND_EVALUATE.split("\n")[:7]
    assert without == README_REMIND_EVALUATE_NOCOMPSCEN.split("\n")[:6]
    assert [line for line in with_pdf if not line.startswith("Each run folder")] == without
    assert with_pdf[2] == "Path to runs: /p/projects/remind/modeltests/remind/output/"


def test_header_magpie_and_other_models() -> None:
    magpie = rm.header_lines("MAgPIE", "2026-10-01", MAGPIE_MYDIR, True)
    assert magpie == README_MAGPIE_EVALUATE_RECENT.split("\n")[:6]
    assert "then use 'weeklyTests' as keyword" in magpie[3]
    other = rm.header_lines("foo", "2026-10-01", "/x", True)  # ifelse(model == "MAgPIE", ..) and model == "REMIND"
    assert other[1] == "This is the result of the automated model tests for foo on 2026-10-01."
    assert other[2] == "Path to runs: /xoutput/"  # paste0(mydir, "output/") adds no slash
    assert "then use 'AMT' as keyword" in other[3]
    assert len(other) == 6


def test_header_model_null() -> None:
    """model = NULL: paste0 drops it, ifelse(logical(0)) is empty, and the compScen test fails only when TRUE."""
    lines = rm.header_lines(None, "2026-09-30", REMIND_MYDIR, False)
    assert lines[1] == "This is the result of the automated model tests for  on 2026-09-30."
    assert lines[3].endswith("then use '' as keyword in the title search")
    assert len(lines) == 6
    with pytest.raises(RParityError, match="^missing value where TRUE/FALSE needed$"):
        rm.header_lines(None, "2026-09-30", REMIND_MYDIR, True)
    readme = rm.Readme()
    with pytest.raises(RParityError, match="^missing value where TRUE/FALSE needed$"):
        readme.begin(None, "2026-09-30", REMIND_MYDIR, True)
    assert readme.lines == lines[:4]  # the four writes before the failing `if` stay in the file


# --------------------------------------------------------------------------- gitInfo (lines 219-222)


def test_git_info() -> None:
    assert rm.git_info(COMMIT, "2026-09-30", MERGES) == README_REMIND_EVALUATE.split("\n")[7:11]
    assert rm.git_info(COMMIT, "2026-09-30", []) == [
        f"Tested commit: {COMMIT}",
        "The test of 2026-09-30 contains these merges:",
    ]
    readme = rm.Readme()
    readme.add_git_info(rm.git_info(COMMIT, "2026-09-30", []))
    assert readme.text == f"Tested commit: {COMMIT}\nThe test of 2026-09-30 contains these merges:\n"


# --------------------------------------------------------------------------- column titles (lines 250-256)


def test_column_titles() -> None:
    assert rm.COL_SEP == "  "
    assert rm.LEN_COLS == (46, 11, 0, 11, 18, 9, 16, 21, 19, 6, 3)
    assert rm.COLUMN_TITLE_LINE == README_REMIND_EVALUATE.split("\n")[11]
    assert rm.COLUMN_TITLE_LINE == README_MAGPIE_EVALUATE_OLD.split("\n")[10]


# --------------------------------------------------------------------------- run lines (lines 275-278)


def test_run_line_formats_a_numeric_runtime(eff: FakeEffects) -> None:
    rowname, row = REMIND_ROWS[4]
    line = rm.run_line(row, rowname=rowname, effects=eff)
    assert line == README_REMIND_EVALUATE.split("\n")[16]
    assert "20.4 mins" in line and not line.endswith("\n")
    assert line.endswith("no ")  # AppResults cut to three characters, trailing blank kept
    assert row["Runtime"] == 1223  # the record is not modified
    assert rm.runtime_formatted(row)["Runtime"] == "20.4 mins"


def test_run_line_leaves_a_missing_runtime_blank(eff: FakeEffects) -> None:
    rowname, row = MAGPIE_ROWS[1]  # v39k_FSECc_BAU: no runstatistics.rda, Runtime is the logical NA
    assert row["Runtime"] is None
    line = rm.run_line(row, rowname=rowname, effects=eff)
    assert line == README_MAGPIE_EVALUATE_RECENT.split("\n")[12]
    assert line[48:59] == " " * 11  # blanks of the Runtime width, not "NA days"
    assert rm.runtime_formatted(row)["Runtime"] is None
    assert rm.runtime_formatted({"Mif": "yes"}) == {"Mif": "yes"}
    assert rm.runtime_formatted({"Runtime": "pending"})["Runtime"] == "pending"
    assert rm.runtime_formatted({"Runtime": True})["Runtime"] is True  # logical, not numeric


def test_run_line_takes_the_rowname_of_a_run_status(eff: FakeEffects) -> None:
    rowname, row = REMIND_ROWS[0]
    record = RunStatus(rowname, "/p/projects/remind/modeltests/remind/output/" + rowname, row)
    assert rm.run_line(record, effects=eff) == README_REMIND_EVALUATE.split("\n")[12]
    assert rm.run_line(record, rowname="other", effects=eff).startswith("other" + " " * 41 + "  3.4 hours")
    with pytest.raises(TypeError, match="rowname"):
        rm.run_line(row, effects=eff)


def test_run_line_strips_exactly_one_newline(eff: FakeEffects, monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr(rm, "print_output", lambda *a, **k: "x\n\n")
    assert rm.run_line({}, rowname="r", effects=eff) == "x\n"


def test_run_line_needs_every_printed_column(eff: FakeEffects) -> None:
    rowname, row = REMIND_ROWS[0]
    brief = {k: v for k, v in row.items() if k != "Conv"}
    with pytest.raises(RParityError, match="^undefined columns selected$"):
        rm.run_line(brief, rowname=rowname, effects=eff)


# --------------------------------------------------------------------------- runsNotStarted (lines 371-377)


def test_scenarios_started_removes_the_time_stamp() -> None:
    assert rm.scenarios_started(REMIND_STARTED) == [
        "default-AMT",
        "SSP2-EU21-PkBudg650-AMT",
        "SSP2-NPi-AMT",
        "SSP2-NPi2025-calibrate-AMT",
        "SSP3-NPi2025-AMT",
    ]
    assert rm.scenarios_started(["noDate", "a_2026-09-28_13.23.51_2026-09-28_13.23.51", "b_2026-09-28"]) == [
        "noDate",
        "a",  # gsub: every occurrence
        "b_2026-09-28",  # the whole pattern or nothing
    ]


def test_runs_not_started_is_r_setdiff() -> None:
    assert rm.runs_not_started(REMIND_STARTED, RUNS_TO_START) == README_REMIND_EVALUATE.split("\n")[19:39]
    # default-AMT first, order of runsToStart, duplicates dropped, started scenarios removed
    assert rm.runs_not_started([], ["b", "a", "b"]) == ["default-AMT", "b", "a"]
    assert rm.runs_not_started(["default-AMT_2026-09-28_13.23.51", "a_2026-09-28_13.23.51"], ["b", "a"]) == ["b"]
    assert rm.runs_not_started(["default-AMT_2026-09-28_13.23.51", "a_2026-09-28_13.23.51"], ["a"]) == []
    assert rm.runs_not_started([], ["default-AMT"]) == ["default-AMT"]


def test_not_started_text() -> None:
    assert rm.not_started_text(["x", "y"]) == " \nThese scenarios did not start at all:\nx\ny\n \n"
    assert rm.not_started_text([]) == " \nThese scenarios did not start at all:\n\n \n"  # write(character(0))
    readme = rm.Readme()
    readme.add_not_started([])
    assert readme.lines == [" ", "These scenarios did not start at all:", "", " "]


# --------------------------------------------------------------------------- summary (lines 421-430)


def test_summary_line() -> None:
    assert rm.summary_line(None) == "Summary: AMT runs look good."
    assert rm.summary_line([]) == "Summary: AMT runs look good."
    assert rm.summary_line(["No runs started"]) == "Summary: No runs started"
    assert rm.summary_line(REMIND_ERRORS) == README_REMIND_EVALUATE.split("\n")[40]
    assert rm.summary_line(["b", "a", "b", "a"]) == "Summary: b. a"


def test_finish_writes_summary_and_fence() -> None:
    readme = rm.Readme()
    assert readme.finish(["a"]) == "Summary: a"
    assert readme.text == "Summary: a\n```\n"


# --------------------------------------------------------------------------- the embedded copies versus migration/

needs_goldens = pytest.mark.skipif(not AMT_GOLDENS.is_dir(), reason=f"{AMT_GOLDENS} is absent (migration/ not linked)")


@needs_goldens
@pytest.mark.parametrize("case", sorted(CASES))
def test_embedded_readme_equals_the_golden_file(case: str) -> None:
    assert CASES[case][1].encode("utf-8") == (AMT_GOLDENS / case / "README.md").read_bytes()


@needs_goldens
@pytest.mark.parametrize("case", sorted(PARTIAL_LINES))
def test_partial_goldens_are_prefixes_of_remind_evaluate(case: str) -> None:
    partial = (AMT_GOLDENS / case / "README.md").read_bytes()
    assert partial == prefix(README_REMIND_EVALUATE, PARTIAL_LINES[case]).encode("utf-8")
    assert json.loads((AMT_GOLDENS / case / "result.json").read_text(encoding="utf-8"))["status"] == "error"


@needs_goldens
@pytest.mark.parametrize("case", sorted(CASES))
def test_embedded_inputs_equal_the_case_files(case: str) -> None:
    kwargs = CASES[case][0]
    spec = json.loads((AMT_CASES / case / "case.json").read_text(encoding="utf-8"))
    result = json.loads((AMT_GOLDENS / case / "result.json").read_text(encoding="utf-8"))
    assert (spec["model"], spec["mydir"], spec["compScen"]) == (kwargs["model"], kwargs["mydir"], kwargs["comp_scen"])
    assert result["frozen"][:10] == kwargs["today"]
    assert result["status"] == "ok"


@needs_goldens
def test_embedded_rows_equal_the_status_goldens() -> None:
    """The per-run records: the gRS rows of remind-evaluate equal the status goldens of the same runs, the
    landuse rows are the status goldens themselves."""
    grs = json.loads((AMT_GOLDENS / "remind-evaluate" / "state" / "output_gRS_rds.json").read_text(encoding="utf-8"))
    by_name = {row["_row"]: row for row in grs["value"]["rows"]}
    for rowname, row in REMIND_ROWS:
        assert {k: by_name[rowname][k] for k in row} == row
        golden = STATUS_GOLDENS / f"remind__modeltests__remind__output__{rowname}.json"
        status = json.loads(golden.read_text(encoding="utf-8"))["rows"][0]
        assert {k: status[k] for k in row} == row
    for rowname, row in MAGPIE_ROWS:
        golden = STATUS_GOLDENS / f"landuse__tests__magpie__output__{rowname}.json"
        status = json.loads(golden.read_text(encoding="utf-8"))["rows"][0]
        assert {k: status[k] for k in row} == row
    to_start = json.loads((AMT_GOLDENS / "remind-evaluate" / "state" / "runsToStart_rds.json").read_text("utf-8"))
    assert [row["_row"] for row in to_start["value"]["rows"]] == RUNS_TO_START
    stderr = (AMT_GOLDENS / "remind-evaluate" / "stderr.txt").read_text(encoding="utf-8").split("\n")
    start = stderr.index("Starting analysis for the list of the following runs:") + 1
    assert stderr[start : start + len(REMIND_STARTED)] == REMIND_STARTED
