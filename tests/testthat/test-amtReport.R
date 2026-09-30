fakeRun <- function(...) {
  utils::modifyList(list(name = "SSP2-NPi-AMT_2026-09-28_10.30.27", runType = "nash", convergence = "converged",
                         modelstat = "2: Locally Optimal", mif = "yes", inAppResults = "yes"), list(...))
}

test_that("runProblems applies the AMT rules", {
  expect_equal(runProblems(fakeRun()), character(0))
  expect_equal(runProblems(fakeRun(convergence = "not_converged")), "Some run(s) did not converge")
  expect_equal(runProblems(fakeRun(runType = "Calib_nash", convergence = "converged")), "Some run(s) did not converge")
  expect_equal(runProblems(fakeRun(runType = "Calib_nash", convergence = "Clb_converged")), character(0))
  expect_equal(runProblems(fakeRun(runType = "testOneRegi EUR", modelstat = "5: Locally Infes")),
               "testOneRegi does not return an optimal solution")
  expect_equal(runProblems(fakeRun(mif = "sumErr")), "Summation checks for some run(s) revealed some gaps")
  expect_equal(runProblems(fakeRun(inAppResults = "no")), "Some run(s) did not report correctly")
  expect_equal(runProblems(fakeRun(inAppResults = NULL)), "Some run(s) did not report correctly")
})

test_that("scenariosNotStarted compares planned and started scenarios by name", {
  started <- c("SSP2-NPi-AMT_2026-09-28_10.30.27", "default-AMT_2026-09-28_13.23.51", "testOneRegi-AMT")
  expect_equal(scenariosNotStarted(c("SSP2-NPi-AMT", "SSP3-NPi-AMT"), started), "SSP3-NPi-AMT")
  expect_equal(scenariosNotStarted(c("SSP2-NPi-AMT"), character(0)), c("default-AMT", "SSP2-NPi-AMT"))
})

test_that("amtSummary lists every problem once", {
  expect_equal(amtSummary(character(0)), "Summary: AMT runs look good.")
  expect_equal(amtSummary(c("a", "b", "a")), "Summary: a. b")
})

test_that("the README has the header, the table and the summary", {
  root <- withr::local_tempdir()
  paths <- list(output = file.path(root, "output"), model = root)
  run <- modelRun(fakeRemindRun(file.path(root, "output", "SSP2-NPi-AMT_2026-09-28_10.30.27")), onCluster = FALSE)
  evaluation <- list(summary = "Summary: AMT runs look good.", gitInfo = c("Tested commit: abc", "merges:"),
                     notStarted = "SSP3-NPi-AMT", testFullResult = "All tests pass")
  readme <- buildReadme(list(run), evaluation, list(cycle = "2026-09-30"), paths, compScen = TRUE)
  expect_equal(readme[1], "```")
  expect_equal(readme[length(readme)], "```")
  expect_true(any(grepl("^Run  ", readme)))
  expect_true(any(grepl("^SSP2-NPi-AMT_2026-09-28_10.30.27 .*1 hours .*Normal completion .*sumErr", readme)))
  expect_true("These scenarios did not start at all:" %in% readme)
  expect_true("SSP3-NPi-AMT" %in% readme)
  expect_true("Summary: AMT runs look good." %in% readme)

  message <- buildMattermostMessage(list(run), evaluation, list(cycle = "2026-09-30", log = "logs/x.log"), paths)
  expect_match(message, "`rs -A` returns:\n```\nFolder")
  expect_match(message, "Sanity checks")
  expect_match(message, "SSP3-NPi-AMT")
  expect_false(grepl("\033", message, fixed = TRUE))
})

test_that("evaluateTestFull reads and moves the test-full log", {
  root <- withr::local_tempdir()
  paths <- list(testFullLog = file.path(root, "test-full.log"), model = file.path(root, "remind"))
  dir.create(file.path(root, "remind", "tests"), recursive = TRUE)
  expect_match(evaluateTestFull(paths), "test-full.log not found")

  writeLines(c("Testing", "[ FAIL 0 | WARN 0 | SKIP 3 | PASS 120 ]"), paths$testFullLog)
  expect_match(suppressMessages(evaluateTestFull(paths)), "^All tests pass")
  expect_false(file.exists(paths$testFullLog))
  expect_length(list.files(file.path(root, "remind", "tests"), pattern = "^test-full-.*\\.log$"), 1)

  writeLines(c("[ FAIL 0 | WARN 1 | SKIP 3 | PASS 120 ]", "[ FAIL 2 | WARN 0 | SKIP 3 | PASS 120 ]"), paths$testFullLog)
  expect_match(suppressMessages(evaluateTestFull(paths)), "^Not all tests pass.*FAIL 2")

  writeLines("no result", paths$testFullLog)
  expect_match(suppressMessages(evaluateTestFull(paths)), "did not run properly")
})

test_that("archiveOldRuns moves runs older than 90 days by the date in their name", {
  root <- withr::local_tempdir()
  paths <- list(output = file.path(root, "output"), archive = file.path(root, "output", "archive"))
  old <- file.path(paths$output, "SSP2-NPi-AMT_2020-01-01_10.00.00")
  recent <- file.path(paths$output, paste0("SSP2-NPi-AMT_", Sys.Date() - 1, "_10.00.00"))
  other <- file.path(paths$output, "notest_2020-01-01_10.00.00")
  for (d in c(old, recent, other)) dir.create(d, recursive = TRUE)
  expect_message(moved <- archiveOldRuns(paths), "moving 1 run")
  expect_equal(basename(moved), basename(old))
  expect_true(dir.exists(file.path(paths$archive, basename(old))))
  expect_true(dir.exists(recent))
  expect_true(dir.exists(other))
  expect_length(suppressMessages(archiveOldRuns(paths)), 0)
})
