test_that("modelRun collects the status of a finished REMIND run", {
  root <- withr::local_tempdir()
  path <- fakeRemindRun(file.path(root, "output", "SSP2-NPi-AMT_2026-09-28_10.30.27"))
  run <- modelRun(path, onCluster = FALSE)

  expect_s3_class(run, "modelRun")
  expect_equal(run$name, "SSP2-NPi-AMT_2026-09-28_10.30.27")
  expect_equal(run$model, "REMIND")
  expect_equal(run$config$title, "SSP2-NPi-AMT")
  expect_equal(run$jobInSlurm, "NA")
  expect_equal(run$runType, "nash")
  expect_equal(run$modelstat, "NA")
  expect_null(run$inAppResults)
  expect_equal(run$mif, "sumErr")
  expect_equal(run$iterations, "26/100")
  expect_equal(run$runStatus, "Normal completion")
  expect_equal(run$warnings, "27")
  expect_equal(run$convergence, "NA")
  expect_equal(run$runtime, 3600)
  expect_equal(run$summationErrors, 2L)
  expect_equal(run$rangeErrors, 0)
  expect_equal(run$fixErrors, 0)
  expect_true(is.na(run$missingProjVars))
  expect_equal(run$status, "completed")
  expect_output(print(run), "SSP2-NPi-AMT_2026-09-28_10.30.27")
})

test_that("as.data.frame gives the getRunStatus row", {
  root <- withr::local_tempdir()
  path <- fakeRemindRun(file.path(root, "run"))
  row <- as.data.frame(modelRun(path, onCluster = FALSE))
  expect_equal(rownames(row), "run")
  expect_equal(names(row), c("jobInSLURM", "RunType", "modelstat", "Mif", "Iter", "RunStatus", "Warnings", "Conv",
                             "Runtime", "summationErrors", "rangeErrors", "fixErrors", "missingProjVars",
                             "projSummationErrors", "projSummationErrorsRegional"))
  expect_equal(row[["Iter"]], "26/100")
})

test_that("brief mode skips the costly fields", {
  root <- withr::local_tempdir()
  run <- modelRun(fakeRemindRun(file.path(root, "run")), detailed = FALSE, onCluster = FALSE)
  expect_false(any(c("warnings", "convergence", "runtime", "summationErrors") %in% names(run)))
  expect_equal(run$runStatus, "Normal completion")
})

test_that("a run without full.log or status line is reported as such", {
  root <- withr::local_tempdir()
  missing <- fakeRemindRun(file.path(root, "missing"))
  unlink(file.path(missing, "full.log"))
  expect_equal(modelRun(missing, onCluster = FALSE)$runStatus, "full.log missing")
  expect_equal(modelRun(missing, onCluster = FALSE)$status, "error")

  interrupted <- fakeRemindRun(file.path(root, "interrupted"), status = NULL)
  expect_equal(modelRun(interrupted, onCluster = FALSE)$runStatus, "Run interrupted")
  expect_equal(modelRun(interrupted, onCluster = FALSE)$iterations, "26/100")
})

test_that("a folder without any run files gives NA values", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "empty"))
  run <- modelRun(file.path(root, "empty"), onCluster = FALSE)
  expect_equal(run$runType, "NA")
  expect_equal(run$mif, "NA")
  expect_equal(run$warnings, "NA")
  expect_true(is.na(run$runtime))
  expect_true(is.na(run$model))
  expect_false("summationErrors" %in% names(run))
})

test_that("getRunStatus combines runs into a table, newest first", {
  root <- withr::local_tempdir()
  old <- fakeRemindRun(file.path(root, "old"), warnings = 1)
  new <- fakeRemindRun(file.path(root, "new"), warnings = 2)
  Sys.setFileTime(old, "2026-01-01 00:00:00")
  Sys.setFileTime(new, "2026-02-01 00:00:00")
  status <- getRunStatus(c(old, new), user = "someone")
  expect_equal(rownames(status), c("new", "old"))
  expect_equal(status[["Warnings"]], c("2", "1"))
  expect_equal(rownames(getRunStatus(c(old, new), sort = "none", user = "someone")), c("old", "new"))
})

test_that("the coarse status follows the SLURM state on the cluster", {
  expect_equal(coarseStatus("NA", "priority pending", TRUE), "pending")
  expect_equal(coarseStatus("Run in progress", "priority", TRUE), "running")
  expect_equal(coarseStatus("Normal completion", "no", TRUE), "completed")
  expect_equal(coarseStatus("Execution error", "no", TRUE), "error")
  expect_equal(coarseStatus("NA", "no", TRUE), "unknown")
  expect_equal(coarseStatus("Normal completion", "NA", FALSE), "completed")
})
