fakeAmtRoot <- function() {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(root, "remind", "output"), recursive = TRUE)
  amtPaths(file.path(root, "remind"))
}

test_that("amtPaths derives everything from the REMIND folder", {
  paths <- fakeAmtRoot()
  expect_equal(paths$root, dirname(paths$model))
  expect_equal(basename(paths$state), "amt-state.json")
  expect_equal(paths$archive, file.path(paths$model, "output", "archive"))
})

test_that("the cycle record survives a write/read round trip", {
  paths <- fakeAmtRoot()
  cycle <- newAmtCycle(NULL)
  expect_equal(cycle$phase, "starting")
  expect_match(cycle$runcode, "^\\.\\*-AMT_[0-9-]+\\|\\.\\*-AMT_[0-9-]+$")
  cycle$scenarios <- c("a-AMT", "b-AMT")
  cycle$commit <- "abc"
  expect_message(writeAmtState(paths, cycle), "state: cycle")
  state <- readAmtState(paths)
  expect_equal(state$scenarios, c("a-AMT", "b-AMT"))
  expect_equal(state$commit, "abc")
  expect_equal(state$phase, "starting")
  expect_true(nzchar(state$updatedAt))
})

test_that("nextAmtPhase alternates between start and evaluate", {
  expect_equal(nextAmtPhase(NULL, "auto"), "start")
  expect_equal(nextAmtPhase(list(phase = "evaluated"), "auto"), "start")
  expect_equal(nextAmtPhase(list(phase = "started"), "auto"), "evaluate")
  expect_equal(nextAmtPhase(list(phase = "failed", failedPhase = "start"), "auto"), "evaluate")
  expect_equal(nextAmtPhase(list(phase = "failed", failedPhase = "evaluate"), "auto"), "start")
  expect_error(nextAmtPhase(list(phase = "evaluating", cycle = "x"), "auto"), "recorded as 'evaluating'")
  expect_error(nextAmtPhase(list(phase = "starting", cycle = "x"), "auto"), "recorded as 'starting'")
  expect_equal(nextAmtPhase(list(phase = "starting"), "evaluate"), "evaluate")
})

test_that("a new cycle carries the commit of the last evaluated one", {
  expect_equal(newAmtCycle(list(phase = "evaluated", commit = "new", lastCommit = "old"))$lastCommit, "new")
  expect_equal(newAmtCycle(list(phase = "evaluated", lastCommit = "old"))$lastCommit, "old")
  expect_equal(newAmtCycle(list(phase = "failed", commit = "new", lastCommit = "old"))$lastCommit, "old")
})

test_that("the state of the old modeltests is migrated", {
  paths <- fakeAmtRoot()
  expect_null(readAmtState(paths))

  saveRDS("13f60fdd", paths$legacyLastCommit)
  expect_equal(readAmtState(paths)$phase, "evaluated")
  expect_equal(readAmtState(paths)$lastCommit, "13f60fdd")
  expect_equal(readAmtState(paths)$commit, "13f60fdd")
  expect_equal(newAmtCycle(readAmtState(paths))$lastCommit, "13f60fdd")

  writeLines("next:start", paths$legacyStatus)
  expect_equal(readAmtState(paths)$phase, "evaluated")

  writeLines("next:evaluate", paths$legacyStatus)
  expect_error(readAmtState(paths), "runcode.rds")
  saveRDS(".*-AMT_2026-09-25|.*-AMT_2026-09-26", paths$runcode)
  saveRDS(data.frame(x = 1:2, row.names = c("a-AMT", "b-AMT")), paths$legacyRunsToStart)
  state <- readAmtState(paths)
  expect_equal(state$phase, "started")
  expect_equal(state$cycle, "2026-09-25")
  expect_equal(state$scenarios, c("a-AMT", "b-AMT"))
  expect_equal(state$lastCommit, "13f60fdd")
  expect_equal(nextAmtPhase(state, "auto"), "evaluate")

  writeLines("evaluateRuns() is running or stopped due to an error", paths$legacyStatus)
  expect_error(readAmtState(paths), "explicitly")
  expect_equal(readAmtState(paths, action = "evaluate")$phase, "started")
  expect_equal(readAmtState(paths, action = "evaluate")$runcode, ".*-AMT_2026-09-25|.*-AMT_2026-09-26")
  expect_equal(readAmtState(paths, action = "start")$phase, "evaluated")

  # once a record exists, the old files are ignored
  suppressMessages(writeAmtState(paths, list(cycle = "2026-10-02", phase = "evaluated")))
  expect_equal(readAmtState(paths)$cycle, "2026-10-02")
})

test_that("the lock keeps a second process out and is removed for a dead one", {
  paths <- fakeAmtRoot()
  lock <- acquireAmtLock(paths)
  expect_true(dir.exists(paths$lock))
  expect_error(acquireAmtLock(paths), "another modeltests\\(\\) is running")
  releaseAmtLock(lock)
  expect_false(dir.exists(paths$lock))

  dir.create(paths$lock)
  writeLines(c(Sys.info()[["nodename"]], "999999999", "2026-01-01 00:00:00"), file.path(paths$lock, "owner"))
  skip_if_not(dir.exists("/proc"))
  expect_message(acquireAmtLock(paths), "left behind by the dead process")
  expect_equal(readLines(file.path(paths$lock, "owner"))[2], as.character(Sys.getpid()))
  expect_length(list.files(paths$root, pattern = "^amt\\.lock\\.stale"), 0)
  releaseAmtLock(paths$lock)

  dir.create(paths$lock)
  writeLines(c("otherhost", "1", "2026-01-01 00:00:00"), file.path(paths$lock, "owner"))
  expect_error(acquireAmtLock(paths), "held by host otherhost")
})
