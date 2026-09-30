# A fake REMIND checkout with the files the AMT touches.
fakeRemindCheckout <- function(root) {
  model <- file.path(root, "remind")
  for (d in c("output", "config", "scripts/start", "modules/01_macro/singleSectorGr", "modules/01_macro/emptyReal",
              "magpie", "tests")) {
    dir.create(file.path(model, d), recursive = TRUE, showWarnings = FALSE)
  }
  writeLines(c("cfg <- list()", "cfg$force_download <- FALSE"), file.path(model, "config", "default.cfg"))
  writeLines(c("title;start", "SSP2-NPi;AMT", "default;AMT", "SSP2-never;AMT", "manual;1"),
             file.path(model, "config", "scenario_config.csv"))
  writeLines(c("selectScenarios <- function(settings, interactive = FALSE, startgroup = '1') {",
               "  settings[settings$start == startgroup, , drop = FALSE]", "}"),
             file.path(model, "scripts", "start", "selectScenarios.R"))
  writeLines('$include "./modules/01_macro/singleSectorGr/realization.gms"',
             file.path(model, "modules", "01_macro", "module.gms"))
  model
}

# Fake tools: git answers with a commit, squeue has no jobs, curl records its payload.
fakeAmtTools <- function(gitResetStatus = 0) {
  fakeBinDir(
    git = paste0("case \"$1\" in log) case \"$*\" in *--merges*) echo 'abc1234 Merge pull request #1 from x/y' ;; ",
                 "*) echo 3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f ;; esac ;; ",
                 "reset) exit ", gitResetStatus, " ;; esac"),
    Rscript = "echo fake start.R",
    make = "echo fake make",
    squeue = ":",
    sbatch = "echo Submitted batch job 4242",
    rsync = ":",
    curl = paste0("cfg=$(echo \"$*\" | sed 's/.*--config //'); ",
                  "cp \"$(grep data-binary \"$cfg\" | sed 's/.*@//; s/\"//')\" \"$(dirname \"$0\")/mattermost.json\""),
    envir = parent.frame()
  )
}

test_that("a full AMT cycle: start, evaluate, start again", {
  root <- withr::local_tempdir()
  model <- fakeRemindCheckout(root)
  bin <- fakeAmtTools()
  token <- "https://mattermost.example.org/hooks/token"

  expect_message(cycle <- modeltests(model, user = "tester", email = FALSE, mattermostToken = token), "start of cycle")
  expect_equal(cycle$phase, "started")
  expect_null(cycle$lastCommit)
  expect_equal(cycle$commit, "3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f")
  expect_equal(cycle$scenarios, c("SSP2-NPi-AMT", "default-AMT", "SSP2-never-AMT"))
  expect_equal(readRDS(file.path(model, "runcode.rds")), cycle$runcode)
  expect_false(dir.exists(file.path(model, "modules", "01_macro", "emptyReal")))
  expect_true(dir.exists(file.path(model, "modules", "01_macro", "singleSectorGr")))
  expect_equal(readLines(file.path(model, "config", "default.cfg"))[2], "cfg$force_download <- FALSE")
  calls <- fakeCalls(bin)
  expect_true(any(grepl("^Rscript start.R --testOneRegi titletag=AMT slurmConfig=--qos=priority", calls)))
  expect_true(any(grepl("^Rscript start.R startgroup=AMT titletag=AMT .*config/scenario_config.csv$", calls)))
  expect_true(any(grepl("^make test-full-slurm$", calls)))
  expect_equal(sum(grepl("^git reset --hard origin/develop", calls)), 2)
  log <- readLines(file.path(root, cycle$log))
  expect_true(any(grepl("start of AMT cycle", log)))
  expect_true(any(grepl("\\$ git pull", log)))
  expect_true(any(grepl("^fake start.R$", log)))
  expect_true(any(grepl("is now 'started'", log)))
  expect_false(dir.exists(file.path(root, "amt.lock")))

  # the runs of this cycle
  today <- format(Sys.Date(), "%Y-%m-%d")
  fakeRemindRun(file.path(model, "output", paste0("SSP2-NPi-AMT_", today, "_10.30.27")))
  fakeRemindRun(file.path(model, "output", paste0("default-AMT_", today, "_13.23.51")), title = "default-AMT",
                summationErrors = FALSE)
  fakeRemindRun(file.path(model, "output", "SSP2-NPi-AMT_2026-01-01_10.30.27"))
  writeLines("[ FAIL 0 | WARN 0 | SKIP 3 | PASS 120 ]", file.path(model, "test-full.log"))

  cycle <- suppressMessages(modeltests(model, user = "tester", email = FALSE, mattermostToken = token, compScen = TRUE))
  expect_equal(cycle$phase, "evaluated")
  expect_equal(readAmtState(amtPaths(model))$phase, "evaluated")
  readme <- readLines(file.path(root, "logs", paste0("README-", cycle$cycle, ".md")))
  expect_true(any(grepl("^Tested commit: 3f5e2a1b", readme)))
  expect_true("(no previous test recorded)" %in% readme)
  expect_true(any(grepl(paste0("^SSP2-NPi-AMT_", today, "_10.30.27 .*Normal completion"), readme)))
  expect_true("SSP2-never-AMT" %in% readme)
  summary <- grep("^Summary:", readme, value = TRUE)
  expect_match(summary, "Some run\\(s\\) did not converge")
  expect_match(summary, "Summation checks")
  expect_match(summary, "did not report correctly")
  expect_match(summary, "did not start")
  expect_true(file.exists(file.path(model, "tests", paste0("test-full-", today, ".log"))))
  expect_true(dir.exists(file.path(model, "output", "archive", "SSP2-NPi-AMT_2026-01-01_10.30.27")))
  payload <- jsonlite::fromJSON(file.path(bin, "mattermost.json"))
  expect_match(payload$text, "All tests pass in `make test-full`")
  expect_match(payload$text, "`rs -A` returns:")
  log <- readLines(file.path(root, cycle$log))
  expect_true(any(grepl("evaluate of AMT cycle", log)))
  expect_true(any(grepl("all AMT jobs finished", log)))
  expect_true(any(grepl("moving 1 run", log)))

  # the next call starts a new cycle with the evaluated commit as reference
  cycle <- suppressMessages(modeltests(model, user = "tester", email = FALSE))
  expect_equal(cycle$phase, "started")
  expect_equal(cycle$lastCommit, "3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f")
})

test_that("a failing start is recorded, reported and followed by an evaluation", {
  root <- withr::local_tempdir()
  model <- fakeRemindCheckout(root)
  bin <- fakeAmtTools(gitResetStatus = 1)
  token <- "https://mattermost.example.org/hooks/token"

  expect_error(suppressMessages(modeltests(model, user = "tester", email = FALSE, mattermostToken = token)), "git reset")
  state <- readAmtState(amtPaths(model))
  expect_equal(state$phase, "failed")
  expect_equal(state$failedPhase, "start")
  expect_match(state$error, "git reset")
  expect_match(jsonlite::fromJSON(file.path(bin, "mattermost.json"))$text, "failed in phase 'start'")
  expect_true(any(grepl("ERROR: git reset", readLines(file.path(root, state$log)))))
  expect_false(dir.exists(file.path(root, "amt.lock")))

  cycle <- suppressMessages(modeltests(model, user = "tester", email = FALSE))
  expect_equal(cycle$phase, "evaluated")
  readme <- readLines(file.path(root, "logs", paste0("README-", cycle$cycle, ".md")))
  expect_match(grep("^Summary:", readme, value = TRUE), "No runs started")
})

test_that("a cycle recorded as running is not touched by auto", {
  root <- withr::local_tempdir()
  model <- fakeRemindCheckout(root)
  fakeAmtTools()
  paths <- amtPaths(model)
  stale <- list(cycle = "2026-09-30", phase = "evaluating", log = "logs/amt-2026-09-30.log",
                runcode = ".*-AMT_2026-09-30", host = "h", pid = 1)
  suppressMessages(writeAmtState(paths, stale))
  expect_error(suppressMessages(modeltests(model, user = "tester", email = FALSE)), "recorded as 'evaluating'")
  expect_equal(readAmtState(paths)$phase, "evaluating")
  cycle <- suppressMessages(modeltests(model, user = "tester", email = FALSE, action = "evaluate"))
  expect_equal(cycle$phase, "evaluated")
})

test_that("a failure keeps the scenarios of the latest checkpoint", {
  root <- withr::local_tempdir()
  model <- fakeRemindCheckout(root)
  fakeBinDir(git = "case \"$1\" in log) echo 3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f ;; esac",
             Rscript = "echo fake start.R", make = "exit 0", squeue = ":")
  # the magpie checkout is missing: its git reset runs in a non-existent folder and fails
  unlink(file.path(model, "magpie"), recursive = TRUE)
  expect_error(suppressMessages(modeltests(model, user = "tester", email = FALSE)), "magpie")
  state <- readAmtState(amtPaths(model))
  expect_equal(state$phase, "failed")
  expect_equal(state$scenarios, c("SSP2-NPi-AMT", "default-AMT", "SSP2-never-AMT"))
  expect_equal(state$commit, "3f5e2a1b9c8d7e6f5a4b3c2d1e0f9a8b7c6d5e4f")
})

test_that("compareScenarios2 is submitted with quoted paths and the runtime is compared", {
  root <- withr::local_tempdir()
  bin <- fakeBinDir(sbatch = "echo Submitted batch job 4242", rsync = ":")
  # the runs are inspected in plain folders, then moved to a folder with a space to check the quoting
  plain <- list(output = file.path(root, "output"))
  paths <- list(output = file.path(root, "out put"), archive = file.path(root, "out put", "archive"))
  runNames <- c("SSP2-NPi-AMT_2026-09-28_10.30.27", "SSP2-NPi-AMT_2026-09-21_10.30.27")
  fakeRemindRun(file.path(plain$output, runNames[1]), runtimeSeconds = 7200)
  fakeRemindRun(file.path(plain$output, runNames[2]), runtimeSeconds = 3600)
  run <- modelRun(file.path(plain$output, runNames[1]), onCluster = FALSE)
  previous <- modelRun(file.path(plain$output, runNames[2]), onCluster = FALSE)
  dir.create(paths$output, recursive = TRUE)
  file.rename(file.path(plain$output, runNames), file.path(paths$output, runNames))
  run$path <- file.path(paths$output, runNames[1])
  previous$path <- file.path(paths$output, runNames[2])
  run$convergence <- previous$convergence <- "converged"
  testthat::local_mocked_bindings(previousRun = function(run, paths) previous)
  problems <- suppressMessages(compareWithPreviousRun(run, paths, compScen = TRUE))
  expect_equal(problems, "Check runtime! Have some scenarios become slower?")
  calls <- fakeCalls(bin)
  expect_true(any(grepl("^rsync -e ssh -av .*fulldata.gdx rse@", calls)))
  sbatch <- grep("^sbatch", calls, value = TRUE)
  expect_length(sbatch, 1)
  expect_match(sbatch, "--job-name=comp_with_SSP2-NPi-AMT_2026-09-21_10.30.27")
  expect_match(sbatch, paste0("--output=", run$path, "/comp_with_"))
  expect_match(sbatch, "--wrap=Rscript scripts/cs2/run_compareScenarios2.R outputdirs='.*out put.*' profileName=default")

  # not again once the PDF exists
  writeLines("", file.path(run$path, "comp_with_x.pdf"))
  suppressMessages(compareWithPreviousRun(run, paths, compScen = TRUE))
  expect_length(grep("^sbatch", fakeCalls(bin)), 1)
})

test_that("modeltests refuses other models and email without gitdir", {
  expect_error(modeltests(model = "MAgPIE"), "REMIND")
  expect_error(modeltests(email = TRUE), "gitdir")
})
