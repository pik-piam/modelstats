# Evaluation phase of the automated model tests (AMT): wait for the runs, inspect
# them, compare with the previous test, report.

amtArchiveAfterDays <- 90

amtEvaluate <- function(paths, cycle, settings) {
  waitForRuns(paths$model, settings$user, settings$pollInterval)
  if (is.null(cycle$commit)) cycle$commit <- gitHead(paths$model)

  runNames <- grep(cycle$runcode, list.dirs(paths$output, full.names = FALSE, recursive = FALSE), value = TRUE)
  amtLog("evaluating ", length(runNames), " run(s): ", paste(runNames, collapse = ", "))
  runs <- lapply(file.path(paths$output, runNames), modelRun, user = settings$user)
  names(runs) <- runNames

  problems <- character(0)
  for (run in runs) {
    problems <- c(problems, runProblems(run))
    problems <- c(problems, compareWithPreviousRun(run, paths, settings$compScen))
  }
  notStarted <- scenariosNotStarted(cycle$scenarios, runNames)
  if (length(notStarted) > 0) {
    amtLog("scenarios that did not start: ", paste(notStarted, collapse = ", "))
    problems <- c(problems, "Some scenario(s) did not start")
  }
  if (length(runNames) == 0) problems <- c(problems, "No runs started")
  testFullResult <- evaluateTestFull(paths)
  archiveOldRuns(paths)

  evaluation <- list(
    summary = amtSummary(problems),
    testFullResult = testFullResult,
    notStarted = notStarted,
    gitInfo = gitInfo(paths$model, cycle)
  )
  amtLog(evaluation$summary)
  readme <- buildReadme(runs, evaluation, cycle, paths, settings$compScen)
  readmeFile <- file.path(paths$logs, paste0("README-", cycle$cycle, ".md"))
  writeLines(readme, readmeFile)
  amtLog("README written to ", readmeFile)
  if (settings$email) publishReadme(readmeFile, settings$gitdir)
  if (!is.null(settings$mattermostToken)) {
    amtLog("sending the report to Mattermost")
    sendMattermostMessage(buildMattermostMessage(runs, evaluation, cycle, paths), settings$mattermostToken)
  }

  cycle$phase <- "evaluated"
  cycle$evaluatedAt <- timeStamp()
  writeAmtState(paths, cycle)
}

# Block until no SLURM job of `user` works in the model folder or a run folder below it
# (the AMT runs and the coupled test of `make test-full-slurm`) any more.
waitForRuns <- function(model, user, pollInterval) {
  amtLog("waiting for the AMT jobs of ", user, " to finish (checking every ", pollInterval, " s)")
  failures <- 0
  lastCount <- -1
  repeat {
    jobs <- runCommand("squeue", c("-u", shQuote(user), "-h", "-o", shQuote("%i %q %T %C %M %j %V %L %e %Z")),
                       capture = TRUE, quiet = TRUE)
    if (jobs$status != 0) {
      failures <- failures + 1
      amtLog("WARNING: squeue failed with exit status ", jobs$status, " (", failures, " times in a row)")
      if (failures > 3) stop("squeue failed more than 3 times in a row")
    } else {
      failures <- 0
      workDirs <- sub("^.* ", "", jobs$output)
      running <- jobs$output[workDirs == model | startsWith(workDirs, paste0(model, "/"))]
      if (length(running) == 0) break
      if (length(running) != lastCount) amtLog(length(running), " job(s) still running")
      lastCount <- length(running)
    }
    Sys.sleep(pollInterval)
  }
  amtLog("all AMT jobs finished")
}

# Tested commit and the merges since the commit of the previous evaluated test.
gitInfo <- function(model, cycle) {
  merges <- if (is.null(cycle$lastCommit)) {
    "(no previous test recorded)"
  } else {
    log <- runCommand("git", c("log", "--merges", "--pretty=oneline", paste0(cycle$lastCommit, "..", cycle$commit),
                               "--abbrev-commit"), cwd = model, capture = TRUE)
    grep("Merge pull request", log$output, value = TRUE)
  }
  c(paste("Tested commit:", cycle$commit),
    paste("The test of", format(Sys.time(), "%Y-%m-%d"), "contains these merges:"),
    merges)
}

# For a finished run: publish the gdx of SSP2-NPi-AMT, compare the runtime with the
# previous run of the same scenario and start compareScenarios2 against it.
# Returns the problems found (character vector).
compareWithPreviousRun <- function(run, paths, compScen) {
  if (!run$convergence %in% c(amtConverged, "not_converged")) {
    amtLog(run$name, " did not converge, skipping the comparison with the previous run")
    return(character(0))
  }
  if (grepl("SSP2-NPi-AMT", run$name) && run$convergence %in% amtConverged) publishExampleGdx(run)

  previous <- previousRun(run, paths)
  if (is.null(previous)) {
    amtLog("no previous converged run of ", run$config$title, " found")
    return(character(0))
  }
  amtLog("previous run of ", run$config$title, ": ", previous$name)
  problems <- character(0)
  if (run$convergence %in% amtConverged && isTRUE(run$runtime > 1.25 * previous$runtime)) {
    amtLog(run$name, " took ", round(run$runtime / 3600, 1), " h, ", previous$name, " took ",
           round(previous$runtime / 3600, 1), " h")
    problems <- c(problems, "Check runtime! Have some scenarios become slower?")
  }
  if (compScen) startCompareScenarios(run, previous)
  problems
}

# The most recent converged run of the same scenario with a mif, in output/ or output/archive/.
previousRun <- function(run, paths) {
  pattern <- paste0("^", gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", run$config$title), amtRunNamePattern)
  candidates <- c(list.dirs(paths$output, recursive = FALSE), list.dirs(paths$archive, recursive = FALSE))
  candidates <- candidates[grepl(pattern, basename(candidates)) & basename(candidates) < run$name]
  for (candidate in candidates[order(basename(candidates), decreasing = TRUE)]) {
    previous <- try(modelRun(candidate), silent = TRUE)
    if (inherits(previous, "try-error")) {
      amtLog("cannot read ", candidate, ": ", conditionMessage(attr(previous, "condition")))
      next
    }
    if (previous$convergence %in% amtConverged && previous$mif %in% c("yes", "sumErr")) return(previous)
  }
  NULL
}

# The fulldata.gdx of a converged SSP2-NPi-AMT run is the test input of remind2::convGDX2MIF.
publishExampleGdx <- function(run) {
  target <- "rse@rse.pik-potsdam.de:/webservice/data/example/remind2_test-convGDX2MIF_SSP2-NPi-AMT.gdx"
  amtLog("updating the gdx on the RSE server with the fulldata.gdx of ", run$name)
  runCommand("rsync", c("-e", "ssh", "-av", shQuote(file.path(run$path, "fulldata.gdx")), target))
}

# Submit compareScenarios2 for the run and its predecessor (once; the PDF lands in the run folder).
startCompareScenarios <- function(run, previous) {
  mifs <- vapply(list(run, previous), function(r) remindMifFile(r$path, r$config), "")
  if (!all(file.exists(mifs))) {
    amtLog("no compareScenarios2 for ", run$name, ": mif missing")
    return(invisible(FALSE))
  }
  if (length(list.files(run$path, pattern = "^comp_with_.*\\.pdf$")) > 0) {
    amtLog("compareScenarios2 PDF already exists for ", run$name)
    return(invisible(FALSE))
  }
  outFileName <- paste0("comp_with_", previous$name)
  # the wrapped command is run by sbatch through a second shell: its values are quoted for that one
  script <- paste("Rscript scripts/cs2/run_compareScenarios2.R",
                  shellArg("outputdirs", paste(c(run$path, previous$path), collapse = ",")),
                  "profileName=default", shellArg("outFileName", outFileName),
                  "; mv", shQuote(paste0(outFileName, ".pdf")), shQuote(run$path))
  jobLog <- file.path(run$path, paste0(outFileName, ".out"))
  args <- c("--qos=standby", shellArg("--job-name", outFileName), "--comment=compareScenarios2",
            shellArg("--output", jobLog), shellArg("--error", jobLog),
            "--mail-type=END", "--time=200", "--mem-per-cpu=8000", shellArg("--wrap", script))
  # run_compareScenarios2.R works only if called from the main folder
  runCommand("sbatch", args, cwd = run$config$remind_folder)
  invisible(TRUE)
}

# Result of `make test-full` from test-full.log, which is moved to <model>/tests/.
evaluateTestFull <- function(paths) {
  log <- paths$testFullLog
  if (!file.exists(log)) return("Could not check for the results of `make test-full`, test-full.log not found")
  status <- tail(grep("\\[ FAIL", readLines(log, warn = FALSE), value = TRUE), 1)
  kept <- file.path(paths$model, "tests", paste0("test-full-", format(file.info(log)$mtime, "%Y-%m-%d"), ".log"))
  dir.create(dirname(kept), showWarnings = FALSE)
  if (file.rename(log, kept)) {
    amtLog("moved ", log, " to ", kept)
  } else {
    amtLog("WARNING: could not move ", log, " to ", kept)
  }
  if (length(status) == 0) {
    paste("`make test-full` did not run properly. Check", kept)
  } else if (!(grepl("FAIL 0", status) && grepl("WARN 0", status))) {
    paste0("Not all tests pass in `make test-full`: ", status, ". Check `", kept, "`")
  } else {
    paste("All tests pass in `make test-full`:", status)
  }
}

# Move AMT runs whose folder date is older than amtArchiveAfterDays to output/archive/.
archiveOldRuns <- function(paths) {
  runs <- list.dirs(paths$output, recursive = FALSE)
  runs <- runs[grepl("AMT", basename(runs))]
  dates <- as.Date(sub(".*_([0-9]{4}-[0-9]{2}-[0-9]{2})_.*$", "\\1", basename(runs)), optional = TRUE)
  old <- runs[!is.na(dates) & dates < Sys.Date() - amtArchiveAfterDays]
  if (length(old) == 0) return(invisible(character(0)))
  amtLog("moving ", length(old), " run(s) older than ", amtArchiveAfterDays, " days to ", paths$archive, ": ",
         paste(basename(old), collapse = ", "))
  dir.create(paths$archive, showWarnings = FALSE)
  moved <- file.rename(old, file.path(paths$archive, basename(old)))
  if (!all(moved)) amtLog("WARNING: could not move ", paste(basename(old[!moved]), collapse = ", "))
  invisible(old[moved])
}

# Commit README.md to the testing_suite repository (its gitlab hook sends the email).
publishReadme <- function(readmeFile, gitdir) {
  if (is.null(gitdir)) stop("email = TRUE needs the gitdir with the clone of the testing_suite repository")
  amtLog("publishing the README in ", gitdir)
  runCommandOrStop("git", c("reset", "--hard", "origin/master"), cwd = gitdir, what = "git reset in gitdir")
  runCommandOrStop("git", "pull", cwd = gitdir, what = "git pull in gitdir")
  file.copy(readmeFile, file.path(gitdir, "README.md"), overwrite = TRUE)
  runCommandOrStop("git", c("add", "README.md"), cwd = gitdir, what = "git add")
  runCommand("git", c("commit", "-m", shQuote("Automated Test Results")), cwd = gitdir)
  runCommandOrStop("git", "push", cwd = gitdir, what = "git push")
}
