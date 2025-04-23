#' modeltests
#'
#' Runs a group of tests for a specific model. A "config/scenario_config.csv" file
#' (relative) to the main folder has to exist in the model, describing the
#' scenarios to be tested. Also, a gitlab repository is needed to push
#' the generated README.md for documentation and automated reporting
#' of test results.
#'
#' @param mydir Path to the folder where the model is found
#' @param gitdir path to the git clone that sends the report via email
#' @param user the user that starts the job and commits the changes
#' @param model Model name
#' @param email whether an email notification will be send or not
#' @param mattermostToken token used for mattermost notifications
#' @param compScen whether compScen has to run or not
#'
#' @author Anastasis Giannousakis, David Klein
#' @seealso \code{\link[lucode2]{package2readme}}
#' @importFrom utils read.csv2
#' @importFrom dplyr mutate pull filter %>%
#' @importFrom lucode2 sendmail
#' @importFrom rlang .data
#' @export
modeltests <- function(mydir = ".",
                       gitdir = NULL,
                       model = NULL,
                       user = NULL,
                       email = TRUE,
                       compScen = TRUE,
                       mattermostToken = NULL) {
  withr::local_dir(mydir)

  message("\n=========================================================\n",
          "Begin of AMT procedure ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " in ", mydir,
          "\n=========================================================\n")

  if (readLines("../.testsstatus") == "next:start") {
    message("Found 'next:start' in ", normalizePath("../.testsstatus"), "\nCalling 'startRuns'")
    startRuns(model = model, user = user, mydir = mydir)
    # make sure next call will evaluate runs
    message("Writing 'next:evaluate' to ", normalizePath("../.testsstatus"))
    writeLines("next:evaluate", con = "../.testsstatus")
  } else if (readLines("../.testsstatus") == "next:evaluate") {
    message("Found 'next:evaluate' in ", normalizePath("../.testsstatus"), "\nCalling 'evaluateRuns'")
    withr::with_dir("output", {
      evaluateRuns(model = model, mydir = mydir, compScen = compScen, email = email,
                   mattermostToken = mattermostToken, gitdir = gitdir, user = user)
    })
    # make sure next call will start runs
    message("Writing 'next:start' to ", normalizePath("../.testsstatus"))
    writeLines("next:start", con = "../.testsstatus")
  } else {
    message("Found ", readLines("../.testsstatus"), " in ", normalizePath("../.testsstatus"), ". Doing nothing")
  }
}

.mattermostBotMessage <- function(message, token) {
  cmd <- paste0("curl -i -X POST -H 'Content-Type: application/json' -d '",
                '{"text": "', message, '"', "}' ",
                token)
  system(cmd, intern = TRUE)
}

.readRuntime <- function(x) {
  stats <- NULL # because of no visible binding note
  load(paste0(x, "/runstatistics.rda"))
  return(stats$runtime)
}

deleteEmptyRealizationFolders <- function() {
  moduleFiles <- system("find modules/ -name 'module.gms'", intern = TRUE)
  for (moduleFile in moduleFiles) {
    moduleDirectory <- sub("module.gms$", "", moduleFile)
    realizations <- grep("realization.gms", readLines(moduleFile), value = TRUE)
    emptyRealizations <- setdiff(
                                 dir(moduleDirectory),
                                 c("module.gms",
                                   "input",
                                   sub("/realization.gms\"$", "", sub("^.*.modules/[0-9a-zA-Z_]{1,}/", "",
                                                                      realizations))))
    if (length(emptyRealizations) > 0) {
      unlink(paste0(moduleDirectory, emptyRealizations), recursive = TRUE)
    }
  }
}

startRuns <- function(model, mydir, user) {

  if (is.null(model)) stop("Model cannot be NULL")
  
  runcode <- paste0(".*-AMT_",
                    format(Sys.Date(), "%Y-%m-%d"),
                    "|",
                    ".*-AMT_",
                    as.Date(format(Sys.Date(), "%Y-%m-%d")) + 1)

  system("git reset --hard origin/develop && git pull")
  # Force downloading of the input data in the first run
  system("sed -i 's/cfg$force_download <- FALSE/cfg$force_download <- TRUE/' config/default.cfg")

  # empty realization folders break the models, but can be left over because git does not
  # delete empty folders if all files in the folder are deleted.
  deleteEmptyRealizationFolders()

  if (model == "REMIND") {
    
    # Do not ask when installing dependencies in piamenv::fixDeps
    Sys.setenv(autoRenvFixDeps = "TRUE") 
    
    message("Configuring and starting single testOneRegi-AMT")
    system(paste("Rscript start.R --testOneRegi titletag=AMT",
                 "slurmConfig=\"--qos=priority --nodes=1 --tasks-per-node=1 --wait --time=2:00:00\""))

    message("Configuring and starting bundle of AMT runs")
    # do not download input data every run, reset force_download
    system("sed -i 's/cfg$force_download <- TRUE/cfg$force_download <- FALSE/' config/default.cfg")

    # now start actual test runs
    system(paste0("Rscript start.R ",
                  "startgroup=AMT titletag=AMT ",
                  "slurmConfig=\"--qos=standby --nodes=1 --tasks-per-node=12 --time=36:00:00\" ",
                  "config/scenario_config.csv"))

    # Create and save a list of runs that should have been started in order
    # to determine later which runs were not started
    settings <- read.csv2("config/scenario_config.csv",
                          stringsAsFactors = FALSE,
                          row.names = 1,
                          comment.char = "#",
                          na.strings = "")

    # Source everything from scripts/start so that all functions are available everywhere
    invisible(sapply(list.files("scripts/start", pattern = "\\.R$", full.names = TRUE), source)) # nolint
    selectScenarios <- NA # avoid buildLibrary to fail with "no visible global function"
    runsToStart <- selectScenarios(settings = settings, interactive = FALSE, startgroup = "AMT")
    row.names(runsToStart) <- paste0(row.names(runsToStart), "-AMT")
    saveRDS(runsToStart, file = paste0(mydir, "/runsToStart.rds"))

    # start make test-full
    # 1. pull changes to magpie develop
    withr::with_dir("magpie", {
      system("git reset --hard origin/develop && git pull")
    })
    # 2. execute test
    system("make test-full-slurm")

  } else if (model == "MAgPIE") {
    # default run to download input data
    system("Rscript start.R runscripts=default submit='SLURM priority'")
    # wait for default run to finish
    repeat {
      Sys.sleep(300)
      jobsInSlurm <- system(paste0("squeue -u ",
                                   user, " -h -o '%i %q %T %C %M %j %V %L %e %Z'"), intern = TRUE)
      if (!any(grepl(paste0(mydir, "$"), jobsInSlurm))) {
        break
      }
    }
    # now start actual test runs
    system("Rscript start.R runscripts=test_runs submit='SLURM standby'")
  }

  saveRDS(runcode, file = paste0(mydir, "/runcode.rds"))
  message("Function 'startRuns' finished.")
}

evaluateRuns <- function(model, # nolint: cyclocomp_linter.
                         mydir, compScen, email, mattermostToken, gitdir, user) {
  message("Current working directory ", normalizePath("."))
  message("Writing 'evaluateRuns() is running or stopped due to an error' to ", normalizePath(paste0(mydir, "../.testsstatus")))
  writeLines("evaluateRuns() is running or stopped due to an error", con = paste0(mydir, "../.testsstatus"))

  lastCommit <- readRDS(paste0(mydir, "/lastcommit.rds"))
  errorList <- NULL
  today <- format(Sys.time(), "%Y-%m-%d")

  # wait for all AMT runs to finish
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - waiting for all AMT runs to finish.")
  errCount <- 0
  repeat {
    jobsInSlurm <- system(paste0("squeue -u ", user, " -h -o '%i %q %T %C %M %j %V %L %e %Z'"), intern = TRUE)
    if (isTRUE(attributes(jobsInSlurm)$status > 0)) {
      # count how often squeue fails
      errCount <- errCount + 1
      if (errCount > 3) stop("squeue had exit status > 0 more than 3 times in a row.")
    } else if (!any(grepl(mydir, jobsInSlurm))) {
      break
    } else {
      # reset if squeue was successful
      errCount <- 0
    }
    Sys.sleep(600)
  }
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - all AMT runs finished.")

  message("Compiling the README.md to be committed to testing_suite repo.")
  commitTested <- sub("commit ", "", system("git log -1", intern = TRUE)[[1]])
  commitsSinceLastTest <- system(paste0("git log --merges --pretty=oneline ",
                                        lastCommit, "..", commitTested,
                                        " --abbrev-commit | grep 'Merge pull request'"), intern = TRUE)
  readme <- file.path(tempdir(), "README.md")
  write("```", readme)
  write(paste0("This is the result of the automated model tests for ", model, " on ", today, "."),
        readme, append = TRUE)
  write(paste0("Path to runs: ", mydir, "output/"), readme, append = TRUE)
  write(paste0("Direct and interactive access to plots: open shinyResults::appResults, then use '",
               ifelse(model == "MAgPIE", "weeklyTests", "AMT"),
               "' as keyword in the title search"), readme, append = TRUE)
  if (model == "REMIND" && compScen == TRUE) {
    write(paste0("Each run folder below should contain a compareScenarios PDF comparing the output of the current and",
                 " the last successful tests (comp_with_RUN-DATE.pdf)"), readme, append = TRUE)
  }
  write(paste0("Note: 'Mif' = 'no' indicates a possible error in output generation, please check!"),
        readme, append = TRUE)
  write(paste0("If you are currently viewing the email: Overview of the last test is in red, ",
               "and of the current test in green"), readme, append = TRUE)

  gitInfo <- c(paste("Tested commit:", commitTested),
               paste("The test of", today, "contains these merges:"),
               commitsSinceLastTest)
  write(gitInfo, readme, append = TRUE)

  isdir <- NULL
  if (model != "MAgPIE") {
    gRSold <- NULL
    if (file.exists("gRS.rds")) {
      gRSold <- readRDS("gRS.rds")
    }
    try(gRS <- rbind(gRSold, getRunStatus(setdiff(dir(), rownames(gRSold)))))
    if (exists("gRS")) {
      saveRDS(gRS, "gRS.rds")
    } else {
      gRS <- getRunStatus(dir())
      saveRDS(gRS, "gRS.rds")
    }
    # List all folders (in 'output') and keep folders only that match the current AMT name (runcode)
    runcode <- readRDS(paste0(mydir, "/runcode.rds"))
    runsStarted <- grep(runcode, list.dirs(full.names = FALSE, recursive = FALSE), value = TRUE)
  } else {
    # if model is MAgPIE ignore runcode and find run folders based on their creation time (last 3 days)
    gRS <- getRunStatus(dir()) # this takes a long time
    # this happens because test run names are hard coded in MAgPIE scripts and thus not readable
    runsStarted <- file.info(dir())
    runsStarted <- filter(runsStarted, isdir == TRUE)
    threeDaysAgo <- Sys.Date() - 3
    runsStarted <- rownames(runsStarted[which(as.Date(format(runsStarted[, "ctime"], "%Y-%m-%d")) > threeDaysAgo), ])
  }

  colSep <- "  "
  coltitles <- c(
    "Run                                           ", "Runtime    ", "", "RunType    ", "RunStatus         ",
    "Iter            ", "Conv                 ", "modelstat          ", "Mif   ", "AppResults"
  )
  write(paste(coltitles, collapse = colSep), readme, append = TRUE)
  lenCols <- c(nchar(coltitles)[-length(coltitles)], 3)

  # add to data changelog
  changelog <- NULL
  if (model == "MAgPIE") {
    for (i in runsStarted[startsWith(runsStarted, "default_")]) {
      changelogVariables <- c(
        lucEmisRaw = "Emissions|CO2|Land RAW|+|Land-use Change",
        tau = "Productivity|Landuse Intensity Indicator Tau",
        cropland = "Resources|Land Cover|+|Cropland",
        irrigated = "Resources|Land Cover|Cropland|Area actually irrigated",
        pasture = "Resources|Land Cover|+|Pastures and Rangelands",
        forest = "Resources|Land Cover|+|Forest",
        other = "Resources|Land Cover|+|Other Land",
        # production: in contrast to all other indicators here, this should
        # be robust to calibration issues, but indicate changes in demand/trade
        production = "Production",
        costs = "Costs",
        foodExp = "Household Expenditure|Food|Expenditure"
      )
      changelog <- file.path(tempdir(), "data-changelog.csv")
      file.copy(file.path(gitdir, "data-changelog.csv"), changelog)
      try({
        magpie4::addToDataChangelog(report = readRDS(file.path(i, "report.rds")),
                                    changelog = changelog,
                                    versionId = i,
                                    years = c(2020, 2050, 2100),
                                    variables = changelogVariables)
      })
    }
  }

  message("Starting analysis for the list of the following runs:\n", paste0(runsStarted, collapse = "\n"))
  for (i in runsStarted) {
    grsi <- getRunStatus(i)
    if ("Runtime" %in% names(grsi) && is.numeric(grsi[["Runtime"]])) {
      grsi["Runtime"] <- format(round(make_difftime(second = grsi[["Runtime"]]), 1))
    }
    write(sub("\n$", "", printOutput(grsi, lenCols = lenCols, colSep = colSep)), readme, append = TRUE)
    if (model == "REMIND") {
      if (!grepl("Calib_nash|testOneRegi", grsi[, "RunType"])
          && !grsi[, "Conv"] %in% c("converged", "converged (had INFES)")) {
        errorList <- c(errorList, "Some run(s) did not converge")
      }
      if (grsi[, "RunType"] == "Calib_nash" && grsi[, "Conv"] != "Clb_converged") {
        errorList <- c(errorList, "Some run(s) did not converge")
      }
      if (grepl("testOneRegi", grsi[, "RunType"]) && grsi[, "modelstat"] != "2: Locally Optimal") {
        errorList <- c(errorList, "testOneRegi does not return an optimal solution")
      }
      if (grsi[, "Mif"] == "sumErr") {
        errorList <- c(errorList, "Summation checks for some run(s) revealed some gaps")
      }
    } else if (model == "MAgPIE") {
      if (paste0(unique(unlist(strsplit(gsub("[^0-9]", "", grsi[, "modelstat"]), split = ""))), collapse = "") != "2") {
        errorList <- c(errorList, "Some run(s) did not converge")
      }
    }
    if (grsi[, "runInAppResults"] != "yes") errorList <- c(errorList, "Some run(s) did not report correctly")
    # For a successful run compare runtime and results with previous AMT run
    # Since there is no column 'Conv' for MAgPIE runs the following will only be performed for REMIND runs
    if (grsi[, "Conv"] %in% c("converged", "converged (had INFES)", "not_converged")) {
      withr::local_dir(i)
      message("Changed to ", normalizePath("."))
      # Use the fulldata.gdx of a successful SSP2-NPi-AMT to update the gdx on the
      # RSE server that is used for testing convGDX2MIF
      if (grepl("SSP2-NPi-AMT", rownames(grsi)) && grsi[, "Conv"] %in% c("converged", "converged (had INFES)")) {
        gdxOnRseServer <- paste0("rse@rse.pik-potsdam.de:/webservice/data/example/",
                                 "remind2_test-convGDX2MIF_SSP2-NPi-AMT.gdx")
        message(paste("Updating the gdx on the RSE server", gdxOnRseServer, "with the fulldata.gdx of", rownames(grsi)))
        system(paste("rsync -e ssh -av fulldata.gdx", gdxOnRseServer))
      }
      cfg <- NULL
      load("config.Rdata")
      sameRuns <- gRS %>% filter(.data$Conv %in% c("converged", "converged (had INFES)"), # runs have to be converged
                                 .data$Mif %in% c("yes", "sumErr"),                       # need to have mifs
                                 grepl(cfg$title, rownames(gRS)),                         # must be the same scenario
                                 !rownames(gRS) %in% basename(cfg$results_folder)) %>%    # but not the current run
                                 rownames()
      if (length(sameRuns) > 0) {
        lastRun <- max(sameRuns[sameRuns < basename(cfg$results_folder)])
        # If it was too long ago, the lastRun may have been moved to the archive in the meantime.
        if (!file.exists(file.path("..", lastRun))) lastRun <- file.path("archive", lastRun)
        fullPathToThisRun <- normalizePath(".")
        fullPathToLastRun <- normalizePath(file.path("..", lastRun))
        # compare runtime for converged run only (skip if not_converged)
        if (grsi[, "Conv"] %in% c("converged", "converged (had INFES)")) {
          currentRunTime <- as.numeric(.readRuntime(fullPathToThisRun), units = "hours")
          lastRunTime    <- as.numeric(.readRuntime(fullPathToLastRun), units = "hours")
          if (length(currentRunTime) > 0 && length(lastRunTime) > 0 && currentRunTime > (1.25 * lastRunTime)) {
            errorList <- c(errorList, "Check runtime! Have some scenarios become slower?")
          }
        }
        # run compareScenarios also for runs that are not_converged
        if (compScen &&
              all(file.exists(paste0(c(fullPathToThisRun, fullPathToLastRun),
                                     "/REMIND_generic_", cfg$title, ".mif"))) &&
              !any(grepl("comp_with_.*.pdf", dir()))) {
          message("Calling compareScenarios2 with ", fullPathToThisRun, " and ", fullPathToLastRun)
          outFileName <- paste0("comp_with_", lastRun)
          cs2com <- paste0(
                           "sbatch --qos=standby",
                           " --job-name=", outFileName,
                           " --comment=compareScenarios2",
                           " --output=", fullPathToThisRun, "/", outFileName, ".out",
                           " --error=", fullPathToThisRun, "/", outFileName, ".out",
                           " --mail-type=END --time=200 --mem-per-cpu=8000",
                           " --wrap=\"Rscript scripts/cs2/run_compareScenarios2.R",
                           " outputDirs=", paste(c(fullPathToThisRun, fullPathToLastRun), collapse = ","),
                           " profileName=default",
                           " outFileName=", outFileName,
                           "; ", "mv ", outFileName, ".pdf ", fullPathToThisRun,
                           "\"")
          cat(cs2com, "\n")
          withr::with_dir(cfg$remind_folder, {
            system(cs2com)
          }) # run_compareScenarios2.R works only if called from the main folder
        }
      }
      withr::local_dir("../")
      message("Finished analysis for ", i, " and changed back to ", normalizePath("."))
    } else if (model != "MAgPIE") {
      message(i, " does not seem to have converged. Skipping!")
    }
  }

  if (model == "REMIND") {
    # Find and print runs not started
    runsToStart <- readRDS(paste0(mydir, "runsToStart.rds"))
    runsToStart <- rownames(runsToStart)
    if (length(runsStarted) < length(runsToStart) + 1) {
      datetimepattern <- "_[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}"
      scenariosStarted <- gsub(datetimepattern, "", runsStarted) # remove date and time from folder name
      runsNotStarted <- setdiff(c("default-AMT", runsToStart), scenariosStarted)
      write(" ", readme, append = TRUE)
      write(paste0("These scenarios did not start at all:"), readme, append = TRUE)
      write(runsNotStarted, readme, append = TRUE)
      write(" ", readme, append = TRUE)
    }

    # Evaluate result of tests/testthat
    currentName <- "../test-full.log"
    dateTag <- format(file.info(currentName)$mtime, "%Y-%m-%d")
    if (is.na(dateTag)) {
      testthatResult <- "Could not check for the results of `make test-full`, test-full.log not found"
    } else {
      logStatus <- readLines(currentName, warn = FALSE)
      logStatus <- tail(grep("\\[ FAIL", logStatus, value = TRUE))
      newName <- paste0("tests/test-full-", dateTag, ".log")
      file.rename(from = currentName, to = paste0("../", newName))
      if (!isTRUE(grepl("FAIL", logStatus))) {
        testthatResult <- paste("`make test-full` did not run properly. Check", normalizePath(paste0("../", newName)))
      } else if (!isTRUE(grepl("FAIL 0", logStatus) & grepl("WARN 0", logStatus))) {
        testthatResult <- paste0("Not all tests pass in `make test-full`: ", logStatus,
                                 ". Check `", normalizePath(paste0("../", newName)), "`")
      } else {
        testthatResult <- paste("All tests pass in `make test-full`:", logStatus)
      }
    }

    # Find (by date in folder name) and move runs older than 'daysback' to archive
    daysback <- 90

    oldRuns <-
      list.files(pattern = ".*AMT.*", include.dirs = TRUE) %>%
      file.info() %>%
      filter(isdir)
    oldRuns$runname <- row.names(oldRuns)
    oldRuns <- oldRuns %>%
      mutate("ctimeDay" = as.Date(gsub(".*_([0-9]{4}-[0-9]{2}-[0-9]{2})_.*$", "\\1", .data$runname))) %>%
      filter(.data$ctimeDay < (Sys.Date() - daysback)) %>%
      pull(.data$runname)

    if (length(oldRuns) > 0) {
      message("Moving ", length(oldRuns), " runs with timestamp older than ",
              daysback, " days (", Sys.Date() - daysback, ") to 'archive':")
      print(oldRuns)
      system(paste("mv", paste(oldRuns, collapse = " "), "archive"))
    }
  }

  if (length(runsStarted) < 1) errorList <- c(errorList, "No runs started")

  if (is.null(errorList)) {
    summary <- paste0("Summary: AMT runs look good.")
  } else {
    summary <- paste0("Summary: ", paste0(unlist(unique(errorList)), collapse = ". "))
  }

  write(summary, readme, append = TRUE)
  write("```", readme, append = TRUE)
  message("Finished compiling README.md")

  if (email) {
    withr::with_dir(gitdir, {
      system("git reset --hard origin/master")
      system("git pull")
      file.copy(c(readme, changelog), ".", overwrite = TRUE)
      system("git add README.md")
      if (file.exists("data-changelog.csv")) {
        system("git add data-changelog.csv")
      }
      system("git commit -m 'Automated Test Results'")
      system("git push")
    })
  }

  message("Composing message and sending it to mattermost channel")
  # for MAgPIE only if warnings/errors occur, for REMIND always display AMT status
  if (!is.null(mattermostToken)) {
    # compose message, each vector element will appear in a new line in the final message.
    message <- NULL
    if (model == "REMIND") {
      rs2 <- utils::capture.output(loopRuns(runsStarted, user = NULL, colors = FALSE, sortbytime = FALSE))
      message <- paste0("Please find below the status of the REMIND automated model tests (AMT) of ",
                        today, ". Runs are here: `/p/projects/remind/modeltests/remind`.")
      message <- c(message, summary)
      message <- c(message, testthatResult)
      message <- c(message, "```", gitInfo, "```")
      message <- c(message, "`rs2 -t` returns:")
      message <- c(message, "```", rs2, "```")
      if (exists("runsNotStarted")) {
        message <- c(message, "These scenarios did not start at all:", runsNotStarted)
      }
    }
    if (!is.null(errorList) && model == "MAgPIE") {
      message <- c(message, paste0("Some ",
        model,
        " tests produce warnings. Please check ",
        "https://gitlab.pik-potsdam.de/",
        ifelse(model == "MAgPIE", "landuse", model),
        "/testing_suite"
      ))
    }

    if (!is.null(message)) {
      message <- paste0(message, collapse = "\n") # put each vector element into new line
      .mattermostBotMessage(message = message, token = mattermostToken)
    }
  }
  
  # only save this if everyting went well
  saveRDS(commitTested, file = paste0(mydir, "/lastcommit.rds"))

  message("Function 'evaluateRuns' finished.")
}
