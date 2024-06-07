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
#' @param test Use this option to run a test of the workflow (no runs will be submitted)
#' The test parameter needs to be of the form "YY-MM-DD"
#' @param email whether an email notification will be send or not
#' @param mattermostToken token used for mattermost notifications
#' @param compScen whether compScen has to run or not
#' @param gitPath Path to the git executable
#'
#' @author Anastasis Giannousakis, David Klein
#' @seealso \code{\link{package2readme}}
#' @importFrom utils read.csv2
#' @importFrom dplyr mutate pull filter %>%
#' @importFrom lucode2 sendmail
#' @importFrom rlang .data
#' @export
modeltests <- function(
    mydir = ".",
    gitdir = NULL,
    model = NULL,
    user = NULL,
    test = NULL,
    email = TRUE,
    compScen = TRUE,
    mattermostToken = NULL,
    gitPath = "/p/system/packages/git/2.16.1/bin/git"  # nolint: absolute_path_linter
) {
  setwd(mydir)

  message("\n=========================================================
  Begin of AMT procedure ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " in ", mydir,
  "\n=========================================================\n")

  if (readLines("../.testsstatus") == "start") {
    message("Found 'start' in ", normalizePath("../.testsstatus"), "\nCalling 'startRuns'")
    startRuns(test = test, model = model, gitPath = gitPath, user = user, mydir = mydir)
    # make sure next call will evaluate runs
    message("Writing 'end' to ", normalizePath("../.testsstatus"))
    writeLines("end", con = "../.testsstatus")
  } else if (readLines("../.testsstatus") == "end") {
    message("Found 'end' in ", normalizePath("../.testsstatus"), "\nCalling 'evaluateRuns'")
    withr::with_dir("output", {
      evaluateRuns(model = model, mydir = mydir, gitPath = gitPath, compScen = compScen, email = email,
                   mattermostToken = mattermostToken, gitdir = gitdir, user = user)
                   })
    # make sure next call will start runs
    message("Writing 'start' to ", normalizePath("../.testsstatus"))
    writeLines("start", con = "../.testsstatus")
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
        sub("/realization.gms\"$", "", sub("^.*.modules/[0-9a-zA-Z_]{1,}/", "", realizations))))
    if (length(emptyRealizations) > 0) {
      unlink(paste0(moduleDirectory, emptyRealizations), recursive = TRUE)
    }
  }
}

startRuns <- function(test, model, mydir, gitPath, user) {
  if (!is.null(test)) {
    runcode <- paste0(".*.20-AMT", test)
    test <- TRUE
  } else {
    test <- FALSE
    runcode <- paste0(".*-AMT_",
                      format(Sys.Date(), "%Y-%m-%d"),
                      "|",
                      ".*-AMT_",
                      as.Date(format(Sys.Date(), "%Y-%m-%d")) + 1)
  }
  if (is.null(model)) stop("Model cannot be NULL")

  if (!test) {
    system(paste0(gitPath, " reset --hard origin/develop && ", gitPath, " pull"))
    # Force downloading of the input data in the first run
    system("sed -i 's/cfg$force_download <- FALSE/cfg$force_download <- TRUE/' config/default.cfg")

    # empty realization folders break the models, but can be left over because git does not
    # delete empty folders if all files in the folder are deleted.
    deleteEmptyRealizationFolders()

    if (model == "REMIND") {
      message("Configuring and starting single default-AMT")
      # Change title before starting the default run
      system(paste0("sed -i 's/cfg$title <- ", '"default"/cfg$title <- "default-AMT"/', "' config/default.cfg"))
      # set the slurmConfig before sourcing start.R to avoid questions about the slurm config
      slurmConfig <- "--qos=priority --nodes=1 --tasks-per-node=12"  # nolint: object_usage_linter
      selectScenarios <- NA  # will be loaded when sourcing start.R
      source("start.R", local = TRUE)
      Sys.sleep(100) # it would be much better to properly wait for the data downloading to finish

      message("Configuring and starting bundle of AMT runs")
      # do not download input data every run, reset force_download
      system("sed -i 's/cfg$force_download <- TRUE/cfg$force_download <- FALSE/' config/default.cfg")

      # now start actual test runs
      system(paste0("Rscript start.R ",
                    "startgroup=AMT titletag=AMT slurmConfig=\"--qos=standby --nodes=1 --tasks-per-node=12\" ",
                    "config/scenario_config.csv"))

      # Create and save a list of runs that should have been started in order to determine later which runs were not started
      settings <- read.csv2("config/scenario_config.csv",
                             stringsAsFactors = FALSE,
                             row.names = 1,
                             comment.char = "#",
                             na.strings = "")
      runsToStart <- selectScenarios(settings = settings, interactive = FALSE, startgroup = "AMT")
      row.names(runsToStart) <- paste0(row.names(runsToStart), "-AMT")
      saveRDS(runsToStart, file = paste0(mydir, "/runsToStart.rds"))

      # start make test-full
      # 1. pull changes to magpie develop
      withr::with_dir("magpie", {
            system(paste0(gitPath, " reset --hard origin/develop && ", gitPath, " pull"))
      })
      # 2. execute test
      system("make test-full-slurm")

    } else if (model == "MAgPIE") {
      # default run to download input data
      system("Rscript start.R runscripts=default submit=slurmpriority")
      # wait for default run to finish
      repeat {
        Sys.sleep(300)
        jobsInSlurm <- system(paste0("/p/system/slurm/bin/squeue -u ",
                                     user, " -h -o '%i %q %T %C %M %j %V %L %e %Z'"), intern = TRUE)
        if (!any(grepl(paste0(mydir, "$"), jobsInSlurm))) {
          break
        }
      }
      # now start actual test runs
      system("Rscript start.R runscripts=test_runs submit=slurmpriority")
    }
  }
  saveRDS(runcode, file = paste0(mydir, "/runcode.rds"))
  saveRDS(test, file = paste0(mydir, "/test.rds"))
  message("Function 'startRuns' finished.")
}

evaluateRuns <- function(model, mydir, gitPath, compScen, email, mattermostToken, gitdir, user, test = NULL) {
  message("Current working directory ", normalizePath("."))
  if (is.null(test)) test <- readRDS(paste0(mydir, "/test.rds"))
  if (!test) {
    message("Writing 'wait' to ", normalizePath(paste0(mydir, "../.testsstatus")))
    writeLines("wait", con = paste0(mydir, "../.testsstatus"))
  }
  lastCommit <- readRDS(paste0(mydir, "/lastcommit.rds"))
  out <- list()
  errorList <- NULL
  today <- format(Sys.time(), "%Y-%m-%d")

  # wait for all AMT runs to finish
  if (!test) {
    message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - waiting for all AMT runs to finish.")
    errCount <- 0
    repeat {
      jobsInSlurm <- system(paste0("/p/system/slurm/bin/squeue -u ", user, " -h -o '%i %q %T %C %M %j %V %L %e %Z'"), intern = TRUE)
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
  }

  message("Compiling the README.md to be committed to testing_suite repo.")
  commitTested <- sub("commit ", "", system(paste0(gitPath, " log -1"), intern = TRUE)[[1]])
  if (!test) saveRDS(commitTested, file = paste0(mydir, "/lastcommit.rds"))
  commitsSinceLastTest <- system(paste0(gitPath, " log --merges --pretty=oneline ",
                           lastCommit, "..", commitTested, " --abbrev-commit | grep 'Merge pull request'"), intern = TRUE)
  myfile <- file.path(ifelse(test, ".", tempdir()), "README.md")
  write("```", myfile)
  write(paste0("This is the result of the automated model tests for ", model, " on ", today, "."), myfile, append = TRUE)
  write(paste0("Path to runs: ", mydir, "output/"), myfile, append = TRUE)
  if (model == "REMIND") {
    write(paste0(c("Responsibilities:",
                   "  Robert     : SSP2EU-EU21",
                   "  Jess / Oli : SSP2EU",
                   "  Bjoern     : SDP",
                   "             : SSP1",
                   "             : SSP5"), collapse = "\n"), myfile, append = TRUE)
  }
  write(paste0("Direct and interactive access to plots: open shinyResults::appResults, then use '",
               ifelse(model == "MAgPIE", "weeklyTests", "AMT"),
               "' as keyword in the title search"), myfile, append = TRUE)
  if (model == "REMIND" && compScen == TRUE) {
    write(paste0("Each run folder below should contain a compareScenarios PDF comparing the output of the current and",
                 " the last successful tests (comp_with_RUN-DATE.pdf)"), myfile, append = TRUE)
  }
  write(paste0("Note: 'Mif' = 'no' indicates a possible error in output generation, please check!"),
        myfile, append = TRUE)
  write(paste0("If you are currently viewing the email: Overview of the last test is in red, ",
               "and of the current test in green"), myfile, append = TRUE)

  gitInfo <- c(paste("Tested commit:", commitTested),
               paste("The test of", today, "contains these merges:"),
               commitsSinceLastTest)
  write(gitInfo, myfile, append = TRUE)

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
    gRS <- getRunStatus(dir())
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
  write(paste(coltitles, collapse = colSep), myfile, append = TRUE)
  lenCols <- c(nchar(coltitles)[-length(coltitles)], 3)

  message("Starting analysis for the list of the following runs:\n", paste0(runsStarted, collapse = "\n"))
  for (i in runsStarted) {
    grsi <- getRunStatus(i)
    if ("Runtime" %in% names(grsi) && is.numeric(grsi[["Runtime"]])) {
      grsi["Runtime"] <- format(round(make_difftime(second = grsi[["Runtime"]]), 1))
    }
    write(sub("\n$", "", printOutput(grsi, lenCols = lenCols, colSep = colSep)), myfile, append = TRUE)
    if (model == "REMIND") {
      if (! grepl("Calib_nash|testOneRegi", grsi[, "RunType"]) && ! grsi[, "Conv"] %in% c("converged", "converged (had INFES)")) {
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
      setwd(i)
      message("Changed to ", normalizePath("."))
      # Use the fulldata.gdx of a successful SSP2EU-NPi-AMT to update the gdx on the RSE server that is used for testing convGDX2MIF
      if (grepl("SSP2EU-PkBudg650-AMT", rownames(grsi)) && grsi[, "Conv"] %in% c("converged", "converged (had INFES)")) {
        gdxOnRseServer <- "rse@rse.pik-potsdam.de:/webservice/data/example/remind2_test-convGDX2MIF_SSP2EU-PkBudg650-AMT.gdx"
        message(paste("Updating the gdx on the RSE server", gdxOnRseServer, "with the fulldata.gdx of", rownames(grsi)))
        system(paste("rsync -e ssh -av fulldata.gdx", gdxOnRseServer))
      }
      cfg <- NULL
      load("config.Rdata")
      sameRuns <- gRS %>% filter(.data$Conv %in% c("converged", "converged (had INFES)"), # runs have to be converged
                                 .data$Mif %in% c("yes", "sumErr"),                       # need to have mifs
                                 grepl(cfg$title, rownames(gRS)),                         # must be the same scenario
                                 ! rownames(gRS) %in% basename(cfg$results_folder)) %>%   # but not the current run
                          rownames()
      if (length(sameRuns) > 0) {
        # compare runtime for converged run only (skip if not_converged)
        if(grsi[, "Conv"] %in% c("converged", "converged (had INFES)")) {
          lastRun <- max(sameRuns[sameRuns < basename(cfg$results_folder)])
          currentRunTime <- as.numeric(.readRuntime("."),                    units = "hours")
          lastRunTime    <- as.numeric(.readRuntime(paste0("../", lastRun)), units = "hours")
          if (currentRunTime > (1.25 * lastRunTime)) {
            errorList <- c(errorList, "Check runtime! Have some scenarios become slower?")
          }
        }
        # run compareScenarios also for runs that are not_converged
        fullPathToThisRun <- normalizePath(".")
        fullPathToLastRun <- normalizePath(file.path("..",lastRun))
        if (compScen &&
            all(file.exists(paste0(c(fullPathToThisRun, fullPathToLastRun), "/REMIND_generic_", cfg$title, ".mif"))) &&
            !any(grepl("comp_with_.*.pdf", dir())) &&
            !test) {
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
          withr::with_dir(cfg$remind_folder, {system(cs2com)}) # run_compareScenarios2.R works only if called from the main folder
        }
      }
      setwd("../")
      message("Finished analysis for ", i, " and changed back to ", normalizePath("."))
    } else {
      message(i, "does not seem to have converged. Skipping!")
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
      write(" ", myfile, append = TRUE)
      write(paste0("These scenarios did not start at all:"), myfile, append = TRUE)
      write(runsNotStarted, myfile, append = TRUE)
      write(" ", myfile, append = TRUE)
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
        testthatResult <- paste("`make test-full` did not run properly. Check", newName)
      } else if (!isTRUE(grepl("FAIL 0", logStatus) & grepl("WARN 0", logStatus))) {
        testthatResult <- paste0("Not all tests pass in `make test-full`: ", logStatus ,". Check `", normalizePath(newName), "`")
      } else {
        testthatResult <- paste("All tests pass in `make test-full`:", logStatus)
      }
    }

    # Find (by date in folder name) and move runs older than 'daysback' to archive
    daysback <- 90

    oldRuns <-
      list.files(pattern=".*AMT.*", include.dirs = TRUE) %>%
      file.info() %>%
      filter(isdir)
    oldRuns$runname <- row.names(oldRuns)
    oldRuns <- oldRuns %>%
      mutate("ctimeDay" = as.Date(gsub(".*_([0-9]{4}-[0-9]{2}-[0-9]{2})_.*$", "\\1", .data$runname))) %>%
      filter(.data$ctimeDay < (Sys.Date() - daysback)) %>%
      pull(.data$runname)

    if (length(oldRuns) > 0) {
      message("Moving ", length(oldRuns), " runs with timestamp older than ", daysback, " days (", Sys.Date() - daysback,") to 'archive':")
      print(oldRuns)
      system(paste("mv", paste(oldRuns, collapse = " "), "archive"))
    }
  }

  if(length(runsStarted) < 1) errorList <- c(errorList, "No runs started")

  if(is.null(errorList)) {
    summary <- paste0("Summary: AMT runs look good.")
  } else {
    summary <- paste0("Summary: ", paste0(unlist(unique(errorList)), collapse = ". "))
  }

  write(summary, myfile, append = TRUE)
  write("```", myfile, append = TRUE)
  message("Finished compiling README.md")

  if (email) {
    message("Copying updated README.md to ", gitdir, " and pushing from there.")
    sendmail(path = gitdir, file = myfile, commitmessage = "Automated Test Results", remote = TRUE, reset = TRUE)
  }

  message("Composing message and sending it to mattermost channel")
  # for MAgPIE only if warnings/errors occur, for REMIND always display AMT status
  if (!is.null(mattermostToken)) {
    # compose message, each vector element will appear in a new line in the final message.
    message <- NULL
    if (model == "REMIND") {
      rs2 <- utils::capture.output(loopRuns(runsStarted, user = NULL, colors = FALSE, sortbytime = FALSE))
      message <- paste0("Please find below the status of the REMIND automated model tests (AMT) of ", today, ". Runs are here: `/p/projects/remind/modeltests/remind`.")
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

  message("Function 'evaluateRuns' finished.")
}
