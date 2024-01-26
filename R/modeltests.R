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
#' @param iamccheck Use this option to turn iamc-style checks on and off
#' @param email whether an email notification will be send or not
#' @param mattermostToken token used for mattermost notifications
#' @param compScen whether compScen has to run or not
#' @param gitPath Path to the git executable
#'
#' @author Anastasis Giannousakis
#' @seealso \code{\link{package2readme}}
#' @importFrom utils read.csv2
#' @importFrom dplyr filter
#' @importFrom piamModelTests iamCheck
#' @importFrom quitte read.quitte
#' @importFrom lucode2 sendmail
#' @importFrom remind2 compareScenarios2
#' @importFrom magclass read.report write.report collapseNames
#' @export
modeltests <- function(
    mydir = ".",
    gitdir = NULL,
    model = NULL,
    user = NULL,
    test = NULL,
    iamccheck = TRUE,
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
                   mattermostToken = mattermostToken, gitdir = gitdir, iamccheck = iamccheck, user = user)
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
  if (model == "MAgPIE") runcode <- paste0(sub("-AMT-", "default", runcode), "|weeklyTests*.")
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

      settings <- read.csv2("config/scenario_config.csv",
                             stringsAsFactors = FALSE,
                             row.names = 1,
                             comment.char = "#",
                             na.strings = "")

      runsToStart <- selectScenarios(settings = settings, interactive = FALSE, startgroup = "AMT")
      row.names(runsToStart) <- paste0(row.names(runsToStart), "-AMT")
      saveRDS(runsToStart, file = paste0(mydir, "/runsToStart.rds"))
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

evaluateRuns <- function(model, mydir, gitPath, compScen, email, mattermostToken, gitdir, iamccheck, user, test = NULL) {
  message("Current working directory ", normalizePath("."))
  if (is.null(test)) test <- readRDS(paste0(mydir, "/test.rds"))
  if (!test) {
    message("Writing 'wait' to ", normalizePath(paste0(mydir, "../.testsstatus")))
    writeLines("wait", con = paste0(mydir, "../.testsstatus"))
  }
  runcode <- readRDS(paste0(mydir, "/runcode.rds"))
  lastCommit <- readRDS(paste0(mydir, "/lastcommit.rds"))
  out <- list()
  errorList <- NULL
  today <- format(Sys.time(), "%Y-%m-%d")

  # wait for all AMT runs to finish
  if (!test) {
    message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - waiting for all AMT runs to finish.")
    repeat {
      jobsInSlurm <- system(paste0("/p/system/slurm/bin/squeue -u ", user,
                                   " -h -o '%i %q %T %C %M %j %V %L %e %Z'"), intern = TRUE)
      if (!any(grepl(mydir, jobsInSlurm))) {
        Sys.sleep(600)
        break
      }
    }
    message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - all AMT runs finished.")
  }

  message("Compiling the README.md to be committed to testing_suite repo.")
  commitTested <- sub("commit ", "", system(paste0(gitPath, " log -1"), intern = TRUE)[[1]])
  commitsSinceLastTest <- system(paste0(gitPath, " log --merges --pretty=oneline ",
                           lastCommit, "..", commitTested, " --abbrev-commit | grep 'Merge pull request'"), intern = TRUE)
  myfile <- paste0(tempdir(), "/README.md")
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
    paths <- grep(runcode, dir(), value = TRUE)
    paths <- file.info(paths)
    paths <- rownames(paths[paths[, "isdir"] == TRUE, ])
  } else {
    # if model is MAgPIE ignore runcode and find paths to report on based on folder creation time (last 3 days)
    gRS <- getRunStatus(dir())
    # this happens because test run names are hard coded in MAgPIE scripts and thus not readable
    paths <- file.info(dir())
    paths <- filter(paths, isdir == TRUE)
    threeDaysAgo <- Sys.Date() - 3
    paths <- rownames(paths[which(as.Date(format(paths[, "ctime"], "%Y-%m-%d")) > threeDaysAgo), ])
  }

  colSep <- "  "
  coltitles <- c(
    "Run                                           ", "Runtime    ", "", "RunType    ", "RunStatus         ",
    "Iter            ", "Conv                 ", "modelstat          ", "Mif", "AppResults"
  )
  write(paste(coltitles, collapse = colSep), myfile, append = TRUE)
  lenCols <- c(nchar(coltitles)[-length(coltitles)], 3)

  message("Starting analysis for the list of the following runs:\n", paste0(paths, collapse = "\n"))
  for (i in paths) {
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
    } else if (model == "MAgPIE") {
      if (paste0(unique(unlist(strsplit(gsub("[^0-9]", "", grsi[, "modelstat"]), split = ""))), collapse = "") != "2") {
        errorList <- c(errorList, "Some run(s) did not converge")
      }
    }
    if (grsi[, "runInAppResults"] != "yes") errorList <- c(errorList, "Some run(s) did not report correctly")
    # For a successful run compare runtime and results with previous AMT run
    if (grsi[, "Conv"] %in% c("converged", "converged (had INFES)")) {
      setwd(i)
      message("Changed to ", normalizePath("."))
      cfg <- NULL
      if (any(grepl(sub("^.*./output/", "", getwd()), rownames(filter(gRS, Conv == "converged", Mif == "yes"))))) {
        load("config.Rdata")
      } else {
        setwd("../")
        message("Skipping ", i, " and changed back to ", normalizePath("."))
        next
      }
      sameRuns <- grep(cfg$title, rownames(filter(gRS, Conv %in% c("converged", "converged (had INFES)"), Mif == "yes")), value = TRUE)
      rmRun <- grep(sub("output/", "", cfg$results_folder), sameRuns)
      sameRuns <- sameRuns[-rmRun]
      if (length(sameRuns) > 0) {
        lastRun <- NULL
        lastRun <- max(sameRuns[sameRuns < sub("output/", "", cfg$results_folder)])
        currentRunTime <- as.numeric(.readRuntime("."), units = "hours")
        lastRunTime <- as.numeric(.readRuntime(paste0("../", lastRun)), units = "hours")
        if (currentRunTime > (1.25 * lastRunTime)) {
          errorList <- c(errorList, "Check runtime! Have some scenarios become slower?")
        }
        if (compScen && !any(grepl("comp_with_.*.pdf", dir()))) {
          folderCompMif <- Conv <- Mif <- NULL
          miffile <- paste0(getwd(), "/REMIND_generic_", cfg$title, ".mif")
          folderCompMif <- lastRun
          compmif <- paste0("../", folderCompMif, paste0("/REMIND_generic_", cfg$title, ".mif"))
          tmp <- read.report(compmif, as.list = FALSE)
          write.report(x = collapseNames(tmp), file = "tmp.mif", scenario = paste0(cfg$title, "_ref"), model = model)
          if (all(file.exists(miffile, "tmp.mif"))) {
            print(i)
            print(compmif)
            if (!any(grepl("comp_with_.*.pdf", dir()))) {
              try(compareScenarios2(c(miffile, "tmp.mif"),
                                    mifHist = "historical.mif",
                                    outputFile = paste0("comp_with_", folderCompMif, ".pdf")))
            }
          }
        }
      }
      setwd("../")
      message("Finished analysis for ", i, " and changed back to ", normalizePath("."))
    }
  }

  if (model == "REMIND") {
    # Find and print runs not started
    runsToStart <- readRDS(paste0(mydir, "runsToStart.rds"))
    if (length(paths) < length(rownames(runsToStart)) + 1) {
      runsNotStarted <- setdiff(c("default-AMT", rownames(runsToStart)), sub("_.*", "", paths))
      write(" ", myfile, append = TRUE)
      write(paste0("These scenarios did not start at all:"), myfile, append = TRUE)
      write(runsNotStarted, myfile, append = TRUE)
      write(" ", myfile, append = TRUE)
    }
  }

  if (iamccheck) {
    a <- NULL
    if (length(paths) > 0) {
      mifs <- paste0(paths, "/REMIND_generic_", sub("_20[0-9][0-9].*.$", "", paths), ".mif")
      mifs <- mifs[file.exists(mifs)]
      try(a <- read.quitte(mifs))
      if (!is.null(a)) {
        out[["iamCheck"]] <- iamCheck(a, cfg = model)
        if (!test) {
          saveRDS(out[["iamCheck"]], file = paste0("iamccheck-", commitTested, ".rds"))
        } else {
          saveRDS(out[["iamCheck"]], file = paste0("iamccheck-", test, ".rds"))
        }
      }
    }
    write(paste0("The IAMC check of these runs is found in /p/projects/remind/modeltests/output/iamccheck-",
                 commitTested, ".rds", "\n"), myfile, append = TRUE)
  }
  
  summary <- ifelse(length(paths) > 0, paste0(unlist(unique(errorList)), collapse = ". "), "No runs started")
  summary <- paste0("Summary of ", today, ": ", ifelse(summary == "", "Tests look good", summary))
  write(summary, myfile, append = TRUE)
  write("```", myfile, append = TRUE)
  message("Finished compiling README.md")

  if (email) {
    message("Copying updated README.md to ", gitdir, " and pushing from there.")
    sendmail(path = gitdir, file = myfile, commitmessage = "Automated Test Results", remote = TRUE, reset = TRUE)
  }
  
  if (test) file.copy(file, paste0(path, "/README.md"), overwrite = TRUE)
  
  if (!test) saveRDS(commitTested, file = paste0(mydir, "/lastcommit.rds"))

  # send message to mattermost channel (for MAgPIE only if warnings/errors occur, for REMIND always display AMT status)
  if (!is.null(mattermostToken)) {
    message <- NULL
    if (model == "REMIND") {
      rs2 <- utils::capture.output(loopRuns(paths, user = NULL, colors = FALSE))
      message <- paste0("Please find below the status of the REMIND automated model tests (AMT) of ", today, ":")
      message <- c(message, "```", gitInfo, "```")
      message <- c(message, "```", rs2, "```")
      message <- c(message, summary)
      message <- paste0(message, collapse = "\n")
      if (exists("runsNotStarted")) {
        message <- paste0(message, "\nThese scenarios did not start at all:\n", paste0(runsNotStarted, collapse = "\n"), "\n")
      }
    }
    if (!is.null(errorList)) {
        message <- paste0(message, "Some ",
                      model,
                      " tests produce warnings. Please check ",
                      "https://gitlab.pik-potsdam.de/",
                      ifelse(model == "MAgPIE", "landuse", model),
                      "/testing_suite",
                      ifelse(model == "REMIND", " or `rs2 -t`", "")
                      )
    }
    if (!is.null(message)) {
      .mattermostBotMessage(message = message, token = mattermostToken)
    }
  }

  message("Function 'evaluateRuns' finished.")
}
