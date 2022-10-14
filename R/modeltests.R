#' modeltests
#'
#' Runs a group of tests for a specific model. A "config/scenario_config_AMT.csv" file
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
#' @importFrom yaml read_yaml
#' @export
modeltests <- function(mydir = ".", gitdir = NULL, model = NULL, user = NULL, test = NULL, iamccheck = TRUE, email = TRUE, compScen = TRUE, mattermostToken = NULL) {


  .mattermostBotMessage <- function(message, token) {
    system(paste0("curl -i -X POST -H 'Content-Type: application/json' -d '", '{"text": "', message, '"', "}' ", token), intern = TRUE)
  }

  .readRuntime <- function(x) {
    stats <- NULL # because of no visible binding note
    load(paste0(x, "/runstatistics.rda"))
    return(stats$runtime)
  }


  if (readLines(paste0(mydir, "/.testsstatus")) == "start") {
    if (!is.null(test)) {
      test_bu <- test
      runcode <- paste0("-AMT-.*.20", test)
      test    <- TRUE
    } else {
      test <- FALSE
      test_bu <- NULL
      runcode <- paste0("-AMT-.*.",format(Sys.Date(), "%Y-%m-%d"),"|","-AMT-.*.",as.Date(format(Sys.Date(), "%Y-%m-%d"))+1)
    }
    if (model == "MAgPIE") runcode <- paste0(sub("-AMT-", "default", runcode),"|weeklyTests*.")
    if (is.null(model)) stop("Model cannot be NULL")

    setwd(mydir)
    if (!test) {
      system("/p/system/packages/git/2.16.1/bin/git reset --hard origin/develop && /p/system/packages/git/2.16.1/bin/git pull")
      system("sed -i 's/cfg$force_download <- FALSE/cfg$force_download <- TRUE/' config/default.cfg")
      a <- system("find modules/ -name 'module.gms'", intern = TRUE) # if empty realization folders exist (which git does not see), delete them
      for (i in a) {
        b <- setdiff(dir(sub("module.gms$", "", i)), c("module.gms", "input", sub("/realization.gms\"$", "", sub("^.*.modules/[0-9a-zA-Z_]{1,}/", "", grep("realization.gms", readLines(i), value = TRUE)))))
        if (length(b) > 0) unlink(paste0(sub("module.gms$", "", i), b), recursive = TRUE)
      }
      if (model == "REMIND") {
        slurmConfig <- "--qos=priority --nodes=1 --tasks-per-node=12"
        system("find . -type d -name output -prune -o -type f -name '*.R' -exec sed -i 's/sbatch/\\/p\\/system\\/slurm\\/bin\\/sbatch/g' {} +")
        changeTitle <- paste0("sed -i 's/cfg$title <- ", '"default"/cfg$title <- "default-AMT-"/', "' config/default.cfg")
        system(changeTitle)
        source("start.R", local = TRUE)
        Sys.sleep(100)
        system("sed -i 's/cfg$force_download <- TRUE/cfg$force_download <- FALSE/' config/default.cfg")
        system("Rscript start.R config/scenario_config_AMT.csv")
        runsToStart  <- read.csv2("config/scenario_config_AMT.csv", stringsAsFactors = FALSE, row.names = 1, comment.char = "#", na.strings = "")
        runsToStart  <- runsToStart[runsToStart$start == 1, ]
      } else if (model == "MAgPIE") {
        system("Rscript start.R runscripts=default submit=slurmpriority") # start default scenario, then wait until it runs to start also the weekly tests script
        Sys.sleep(300)
        repeat {
          if (!any(grepl(paste0(mydir,"$"), system(paste0("/p/system/slurm/bin/squeue -u ", user, " -h -o '%i %q %T %C %M %j %V %L %e %Z'"), intern = TRUE)))) {
            system("Rscript start.R runscripts=test_runs submit=slurmpriority")
            break
          }
        }
      }
    }
    saveRDS(runcode, file = paste0(mydir, "/runcode.rds"))
    saveRDS(test, file = paste0(mydir, "/test.rds"))
    saveRDS(test_bu, file = paste0(mydir, "/test_bu.rds"))
    if (model == "REMIND" & is.null(test_bu)) saveRDS(runsToStart, file = paste0(mydir, "/runsToStart.rds"))
    writeLines("end", con = paste0(mydir, "/.testsstatus"))
  } else if (readLines(paste0(mydir, "/.testsstatus")) == "end") {
    setwd(mydir)
    writeLines("wait", con = paste0(mydir, "/.testsstatus"))
    test    <- readRDS(paste0(mydir, "/test.rds"))
    test_bu <- readRDS(paste0(mydir, "/test_bu.rds"))
    runcode <- readRDS(paste0(mydir, "/runcode.rds"))
    lastCommit <- readRDS(paste0(mydir, "/lastcommit.rds"))
    if (model == "REMIND") runsToStart <- readRDS(paste0(mydir, "runsToStart.rds"))
    out <- list()
    errorList <- NULL

    if (!test) {
      repeat {
        if (!any(grepl(mydir, system(paste0("/p/system/slurm/bin/squeue -u ", user, " -h -o '%i %q %T %C %M %j %V %L %e %Z'"), intern = TRUE)))) {
          Sys.sleep(600)
          break
        }
      }
    }


    setwd("output")

    if (model != "MAgPIE") {
      gRSold <- readRDS("gRS.rds")
      try(gRS <- rbind(gRSold, getRunStatus(setdiff(dir(), rownames(gRSold)))))
      if (exists("gRS")) {
        saveRDS(gRS, "gRS.rds")
      } else {
        gRS <- getRunStatus(dir())
        saveRDS(gRS, "gRS.rds")
      }
    } else {
      gRS <- getRunStatus(dir())
    }

    paths <- grep(runcode, dir(), value = TRUE)
    paths <- file.info(paths)
    paths <- rownames(paths[paths[, "isdir"] == TRUE, ])

    commit <- sub("commit ", "", system("/p/system/packages/git/2.16.1/bin/git log -1", intern = TRUE)[[1]])
    commits <- system(paste0("/p/system/packages/git/2.16.1/bin/git log --merges --pretty=oneline ", lastCommit, "..", commit, " --abbrev-commit | grep 'Merge pull request'"), intern = TRUE)
    myfile <- paste0(tempdir(), "/README.md")
    write("```", myfile)
    write(paste0("This is the result of the automated ", model, " testing suite on ", format(Sys.time(), "%Y-%m-%d"), "."), myfile, append = TRUE)
    write(paste0("Path to runs:", mydir, "output/"), myfile, append = TRUE)
    write(paste0("Direct and interactive access to plots: open shinyResults::appResults, then use '", ifelse(model=="MAgPIE", "weeklyTests", strsplit(runcode, "\\.")[[1]][1]), "' as keyword in the title search"), myfile, append = TRUE)
if (model == "REMIND" & compScen == TRUE) write(paste0("Each run folder below should contain a compareScenarios PDF comparing the output of the current and the last successful tests (comp_with_RUN-DATE.pdf)"), myfile, append = TRUE)
    write(paste0("Note: 'Mif' = FALSE indicates a possible error in output generation, please check!"), myfile, append = TRUE)
    write(paste0("If you are currently viewing the email: Overview of the last test is in red, and of the current test in green"), myfile, append = TRUE)
    write(paste0("Tested commit: ", commit), myfile, append = TRUE)
    write(paste0("The test of ", format(Sys.time(), "%Y-%m-%d"), " contains these merges:"), myfile, append = TRUE)
    write(commits, myfile, append = TRUE)
#    write(paste0("View merge range on github: https://github.com/",model,"model/",model",/pulls?q=is%3Apr+is%3Amerged+",lastCommit,"..",commit),myfile,append=TRUE)
    colSep <- "  "
    coltitles <- c("Run                               ", "Runtime    ", "inSlurm", "RunType    ", "RunStatus         ",
                   "Iter            ", "Conv                 ", "modelstat          ", "Mif     ", "inAppResults")
    write(paste(coltitles, collapse = colSep), myfile, append = TRUE)
    for (i in paths) {
      grsi <- getRunStatus(i)
      write(sub("\n$", "", printOutput(grsi, lenCols = nchar(coltitles), colSep = colSep)), myfile, append = TRUE)
      if (model == "REMIND" & grsi[, "RunType"] != "Calib_nash" & grsi[, "Conv"] != "converged" & !grepl("testOneRegi", i)) errorList <- c(errorList, "Some run(s) did not converge")
      if (model == "REMIND" & grsi[, "RunType"] == "Calib_nash" & grsi[, "Conv"] != "Clb_converged") errorList <- c(errorList, "Some run(s) did not converge")
      if (grsi[, "modelstat"] != "2: Locally Optimal" & grepl("testOneRegi", i)) errorList <- c(errorList, "testOneRegi does not return an optimal solution")
      if (model == "MAgPIE") if (grsi[, "Iter"] != "y2100")  errorList <- c(errorList, "Some run(s) did not converge")
      if (file.exists(paste0(i, "/config.yml"))) { # try to capture MAgPIE's convergence target from the number of time steps
        cfg <- read_yaml(paste0(i, "/config.yml"))
        timeSteps <- cfg[["gms"]][["c_timesteps"]]
        if (!grepl("[^0-9]", timeSteps) & as.numeric(gsub("[^0-9]", "", timeSteps)) < 1000) {
          convTarget <- paste0("y", as.character(1990 + timeSteps * 5))
        } else {
          convTarget <- paste0("y", gsub("[^0-9]", "", timeSteps))
        }
      }
      if (model == "MAgPIE") if (grsi[, "Iter"] != convTarget)  errorList <- c(errorList, "Some run(s) did not converge")
      if (grsi[, "runInAppResults"] != "TRUE") errorList <- c(errorList, "Some run(s) did not report correctly")
      if (grsi[, "Conv"] == "converged") {
        setwd(i)
        cfg <- NULL
        if (any(grepl(sub("^.*./output/", "", getwd()), rownames(filter(gRS, Conv == "converged", Mif == TRUE))))) {
          load("config.Rdata")
        } else {
          setwd("../")
          next
        }
        sameRuns <- grep(cfg$title, rownames(filter(gRS, Conv == "converged", Mif == TRUE)), value = TRUE)
        rmRun <- grep(sub("output/", "", cfg$results_folder), sameRuns)
        sameRuns <- sameRuns[-rmRun]
        if (length(sameRuns) > 0) {
          lastRun <- NULL
          lastRun <- max(sameRuns[sameRuns < sub("output/", "", cfg$results_folder)])
          if (as.numeric(.readRuntime("."), units = "hours") > (1.25 * as.numeric(.readRuntime(paste0("../", lastRun)), units = "hours"))) errorList <- c(errorList, "Check runtime! Have some scenarios become slower?")
          if (compScen & !any(grepl("comp_with_.*.pdf", dir()))) {
            folder_comp_mif  <- Conv <- Mif <- NULL
            miffile <- paste0(getwd(), "/REMIND_generic_", cfg$title, ".mif")
            folder_comp_mif <- lastRun
            compmif <- paste0("../", folder_comp_mif, paste0("/REMIND_generic_", cfg$title, ".mif"))
            tmp <- read.report(compmif, as.list = FALSE)
            write.report(x = collapseNames(tmp), file = "tmp.mif", scenario = paste0(cfg$title, "_ref"), model = model)
            if (all(file.exists(miffile, "tmp.mif"))) {
              print(i)
              print(compmif)
              if (!any(grepl("comp_with_.*.pdf", dir()))) try(compareScenarios2(c(miffile, "tmp.mif"), mifHist = "historical.mif", outputFile = paste0("comp_with_", folder_comp_mif, ".pdf")))
            }
          }
        }
        setwd("../")
      }
    }
    if (model == "REMIND") if (length(paths) != length(rownames(runsToStart))+1) {
       runsNotStarted <- setdiff(c("default-AMT-", rownames(runsToStart)), sub("_.*", "", paths))
       write(" ", myfile, append = TRUE)
       write(paste0("These scenarios did not start at all:"), myfile, append = TRUE)
       write(runsNotStarted, myfile, append=TRUE)
       write(" ", myfile, append = TRUE)
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
              saveRDS(out[["iamCheck"]], file = paste0("iamccheck-", commit, ".rds"))
           } else {
             saveRDS(out[["iamCheck"]], file = paste0("iamccheck-", test_bu, ".rds"))
           }
        }
      }
      write(paste0("The IAMC check of these runs is found in /p/projects/remind/modeltests/output/iamccheck-", commit, ".rds", "\n"), myfile, append = TRUE)
    }
    tmp <- ifelse(length(paths) > 0, paste0(unlist(unique(errorList)), collapse = ". "), "No runs started")
    write(paste0("Summary of ", format(Sys.time(), "%Y-%m-%d"), ": ", ifelse(tmp == "", "Tests look good" , tmp)), myfile, append = TRUE)
    write("```", myfile, append = TRUE)
    if (email) sendmail(path = gitdir, file = myfile, commitmessage = "Automated Test Results", remote = TRUE, reset = TRUE)
    if (!is.null(errorList) & !is.null(mattermostToken)) .mattermostBotMessage(message = paste0("Some ", model, " tests produce warnings, check https://gitlab.pik-potsdam.de/", ifelse(model == "MAgPIE", "landuse", model), "/testing_suite"), token = mattermostToken)
    writeLines("start", con = paste0(mydir, "/.testsstatus"))
    if (!test) saveRDS(commit, file = paste0(mydir, "/lastcommit.rds"))
  }
}
