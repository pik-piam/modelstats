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
#' @importFrom remind2 compareScenarios
#' @importFrom magclass read.report write.report2 collapseNames
#' @export
modeltests <- function(mydir = ".", gitdir = NULL, model = NULL, user = NULL, test = NULL, iamccheck = TRUE, email = TRUE, compScen = TRUE, mattermostToken = NULL) {
  
  
  .mattermostBotMessage <- function(message, token) {
    system(paste0("curl -i -X POST -H 'Content-Type: application/json' -d '", '{"text": "', message, '"', "}' ", token), intern = TRUE)
  }
  
  if (readLines(paste0(mydir, "/.testsstatus")) == "start") {
    if (!is.null(test)) {
      test_bu <- test
      runcode <- paste0("-AMT-.*.202[1-9]-", test)
      test    <- TRUE
    } else {
      test <- FALSE
      test_bu <- NULL
      runcode <- paste0("-AMT-.*.",format(Sys.time(), "%Y-%m-%d"))
    }
    if (model == "MAgPIE") runcode <- "weeklyTests*."
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
        argv <- "config/scenario_config_AMT.csv"
        slurmConfig <- "--qos=priority --time=12:00:00 --nodes=1 --tasks-per-node=12"
        system("find . -type d -name output -prune -o -type f -name '*.R' -exec sed -i 's/sbatch/\\/p\\/system\\/slurm\\/bin\\/sbatch/g' {} +")
        write("slurmConfig <- '--qos=priority --time=12:00:00 --nodes=1 --tasks-per-node=12'", file = ".Rprofile", append = TRUE)
        system("Rscript start.R config/scenario_config_AMT.csv")
        runsToStart  <- read.csv2("config/scenario_config_AMT.csv", stringsAsFactors = FALSE, row.names = 1, comment.char = "#", na.strings = "")
        runsToStart  <- runsToStart[runsToStart$start==1,]
      } else if (model == "MAgPIE") {
        system("Rscript start.R runscripts=test_runs submit=slurmpriority")
      }
    }
    saveRDS(runcode, file = paste0(mydir, "/runcode.rds"))
    saveRDS(test, file = paste0(mydir, "/test.rds"))
    saveRDS(test_bu, file = paste0(mydir, "/test_bu.rds"))
    if (model == "REMIND") saveRDS(runsToStart, file = paste0(mydir, "/runsToStart.rds"))
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
        if (!any(grepl(mydir, system(paste0("/p/system/slurm/bin/squeue -u ", user, " -h -o '%i %q %T %C %M %j %V %L %e %Z'"), intern = TRUE)))) break
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
    write(paste0("This is the result of the automated ", model, " testing suite."), myfile, append = TRUE)
    write(paste0("Path to runs:", mydir, "output/"), myfile, append = TRUE)
    write(paste0("Direct and interactive access to plots: open shinyResults::appResults, then use '", strsplit(runcode, "\\.")[[1]][1], "' as keyword in the title search"), myfile, append = TRUE)
if (model == "REMIND" & compScen == TRUE) write(paste0("Further, each folder below should contain a compareScenarios PDF comparing the output of the current and the last tests (comp_with_RUN-DATE.pdf)"), myfile, append = TRUE)
    write(paste0("Note: 'Mif' = FALSE indicates a possible error in output generation, please check!"), myfile, append = TRUE)
    write(paste0("If you are currently viewing the email: Overview of the last test is in red, and of the current test in green"), myfile, append = TRUE)
    write(paste0("Tested commit: ", commit), myfile, append = TRUE)
    write(paste0("The test of ", format(Sys.time(), "%Y-%m-%d"), " contains these merges:"), myfile, append = TRUE)
    write(commits, myfile, append = TRUE)
#    write(paste0("View merge range on github: https://github.com/",model,"model/",model",/pulls?q=is%3Apr+is%3Amerged+",lastCommit,"..",commit),myfile,append=TRUE)
    write("Run                                jobInSlurm          RunType            RunStatus         Iter             Conv            modelstat      Mif           runInAppResults", myfile, append = TRUE)
    for (i in paths) {
      grsi <- getRunStatus(i)
      write(sub("\n$", "", printOutput(grsi, 34)), myfile, append = TRUE)
      if (model == "REMIND") if (grsi[,"Conv"] != "converged") errorList <- c(errorList,"Some run(s) did not converge")
      if (model == "MAgPIE") if (grsi[,"Iter"] != "y2100")  errorList <- c(errorList,"Some run(s) did not converge")
      if (grsi[,"Mif"] != "TRUE") errorList <- c(errorList,"Some run(s) did not report correctly")
      if (compScen) {
        setwd(i)
        cfg <- NULL
        if (any(grepl(sub("^.*./output/", "", getwd()), rownames(filter(gRS, Conv == "converged", Mif == TRUE))))) {
          load("config.Rdata")
        } else {
          setwd("../")
          next
        }
        if (!any(grepl("comp_with_.*.pdf", dir()))) {
          folder_comp_mif  <- Conv <- Mif <- NULL
          miffile <- paste0(getwd(), "/REMIND_generic_", cfg$title, "_withoutPlus.mif")
          sameRuns <- grep(cfg$title, rownames(filter(gRS, Conv == "converged", Mif == TRUE)), value = TRUE)
          rmRun <- grep(sub("output/", "", cfg$results_folder), sameRuns)
          sameRuns <- sameRuns[-rmRun]
          if (length(sameRuns) > 0) {
            folder_comp_mif <- max(sameRuns)
            compmif <- paste0("../", folder_comp_mif, paste0("/REMIND_generic_", cfg$title, "_withoutPlus.mif"))
            tmp <- read.report(compmif, as.list = FALSE)
            write.report2(x = collapseNames(tmp), file = "tmp.mif", scenario = paste0(cfg$title, "_ref"), model = model)
            if (all(file.exists(miffile, "tmp.mif"))) {
              if (!any(grepl("comp_with_.*.pdf", dir()))) try(compareScenarios(c(miffile, "tmp.mif"), hist = "historical.mif", fileName = paste0("comp_with_", folder_comp_mif, ".pdf")))
            }
          }
        }
        setwd("../")
      }
    }
    if (model == "REMIND") if (length(paths)!=length(rownames(runsToStart))) {
       runsNotStarted <- setdiff(rownames(runsToStart), sub("_.*","",paths))
       write(paste0("These scenarios did NOT start at all:"), myfile, append = TRUE)
       write(runsNotStarted, myfile, append=TRUE)
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
    write(errorList, myfile, append = TRUE)
    write("```", myfile, append = TRUE)
    if (email) sendmail(path = gitdir, file = myfile, commitmessage = "Automated Test Results", remote = TRUE, reset = TRUE)
    if (!is.null(errorList) & !is.null(mattermostToken)) .mattermostBotMessage(message = paste0(model, " tests have failed"), token = mattermostToken) 
    writeLines("start", con = paste0(mydir, "/.testsstatus"))
    saveRDS(commit, file = paste0(mydir, "/lastcommit.rds"))
  }
}
