promptAndRun <- function(mydir = ".", user = NULL, daysback = 3) {
  if (is.null(user)) user <- Sys.info()[["user"]]
  if (user == "") user <- Sys.info()[["user"]]
  if (daysback == "") daysback <- 3

  if (mydir == ".") {
    loopRuns(".", user = user)
  } else if (mydir == "") {
    loopRuns(choose_folder("."), user = user)
  } else if (mydir == "-f") {
    loopRuns(dir(), user = user)
  } else if (mydir == "-cr") {
    myruns <- system(paste0("squeue -u ", user, " -h -o '%Z'"), intern = TRUE)
    runnames <- system(paste0("squeue -u ", user, " -h -o '%j'"), intern = TRUE)

    myruns2 <- system(paste0("sacct -u ", user, " -s cd,f,cancelled,timeout,oom -S ", as.Date(format(Sys.Date(), "%Y-%m-%d")) - as.numeric(daysback), " --format WorkDir -P -n"), intern = TRUE)
 #   myruns2<-myruns2[!grepl("^$",myruns2)]
    myruns <- c(myruns, myruns2)
    runnames2 <- system(paste0("sacct -u ", user, " -s cd,f,cancelled,timeout,oom -S ", as.Date(format(Sys.Date(), "%Y-%m-%d")) - as.numeric(daysback), " --format JobName -P -n"), intern = TRUE)
#    runnames2<-runnames2[!grepl("^batch$",runnames2)]
    runnames <- c(runnames, runnames2)
    if (any(grepl("mag-run", runnames))) {
      myruns <- myruns[-which(runnames %in% c("default", "batch"))]
      runnames <- runnames[-which(runnames %in% c("default", "batch"))]
    } else {
      myruns <- myruns[-which(runnames %in% c("batch"))]
      runnames <- runnames[-which(runnames %in% c("batch"))]
    }
    if (length(myruns) == 0) {
      return("No runs found for this user. To change the reporting period (days) of the tool you need to specify also a user, e.g. rs2 -cr USER 1")
    }
    coupled <- rem <- NULL
    for (i in 1:length(runnames)) {
      if (any(grepl(runnames[[i]], myruns[[i]]), grepl("mag-run", runnames[[i]]))) {
#       if (grepl(runnames[[i]],myruns[[i]])) {
        next
      } else {
        coupled <- c(coupled, paste0(paste0(myruns[[i]], "/output/", runnames[[i]], "-rem-"), seq(10)))
        rem <- c(rem, i)
      }
    }
    if (!is.null(rem)) {
      myruns <- myruns[-rem] # remove coupled parent-job
      myruns <- c(myruns, coupled) # add coupled paths
    }
    myruns <- myruns[file.exists(myruns)] # keep only existing paths
    myruns <- unique(myruns[!is.na(myruns)])

    if (length(myruns) == 0) {
      return("No runs found for this user. To change the reporting period (days) of the tool you need to specify also a user, e.g. rs2 -cr USER 1")
    } else {
      message("")
      message("Type 'rs2 -cr username DAYS' with DAYS an integer denoting how many days you want results from")
      message("")
      message("Found these runs")
    }

#    if (length(myruns)>40) {
#        message("Excuse me? > 40 runs? You need a cluster only for yourself it seems")
#    } else if (length(myruns)>15) {
#        message("Please wait, I found more than 15 runs (wow)")
#    } else if (length(myruns)>5) {
#        message("Please wait while I gather information on your current and recently completed runs")
#    }
#    message("Found these runs (only first 50 shown)")
    print(myruns[1:min(1500, length(myruns))])
    colSep <- "  "
    options(width = 200)
    len1stcol <- min(max(nchar(basename(myruns))), 50)
    coltitles <- c(paste(rep(" ", len1stcol), collapse = ""),
    "Runtime    ", "inSlurm", "RunType    ", "RunStatus         ", "Iter            ",
    "Conv                 ", "modelstat          ", "Mif     ", "inAppResults")
    message(paste(coltitles, collapse = colSep))
    for (i in myruns[1:min(1500, length(myruns))]) {
      try(message(sub("^\\[1\\]|\n$", "", printOutput(getRunStatus(i, user = user),
          lenCols = c(nchar(coltitles)[-length(coltitles)], 5), colSep = colSep))), silent = TRUE)
    }
  } else {
    loopRuns(mydir, user = user)
  }

}
