promptAndRun <- function(mydir = ".", user = NULL, daysback = 3) {
  if (is.null(user)) user <- Sys.info()[["user"]]
  if (user == "") user <- Sys.info()[["user"]]
  if (daysback == "") daysback <- 3

  if (mydir == ".") {
    loopRuns(".", user = user)
  } else if (mydir == "") {
    dirs <- list.dirs(".", recursive = FALSE)
    chosendirs <- gms::chooseFromList(dirs, type = "folders")
    loopRuns(if (length(chosendirs) == 0) "exit" else chosendirs, user = user)
  } else if (mydir == "-f") {
    loopRuns(dir(), user = user)
  } else if (mydir == "-t") {
    amtPath <- "/p/projects/remind/modeltests/output/"
    amtPattern <- readRDS("/p/projects/remind/modeltests/runcode.rds")
    amtDirs<-paste0(amtPath, dir(path = amtPath, pattern = amtPattern))
    loopRuns(amtDirs, user = user)
  } else if (mydir %in% c("-cr", "-a", "-c")) {
    myruns <- system(paste0("squeue -u ", user, " -h -o '%Z'"), intern = TRUE)
    runnames <- system(paste0("squeue -u ", user, " -h -o '%j'"), intern = TRUE)

    if (mydir %in% c("-cr", "-c")) {
      myruns2 <- system(paste0("sacct -u ", user, " -s cd,f,cancelled,timeout,oom -S ", as.Date(format(Sys.Date(), "%Y-%m-%d")) - as.numeric(daysback), " --format WorkDir -P -n"), intern = TRUE)
      # myruns2<-myruns2[!grepl("^$",myruns2)]
      myruns <- c(myruns, myruns2)
      runnames2 <- system(paste0("sacct -u ", user, " -s cd,f,cancelled,timeout,oom -S ", as.Date(format(Sys.Date(), "%Y-%m-%d")) - as.numeric(daysback), " --format JobName -P -n"), intern = TRUE)
      # runnames2<-runnames2[!grepl("^batch$",runnames2)]
      runnames <- c(runnames, runnames2)
    }

    if (any(grepl("mag-run", runnames))) {
      deleteruns <- which(runnames %in% c("default", "batch"))
    } else {
      deleteruns <- which(runnames %in% c("batch"))
    }
    if (length(deleteruns) > 0) {
      myruns <- myruns[-deleteruns]
      runnames <- runnames[-deleteruns]
    }

    if (length(myruns) == 0) {
      return("No runs found for this user. To change the reporting period (days) of the tool you need to specify also a user, e.g. rs2 -c USER 1")
    }
    coupled <- rem <- NULL
    for (i in 1:length(runnames)) {
      if (any(grepl(runnames[[i]], myruns[[i]]), grepl("mag-run", runnames[[i]]))) {
#       if (grepl(runnames[[i]],myruns[[i]])) {
        next
      } else {
        coupled <- c(coupled, paste0(paste0(myruns[[i]], "/output/", runnames[[i]], "-rem-"), seq(10)))
        coupled <- c(coupled, paste0(myruns[[i]], "/output/", runnames[[i]])) # for coupled runs in parallel mode
        rem <- c(rem, i)
      }
    }
    if (!is.null(rem)) {
      myruns <- myruns[-rem] # remove coupled parent-job
      myruns <- c(myruns, coupled) # add coupled paths
    }
    myruns <- myruns[file.exists(myruns)] # keep only existing paths
    myruns <- sort(unique(myruns[!is.na(myruns)]))

    if (length(myruns) == 0) {
      return("No runs found for this user. To change the reporting period (days) of the tool you need to specify also a user, e.g. rs2 -c USER 1")
    } else {
      message("")
      if (mydir %in% c("-cr", "-c") && daysback != 3) {
        message("Type 'rs2 -a username' to get only active runs in slurm")
      } else {
        message("Type 'rs2 -c username DAYS' with DAYS an integer denoting how many days you want results from")
      }
      message("")
      message("Found ", length(myruns), if (mydir == "-a") " active", " runs.",
              if (length(myruns)/as.numeric(daysback) > 20) " Excuse me? You need a cluster only for yourself it seems.")
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
