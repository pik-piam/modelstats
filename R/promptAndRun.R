#' promptAndRun
#'
#' prompts runs, will be called by rs2
#'
#' @param mydir a dir or vector of dirs
#' @param user the user whose runs will be shown
#' @param daysback integer defining the number of days -c will look back in time to find runs
#'
#' @author Anastasis Giannousakis, Oliver Richters
#' @import crayon
#' @importFrom gtools mixedsort
#' @export
#' @examples
#' \dontrun{
#'   promptAndRun()
#' }
#'
promptAndRun <- function(mydir = ".", user = NULL, daysback = 3) {
  mydir <- strsplit(mydir, ',')[[1]]
  if (is.null(user) || user == "") user <- Sys.info()[["user"]]
  if (daysback == "") daysback <- 3
  colors <- ! any(grepl("-.*b.*", mydir))
  if (isFALSE(colors)) mydir <- gsub("b", "", mydir)
  if (isTRUE(mydir == ".")) {
    loopRuns(".", user = user, colors = colors)
  } else if (length(mydir) == 0 || isTRUE(mydir == "")) {
    if (sum(file.exists(c("full.gms", "log.txt", "config.Rdata", "prepare_and_run.R", "prepareAndRun.R"))) >= 4) {
      loopRuns(".", user = user, colors = colors)
    } else {
      folder <- if (sum(file.exists(c("output", "output.R", "start.R", "main.gms"))) == 4) "./output" else "."
      dirs <- c(folder, list.dirs(folder, recursive = FALSE))
      chosendirs <- gms::chooseFromList(dirs, type = "folders")
      loopRuns(if (length(chosendirs) == 0) "exit" else chosendirs, user = user)
    }
  } else if (isTRUE(mydir == "-f")) {
    folder <- if (sum(file.exists(c("output", "output.R", "start.R", "main.gms"))) == 4) "output" else "."
    # load all directories with a config file plus all that look like coupled runs to include them if they are pending
    loopRuns(file.path(folder, dir(folder)), user = user, colors = colors)
  } else if (isTRUE(mydir == "-t")) {
    amtPath <- "/p/projects/remind/modeltests/output/"
    amtPattern <- readRDS("/p/projects/remind/modeltests/runcode.rds")
    amtDirs <- dir(path = amtPath, pattern = amtPattern, full.names = TRUE)
    loopRuns(amtDirs, user = user, colors = colors)
  } else if (isTRUE(mydir == "-s")) {
    folder <- if (sum(file.exists(c("output", "output.R", "start.R", "main.gms"))) == 4) "output" else "."
    dirs <- mixedsort(grep("^C_.*-(rem|mag)-[0-9]+$", dir(folder), value = TRUE))
    lastdirs <- NULL
    for (r in unique(gsub("-(rem|mag)-[0-9]+$", "", dirs))) {
      lastdirs <- c(lastdirs, dirs[min(which(gsub("-(rem|mag)-[0-9]+$", "", dirs) == r))])
    }
    loopRuns(file.path(folder, lastdirs), user = user, colors = colors, sortbytime = FALSE)
  } else if (all(mydir %in% c("-cr", "-a", "-c"))) {
    myruns <- system(paste0("squeue -u ", user, " -h -o '%Z'"), intern = TRUE)
    runnames <- system(paste0("squeue -u ", user, " -h -o '%j'"), intern = TRUE)

    if (all(mydir %in% c("-cr", "-c"))) {
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
      return(paste0("No runs found for this user. You can change the reporting period (here: 5 days) by running 'rs2 -c ", user, " 5"))
    }
    # add REMIND-MAgPIE coupled runs where run directory is not the output directory
    # these lines also drops all other slurm jobs such as remind preprocessing etc.
    coupled <- rem <- NULL
    for (i in 1:length(runnames)) {
      if (! any(grepl(runnames[[i]], myruns[[i]]), grepl("mag-run", runnames[[i]]))) {
        coupled <- c(coupled, paste0(myruns[[i]], "/output/", runnames[[i]])) # for coupled runs in parallel mode
        rem <- c(rem, i)
      }
    }
    if (!is.null(rem)) {
      myruns <- myruns[-rem] # remove coupled parent-job and all other slurm jobs
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
    print(myruns[1:min(100, length(myruns))])
    loopRuns(myruns, user = user, colors = colors, sortbytime = FALSE)
  } else {
    loopRuns(mydir, user = user, colors = colors)
  }

}
