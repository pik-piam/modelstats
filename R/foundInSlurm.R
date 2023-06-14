#' foundInSlurm
#'
#' Is the run found in SLURM?
#'
#' @param mydir Path to the folder(s) where the run(s) is(are) performed
#' @param user the user whose runs will be sought for
#'
#' @author Anastasis Giannousakis, Oliver Richters
#' @importFrom gdx readGDX
#' @export
foundInSlurm <- function(mydir = ".", user = NULL) {

  if (is.null(user)) user <- Sys.info()[["user"]]
  suppressWarnings(mydir <- normalizePath(mydir))
  runname <- basename(mydir)
  if (grepl("C_.*-(rem|mag)-[0-9]+$", mydir)) {
    mydir <- dirname(dirname(mydir))
  }

  squeueresult <- system("/p/system/slurm/bin/squeue -h -o '%u %Z %j %M %T %q'", intern = TRUE)
  squeuefiltered <- grep(mydir, squeueresult, value = TRUE, fixed = TRUE)
  squeuefiltered <- grep(runname, squeuefiltered, value = TRUE, fixed = TRUE)
  if (length(squeuefiltered) == 0 && grepl("^C_.*-mag-[0-9]+$", runname)) {
    runrem <- gsub("-mag-", "-rem-", runname)
    squeuefiltered <- grep(runrem, squeueresult, value = TRUE, fixed = TRUE)
    if (length(squeuefiltered) > 0) return("REMIND?")
  }
  if (length(squeuefiltered) > 0) {
    time <- rev(strsplit(squeuefiltered, " ")[[1]])[[3]]
    qos <- rev(strsplit(squeuefiltered, " ")[[1]])[[1]]
    yourrun <- any(grepl(paste0("^", user, " "), squeuefiltered))
    pending <- if (length(squeuefiltered) == 1 && grepl("PENDING [A-Za-z]*$", squeuefiltered)) " pending" else NULL
    startup <- if (grepl("^[0-5]:[0-9]{2}$", time) && is.null(pending)) " startup" else NULL
    if (yourrun) {
      return(paste0(qos, startup, pending))
    } else {
      runuser <- strsplit(squeuefiltered, " ")[[1]][[1]]
      return(paste0(runuser, startup, pending))
    }
  } else {
    return("no")
  }
}
