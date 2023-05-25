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
  if (grepl("C_.*-rem-[0-9]+$", mydir)) {
    mydir <- dirname(dirname(mydir))
  }

  squeueresult <- system("/p/system/slurm/bin/squeue -h -o '%u %Z %j %T %q'", intern = TRUE)
  squeueresult <- grep(mydir, squeueresult, value = TRUE, fixed = TRUE)
  squeueresult <- grep(runname, squeueresult, value = TRUE, fixed = TRUE)
  if (length(squeueresult) > 0) {
    qos <- rev(strsplit(squeueresult, " ")[[1]])[[1]]
    yourrun <- any(grepl(paste0("^", user, " "), squeueresult))
    pending <- if (length(squeueresult) == 1 && grepl("PENDING [A-Za-z]*$", squeueresult)) " pending" else NULL
    if (yourrun) {
      return(paste0(qos, pending))
    } else {
      runuser <- strsplit(squeueresult, " ")[[1]][[1]]
      return(paste(runuser, pending))
    }
  } else {
    return("no")
  }
}
