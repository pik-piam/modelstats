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

  squeueresult <- system("/p/system/slurm/bin/squeue -h -o '%u %Z %j %T %q'", intern = TRUE)
  squeueresult <- grep(mydir, squeueresult, value = TRUE)
  if (length(squeueresult) == 1 && grepl("PENDING [A-Za-z]*$", squeueresult)) {
    return(paste("PD", rev(strsplit(squeueresult, " ")[[1]])[[1]]))
  } else if (length(squeueresult) > 0) {
    yourrun <- any(grepl(paste0("^", user, " "), squeueresult))
    if (yourrun) {
      return("TRUE")
    } else {
      runuser <- strsplit(squeueresult, " ")[[1]][[1]]
      return(runuser)
    }
  } else {
    return("FALSE")
  }
}
