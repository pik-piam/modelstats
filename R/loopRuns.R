#' loopRuns
#'
#' Returns the output for getRunStatus nicely
#'
#' @param mydir a dir or vector of dirs
#' @param user the user whose runs will be shown
#'
#' @author Anastasis Giannousakis
#' @import crayon
#' @export
#' @examples
#' \dontrun{
#'
#' loopRuns(dir())
#' }
#'
loopRuns <- function(mydir, user = NULL) {

  if (is.null(user)) user <- Sys.info()[["user"]]
  if (length(mydir) == 0) return("No runs found")
  if (mydir[[1]] == "exit") return(NULL)

  red <- make_style("orangered")

  a <- file.info(mydir)
  a <- a[a[, "isdir"] == TRUE, ]
  mydir <- rownames(a[order(a[, "mtime"], decreasing = TRUE), ])
  len <- max(c(15, nchar(mydir)))
  len <- min(67, len)


  cat("\n")
  if (!file.exists("/p")) {
    cat(paste0("Folder", paste0(rep(" ", len - 5), collapse = ""), "RunType           RunStatus        Mif             Conv           Iter          modelstat    \n"))
  } else {
    cat(paste0("Folder", paste0(rep(" ", len - 5), collapse = ""), "Runtime              JobInSlurm          RunType            RunStatus         Iter             Conv            modelstat      Mif           runInAppResults \n"))
  }

  for (i in mydir) {

    if (!file.exists(paste0(i, "/config.Rdata"))) next # do not report on folders that don't contain runs
    try(out <- printOutput(getRunStatus(i, user = user), len1stcol = len))
    if (grepl(" y2| nlp_", out)) {
      if (grepl("not_converged|Execution erro|Compilation er|missing|interrupted", out)) {
        cat(red(out))
      } else if (grepl(" converged|Clb_converged", out)) {
        cat(underline(green(out)))
      } else if (grepl("222", out)) {
        cat(green(out))
      } else if (grepl("Run in progress", out)) {
        cat(cyan(out))
      } else if (all(grepl(" NA ", out) & grepl("FALSE", out))) {
        cat(red(out))
      } else {
        cat(cyan(out))
      }
    } else {
      if (grepl("not_converged|Execution erro|Compilation er|missing|interrupted", out)) {
        cat(red(out))
      } else if (grepl(" converged|Clb_converged", out)) {
        cat(underline(green(out)))
      } else if (grepl(" 2 ", out) && !grepl("nash ", out)) {
        cat(green(out))
      } else if (grepl("Run in progress", out)) {
        cat(cyan(out))
      } else if (all(grepl(" NA ", out) & grepl("FALSE", out))) {
        cat(red(out))
      } else {
        cat(cyan(out))
      }
    }

  }

}
