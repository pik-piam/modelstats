#' loopRuns
#'
#' Returns the output for getRunStatus nicely
#'
#' @param dir dir or vector of dirs
#'
#'
#' @author Anastasis Giannousakis
#' @importFrom crayon red blue green
#' @export

loopRuns <- function(dir) {

  cat("Folder                                       Conv                Last10             Iter              modelstat        RunType\n")

  for (i in dir ) {
    
    out <- printOutput(getRunStatus(i))
    if (grepl("not_converged",out)) {
      cat(red(out))
    } else if (grepl(" converged",out)){
      cat(green(out))
    } else {
      cat(blue(out))
    } 
  }
  
}
