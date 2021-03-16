#' loopRuns
#'
#' Returns the output for getRunStatus nicely
#'
#' @param mydir mydir or vector of dirs
#'
#'
#' @author Anastasis Giannousakis
#' @importFrom crayon red blue green underline
#' @export

loopRuns <- function(mydir) {

  if (mydir[[1]]=="exit") return(NULL)
  
  a <- file.info(mydir)
  a <- a[a[,"isdir"]==TRUE,]
  mydir <- rownames(a[order(a[,"atime"],decreasing = T),])
  
  cat("\n")
  if (!file.exists("/p")) {
    cat("Folder                                                           Mif                 Conv               Iter              modelstat        RunType\n")
  } else {
    cat("Folder                                                           Mif                 Conv                Iter               modelstat         RunType          JobInSlurm \n")
  }
  for (i in mydir ) {
    
    out <- printOutput(getRunStatus(i))
    if (grepl("not_converged",out)) {
      cat(red(out))
    } else if (grepl(" converged",out)){
      cat(underline(green(out)))
    } else if (all(grepl(" NA ",out) & grepl("FALSE",out))) {
      cat(red(out))
    } else {
      cat(blue(out))
    } 
  }
  
}
