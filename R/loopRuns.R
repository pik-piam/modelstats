#' loopRuns
#'
#' Returns the output for getRunStatus nicely
#'
#' @param mydir a dir or vector of dirs
#'
#'
#' @author Anastasis Giannousakis
#' @import crayon
#' @export
#' @examples 
#' 
#' \dontrun{ 
#' 
#' loopRuns(dir())
#' 
#' }
#' 
#' 
#' 

loopRuns <- function(mydir) {

  if (mydir[[1]]=="exit") return(NULL)

  red<-make_style("orangered")

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
    } else if (all(grepl(" NA ",out) & grepl("FALSE.*.TRUE",out))) {
      cat(cyan(out))
    } else if (all(grepl(" NA ",out) & grepl("FALSE",out))) {
      cat(red(out))
    } else {
      cat(cyan(out))
    } 
  }
  
}
