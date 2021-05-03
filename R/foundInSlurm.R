#' foundInSlurm
#'
#' Is the run found in SLURM?
#'
#' @param mydir Path to the folder(s) where the run(s) is(are) performed
#' @param user the user whose runs will be sought for
#'
#' @author Anastasis Giannousakis
#' @importFrom gdx readGDX
#' @export
foundInSlurm<-function(mydir=".",user=NULL){

  if (is.null(user)) user <- Sys.info()[["user"]]
  if (grepl("^C_",mydir) ) {
    mydir <- sub("-rem-[1-9]$","",mydir)
  } else {
    suppressWarnings(mydir <- normalizePath(mydir))
  }
  
  
  if (any(grepl(mydir,system(paste0("/p/system/slurm/bin/squeue -h -o '%j %Z'"),intern=TRUE) )) ) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  

}

