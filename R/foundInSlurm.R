#' foundInSlurm
#'
#' Is the run found in SLURM?
#'
#' @param dir Path to the folder(s) where the run(s) is(are) performed
#'
#'
#' @author Anastasis Giannousakis
#' @importFrom gdx readGDX
#' @export
foundInSlurm<-function(dir="."){

  if (grepl("^C_",dir) ) {
    dir <- sub("-rem-[1-9]$","",dir)
  } else {
    suppressWarnings(dir <- normalizePath(dir))
  }
  
  
  if (any(grepl(dir,system(paste0("squeue -h -o '%T %j %Z'"),intern=TRUE) )) ) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  

}

