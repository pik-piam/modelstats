#' getRunStatus
#'
#' Returns the current status of a run or a vector of runs
#'
#' @param dir Path to the folder(s) where the run(s) is(are) performed
#' @param sort how to sort (nf=newest first)
#' @param onlyrunning show only currently running runs
#'
#'
#' @author Anastasis Giannousakis
#' @importFrom gdx readGDX
#' @importFrom utils tail
#' @export
getRunStatus<-function(dir=".",sort="nf",onlyrunning=FALSE){
  
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
  onCluster <- file.exists("/p")
  out<-data.frame()
  
  a <- file.info(dir)
  a <- a[a[,"isdir"]==TRUE,]
  if (sort=="nf") dir <- rownames(a[order(a[,"mtime"],decreasing = T),])
  
  for (i in dir) {
    
    if (onCluster) out[i,"jobInSLURM"] <- foundInSlurm(i)
    
    if (onCluster) if (!out[i,"jobInSLURM"] & onlyrunning) {
     out <- out[setdiff(rownames(out),i),]
     next
    }
    
    fle <- paste0(i,"/runstatistics.rda")
    gdx <- paste0(i,"/fulldata.gdx")
    fulllst <- paste0(i,"/full.lst")
    fulllog <- paste0(i,"/full.log")
    
    stats <- NULL
    runtype <- NULL
    
    if (file.exists(fle)) {
      load(fle)
      if(any(grepl("config",names(stats)))) {
        out[i,"RunType"] <- stats[["config"]][["gms"]][["optimization"]]
      } else {
        out[i,"RunType"] <- sub("         !! def = nash","",sub("^ .*.ion  ","",system(paste0("grep 'setGlobal optimization  ' ",fulllst),intern=TRUE)))
      }
      if(any(grepl("modelstat",names(stats)))) out[i,"modelstat"] <- stats[["modelstat"]]
    } else {
      if (file.exists(gdx)) out[i,"modelstat"] <- as.numeric(readGDX(gdx,"o_modelstat", format="first_found"))
    }
    
    if (file.exists(fulllog)) {
      suppressWarnings(try(loop <- sub("^.*.= ","",system(paste0("grep 'LOOPS' ",fulllog," | tail -1"),intern=TRUE)),silent = TRUE))
      if (length(loop)>0) out[i,"Iter"] <- loop
    } else {
      out[i,"Iter"] <- "NA"
    }
      
    if (file.exists(fulllst)) {
      if (length(out[i,"RunType"])>0) 
      if (out[i,"RunType"]=="nash" & !is.na(out[i,"RunType"])) {
        
        totNoOfIter <- tail(system(paste0("grep 'cm_iteration_max = [1-9].*.;$' ",fulllst),intern=TRUE),n=1)
        if (length(totNoOfIter)>0) out[i,"Iter"] <- paste0(out[i,"Iter"],"/",sub(";","",sub("^.*.= ","",totNoOfIter)))
        
        iters <- suppressWarnings(system(paste0("grep 'PARAMETER o_modelstat          =           ' ",fulllst),intern=TRUE))
        out[i,"Last10"]<-substrRight(paste(as.numeric(sub("critical solver status for solution","",sub("^.*.=","",iters))),collapse=""),10)
        
        if (length(system(paste0("grep 'Convergence threshold' ",fulllst),intern=TRUE))>1) {
          out[i,"Conv"] <- "converged"
        } else {
          out[i,"Conv"] <- "not_converged"
        } 
      } else {
        out[i,"Conv"] <- "NA"
      }
      
    }
    
  }
   return(out)


}

