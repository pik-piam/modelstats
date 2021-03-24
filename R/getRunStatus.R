#' getRunStatus
#'
#' Returns the current status of a run or a vector of runs
#'
#' @param mydir Path to the folder(s) where the run(s) is(are) performed
#' @param sort how to sort (nf=newest first)
#'
#'
#' @author Anastasis Giannousakis
#' @examples 
#' 
#' \dontrun{ 
#' 
#' a <- getRunStatus(dir())
#' 
#' }
#' 
#' @importFrom gdx readGDX
#' @importFrom utils tail
#' @export
getRunStatus<-function(mydir=dir(),sort="nf"){
  
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
  p3<-function(x) paste(x[[3]])
  rem<-function(x) return(x[-which(x=="")])
  
  mydir <- normalizePath(mydir)
  
  onCluster <- file.exists("/p")
  out<-data.frame()
  
  a <- file.info(mydir)
  a <- a[a[,"isdir"]==TRUE,]
  if (sort=="nf") mydir <- rownames(a[order(a[,"mtime"],decreasing = T),])
  
  for (i in mydir) {
    
    ii <- i
    i <- sub(paste0(dirname(i),"/"),"",i)
    
    if (onCluster) out[i,"jobInSLURM"] <- foundInSlurm(ii)
    
#    if (onCluster) if (!out[i,"jobInSLURM"] & onlyrunning) {
#     out <- out[setdiff(rownames(out),i),]
#     next
#    }

 
    cfgf <- paste0(ii,"/config.Rdata")
    fle <- paste0(ii,"/runstatistics.rda")
    gdx <- paste0(ii,"/fulldata.gdx")
    fulllst <- paste0(ii,"/full.lst")
    fulllog <- paste0(ii,"/full.log")
    logtxt <- paste0(ii,"/log.txt")
    
    stats <- NULL
    runtype <- NULL
    cfg<-NULL
    
    # RunType
    if (file.exists(cfgf)) {
      load(cfgf)
        out[i,"RunType"] <- cfg[["gms"]][["optimization"]]
        if (cfg[["gms"]][["CES_parameters"]]=="calibrate") out[i,"RunType"]<-paste0("Calib_",out[i,"RunType"])
        totNoOfIter <- cfg[["gms"]][["cm_iteration_max"]]
      } else if (file.exists(fulllst)) {
        out[i,"RunType"] <- sub("         !! def = nash","",sub("^ .*.ion  ","",system(paste0("grep 'setGlobal optimization  ' ",fulllst),intern=TRUE)))
        chck <- sub("       !! def = load","",sub("^ .*.ers  ","",system(paste0("grep 'setglobal CES_parameters  ' ",fulllst),intern=TRUE)))
        if (chck=="calibrate") out[i,"RunType"]<-paste0("Calib_",out[i,"RunType"])
      }
      
    # modelstat & runInAppResults
    if (onCluster) out[i,"runInAppResults"] <- "NA"
    if (file.exists(fle)) {
      load(fle)
      if(any(grepl("modelstat",names(stats)))) out[i,"modelstat"] <- stats[["modelstat"]]
      if (onCluster && any(grepl("id",names(stats)))) {
        ovdir<-"/p/projects/rd3mod/models/results/remind/"
        id <- paste0(ovdir,stats[["id"]],".rds")
        if (file.exists(id) && all(file.info(Sys.glob(paste0(ovdir,"overview.rds")))$mtime>file.info(id)$mtime)) 
          out[i,"runInAppResults"] <- TRUE
      } else {
        if (onCluster) out[i,"runInAppResults"] <- FALSE
        }
    } else {
      if (file.exists(gdx)) out[i,"modelstat"] <- as.numeric(readGDX(gdx,"o_modelstat", format="first_found"))
    }
    
    # Iter
    out[i,"Iter"] <- "NA"
    if (file.exists(fulllog)) {
      suppressWarnings(try(loop <- sub("^.*.= ","",system(paste0("grep 'LOOPS' ",fulllog," | tail -1"),intern=TRUE)),silent = TRUE))
      if (length(loop)>0) out[i,"Iter"] <- loop
      if (!out[i,"RunType"]%in%c("nash","Calib_nash") & length(totNoOfIter)>0) out[i,"Iter"] <- paste0(out[i,"Iter"],"/",sub(";","",sub("^.*.= ","",totNoOfIter)))
    }
    
    # Conv  
    if (file.exists(fulllst)) {      if (length(out[i,"RunType"])>0)
      
      if (grepl("nash",out[i,"RunType"]) & !is.na(out[i,"RunType"])) {
        
        if (cfg[["gms"]][["cm_nash_autoconverge"]]>0) {
          totNoOfIter <- tail(suppressWarnings(system(paste0("grep 'cm_iteration_max = [1-9].*.;$' ",fulllst),intern=TRUE)),n=1) 
        } else {
          totNoOfIter <- cfg[["gms"]][["cm_iteration_max"]]
        }
        if (exists("totNoOfIter") && any(grepl("cm_iteration_max",cfg))) if (length(totNoOfIter)>0 & !cfg[["gms"]][["cm_iteration_max"]] > out[i,"Iter"]) out[i,"Iter"] <- paste0(out[i,"Iter"],"/",sub(";","",sub("^.*.= ","",totNoOfIter)))
        
        if (length(suppressWarnings(system(paste0("grep 'Convergence threshold' ",fulllst),intern=TRUE)))>1) {
          out[i,"Conv"] <- "converged"
        } else if (length(suppressWarnings(system(paste0("grep 'Nash did NOT' ",fulllst),intern=TRUE)))>1) {
          out[i,"Conv"] <- "not_converged"
        } else {
          iters <- suppressWarnings(system(paste0("grep -A 15 'PARAMETER p80_repy  sum' ",fulllst),intern=TRUE))
          if (length(iters)>0) {
            iters <- grep("^$|--|modelstat",iters,invert = TRUE,value=TRUE)
     
              iters<-tail(sapply(iters, strsplit, split = " "),n=120)
              
              b<-paste0(sapply(iters,rem)[3,],collapse="")
              iters<-gsub(" |0|\\.","",b[[1]])
              out[i,"Conv"]<-substr(iters,nchar(iters)-13,nchar(iters)) # a function is needed that extracts a summary of each iteration, not just the last one
             
          }
        }
      } else {
        out[i,"Conv"] <- "NA"
      }
      
    } # END Conv
    
    # Calib Iter
    if (file.exists(logtxt) & grepl("Calib",out[i,"RunType"])) {
      calibiter <- length(suppressWarnings(system(paste0("grep 'CES calibration iteration' ",logtxt),intern=TRUE)))
      if (calibiter>0) out[i,"Iter"]<-paste0(out[i,"Iter"]," ","Clb: ",calibiter)
      if (!is.null(out[i,"Conv"])) if (out[i,"Conv"]=="converged" & length(system(paste0("find ",i," -name 'input_*.gdx'"),intern = TRUE))>10) out[i,"Conv"]<-paste0("Clb","_converged")
    }
    
    # MIF
    if (file.exists(cfgf)) {
      miffile <- paste0(ii,"/REMIND_generic_",cfg[["title"]],".mif")
      out[i,"Mif"] <- FALSE
      if (file.exists(miffile)) {
        if (file.info(miffile)[["size"]]>99999) out[i,"Mif"] <- TRUE
      } 
    }
    
    
  } # END DIR LOOP
  
  return(out)

}

