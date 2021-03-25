promptAndRun<-function(mydir=".",user=NULL) {
  if (is.null(user)) user <- Sys.info()[["user"]]
  if (mydir==".") {
    loopRuns(".")
  } else if (mydir=="") {
    loopRuns(choose_folder("."))
  } else if (mydir=="-f") {
    loopRuns(dir())
  } else if (mydir=="-cr") {
    myruns<-system(paste0("squeue -u ",user," -h -o '%Z'"),intern=TRUE)
    runnames<-system(paste0("squeue -u ",user," -h -o '%j'"),intern=TRUE)
    coupled<-rem<-NULL
    for (i in 1:length(runnames)) {
      if (grepl(runnames[[i]],myruns[[i]])) {
        next
      } else {
        coupled<-c(coupled,paste0(myruns[[i]],"/output/",runnames[[i]],"-rem-1"))
        coupled<-c(coupled,paste0(myruns[[i]],"/output/",runnames[[i]],"-rem-2"))
        coupled<-c(coupled,paste0(myruns[[i]],"/output/",runnames[[i]],"-rem-3"))
        coupled<-c(coupled,paste0(myruns[[i]],"/output/",runnames[[i]],"-rem-4"))
        coupled<-c(coupled,paste0(myruns[[i]],"/output/",runnames[[i]],"-rem-5"))
        coupled<-c(coupled,paste0(myruns[[i]],"/output/",runnames[[i]],"-rem-6"))
        coupled<-c(coupled,paste0(myruns[[i]],"/output/",runnames[[i]],"-rem-7"))
        coupled<-c(coupled,paste0(myruns[[i]],"/output/",runnames[[i]],"-rem-8"))
        coupled<-c(coupled,paste0(myruns[[i]],"/output/",runnames[[i]],"-rem-9"))
        coupled<-c(coupled,paste0(myruns[[i]],"/output/",runnames[[i]],"-rem-10"))
        rem <- c(rem,i)
      }
    }
    myruns<-myruns[-rem] # remove coupled parent-job
    myruns <- c(myruns,coupled) # add coupled paths
    myruns<-myruns[file.exists(myruns)] # keep only existing paths
    
    options(width=150)
    getRunStatus(myruns)
    
  } else {
    loopRuns(mydir)
  }
  
}

