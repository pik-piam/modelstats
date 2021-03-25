promptAndRun<-function(mydir=".") {
  
  if (mydir==".") {
    loopRuns(".")
  } else if (mydir=="") {
    loopRuns(choose_folder("."))
  } else if (mydir=="-f") {
    loopRuns(dir())
  } else if (mydir=="-cr") {
    myruns<-system(paste0("squeue -u ","hilaire"," -h -o '%Z'"),intern=TRUE)
    runnames<-system(paste0("squeue -u ","hilaire"," -h -o '%j'"),intern=TRUE)
    coupled<-NULL
    for (i in 1:length(runnames)) {
      if (grepl(runnames[[i]],myruns[[i]])) {
        next
      } else {
        coupled<-c(coupled,paste0(myruns[[i]],"/output/",runnames[[i]],"-rem-1"))
        coupled<-c(coupled,paste0(myruns[[i]],"/output/",runnames[[i]],"-rem-2"))
        coupled<-c(coupled,paste0(myruns[[i]],"/output/",runnames[[i]],"-rem-3"))
        coupled<-c(coupled,paste0(myruns[[i]],"/output/",runnames[[i]],"-rem-4"))
        coupled<-c(coupled,paste0(myruns[[i]],"/output/",runnames[[i]],"-rem-5"))
      }
    }
    myruns <- c(myruns,coupled)
    
    options(width=150)
    getRunStatus(myruns)

  } else {
    loopRuns(mydir)
  }

}

