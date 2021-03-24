promptAndRun<-function(mydir=".") {
  
  if (mydir==".") {
    loopRuns(".")
  } else if (mydir=="") {
    loopRuns(choose_folder("."))
  } else if (mydir=="-f") {
    loopRuns(dir())
  } else if (mydir=="-cr") {
    myruns<-system(paste0("squeue -u ",Sys.info()[["user"]]," -h -o '%Z'"),intern=TRUE)
    options(width=150)
    getRunStatus(myruns)
  } else {
    loopRuns(mydir)
  }

}

