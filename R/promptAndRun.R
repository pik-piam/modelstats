promptAndRun<-function(mydir=".") {
  
  if (mydir==".") {
    loopRuns(".")
  } else if (mydir=="") {
    loopRuns(choose_folder("."))
  } else {
    loopRuns(mydir)
  }

}

