promptAndRun<-function(mydir=".") {
  
  if (mydir==".") {
    loopRuns(".")
  } else if (mydir=="") {
    loopRuns(choose_folder("."))
  } else if (mydir=="-f") {
    loopRuns(dir())
  } else {
    loopRuns(mydir)
  }

}

