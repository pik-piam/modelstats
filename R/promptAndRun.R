promptAndRun<-function(mydir=".") {
  
  if (mydir==".") {
    loopRuns(".")
  } else if (mydir=="") {
    loopRuns(choose_folder("output"))
  } else {
    loopRuns(mydir)
  }

}

