#' modeltests
#'
#' Runs a group of tests for a specific model. A "config/scenario_config_AMT.csv" file
#' (relative) to the main folder has to exist in the model, describing the 
#' scenarios to be tested. Also, a gitlab repository is needed to push 
#' the generated README.md for documentation and automated reporting
#' of test results.
#'
#' @param mydir Path to the folder where the model is found
#' @param gitdir path to the git clone that sends the report via email
#' @param user the user that starts the job and commits the changes
#' @param model Model name
#' @param test Use this option to run a test of the workflow (no runs will be submitted)
#'
#'
#' @author Anastasis Giannousakis
#' @seealso \code{\link{package2readme}}
#' @importFrom utils read.csv2
#' @importFrom piamModelTests iamCheck
#' @importFrom quitte read.quitte
#' @importFrom lucode2 sendmail
#' @export
modeltests<-function(mydir=".",gitdir=NULL, model=NULL,user=NULL,test=NULL){

  if (!is.null(test)) {
    runcode <- paste0("-AMT-.*.202[1-9]-",test)
    test <- TRUE
  }
  if (is.null(model)) stop("Model cannot be NULL")

  setwd(mydir)
  if (!test) {
    system("git reset --hard origin/develop && git pull")
    argv <- "config/scenario_config_AMT.csv"
    slurmConfig <- "--qos=priority --time=06:00:00 --nodes=1 --tasks-per-node=12"
    source("start.R",local=TRUE)
  }

  out<-list()
  modelinerror = FALSE

  if (!test) {
    runcode<- paste0("-AMT-.*.202[1-9]-[0-1][0-9]-",format(Sys.Date(),"%d"))
    repeat {
      if(!any(grepl(runcode,system(paste0("squeue -u ",user," -h -o '%i %q %T %C %M %j %V %L %e %Z'"),intern=TRUE) ))) break
    }
  }
  
  
  setwd("output/")
  paths<-grep(runcode,dir(),v=T)
  myfile<-paste0(tempdir(),"/README.md")
  write(paste0("This is the result of the automated REMIND testing suite. Tested commit: ",system("git log -1",intern=TRUE)[[1]]),myfile)
  write(paste0("Date: ",date(),". Path to runs: /p/projects/remind/modeltests/output/ . If 'Mif' is FALSE the reporting has failed, possible error in pik-piam/remind2"),myfile,append=TRUE)
  write("Scenario                                                         Mif                  Conv                Iter               modelstat         RunType          jobInSlurm",myfile,append=TRUE)
  for (i in paths) {
    write(printOutput(getRunStatus(i)),myfile,append = TRUE)
  }

  if (length(paths)>0) {
   a <- read.quitte(mifs)
   out[["iamCheck"]] <- iamCheck(a,model)
  }

  
  sendmail(path=gitdir,file=myfile,commitmessage="Automated Test Results",remote=TRUE,reset=TRUE)
}



