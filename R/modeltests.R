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
#'
#'
#' @author Anastasis Giannousakis
#' @seealso \code{\link{package2readme}}
#' @importFrom utils read.csv2
#' @importFrom iamc iamCheck
#' @importFrom quitte read.quitte
#' @importFrom lucode2 sendmail
#' @export
modeltests<-function(mydir=".",gitdir=NULL, model=NULL,user=NULL){

  if (is.null(model)) stop("Model cannot be NULL")

  setwd(mydir)
  system("git reset --hard origin/develop && git pull")
  argv <- "config/scenario_config_AMT.csv"
  slurmConfig <- "--qos=priority --nodes=1 --tasks-per-node=12"
  source("start.R",local=TRUE)

  out<-list()
  modelinerror = FALSE

  runcode<- paste0("-AMT-.*.202[1-9]-[0-1][0-9]-",format(Sys.Date(),"%d"))
  repeat {
    if(!any(grepl(runcode,system(paste0("squeue -u ",user," -h -o '%i %q %T %C %M %j %V %L %e %Z'"),intern=TRUE) ))) break
  }

  setwd("output/")
  paths<-grep(runcode,dir(),v=T)
  myfile<-paste0(tempdir(),"/README.md")
  write("Folder                                                           Mif                  Conv                Iter               modelstat         RunType          jobInSlurm",myfile)
  for (i in paths) {
    write(printOutput(getRunStatus(i)),myfile,append = T)  
  }

  sendmail(path=gitdir,file=myfile,commitmessage="Automated Test Results",remote=TRUE,reset=TRUE)
}


