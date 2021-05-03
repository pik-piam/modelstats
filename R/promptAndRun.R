promptAndRun<-function(mydir=".",user=NULL,daysback=3) {
  if (is.null(user)) user <- Sys.info()[["user"]]
  if (user=="") user <- Sys.info()[["user"]]
  if (daysback=="") daysback=3 

  if (mydir==".") {
    loopRuns(".",user=user)
  } else if (mydir=="") {
    loopRuns(choose_folder("."),user=user)
  } else if (mydir=="-f") {
    loopRuns(dir(),user=user)
  } else if (mydir=="-cr") {
    myruns<-system(paste0("squeue -u ",user," -h -o '%Z'"),intern=TRUE)
    runnames<-system(paste0("squeue -u ",user," -h -o '%j'"),intern=TRUE)
 
    myruns2<-system(paste0("sacct -u ",user," -s cd,f,cancelled -S ",as.Date(format(Sys.Date(),"%Y-%m-%d"))-as.numeric(daysback)," --format WorkDir -P -n"),intern=T)
 #   myruns2<-myruns2[!grepl("^$",myruns2)]
    myruns<-c(myruns,myruns2)
    runnames2<-system(paste0("sacct -u ",user," -s cd,f,cancelled -S ",as.Date(format(Sys.Date(),"%Y-%m-%d"))-as.numeric(daysback)," --format JobName -P -n"),intern=T)
#    runnames2<-runnames2[!grepl("^batch$",runnames2)]
    runnames<-c(runnames,runnames2)
    if (any(grepl("mag-run",runnames))) {
      myruns<-myruns[-which(runnames%in%c("default","batch"))]
      runnames<-runnames[-which(runnames%in%c("default","batch"))]
    } else {
      myruns<-myruns[-which(runnames%in%c("batch"))]
      runnames<-runnames[-which(runnames%in%c("batch"))]
    }
    if (length(myruns)==0) {
      return("No runs found for this user")
    } else {
      message("")
      message("!!! NEW FEATURE FOR BETTER OUTPUT: type 'rs2 -cr username DAYS' with DAYS an integer denoting how many days you want results from")
      message("")
      message("Found these runs") 
    }
 
    coupled<-rem<-NULL
    for (i in 1:length(runnames)) {
      if (any(grepl(runnames[[i]],myruns[[i]]),grepl("mag-run",runnames[[i]]))) {
#       if (grepl(runnames[[i]],myruns[[i]])) {
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
    if (!is.null(rem)) {
      myruns<-myruns[-rem] # remove coupled parent-job
      myruns <- c(myruns,coupled) # add coupled paths
      myruns<-myruns[file.exists(myruns)] # keep only existing paths
    }
#    if (length(myruns)>40) {
#        message("Excuse me? > 40 runs? You need a cluster only for yourself it seems")
#    } else if (length(myruns)>15) { 
#        message("Please wait, I found more than 15 runs (wow)")
#    } else if (length(myruns)>5) {
#        message("Please wait while I gather information on your current and recently completed runs")
#    }
#    message("Found these runs (only first 50 shown)")
    print(myruns[1:min(1500,length(myruns))])
    options(width=200)
    message(paste0("                                                   ","JobInSlurm          RunType            RunStatus         Iter             Conv            modelstat      Mif           runInAppResults"))
    for (i in myruns[1:min(1500,length(myruns))]) message(sub("^\\[1\\]|\n$","",printOutput(getRunStatus(i,user=user),len1stcol=50)))
    
  } else {
    loopRuns(mydir,user=user)
  }
  
}

