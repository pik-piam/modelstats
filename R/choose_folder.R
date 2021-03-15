choose_folder <- function(folder,title="Please choose a folder") {
  dirs <- NULL
  
  # Detect all output folders containing fulldata.gdx
  # For coupled runs please use the outcommented text block below
  
  dirs <- list.dirs(folder,recursive = FALSE)
  
  # DK: The following outcommented lines are specially made for listing results of coupled runs
  #runs <- findCoupledruns(folder)
  #dirs <- findIterations(runs,modelpath=folder,latest=TRUE)
  #dirs <- sub("./output/","",dirs)
  
  dirs <- c("all",dirs)
  cat("\n\n",title,":\n\n")
  cat(paste(1:length(dirs), dirs, sep=": " ),sep="\n")
  cat(paste(length(dirs)+1, "Search by the pattern.\n", sep=": "))
  cat(paste(length(dirs)+2, "None.\n", sep=": "))
  cat("\nNumber: ")
  identifier <- get_line()
  identifier <- strsplit(identifier,",")[[1]]
  tmp <- NULL
  for (i in 1:length(identifier)) {
    if (length(strsplit(identifier,":")[[i]]) > 1) tmp <- c(tmp,as.numeric(strsplit(identifier,":")[[i]])[1]:as.numeric(strsplit(identifier,":")[[i]])[2])
    else tmp <- c(tmp,as.numeric(identifier[i]))
  }
  identifier <- tmp
  # PATTERN
  if(length(identifier==1) && identifier==(length(dirs)+1)){
    cat("\nInsert the search pattern or the regular expression: ")
    pattern <- get_line()
    id <- grep(pattern=pattern, dirs[-1])
    # lists all chosen directories and ask for the confirmation of the made choice
    cat("\n\nYou have chosen the following directories:\n")
    cat(paste(1:length(id), dirs[id+1], sep=": "), sep="\n")
    cat("\nAre you sure these are the right directories? (y/n): ")
    answer <- get_line()
    if(answer=="y"){
      return(dirs[id+1])
    } else choose_folder(folder,title)
    # 
  } else if(length(identifier==1) && identifier==(length(dirs)+2)){
    return("exit")
  } else if(any(dirs[identifier] == "all")){
    identifier <- 2:length(dirs)
    return(dirs[identifier])
  } else return(dirs[identifier])
}
