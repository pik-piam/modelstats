choose_folder <- function(folder,title="Please choose a folder") {
  dirs <- NULL

  dirs <- list.dirs(folder,recursive = FALSE)
  
  dirs <- c("all",dirs)
  cat("\n\n",title,":\n\n")
  cat(paste(1:length(dirs), dirs, sep=": " ),sep="\n")
  cat(paste(length(dirs)+1, "Search by a pattern.\n", sep=": "))
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
    return(dirs[id+1])
    # 
  } else if(length(identifier==1) && identifier==(length(dirs)+2)){
    return("exit")
  } else if(any(dirs[identifier] == "all")){
    identifier <- 2:length(dirs)
    return(dirs[identifier])
  } else return(dirs[identifier])
}
