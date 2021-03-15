get_line <- function(){
  # gets characters (line) from the terminal of from a connection
  # and stores it in the return object
  if(interactive()){
    s <- readline()
  } else {
    con <- file("stdin")
    s <- readLines(con, 1, warn=FALSE)
    on.exit(close(con))
  }
  return(s);
}
