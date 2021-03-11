#' printOutput
#'
#' Returns the output for getRunStatus nicely
#'
#' @param string String to print
#'
#'
#' @author Anastasis Giannousakis
#' @importFrom gdx readGDX
#' @importFrom utils tail
#' @export
printOutput<-function(string){

  formatstr <- function(x,len) {
    if (nchar(x) > len) {
      return(substr(x,1,len))
    } else {
      teil2 <- paste(rep(" ",len-nchar(x)),collapse = "")
      return(paste0(x,teil2))
    }
  }
  
  
  if (length(string)==0) return("")
  out<-""
  templ <- data.frame("RunType"="NA", "modelstat"="NA",   "Iter"="NA",      "Conv"="NA",stringsAsFactors = F)
  if (file.exists("/p")) templ <- data.frame("RunType"="NA", "modelstat"="NA",   "Iter"="NA",      "Conv"="NA","JobInSlurm" = "NA" ,stringsAsFactors = F)
  templ[,names(string)]<-string
  rownames(templ) <- rownames(string)
  string <- templ
  for (i in 1:length(string)) {
    out <- paste0(formatstr(unname(string)[[i]],i+15),out)
  }
 return(paste0(formatstr(rownames(string),50),out,"\n")) 
#  if(is.data.frame(string)) print(unname(string))

  
  
}

