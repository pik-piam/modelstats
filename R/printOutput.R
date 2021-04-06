#' printOutput
#'
#' Returns the output for getRunStatus nicely
#'
#' @param string String to print
#' @param len1stcol length of first column
#'
#' @author Anastasis Giannousakis
#' @export
printOutput<-function(string,len1stcol=67){

  formatstr <- function(x,len) {
    if (is.na(x)) return("                 ")
    if (nchar(x) > len) {
      return(substr(x,1,len))
    } else {
      teil2 <- paste(rep(" ",len-nchar(x)),collapse = "")
      return(paste0(x,teil2))
    }
  }
  
  
  if (length(string)==0) return("")
  out<-""
  templ <- data.frame("RunType"="NA", "modelstat"="NA",   "Iter"="NA",      "Conv"="NA","Mif"="NA","RunStatus"="NA",stringsAsFactors = F)
  if (file.exists("/p")) templ <- data.frame("jobInSLURM" = "NA" ,"RunType"="NA", "modelstat"="NA",   "Iter"="NA",   "Conv"="NA","Mif"="NA","RunStatus"="NA","runInAppResults"="NA", stringsAsFactors = F)
  templ[,names(string)] <- string
  rownames(templ) <- rownames(string)
  string <- templ
  for (i in 1:length(string)) {
    out <- paste0(formatstr(unname(string)[[i]],i+11),out)
  }
 return(paste0(formatstr(rownames(string),len1stcol)," ",out,"\n")) 
#  if(is.data.frame(string)) print(unname(string))

  
  
}

