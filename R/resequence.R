#' Resorting and filtering dates
#' 
#' @param data a dataframe
#' @param vars a character vector identifying columns in the dataframe to sequence
#' @param unity a string identifying how multiple entries may be glued together.
#' By default, tidyr::unite() glues using the underscore "_".
#' @return a dataframe/columns
#' @import lubridate
#' @examples
#' \dontrun{
#' TODO
#' }
#' @export
resequence <- function(data, vars, unity = "_"){
  
  require(lubridate)
  
  len <- length(vars)
  
  out <- apply(data[,vars], 1, function(x) {
    dates <- sort(unlist(strsplit(unique(na.omit(x)),unity)))
    
    if (length(dates) < len){
      dates <- interleave(dates, which(is.na(x)))
    }
    
    if (length(dates) > len){
      if (sum((!grepl("-01-01", dates))*1)>=len) dates <- dates[!grepl("-01-01",dates)]
      if (sum((!grepl("9999", dates))*1)>=len) dates <- dates[!grepl("9999",dates)]
      
      dmax <- max(as.duration(interval(dates[1:(length(dates)-1)],dates[2:(length(dates))])))
      dmax <- which(as.duration(interval(dates[1:(length(dates)-1)],dates[2:(length(dates))]))==as.duration(dmax))
      dates <- dates[c(1,dmax+1)]
    }
    
    dates
  })
  
  t(out)
  
}