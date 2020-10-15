#' Capitalises all words
#'
#' Capitalises all words in a string to enable comparison
#' @param s A string
#' @param strict By default FALSE
#' @details The function capitalises all words in the strings passed to it,
#' as well as trimming all white space from the starts and ends of the strings.
#' @return A capitalised, trimmed string
#' @import textclean
#' @import english
#' @import stringr
#' @importFrom utils as.roman
#' @importFrom dplyr if_else
#' @examples
#' \dontrun{
#' e <- entitle("A treaty concerning things")
#' e==c("A Treaty Concerning Things")
#' }
#' @export
entitle <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)), {
    s <- substring(s, 2);
    if (strict) tolower(s) else s
    }
    , sep = "", collapse = " ")
  out <- sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  out[out == "NANA"] <- NA
  out <- trimws(out)
  out <- gsub("\\.(?=\\.*$)", "", out, perl = TRUE)
  out <- gsub("U.K.", "UK", out)
  out <- gsub("U.S.S.R.", "USSR", out)
  out <- gsub("U.S. ", "USA", out)
  out <- gsub("Art\\.", "Article", out)
  out <- gsub("\\#", "Number ", out)
  out <- textclean::add_comma_space(out)
  out <- textclean::mgsub(out,
                          paste0("(?<!\\w)", as.roman(1:100), "(?!\\w)"),
                          as.numeric(1:100),
                          safe = TRUE, perl = TRUE)
  ords <- english::ordinal(1:100)
  ords <- paste0(ords,
                 if_else(stringr::str_count(ords, "\\S+") == 2,
                         paste0("|", gsub(" ", "-", as.character(ords))),
                         ""))
  out <- textclean::mgsub(out,
                          paste0("(?<!\\w)", ords, "(?!\\w)"),
                          as.numeric(1:100),
                          safe = TRUE, perl = TRUE,
                          ignore.case = TRUE, fixed = FALSE)
  out
}

#' Resorting and filtering dates
#'
#' Resorting and filtering dates
#' @param data a dataframe
#' @param vars a character vector identifying columns in the dataframe to sequence
#' @param unity a string identifying how multiple entries may be glued together.
#' By default, tidyr::unite() glues using the underscore "_".
#' @return a dataframe/columns
#' @import lubridate
#' @importFrom stats na.omit
#' @examples
#' \dontrun{
#' data <- data.frame(Sign = c("2000-01-01", "2001-01-01", "2001-01-01_2000-01-01", "2000-01-01", NA),
#'                    Force = c("2001-01-01", "2000-01-01", "2001-01-01", NA, "2001-01-01"))
#' resequence(data, c("Sign","Force"))
#' }
#' @export
resequence <- function(data, vars, unity = "_"){

  len <- length(vars)

  out <- apply(data[,vars], 1, function(x) {
    dates <- sort(unlist(strsplit(unique(na.omit(x)),unity)))

    if (length(dates) < len){
      dates <- interleave(dates, which(is.na(x)))
    }

    if (length(dates) > len){
      if (sum((!grepl("-01-01", dates))*1)>=len) dates <- dates[!grepl("-01-01",dates)]
      if (sum((!grepl("9999", dates))*1)>=len) dates <- dates[!grepl("9999",dates)]

      dmax <- max(lubridate::as.duration(interval(dates[1:(length(dates)-1)],dates[2:(length(dates))])))
      dmax <- which(lubridate::as.duration(interval(dates[1:(length(dates)-1)],dates[2:(length(dates))]))==as.duration(dmax))
      dates <- dates[c(1,dmax+1)]
    }

    dates
  })

  t(out)

}

#' Fills missing data by lookup
#'
#' Fills missing data where known by other observations with the same id/index
#' @param df a dataframe
#' @param id a string identifying a column in the dataframe for indexing
#' @param var a string identifying a column or columns in the dataframe
#' to be filled
#' @return a dataframe
#' @examples
#' \dontrun{
#' myData <- repaint(myData, id="StatID", var=c("Area","Region")
#' }
#' @export
repaint <- function(df, id, var){
  for(co in var){
    for(ea in unique(df[,id])){
      if(any(!is.na(df[df[, id]==ea, co])) &
         any(is.na(df[df[, id]==ea, co]))){
        df[df[,id]==ea &
             is.na(df[,co]), co] <- df[df[,id]==ea &
                                         !is.na(df[,co]), co][1]
      }
    }
  }
  df
}

#' Resetting century of future events
#'
#' Resets the century of (unlikely) future events
#' @param dates First variable to be used, required.
#' Can be a character or date variable.
#' @param sep Year from which to make a cut-off and return the last century.
#' By default this is the system date, but can be specified.
#' @return A vector the same length as \code{dates}
#' @details This function will use the system date as the cut-off
#' for identifying past or future events.
#' It is geared towards numeric dates (i.e. "12") not named dates (i.e. "Dec"),
#' though these dates should be correctly parsed regardless of order or separator.
#' It will return any datestamps of "9999-12-31",
#' which may be interpreted as unknown/future date, as is.
#' Please note that different types of separators in the same vector,
#' for example c("2004-12-12","12/12/2004", "12.12.2004"),
#' may confuse the algorithm.
#' In other words, this function is built for internally consistent codes.
#' @examples
#' \dontrun{
#' recent(head(tfd_agree$Sign))
#' }
#' @export
recent <- function(dates, sep = NULL){

  miss <- which(is.na(dates))
  if(length(miss)>0) dates <- dates[-miss]

  # get separator
  if(is.null(sep)) sep <- substr(gsub("\\d","",dates),1,1)[1]
  if(sep==".") sep <- "\\."
  # get threshold
  thresh <- as.numeric(substr(Sys.Date(),1,4))
  # get elements
  x <- matrix(as.numeric(unlist(strsplit(dates, sep))), ncol=3, byrow = T)

  # order
  ypos <- which(apply(x, 2, function(y) any(y>31) | any(nchar(y)==4)))
  if(length(ypos)!=1) ypos <- 3
  dpos <- setdiff(which(apply(x, 2, function(y) any(y>12) & all(y<32))), ypos)
  if(length(dpos)==0) dpos <- setdiff(c(1,3), ypos)
  mpos <- setdiff(setdiff(1:3, ypos), dpos)

  if(nrow(x)==1){
    y <- ifelse(nchar(x[ypos])>2,
                ifelse(x[ypos] > thresh & x[ypos]!="9999",
                       paste0("19", x[ypos] %% 100),
                       formatC(x[ypos], width = 4, flag = "0")), # if 4, keep or correct
                ifelse(x[ypos] > (thresh %% 100),
                       paste0("19", formatC(x[ypos], width = 2, flag = "0")), # if more than now, likely last century
                       paste0("20", formatC(x[ypos], width = 2, flag = "0")))) # if less than now, likely this century
    m <- formatC(x[mpos], width = 2, flag = "0")
    if(m=="00") m <- "01"
    d <- formatC(x[dpos], width = 2, flag = "0")
    if(d=="00") d <- "01"
    out <- paste(y,m,d,sep = "-")
  } else {
    out <- apply(x, 1, function(x){
      y <- ifelse(nchar(x[ypos])>2,
                  ifelse(x[ypos] > thresh & x[ypos]!="9999",
                         paste0("19",x[ypos] %% 100),
                         formatC(x[ypos], width = 4, flag = "0")), # if 4, keep or correct
                  ifelse(as.numeric(x[ypos]) > (thresh %% 100),
                         paste0("19", formatC(x[ypos], width = 2, flag = "0")), # if more than now, likely last century
                         paste0("20", formatC(x[ypos], width = 2, flag = "0")))) # if less than now, likely this century
      m <- formatC(as.numeric(x[mpos]), width = 2, flag = "0")
      if(m=="00") m <- "01"
      d <- formatC(as.numeric(x[dpos]), width = 2, flag = "0")
      if(d=="00") d <- "01"
      paste(y,m,d,sep = "-")
    })
    out <- gsub("NA-NA-NA", NA_character_, out)
  }

  if(length(miss)>0) out <- interleave(out, miss) # intersperse NAs back in
  out
}

