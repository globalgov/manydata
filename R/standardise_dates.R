#' Create nested vectors of dates from vague date inputs
#'
#' Create nested vectors of dates for vague date inputs, ambiguous and ranged dates, into a range of dates
#' @param ... One (ymd) or three (y, m, d) variables
#' @details The function seeks to convert ambiguous and ranged dates into a range of dates,
#' and extends the date parsing of other packages to more historical and future dates. 
#' The function allows only for dmy or ymd date formats at present, since mdy may introduce errors.  
#' @return Nested vector of POSIXct dates that includes a range of dates
#' @importFrom anytime anydate
#' @importFrom stringr str_detect
#' @importFrom stringr str_split
#' @importFrom lubridate as_date
#' @importFrom lubridate dmy
#' @examples
#' \dontrun{
#' checkdates <- function(){
#' testdates <- c("2010-01-01", "2010", "2010-01",
#' "2010-01-00", "2010-00-00",
#' "2010-01-??", "2010-??-??",
# "2010-01-NA", "2010-NA-NA",
#' "2010:2011",
#' "2010-01-01_03", "2010-01_03",
#' "2010-01-01:03", "2010-01:03",
#' "9999-12-31", "2599-12-31",
#' "1712-01-01", "712-01-01", "712 AD",
#' "-1712-01-01", "-712-01-01", "712 BC")
#' datestest <- tibble::tibble(
#' origin = testdates,
#' lubridate = lubridate::as_date(testdates),
#' anytime = anytime::anydate(testdates),
#' qData = qData::standardise_dates(testdates)
#' ) datestest %>% print(n = 25)}
#' }
#' @export
standardise_dates <- standardize_dates <- function(...){
  
  # First step: if necessary, join multiple columns
  dots <- list(...)
  if(length(dots)==1){
    dates <- unlist(dots)
  } else if (length(dots)==3){
    dots <- purrr::map(dots, as.character)
    dates <- unlist(purrr::pmap_chr(dots, paste, sep = "-"))
  } else stop("Either you need to pass standardise_dates() one variable (i.e. 'yyyy-mm-dd' or three (yyyy, mm, dd).")
  
  # Second step: standardise inputs
  dates <- stringr::str_replace_all(dates, "\\.", "-") # standardising separaters
  dates <- stringr::str_replace_all(dates, "\\/", "-") # standardising separaters
  dates <- stringr::str_replace_all(dates, "-([:digit:])-", "-0\\1-") # standardising months
  dates <- stringr::str_replace_all(dates, "-([:digit:])$", "-0\\1") # standardising days
  dates <- stringr::str_replace_all(dates, "^([:digit:])-", "0\\1-") # standardising days 2
  dates <- stringr::str_replace_all(dates, "-00|-\\?\\?|-NA", "") # standardising ambiguities
  dates <- stringr::str_replace_all(dates, "_", ":") # standardising ranges
  
  if (stringr::str_detect(dates, "^[:digit:]{1,2}-[:digit:]{1,2}-[:digit:]{4}$")) { # Correct order and dates format if need
    dates <- lubridate::dmy(dates)
  } else if (stringr::str_detect(dates, "^[:digit:]{1,2}-[:digit:]{1,2}-[:digit:]{2}$")) { 
    thresh <- as.numeric(substr(Sys.Date(),1,4))
    x <- matrix(as.numeric(unlist(strsplit(dates, "-"))), ncol=3, byrow = T)
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
  }
    dates <- anytime::anydate(out)
  }
  
  dates

  # TODO: convert historical dates

  # Second step: set up functions
  date_disambig <- function(d){

    date_range <- function(start, finish){
      lubridate::as_date(anytime::anydate(start):anytime::anydate(finish))
    }

    if(stringr::str_detect(d, "^[:digit:]{4}:[:digit:]{4}$")){ # year range
      brackets <- stringr::str_split(d, ":")
      start <- paste0(brackets[[1]][1], "-01-01")
      finish <- paste0(brackets[[1]][2], "-12-31")
      d <- date_range(start, finish)
      d
    } else if(stringr::str_detect(d, "^[:digit:]{4}-[:digit:]{2}:[:digit:]{2}$")){ # month range
      brackets <- stringr::str_split(d, ":")
      start <- paste0(brackets[[1]][1], "-01")
      finish <- paste(stringr::str_split(start, "-")[[1]][1],
                       brackets[[1]][2],
                       lubridate::days_in_month(as.numeric(brackets[[1]][2])),
                       sep = "-")
      d <- date_range(start, finish)
      d
    } else if(stringr::str_detect(d, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:[:digit:]{2}$")){ # day range
      brackets <- stringr::str_split(d, ":")
      start <- brackets[[1]][1]
      finish <- paste(stringr::str_split(start, "-")[[1]][1],
                      stringr::str_split(start, "-")[[1]][2],
                      brackets[[1]][2],
                      sep = "-")
      d <- date_range(start, finish)
      d
    } else if(stringr::str_detect(d, "^[:digit:]{4}$")){ # year only
      d <- date_range(paste0(d, "-01-01"), paste0(d, "-12-31"))
      d
    } else if(stringr::str_detect(d, "^[:digit:]{4}-[:digit:]{2}$")){ # month only
      start <- paste0(d, "-01")
      finish <- paste0(d, "-", days_in_month(month(ymd(start))))
      d <- date_range(start, finish)
      d
    } else if(stringr::str_detect(d, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$")){ # full date
      if(d > Sys.Date() + lubridate::years(25)){
        d <- "9999-12-31" # convert future dates
      }
      d <- anytime::anydate(d)
    }
    
    # d <- unlist(d)
    d <- as.character(d)
    d
  }

  # Third step: apply functions
  anytime::anydate(unlist(lapply(dates, function(x) date_disambig(x))))
  # see hoist(), unnest_wider(), and unnest_longer()
  
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
  .Deprecated("qData::standardise_dates")
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


