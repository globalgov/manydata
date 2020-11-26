#' Create nested vectors of dates from vague date inputs
#' 
#' Create nested vectors of dates for vague date inputs, ambiguous and ranged dates, into a range of dates
#' @param x String vector of potential dates
#' @details The function seeks to convert ambiguous and ranged dates into a range of dates, 
#' and extends the date parsing of other packages to more historical and future dates.
#' @return Nested vector of POSIXct dates that includes a range of dates.
#' @importFrom anytime anydate
#' @importFrom stringr str_detect
#' @importFrom stringr str_split
#' @importFrom lubridate as_date
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
#' qDatr = qDatr::ambig_date(testdates)
#' ) datestest %>% print(n = 25)}
#' }
#' @export
ambig_date <- function(x){
  
  # First step: regularise/standardise inputs
  dates <- x
  dates <- stringr::str_replace_all(dates, "-00|-\\?\\?|-NA", "") # standardising ambiguities
  dates <- stringr::str_replace_all(dates, "_", ":") # standardising ranges
  # TODO: convert future dates
  # dates <- stringr::str_replace_all(dates, "01:01:2022 - 12:12:9999", "01:01:9999")
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
      d <- anytime::anydate(d)
    }
    # d <- unlist(d)
    d
  }

  # Third step: apply functions
  lapply(dates, function(x) date_disambig(x))
  # see hoist(), unnest_wider(), and unnest_longer()
  
}


