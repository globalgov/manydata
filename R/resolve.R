#' Resolve ambiguous variables according to preferred output 
#' 
#' @description The resolve family of function resolves various ambiguous data formats
#' according to user preferences.For date ranges the function resolves internal ranges
#' according to preferred output before moving into resolving across datasets in a database.
#' @name resolve
#' @param var Variable to be resolved
#' @param resolve How do you want date ranges to be solved?
NULL

#' @rdname resolve
#' @importFrom purrr map
#' @example resolve_min(qStates::states$COW$Beg)
#' @export
resolve_min <- function(var){
  
  var <- dplyr::if_else(stringr::str_detect(var, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$"), as.character(resolve_dates(var, "min")), as.character(var)) 
    
  unlist(purrr::map(var, function(x) min(x)))
}

#' @rdname resolve
#' @importFrom purrr map
#' @example resolve_max(qStates::states$COW$End)
#' @export 
resolve_max <- function(var){
  
  var <- dplyr::if_else(stringr::str_detect(var, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$"), as.character(resolve_dates(var, "max")), as.character(var)) 
  
  unlist(purrr::map(var, function(x) max(x)))
}

#' @rdname resolve
#' @importFrom purrr map
#' @example resolve_mean(qStates::states$COW$ID)
#' @export
resolve_mean <- function(var) {
  
  var <- dplyr::if_else(stringr::str_detect(var, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$"), as.character(resolve_dates(var, "mean")), as.character(var)) 
  
  
  if(is.character(var[[1]])) {
    resolve_median(var)
  } else {
    unlist(purrr::map(var, function(x) mean(x)))
  }
}

#' @rdname resolve
#' @importFrom purrr map
#' @example resolve_median(qStates::states$COW$Beg)
#' @export
resolve_median <- function(var){
  
  var <- dplyr::if_else(stringr::str_detect(var, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$"), as.character(resolve_dates(var)), as.character(var)) 
  
  unlist(purrr::map(var, function(x){
    if(length(x)==1){
      x
    } else {
      if(is.numeric(var[[1]])){
        median(x)
      } else {
        x[round(length(x)/2)]
      }
    }
  }))
}

#' @rdname resolve
#' @importFrom purrr map
#' @example resolve_mode(qStates::states$COW$Label)
#' @export
resolve_mode <- function(var){
  
  var <- dplyr::if_else(stringr::str_detect(var, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$"), as.character(resolve_dates(var)), as.character(var))
  
  if(lubridate::is.Date(var)) {
    var <- as.character(var)
  }
  
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  unlist(purrr::map(var, function(x) Mode(x)))
}

#' @rdname resolve
#' @details `resolve_dates()` resolves ranged dates into single vectors
#' This function resolves ranged dates created with `standardise_dates()`
#' by the choice resolve of minimum, maximun or mean dates.
#' @param var Ranged dates variable returned by `standardise_dates()`
#' @param resolve How do you want date ranges to be solved?
#' @import lubridate
#' @import stringr
#' @return a date column
#' @examples
#' dates <- data.frame(dates = c("2010-01-01:2010-12-31", "1816-01-01:1916-01-01"))
#' resolve_dates(dates$dates, resolve = "min")
#' resolve_dates(dates$dates, resolve = "max")
#' resolve_dates(dates$dates, resolve = "mean")
#' @export
resolve_dates <- function(var, resolve = NULL) {
  
  if(!is.character(var)) {
    stop("Please make sure date column has been parsed with standardise_dates() first")
  }
  
  if(missing(resolve)) {
    resolve = "mean"
  }

  dates <- unlist(var)
  
  min_date <- function(x) {
    dates <- lapply(x, function(d) {
      splitd <- stringr::str_split(d, ":")[[1]]
      mindate <- paste0(splitd[[1]][1])
      dates <- mindate
      dates
    })
  }
  
  min_negdate <- function(x) {
    dates <- lapply(x, function(d) {
      splitd <- stringr::str_split(d, ":")[[1]]
      d <- paste0(splitd[[1]][1])
      ndate <- as.numeric(lubridate::ymd(d))
      dzero <- as.numeric(lubridate::as_date("0000-01-01"))
      dt <- as.numeric(lubridate::as_date("0000-01-01"))
      negdate <- dzero - ndate
      histdate <- as.numeric(lubridate::as_date(negdate) + dt)
      dates <- lubridate::as_date(histdate)
      dates
    })
  }
  
  max_date <- function(x) {
    dates <- lapply(x, function(d) {
      splitd <- stringr::str_split(d, ":")
      maxdate <- paste0(splitd[[1]][2])
      dates <- maxdate
      dates
    })
  }
  
  max_negdate <- function(x) {
    dates <- lapply(x, function(d) {
      splitd <- stringr::str_split(d, ":")
      d <- paste0(splitd[[1]][2])
      ndate <- as.numeric(lubridate::ymd(d))
      dzero <- as.numeric(lubridate::as_date("0000-01-01"))
      dt <- as.numeric(lubridate::as_date("0000-01-01"))
      negdate <- dzero - ndate
      histdate <- as.numeric(lubridate::as_date(negdate) + dt)
      h <- histdate + 730
      dates <- lubridate::as_date(h)
      dates
    })
  }
  
  mean_date <- function(x) {
    dates <- lapply(x, function(d) {
      splitd <- stringr::str_split(d, ":")[1]
      yb <- paste0(splitd[[1]][1])
      ye <- paste0(splitd[[1]][2])
      s1 <-  stringr::str_split(yb, "-")
      s2 <- stringr::str_split(ye, "-")
      y1 <- paste0(s1[[1]][1])
      m1 <- paste0(s1[[1]][2])
      d1 <- paste0(s1[[1]][3])
      y2 <- paste0(s2[[1]][1])
      m2 <- paste0(s2[[1]][2])
      d2 <- paste0(s2[[1]][3])
      # Special cases
      if (is.na(d)){
          meandate <- NA #Did this small tweak to deal with some of the NA related warnings, replicate this for the other functions
      } else if (!stringr::str_detect(d, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$")){
        meandate <- d #Did this small tweak to deal with some of the single date related warnings
      } else {
        if(m1 == m2 & y1 == y2) {
        if(is.na(d)){
          meandate <- NA #Did this small tweak to deal with some of the NA related warnings
        } else {
        meanday <- (as.numeric(d1) + as.numeric(d2))/2
        meanday <- as.character(meanday)
        meandate <- ifelse(stringr::str_detect(meanday, "^[:digit:]{2}.[:digit:]{1}$"), paste0(stringr::str_split(meanday, "\\.")[[1]][1]), meanday) 
        meandate  <- paste0(y1, "-", m1, "-", meandate)
        }
      } else if(m1 != m2 & y1 == y2) {
        meandate <- paste0(y1, "-07-02") #July 2nd is the middle of a regular year
      } else if(y1 != y2) {
        meanyear <- (as.numeric(y1) + as.numeric(y2))/2 # unique dates triggered
        # this warning here since y1 = date and y2 = NA
        if(is.na(meanyear)) {
          meandate <- meanyear 
        } else if(stringr::str_detect(meanyear, "^[:digit:]{4}$")) {
          meandate <- paste0(meanyear, "-01-01")  
        } else {
          decimalyear <- stringr::str_split(meanyear, ".") [[1]]
          meandate <- paste0(decimalyear[[1]][1], "-07-02")   
          }
        } 
      }
      dates <- meandate
    })
    dates
  }
  
  mean_negdate <- function(x) {
    dates <- lapply(x, function(d) {
      splitd <- stringr::str_split(d, ":")[1]
      yb <- paste0(splitd[[1]][1])
      ye <- paste0(splitd[[1]][2])
      s1 <-  stringr::str_split(yb, "-")
      s2 <- stringr::str_split(ye, "-")
      y1 <- paste0(s1[[1]][1])
      m1 <- paste0(s1[[1]][2])
      d1 <- paste0(s1[[1]][3])
      y2 <- paste0(s2[[1]][1])
      m2 <- paste0(s2[[1]][2])
      d2 <- paste0(s2[[1]][3])
      # Special cases
      if (is.na(d)){
        meandate <- NA #Did this small tweak to deal with some of the NA related warnings, replicate this for the other functions
      } else if (!stringr::str_detect(d, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$")){
        meandate <- d #Did this small tweak to deal with some of the single date related warnings
      } else {
        if(m1 == m2 & y1 == y2) {
          meanday <- (as.numeric(d1) + as.numeric(d2))/2
          meanday <- as.character(meanday)
          meandate <- ifelse(stringr::str_detect(meanday, "^[:digit:]{2}.[:digit:]{1}$"), paste0(stringr::str_split(meanday, "\\.")[[1]][1]), meanday) 
          meandate  <- paste0(y1, "-", m1, "-", meandate)
          ndate <- as.numeric(lubridate::ymd(meandate))
          dzero <- as.numeric(lubridate::as_date("0000-01-01"))
          dt <- as.numeric(lubridate::as_date("0000-01-01"))
          negdate <- dzero - ndate
          histdate <- as.numeric(lubridate::as_date(negdate) + dt)
          h <- histdate + 576
          dates <- lubridate::as_date(h)
        } else if(m1 != m2 & y1 == y2) {
          meandate <- paste0("-", y1, "-07-02") #July 2nd is the middle of a regular year
          ndate <- as.numeric(lubridate::ymd(meandate))
          dzero <- as.numeric(lubridate::as_date("0000-01-01"))
          dt <- as.numeric(lubridate::as_date("0000-01-01"))
          negdate <- dzero - ndate
          histdate <- as.numeric(lubridate::as_date(negdate) + dt)
          h <- histdate + 366
          dates <- lubridate::as_date(h)
        } else if(y1 != y2) {
          meanyear <- (as.numeric(y1) + as.numeric(y2))/2
          if(stringr::str_detect(meanyear, "^[:digit:]{4}$")) {
            meandate <- paste0("-", meanyear, "-01-01")
            ndate <- as.numeric(lubridate::ymd(meandate))
            dzero <- as.numeric(lubridate::as_date("0000-01-01"))
            dt <- as.numeric(lubridate::as_date("0000-01-01"))
            negdate <- dzero - ndate
            histdate <- as.numeric(lubridate::as_date(negdate) + dt)
            dates <- lubridate::as_date(histdate)
          } else {
            decimalyear <- stringr::str_split(meanyear, ".") [[1]]
            meandate <- paste0("-", decimalyear[[1]][1], "-07-02")
            ndate <- as.numeric(lubridate::ymd(meandate))
            dzero <- as.numeric(lubridate::as_date("0000-01-01"))
            dt <- as.numeric(lubridate::as_date("0000-01-01"))
            negdate <- dzero - ndate
            histdate <- as.numeric(lubridate::as_date(negdate) + dt)
            h <- histdate + 366
            dates <- lubridate::as_date(h)
          }
        }
      }
      dates
    })
    dates
  }
  
  if (resolve == "min") {
    dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$"), min_date(dates), dates)
    dates <- ifelse(stringr::str_detect(dates, "^-[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:-[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$"), min_negdates(dates), dates)
  } else if (resolve == "max") {
    dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$"), max_date(dates), dates)
    dates <- ifelse(stringr::str_detect(dates, "^-[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:-[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$"), max_negdates(dates), dates)
  } else if (resolve == "mean") {
    dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$"), mean_date(dates), dates)
    dates <- ifelse(stringr::str_detect(dates, "^-[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:-[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$"), mean_negdates(dates), dates)
  }

  dates <- unlist(dates, use.names = FALSE)
  dates <- anytime::anydate(dates) # Solution: put a suppresswarnings here, or use anytime, lubridate spits out lots of warnings when dealing with NA's
  dates
}
