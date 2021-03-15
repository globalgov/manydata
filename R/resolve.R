#' Resolve ranged dates into single vectors
#' 
#' @name resolve
#' @param dbase A qPackage database object
#' @param dset A dataset label from within that database
#' @param key An ID column to collapse by
NULL

#' Resolve Date Ranges
#'
#' @details This function resolves ranged dates created with `standardise_dates()`
#' by the choice type of minimum, maximun or mean dates.
#' @param dates Ranged dates variable returned by `standardise_dates()`
#' @param type How do you want the range to be solved?
#' @import lubridate
#' @import stringr
#' @return a date column
#' @examples
#' @export
resolve_dates <- function(dates, type = c("mean", "min", "max")) {
  
  if(!is.character(dates)) {
    stop("Please make sure date column has been parsed with standardise_dates() first")
  }
  
  if(missing(type)) {
    resolve_dates(dates, type = "mean")
  } 
  
  min_date <- function(x) {
    dates <- lapply(x, function(d) {
      splitd <- stringr::str_split(d, ":") [[1]]
      mindate <- paste0(splitd[[1]][1])
      dates <- mindate
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
      #todo = fix issues with calculations here
      dates
    })
  }
  
  max_date <- function(x) {
    dates <- lapply(x, function(d) {
      splitd <- stringr::str_split(d, ":")
      maxdate <- paste0(splitd[[1]][2])
      dates <- maxdate
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
      dates <- lubridate::as_date(histdate)
      #todo = fix issues with calculations here
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
      y2 <- paste0(s2[[1]][1])
      m2 <- paste0(s2[[1]][2])
      if(m1 == m2) {
        meandate <- paste0(y1, "-", m1, "-",  "15")
      } else if(m1 != m2 & y1 == y2) {
        meandate <- paste0(y1, "-07-02") #July 2nd is the middle of a regular year
      } else if(y1 != y2) {
        meanyear <- (as.numeric(y1) + as.numeric(y2))/2
        if(is.na(meanyear)) {
          meandate <- meanyear 
        } else if(stringr::str_detect(meanyear, "^[:digit:]{4}$")) {
          meandate <- paste0(meanyear, "-01-01")  
        } else {
          decimalyear <- stringr::str_split(meanyear, ".") [[1]]
          meandate <- paste0(decimalyear[[1]][1], "-07-02")   
        }
      }
      dates <- meandate
    })
  }
  
  mean_negdate <- function(x) {
    dates <- lapply(x, function(d) {
      splitd <- stringr::str_split(d, ":")[[1]]
      yb <- paste0(splitd)[[1]]
      ye <- paste0(splitd)[[2]]
      s1 <-  stringr::str_split(yb, "-")
      s2 <- stringr::str_split(ye, "-")
      y1 <- paste0(s1[[1]][1])
      m1 <- paste0(s1[[1]][2])
      y2 <- paste0(s2[[1]][1])
      m2 <- paste0(s2[[1]][2])
      if(m1 == m2) {
        meandate <- paste0("-", y1, "-", m1, "-",  "15")
      } else if(m1 != m2 & y1 == y2) {
        meandate <- paste0("-", y1, "-07-02") #July 2nd is the middle of a regular year
      } else if(y1 != y2) {
        meanyear <- (as.numeric(y1) + as.numeric(y2))/2
        if(is.na(meanyear)) {
          meandate <- meanyear 
        } else if(stringr::str_detect(meanyear, "^[:digit:]{4}$")) {
          meandate <- paste0("-", meanyear, "-01-01")  
        } else {
          decimalyear <- stringr::str_split(meanyear, ".") [[1]]
          meandate <- paste0("-", decimalyear[[1]][1], "-07-02")  
        }
      } 
      ndate <- as.numeric(lubridate::ymd(meandate))
      dzero <- as.numeric(lubridate::as_date("0000-01-01"))
      dt <- as.numeric(lubridate::as_date("0000-01-01"))
      negdate <- dzero - ndate
      histdate <- as.numeric(lubridate::as_date(negdate) + dt)
      dates <- lubridate::as_date(histdate)
      #todo = fix issues with calculations here
      dates
    })
  }
  
  if(type == "min") {
    dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$"), min_date(dates), dates)
    dates <- ifelse(stringr::str_detect(dates, "^-[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:-[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$"), min_negdates(dates), dates)
  } else if(type == "max") {
    dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$"), max_date(dates), dates)
    dates <- ifelse(stringr::str_detect(dates, "^-[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:-[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$"), max_negdates(dates), dates)
  } else if(type == "mean") {
    dates <- ifelse(stringr::str_detect(dates, "^[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$"), mean_date(dates), dates)
    dates <- ifelse(stringr::str_detect(dates, "^-[:digit:]{4}-[:digit:]{2}-[:digit:]{2}:-[:digit:]{4}-[:digit:]{2}-[:digit:]{2}$"), mean_negdates(dates), dates)
  } else dates <- dates
  
  dates <- unlist(dates, use.names = FALSE)
  dates <- lubridate::as_date(dates)
  dates
}

#' @rdname resolve
#' @importFrom purrr map
#' @export
resolve_min <- function(var){
  unlist(purrr::map(var, function(x) min(x)))
}

#' @rdname resolve
#' @importFrom purrr map
#' @export
resolve_max <- function(var){
  unlist(purrr::map(var, function(x) max(x)))
}

#' @rdname resolve
#' @importFrom purrr map
#' @export
resolve_mean <- function(var){
  if(is.character(var[[1]])){
    resolve_median(var)
  } else {
    unlist(purrr::map(var, function(x) mean(x)))
  }
}

#' @rdname resolve
#' @importFrom purrr map
#' @export
resolve_median <- function(var){
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
#' @export
resolve_mode <- function(var){
  Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  unlist(purrr::map(var, function(x) Mode(x)))
}



