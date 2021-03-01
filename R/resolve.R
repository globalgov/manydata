#' Resolve ranged dates into single vectors
#' 
#' @name resolve
#' @param dbase A qPackage database object
#' @param dset A dataset label from within that database
#' @param key An ID column to collapse by
NULL

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



