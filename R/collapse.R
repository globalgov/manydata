#' Collapse a database into a single data frame/tibble
#' 
#' @name collapse
#' @param dbase A qPackage database object
#' @param dset A dataset label from within that database
#' @param key An ID column to collapse by
#' @param resolve how do you want differences to be resolved?
NULL

#' @rdname collapse
#' @importFrom purrr pluck
#' @export
collapse_select <- function(dbase, dset){
  purrr::pluck(dbase, dset)
}

#' @rdname collapse
#' @importFrom purrr reduce
#' @importFrom dplyr full_join
#' @export
collapse_full <- function(dbase, key, resolve = "mean"){
  if(resolve == "max") {
    key = resolve_mean(key)
  } else if (resolve == "min") {
    key = resolve_min(key)
  } else {
    key = resolve_mean(key)
  }
  purrr::reduce(dbase, function(x, y) dplyr::full_join(x, y, by = key))
}

#' @rdname collapse
#' @importFrom purrr reduce
#' @importFrom dplyr inner_join
#' @export
collapse_consensus <- function(dbase, key){
  if(resolve == "max") {
    key = resolve_mean(key)
  } else if (resolve == "min") {
    key = resolve_min(key)
  } else {
    key = resolve_mean(key)
  }
  purrr::reduce(dbase, function(x, y) dplyr::inner_join(x, y, by = key))
}
