#' Collapse a database into a single data frame/tibble
#' 
#' @description The collapse family of functions collapses
#' lists of datasets that form a database in qPackages
#' according to user preferences.
#' @name collapse
#' @param dbase A qPackage database object
#' @param dset A dataset label from within that database
#' @param key An ID column to collapse by
#' @param resolve How do you want differences to be resolved?
#' It takes "max", "min", "mean", "mode" and "median" options.
#'
NULL

#' @rdname collapse
#' @importFrom purrr pluck
#' @example collapse_select(states, "COW")
#' @export
collapse_select <- function(dbase, dset){
  purrr::pluck(dbase, dset)
}

#' @rdname collapse
#' @importFrom purrr reduce
#' @importFrom dplyr full_join
#' @example collapse_full(states, "ID")
#' @export
collapse_full <- function(dbase, key, resolve = NULL){
  if(is.null(resolve)) {
    key = resolve_mean(key)
  } else if(resolve == "max") {
    key = resolve_max(key)
  } else if (resolve == "min") {
    key = resolve_min(key)
  } else if (resolve == "median") {
    key = resolve_median(key)
  } else if (resolve == "mode") {
    key = resolve_mode(key)
  }
  purrr::reduce(dbase, function(x, y) dplyr::full_join(x, y, by = key))
}

#' @rdname collapse
#' @importFrom purrr reduce
#' @importFrom dplyr inner_join
#' @example collapse_consensus(states, "ID")
#' @export
collapse_consensus <- function(dbase, key, resolve = NULL){
  if(is.null(resolve)) {
    key = resolve_mean(key)
  } else if(resolve == "max") {
    key = resolve_max(key)
  } else if (resolve == "min") {
    key = resolve_min(key)
  } else if (resolve == "median") {
    key = resolve_median(key)
  } else if (resolve == "mode") {
    key = resolve_mode(key)
  }
  purrr::reduce(dbase, function(x, y) dplyr::inner_join(x, y, by = key))
}
