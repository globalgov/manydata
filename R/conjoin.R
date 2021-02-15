#' @export
conjoin_full <- function(dbase, key){
  purrr::reduce(dbase, function(x, y) dplyr::full_join(x, y, by = key))
}

#' @export
conjoin_consensus <- function(dbase, key){
  purrr::reduce(dbase, function(x, y) dplyr::inner_join(x, y, by = key))
}