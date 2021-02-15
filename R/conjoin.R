#' @export
conjoin_full <- function(dbase, key){
  purrr::reduce(dbase, function(x, y) dplyr::full_join(x, y, by = key))
}