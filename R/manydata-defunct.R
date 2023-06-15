#' Functions that have been renamed, superseded, or are no longer working
#' 
#' `r lifecycle::badge("deprecated")`
#' Generally these functions have been superseded or renamed.
#' Upon using them, a message is provided directing the user to the new function.
#' However, at this stage of package development,
#' we generally clear older defunct functions at each minor release,
#' and so you are strongly encouraged to use the new functions/names/syntax
#' wherever possible and update your scripts accordingly.
#' @name defunct
#' @keywords internal
NULL

#' @describeIn defunct Deprecated on 2022-10-18.
#' @export
coalesce_compatible <- function(.data) {
  .Defunct("tidyr::fill()", package = "manydata")
  coalesce_compatible(.data)
}

#' @describeIn defunct Removed on 2023-06-15.
#' @export
network_map <- function(.data) {
  .Defunct("manynet::autographr()", package = "manydata")
  network_map(.data)
}
