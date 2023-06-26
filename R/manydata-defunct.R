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
  .Defunct("manydata::map_plot()", package = "manydata")
  network_map(.data)
}

#' @describeIn defunct Removed on 2023-06-19.
#' @export
db_plot <- function(.data) {
  .Defunct("manydata::plot_categories()", package = "manydata")
  db_plot(.data)
}

#' @describeIn defunct Removed on 2023-06-19.
#' @export
db_comp <- function(.data) {
  .Defunct("manydata::compare_categories()", package = "manydata")
  db_comp(.data)
}

#' @describeIn defunct Removed on 2023-06-19.
#' @export
retrieve_bilaterals <- function(.data) {
  .Defunct("manydata::call_treaties()", package = "manydata")
  retrieve_bilaterals(.data)
}

#' @describeIn defunct Removed on 2023-06-19.
#' @export
retrieve_multilaterals <- function(.data) {
  .Defunct("manydata::call_treaties()", package = "manydata")
  retrieve_multilaterals(.data)
}

#' @describeIn defunct Removed on 2023-06-19.
#' @export
retrieve_links <- function(.data) {
  .Defunct("manydata::call_treaties()", package = "manydata")
  retrieve_links(.data)
}

#' @describeIn defunct Removed on 2023-06-19.
#' @export
retrieve_membership_list <- function(.data) {
  .Defunct("manydata::call_treaties()", package = "manydata")
  retrieve_membership_list(.data)
}

#' @describeIn defunct Removed on 2023-06-19.
#' @export
data_source <- function(.data) {
  .Defunct("manydata::call_sources()", package = "manydata")
  data_source(.data)
}

#' @describeIn defunct Removed on 2023-06-19.
#' @export
get_packages <- function(.data) {
  .Defunct("manydata::call_packages()", package = "manydata")
  get_packages(.data)
}

#' @describeIn defunct Removed on 2023-06-19.
#' @export
open_codebook <- function(.data) {
  .Defunct("manydata::call_sources()", package = "manydata")
  open_codebook(.data)
}

# #' @describeIn defunct Removed on 2023-06-19.
# #' @export
# retrieve_texts <- function(.data) {
#   .Defunct("manydata::call_texts()", package = "manydata")
#   retrieve_texts(.data)
# }

#' @describeIn defunct Removed on 2023-06-19.
#' @export
data_contrast <- function(.data) {
  .Defunct("manydata::compare_data()", package = "manydata")
  data_contrast(.data)
}

#' @describeIn defunct Removed on 2023-06-19.
#' @export
data_evolution <- function(.data) {
  .Defunct("manydata::compare_data()", package = "manydata")
  data_evolution(.data)
}
