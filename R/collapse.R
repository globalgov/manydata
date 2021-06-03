#' #' Select one or multiple columns from a list of datasets
#' #' 
#' #' collapse_select allows users to select one or multiple columns
#' #' from a dataset in a database.
#' #' @param dbase A qPackage database object
#' #' @param dset A dataset label from within that database
#' #' @return A tibble with the collapsed data.
#' #' @importFrom purrr pluck
#' #' @examples
#' #' \dontrun{
#' #' collapse_select(qStates::states, "COW")
#' #' }
#' #' @export
#' 
#' collapse_select <- function(dbase, dset){
#'   purrr::pluck(dbase, dset)
#' }
#' 
#' #' Collapse a database into a single data frame/tibble
#' #' 
#' #' collapse_full() allows users to perform a full join on one or
#' #' more datasets in a database for further analysis.
#' #' @param dbase A qPackage database object (can be subsetted)
#' #' @param key An ID column to collapse by
#' #' @param resolve How do you want differences to be resolved?
#' #' It takes "max", "min", "mean", "mode" and "median" options.
#' #' @return A tibble with the collapsed data.
#' #' @examples
#' #' \dontrun{
#' #' collapse_full(qStates::states[1:2], "ID")
#' #' }
#' #' @importFrom purrr reduce
#' #' @importFrom dplyr full_join
#' #' @export
#' 
#' collapse_full <- function(dbase, key, resolve = NULL){
#'   
#'   if(is.null(resolve) || resolve == "mean") {
#'     for (i in c(1:length(dbase))) {
#'       dbase[[i]][key] <- resolve_mean(pull(dbase[[i]], key))
#'     }
#'   } else if(resolve == "max") {
#'     for (i in c(1:length(dbase))) {
#'       dbase[[i]][key] <- resolve_max(pull(dbase[[i]], key))
#'     }
#'   } else if (resolve == "min") {
#'     for (i in c(1:length(dbase))) {
#'       dbase[[i]][key] <- resolve_min(pull(dbase[[i]], key))
#'     }
#'   } else if (resolve == "median") {
#'     for (i in c(1:length(dbase))) {
#'       dbase[[i]][key] <- resolve_median(pull(dbase[[i]], key))
#'     }
#'   } else if (resolve == "mode") {
#'     for (i in c(1:length(dbase))) {
#'       dbase[[i]][key] <- resolve_mode(pull(dbase[[i]], key))
#'     }
#'   }
#'   
#'   purrr::reduce(dbase, function(x, y) dplyr::full_join(x, y, by = key))
#' }
#' 
#' #' Collapse a database into a single data frame/tibble
#' #' 
#' #' collapse_inner() allows users to perform an inner join on one or
#' #' more datasets in a database for further analysis.
#' #' @param dbase A qPackage database object (can be subsetted)
#' #' @param key An ID column to collapse by
#' #' @param resolve How do you want differences to be resolved?
#' #' It takes "max", "min", "mean", "mode" and "median" options.
#' #' @return A tibble with the collapsed data.
#' #' @examples
#' #' \dontrun{
#' #' collapse_inner(qStates::states[1:2], "ID")
#' #' }
#' #' @importFrom purrr reduce
#' #' @importFrom dplyr inner_join
#' #' @export
#' 
#' collapse_inner <- function(dbase, key, resolve = NULL){
#' # Let's rename this one collapse_inner for consistency ?
#'   if(is.null(resolve) || resolve == "mean") {
#'     for (i in c(1:length(dbase))) {
#'       dbase[[i]][key] <- resolve_mean(pull(dbase[[i]], key))
#'     }
#'   } else if(resolve == "max") {
#'     for (i in c(1:length(dbase))) {
#'       dbase[[i]][key] <- resolve_max(pull(dbase[[i]], key))
#'     }
#'   } else if (resolve == "min") {
#'     for (i in c(1:length(dbase))) {
#'       dbase[[i]][key] <- resolve_min(pull(dbase[[i]], key))
#'     }
#'   } else if (resolve == "median") {
#'     for (i in c(1:length(dbase))) {
#'       dbase[[i]][key] <- resolve_median(pull(dbase[[i]], key))
#'     }
#'   } else if (resolve == "mode") {
#'     for (i in c(1:length(dbase))) {
#'       dbase[[i]][key] <- resolve_mode(pull(dbase[[i]], key))
#'     }
#'   }
#'   
#'   purrr::reduce(dbase, function(x, y) dplyr::inner_join(x, y, by = key))
#' }
#' 
#' #' Collapse a database into a single data frame/tibble
#' #' 
#' #' collapse_left() allows users to perform a left join on one or
#' #' more datasets in a database for further analysis.
#' #' @param dbase A qPackage database object (can be subsetted)
#' #' @param key An ID column to collapse by
#' #' @param resolve How do you want differences to be resolved?
#' #' It takes "max", "min", "mean", "mode" and "median" options.
#' #' @return A tibble with the collapsed data.
#' #' @examples
#' #' \dontrun{
#' #' collapse_left(qStates::states[1:2], "ID")
#' #' }
#' #' @importFrom purrr reduce
#' #' @importFrom dplyr left_join
#' #' @export
#' 
#' collapse_left <- function(dbase, key, resolve = NULL){
#'   
#'   if(is.null(resolve) || resolve == "mean") {
#'     for (i in c(1:length(dbase))) {
#'       dbase[[i]][key] <- resolve_mean(pull(dbase[[i]], key))
#'     }
#'   } else if(resolve == "max") {
#'     for (i in c(1:length(dbase))) {
#'       dbase[[i]][key] <- resolve_max(pull(dbase[[i]], key))
#'     }
#'   } else if (resolve == "min") {
#'     for (i in c(1:length(dbase))) {
#'       dbase[[i]][key] <- resolve_min(pull(dbase[[i]], key))
#'     }
#'   } else if (resolve == "median") {
#'     for (i in c(1:length(dbase))) {
#'       dbase[[i]][key] <- resolve_median(pull(dbase[[i]], key))
#'     }
#'   } else if (resolve == "mode") {
#'     for (i in c(1:length(dbase))) {
#'       dbase[[i]][key] <- resolve_mode(pull(dbase[[i]], key))
#'     }
#'   }
#'   
#'   purrr::reduce(dbase, function(x, y) dplyr::left_join(x, y, by = key))
#' }
#' 
#' #' Collapse a database into a single data frame/tibble
#' #' 
#' #' collapse_right() allows users to perform a left join on one or
#' #' more datasets in a database for further analysis.
#' #' @param dbase A qPackage database object (can be subsetted)
#' #' @param key An ID column to collapse by
#' #' @param resolve How do you want differences to be resolved?
#' #' It takes "max", "min", "mean", "mode" and "median" options.
#' #' @return A tibble with the collapsed data.
#' #' @examples
#' #' \dontrun{
#' #' collapse_right(qStates::states[1:2], "ID")
#' #' }
#' #' @importFrom purrr reduce
#' #' @importFrom dplyr right_join
#' #' @export
#' 
#' collapse_right <- function(dbase, key, resolve = NULL){
#'   
#'   if(is.null(resolve) || resolve == "mean") {
#'     for (i in c(1:length(dbase))) {
#'       dbase[[i]][key] <- resolve_mean(pull(dbase[[i]], key))
#'     }
#'   } else if(resolve == "max") {
#'     for (i in c(1:length(dbase))) {
#'       dbase[[i]][key] <- resolve_max(pull(dbase[[i]], key))
#'     }
#'   } else if (resolve == "min") {
#'     for (i in c(1:length(dbase))) {
#'       dbase[[i]][key] <- resolve_min(pull(dbase[[i]], key))
#'     }
#'   } else if (resolve == "median") {
#'     for (i in c(1:length(dbase))) {
#'       dbase[[i]][key] <- resolve_median(pull(dbase[[i]], key))
#'     }
#'   } else if (resolve == "mode") {
#'     for (i in c(1:length(dbase))) {
#'       dbase[[i]][key] <- resolve_mode(pull(dbase[[i]], key))
#'     }
#'   }
#'   
#'   purrr::reduce(dbase, function(x, y) dplyr::right_join(x, y, by = key))
#' }
