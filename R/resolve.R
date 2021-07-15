#' #' Fills missing data by lookup
#' #'
#' #' Fills missing data where known by other observations with the same id/index
#' #' @param df a dataframe
#' #' @param id a string identifying a column in the dataframe for indexing
#' #' @param var a string identifying a column or columns in the dataframe
#' #' to be filled
#' #' @return a dataframe
#' #' @examples
#' #' \dontrun{
#' #' myData <- repaint(myData, id="StatID", var=c("Area","Region")
#' #' }
#' #' @export
#' repaint <- function(df, id, var) {
#'   for (co in var) {
#'     for (ea in unique(df[, id])) {
#'       if (any(!is.na(df[df[, id] == ea, co])) &
#'           any(is.na(df[df[, id] == ea, co]))) {
#'         df[df[,id] == ea &
#'              is.na(df[, co]), co] <- df[df[, id] == ea &
#'                                           !is.na(df[, co]), co][1]
#'       }
#'     }
#'   }
#'   df
#' }
