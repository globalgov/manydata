#' Find elements within manydata
#' @name find
#' @inheritParams scores
NULL

#' @rdname find
#' @param id_col The name of the column containing IDs. 
#'   Default is "ID".
#' @export
find_ID <- function(df, id_col = "ID"){
  if (!id_col %in% names(df)) {
    id_col <- names(df)[endsWith(names(df), "ID")][1]
    if(is.null(id_col)) {
      stop(paste("The data frame does not contain a", id_col, "column."))
    }}
  return(id_col)
}

getID <- function(datacube){
  if(grepl("ID", names(datacube[[1]])[2])){
    c(names(datacube[[1]])[1], names(datacube[[1]])[2])
  } else names(datacube[[1]])[1]
}

#' @rdname find
#' @param ... Data frames to compare
#' @export
find_common_ID <- function(..., id_col = "ID"){
  dfs <- list(...)
  if(length(dfs) < 2) stop("Please provide at least two data frames to compare.")
  id_cols <- sapply(dfs, function(df) names(df)[endsWith(names(df), "ID")])
  id_col <- Reduce(intersect, id_cols)[1]
  if(is.null(id_col)) stop(paste("The data frame does not contain a common ID column."))
  return(id_col)
}

#' Creates Numerical IDs from Signature Dates
#' @description
#'   Agreements should have a unique identification
#'   number that is meaningful, we condense their
#'   signature dates to produce this number.
#' @param date A date variable
#' @return A character vector with condensed dates
#' @import stringr
#' @examples
#' \dontrun{
#' IEADB <- dplyr::slice_sample(manyenviron::agreements$IEADB, n = 10)
#' code_dates(IEADB$Title)
#' }
#' @export
find_year <- function(date) {
  # Step 1: collapse dates
  uID <- stringr::str_remove_all(date, "-")
  # Step 2: code missing dates as far future dates to facilitate identification
  uID[is.na(uID)] <- paste0(sample(5000:9999, 1), "NULL")
  # Step 3: remove ranges, first date is taken
  uID <- stringr::str_replace_all(uID, "\\:[:digit:]{8}$", "")
  # Step 4: keep year only
  uID <- ifelse(nchar(uID) > 4, substr(uID, 1, nchar(uID) - 4), uID)
  uID
}

#' @rdname find
#' @examples
#' find_duplicates(emperors$Wikipedia)
#' @export
find_duplicates <- function(df, id_col = "ID"){
  
  # Check if the data frame has a '..ID' column
  id_col <- find_ID(df, id_col)
  
  # Find the duplicates for the specified ID column
  non_na_ids <- !is.na(df[[id_col]])
  dupes <- duplicated(df[[id_col]][non_na_ids]) | duplicated(df[[id_col]][non_na_ids], fromLast = TRUE)
  out <- df[which(non_na_ids)[dupes], ]
  if(nrow(out)==0){
    message(paste("There are no duplicate IDs in the", id_col, "column."))
    return(NULL)
  } else out
}

