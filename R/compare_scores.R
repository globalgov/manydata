#' @title Scoring Functions for Data Quality Checks
#' @description A set of functions to score various aspects of data quality,
#'   including date consistency, duplicates, recency, frequency, time, coding,
#'   comments, sources, missing values, and variables.
#' @param df A data frame to be scored.
#' @details
#'   These functions are designed to help assess the quality of data in a data frame.
#'   Each function checks a specific aspect of the data and returns a score or
#'   a message indicating the quality of that aspect.
#'   The functions include:
#'   - `score_date_consistency`: Proportion of invalid date pairs (End <= Begin).
#'   - `score_duplicates`: Proportion of duplicate IDs.
#' @name scores

#' @rdname scores
#' @examples
#' score_obs_no(HUGGO)
#' @export
score_obs_no <- function(df) {
  nrow(df)
}

#' @rdname scores
#' @examples
#' score_var_no(HUGGO)
#' @export
score_var_no <- function(df) {
  ncol(df[,!grepl("ID", names(df))])
}


#' @rdname scores
#' @param id_col The name of the column containing IDs. 
#'   Default is "ID".
#' @examples
#' score_duplicates(emperors)
#' @export
score_duplicates <- function(df, id_col = "ID") {
  
  # If df is a list, apply the function to each element
  if(is.list(df) && !is.data.frame(df)) { 
    return(vapply(df, score_duplicates, numeric(1))) 
  }
  
  # Count the number of duplicates in the specified ID column
  dupls <- find_duplicates(df, id_col)
  sum_duplicated <- nrow(dupls)

  if (sum_duplicated > 0) {
    message(paste("There are", sum_duplicated, "duplicate IDs in the", id_col, "column."))
    print(dupls)
  } else {
    message(paste("There are no duplicate IDs in the", id_col, "column."))
  }
  sum_duplicated/nrow(df)
}

