#' @title Scoring Functions for Data Quality Checks
#' @description A set of functions to score various aspects of data quality,
#'   including date consistency, duplicates, recency, frequency, time, coding,
#'   comments, sources, missing values, and variables.
#'
#'   According to the literature, data quality can be assessed
#'   by checking for consistency, completeness, accuracy, timeliness, and
#'   uniqueness of the data.
#'   Consistency means that the data is logically coherent,
#'   completeness means that all required data is present,
#'   accuracy means that the data is correct and reliable,
#'   timeliness means that the data is up-to-date,
#'   and uniqueness means that there are no duplicate records.
#' @param df A data frame to be scored.
#' @details
#'   These functions are designed to help assess the quality of data in a data frame.
#'   Each function checks a specific aspect of the data and returns a score or
#'   a message indicating the quality of that aspect.
#'   The functions include:
#'   - `score_date_consistency`: Proportion of invalid date pairs (End <= Begin).
#'   - `score_duplicates`: Proportion of duplicate IDs.
#' @references
#'   Wang, R. Y., & Strong, D. M. (1996). Beyond accuracy: What data quality means to data consumers.
#'   Journal of Management Information Systems, 12(4), 5-34.
#' @name scores

#' @rdname scores
#' @examples
#' score_dataset(emperors)
#' @export
score_dataset <- function(df) {
  if(is.list(df) && !is.data.frame(df)) { 
    return(vapply(df, score_dataset, numeric(1))) 
  }
  (score_obs_no(df) * score_var_no(df)) * score_completeness(df)
}

#' @rdname scores
#' @examples
#' score_obs_no(emperors)
#' @export
score_obs_no <- function(df) {
  nrow(df)
}

#' @rdname scores
#' @examples
#' score_var_no(emperors)
#' @export
score_var_no <- function(df) {
  ncol(df[,!grepl("ID", names(df))])
}

#' @rdname scores
#' @examples
#' score_completeness(emperors)
#' @export
score_completeness <- function(df) {
  
  # Count the number of missing values in each column
  missing_counts <- colSums(is.na(df[,!grepl("ID", names(df))]))
  
  # Create a message with the number of missing values for each column
  message(paste("Missing values per variable:", 
                paste(names(missing_counts[missing_counts>0]), 
                      missing_counts[missing_counts>0], 
                      sep = ": ", collapse = ", ")))
  
  # Return the counts of missing values
  return(1 - (sum(missing_counts)/sum(is.na(df[,!grepl("ID", names(df))])|!is.na(df[,!grepl("ID", names(df))]))))
}


#' @rdname scores
#' @param id_col The name of the column containing IDs. 
#'   Default is "ID".
#' @examples
#' score_obs_info(emperors)
#' @export
score_obs_info <- function(df, id_col = "ID") {
  
  # If df is a list, apply the function to each element
  if(is.list(df) && !is.data.frame(df)) { 
    return(vapply(df, score_obs_info, numeric(1))) 
  }
  
  # Count the number of duplicates in the specified ID column
  dupls <- find_duplicates(df, id_col)
  if(is.null(dupls)) sum_duplicated <- 0 else 
    sum_duplicated <- nrow(dupls)

  if (sum_duplicated > 0) {
    message(paste("There are", sum_duplicated, "duplicate IDs in the", id_col, "column."))
    print(dupls)
  } else {
    message(paste("There are no duplicate IDs in the", id_col, "column."))
  }
  sum_duplicated/nrow(df)
}

