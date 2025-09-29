#' @title Scoring Functions for Data Quality Checks
#' @description A set of functions to assess various aspects of data quality.
#'   including a comprehensive dataset score as well as individual scores
#'   for specific data quality dimensions such as 
#'   date consistency, duplicates, recency, frequency, time, coding,
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
  if(is.list(df) && !is.data.frame(df)) { 
    return(vapply(df, score_obs_no, numeric(1))) 
  }
  nrow(df)
}

#' @rdname scores
#' @examples
#' score_var_no(emperors)
#' @export
score_var_no <- function(df) {
  if(is.list(df) && !is.data.frame(df)) { 
    return(vapply(df, score_var_no, numeric(1))) 
  }
  ncol(df[,!grepl("ID", names(df))])
}

#' @rdname scores
#' @examples
#' score_completeness(emperors)
#' @export
score_completeness <- function(df) {
  
  if(is.list(df) && !is.data.frame(df)) { 
    return(vapply(df, score_completeness, numeric(1))) 
  }
  
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
#' @examples
#' score_date_consistency(emperors)
#' @export
score_date_consistency <- function(df) {
  
  # If df is a list, apply the function to each element
  if(is.list(df) && !is.data.frame(df)) { 
    return(vapply(df, score_date_consistency, numeric(1))) 
  }
  
  # Check if requisite columns exist
  if (!"Begin" %in% names(df)) {
    stop("The data frame does not contain a 'Begin' column.")
  }
  if (!"End" %in% names(df)) {
    stop("The data frame does not contain an 'End' column.")
  }
  
  # Convert Begin to Date class, handling NA values
  test <- data.frame(Title = df[,1],
                     Begin = as.Date(df$Begin, FUN = vmax),
                     End = as.Date(df$End, FUN = vmin))

  # Remove rows where either Begin or End is NA
  test <- test[stats::complete.cases(test), ]

  # Check if there are End dates earlier or on the same day as the Begin dates
  invalid_dates <- test$End <= test$Begin
  percent_invalid <- sum(invalid_dates)/nrow(df)
  if(percent_invalid > 0){
    message(paste0("There are ", sum(invalid_dates), 
                   " potentially invalid date pairs (End <= Begin). ",
                   "This is ", round(percent_invalid * 100, 2), 
                   "% of the data. ",
                   "Please check the Begin and End dates for correctness."))
    print(test[invalid_dates,])
  } else {
    message("All date pairs are valid (End > Begin).")
  }
  percent_invalid
}

#' @rdname scores
#' @examples
#' score_date_scope(emperors)
#' @export
score_date_scope <- function(df) {
  
  # If df is a list, apply the function to each element
  if(is.list(df) && !is.data.frame(df)) { 
    return(vapply(df, score_date_scope, numeric(1))) 
  }
  
  # Convert Begin and End to Date class, handling NA values
  df$Begin <- as.Date(df$Begin, FUN = vmin)
  df$End <- as.Date(df$End, FUN = vmax)
  
  # Calculate the time scope
  time_scope <- max(df$End, na.rm = TRUE) - min(df$Begin, na.rm = TRUE)
  
  # Create a message with the time scope
  message(paste("The total time scope is", time_scope, "days,",
                "from", min(df$Begin, na.rm = TRUE), "to", max(df$End, na.rm = TRUE)))
  
  # Return the time scope
  return(time_scope)
  
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

#' @rdname scores
#' @export
score_coding <- function(df) {

  # If df is a list, apply the function to each element
  if(is.list(df) && !is.data.frame(df)) { 
    return(vapply(df, score_coding, numeric(1))) 
  }
  
  # Check if the data frame has a 'Coder' column
  if (!"Coder" %in% names(df)) {
    message("The data frame does not contain a 'Coder' column.")
    num_coders <- 0
  } else {
    # Count the number of unique coding values
    coders <- strsplit(df$Coder, ",")
    num_coders <- mean(lengths(coders))
  }
  
  # Create a message with the number of unique codings
  message(paste("The data frame has", num_coders, "coders named per row."))
  
  # Return the number of unique codings
  return(num_coders)
}

#' @rdname scores
#' @export
score_comments <- function(df) {

  # If df is a list, apply the function to each element
  if(is.list(df) && !is.data.frame(df)) { 
    return(vapply(df, score_comments, numeric(1))) 
  }
  
  # Check if the data frame has a 'Comments' column
  if (!"Comments" %in% names(df)) {
    message("The data frame does not contain a 'Comments' column.")
    num_comments <- 0
  } else {
    # Count the number of comments
    num_comments <- sum(!is.na(df$Comments) & df$Comments != "")
  }
  
  # Create a message with the number of comments
  message(paste("The data frame has", num_comments, "comments."))
  
  # Return the number of comments
  return(num_comments/nrow(df))
}

#' @rdname scores
#' @examples
#' score_var_info(emperors)
#' @export
score_var_info <- function(df) {

  # If df is a list, apply the function to each element
  if(is.list(df) && !is.data.frame(df)) { 
    return(vapply(df, score_var_info, numeric(1))) 
  }
  
  # Check if the data frame has any columns
  if (ncol(df) == 0) {
    stop("The data frame has no columns.")
  }
  
  # Count the number of variables (columns)
  num_vars <- ncol(df)
  
  # Create a message with the number of variables
  message(paste("The data frame has", num_vars, "variables."))
  
  # Return the number of variables
  return(num_vars)
}

# 
# #' @rdname scores
# #' @examples
# #' score_timeliness_frequency(IEADB)
# #' @export
# score_timeliness_recency <- function(df) {
# }

# #' @rdname scores
# #' @examples
# #' score_timeliness_frequency(IEADB)
# #' @export
# score_timeliness_frequency <- function(df) {
# }

# #' @rdname scores
# #' @examples
# #' score_sources(emperors)
# #' @export
# score_sources <- function(df) {
# }

