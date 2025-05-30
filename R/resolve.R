#' Resolving multiple observations of the same variable into one
#' @description
#'   This family of functions provides row-wise summarization for 
#'   data frames or tibbles, 
#'   returning a single value per row based on specified columns.
#'   They are useful for tasks like extracting typical or summary values from
#'   multiple variables, simplifying wide data structures,
#'   and imputing representative values.
#' @param .data A data frame or tibble containing the variables.
#' @param vars A vector of variables from `.data` to be resolved or converged.
#'   If this argument is left unspecified, 
#'   then all variables will be merged together.
#' @param na.rm Logical whether missing values (NAs) should be removed
#'   before operation of the function.
#'   Note that unlike how the `na.rm` argument operates in functions in
#'   base R, e.g. `max()`, here the default is TRUE.
#' @name resolving
#' @examples
#' test <- data.frame(preferred_dataset = c(1,6,NA), 
#'                    more_comprehensive = c(1,3,3), 
#'                    precise_where_available = c(NA,3.3,4.1))
#' test
NULL

#' @rdname resolving
#' @section Unite: 
#'   Uniting returns all the unique values as a set, 
#'   separated by commas and contained within braces.
#'   Note that uniting always returns a character/string vector,
#'   which enables it to accommodate different classes of variables.
#'   The order of the values reflects their first appearance;
#'   that is, they are not ordered by increasing value.
#' @examples
#' resolve_unite(test)
#' @export
resolve_unite <- function(.data, vars, na.rm = TRUE){
  if(missing(vars)) vars <- names(.data)
  toRes <- dplyr::select(.data, dplyr::all_of(vars))
  apply(toRes, 1, function(x) paste0("{", paste(unique(
    `if`(na.rm, na.omit(x), x)), collapse = ","),
    "}"))
}

#' @rdname resolving
#' @section Coalesce: 
#'   Coalescing returns a vector of the first non-missing values
#'   found when reading the variables from left to right.
#'   That is, missing values in the first vector may be filled by
#'   observations in the second vector, or later vectors if the second
#'   vector also misses an observation for that cell.
#'   Variables can be reordered manually.
#' @examples
#' resolve_coalesce(test)
#' @export
resolve_coalesce <- function(.data, vars){
  if(missing(vars)) vars <- names(.data)
  .data <- as.data.frame(.data)
  toCoal <- dplyr::select(.data, dplyr::all_of(vars))
  out <- .data %>% dplyr::mutate(dplyr::coalesce(!!!as.data.frame(toCoal))) %>% 
    dplyr::pull(var = -1)
  if(class(out) != class(.data[,vars[1]]))
    class(out) <- class(.data[,vars[1]])
  out
}

#' @rdname resolving
#' @section Min and Max: 
#'   These functions return a vector containing 
#'   each row's minimum or maximum value.
#'   Note that these functions work not only on numeric and date vectors,
#'   but also on character string vectors.
#'   For character data, these functions will return the shortest
#'   or longest strings, respectively, in each row.
#' @examples
#' resolve_min(test)
#' @export
resolve_min <- function(.data, vars, na.rm = TRUE){
  if(missing(vars)) vars <- names(.data)
  .data <- as.data.frame(.data)
  toRes <- dplyr::select(.data, dplyr::all_of(vars))
  out <- apply(toRes, 1, function(x) min(x, na.rm = na.rm))
  if(class(out) != class(.data[,vars[1]]))
    class(out) <- class(.data[,vars[1]])
  out
}

#' @rdname resolving
#' @examples
#' resolve_max(test)
#' @export
resolve_max <- function(.data, vars, na.rm = TRUE){
  if(missing(vars)) vars <- names(.data)
  .data <- as.data.frame(.data)
  toRes <- dplyr::select(.data, dplyr::all_of(vars))
  out <- apply(toRes, 1, function(x) max(x, na.rm = na.rm))
  if(class(out) != class(.data[,vars[1]]))
    class(out) <- class(.data[,vars[1]])
  out
}

#' @rdname resolving
#' @section Random: 
#'   This function returns a vector of values selected randomly
#'   from among the values contained in each row.
#'   Note that by default `na.rm = TRUE`, which means that missing data
#'   will not be selected at random by default, 
#'   which can also change the probability distribution by each row.
#'   Where `na.rm = FALSE`, the probability of each value being selected 
#'   is uniform.
#' @examples
#' resolve_random(test)
#' @export
resolve_random <- function(.data, vars, na.rm = TRUE) {
  if (missing(vars)) vars <- names(.data)
  toRes <- dplyr::select(.data, dplyr::all_of(vars))
  
  if (!na.rm) {
    # Sample columns per row (including NAs)
    mat <- as.matrix(toRes)
    n <- nrow(mat)
    m <- ncol(mat)
    col_idx <- sample.int(m, n, replace = TRUE)
    row_idx <- seq_len(n)
    return(mat[cbind(row_idx, col_idx)])
  } else {
    # Long format: filter NAs, sample one value per row
    toRes %>%
      mutate(.row = row_number()) %>%
      pivot_longer(-.row) %>%
      filter(!is.na(value)) %>%
      group_by(.row) %>%
      slice_sample(n = 1) %>%
      arrange(.row) %>%
      pull(value)
  }
}

#' @rdname resolving
#' @section Precision: 
#'   This function returns a vector that maximises the precision of the values
#'   in each row. 
#'   For numeric vectors, precision is expressed in significant digits,
#'   such that 1.01 would be more precise than 1.
#'   For character vectors, precision is expressed in terms of the 
#'   character length proportional to the max character length in the row.
#'   This applies also to messydates, meaning
#'   precision is expressed in the lowest level date component specified,
#'   such that 2008-10 would be more precise than 2008,
#'   and 2008-10-10 would be more precise still.
#' @examples
#' resolve_precision(test)
#' @export
resolve_precision <- function(.data, vars){
  if(missing(vars)) vars <- names(.data)
  toRes <- dplyr::select(.data, dplyr::all_of(vars))
  apply(toRes, 1, function(x) x[which.max(precision(x))])
}

#' @rdname resolving
#' @section Mean and median: 
#'   These functions return a vector of the means or medians, respectively, 
#'   of the values in each row.
#' @examples
#' resolve_mean(test)
#' @export
resolve_mean <- function(.data, vars, na.rm = TRUE) {
  if (missing(vars)) vars <- names(.data)
  toRes <- dplyr::select(.data, dplyr::all_of(vars))
  
  mat <- as.matrix(toRes)
  rowMeans(mat, na.rm = na.rm)
}

#' @rdname resolving
#' @examples
#' resolve_mode(test)
#' @export
resolve_mode <- function(.data, vars, na.rm = TRUE) {
  if (missing(vars)) vars <- names(.data)
  toRes <- dplyr::select(.data, dplyr::all_of(vars))
  
  toRes %>%
    mutate(.row = row_number()) %>%
    pivot_longer(-.row) %>%
    { if (na.rm) filter(., !is.na(value)) else . } %>%
    group_by(.row, value) %>%
    summarise(n = n(), .groups = "drop_last") %>%
    filter(n == max(n, na.rm = TRUE)) %>%
    slice(1) %>%  # break ties arbitrarily (first mode)
    ungroup() %>%
    arrange(.row) %>%
    pull(value)
}

#' @rdname resolving
#' @examples
#' resolve_median(test)
#' @export
resolve_median <- function(.data, vars, na.rm = TRUE) {
  if (missing(vars)) vars <- names(.data)
  toRes <- dplyr::select(.data, dplyr::all_of(vars))
  
  mat <- as.matrix(toRes)
  apply(mat, 1, median, na.rm = na.rm)
}

#' @rdname resolving
#' @section Consensus: 
#'   This function returns a vector of consensus values,
#'   i.e. where there is no variation in values by each row.
#'   If the values (excluding missing values by default) are not equivalent,
#'   then an NA is returned for that row.
#' @examples
#' resolve_consensus(test)
#' @export
resolve_consensus <- function(.data, vars, na.rm = TRUE) {
  if (missing(vars)) vars <- names(.data)
  toRes <- dplyr::select(.data, dplyr::all_of(vars))
  mat <- as.matrix(toRes)
  
  apply(mat, 1, function(row) {
    values <- if (!na.rm) row else row[!is.na(row)]
    if (length(values) == 0) return(NA)
    if (length(unique(values)) == 1) return(values[1])
    return(NA)
  })
}

#' @export
precision.numeric <- function(x){

  # Convert all to scientific notation
  num_str <- format(x, scientific = TRUE, na.encode = FALSE)
  num_str[grepl("NA",num_str)] <- NA
  
  # Extract significant digits
  sig_digits <- gsub("e.*", "", num_str)  # Remove exponent part
  sig_digits <- gsub("\\.", "", sig_digits) # Remove decimal point
  
  # Count nonzero significant digits
  out <- nchar(gsub("^0+|0+$", "", sig_digits)) # Remove leading zeros and count
  out/max(out, na.rm = TRUE)
}

#' @export
precision.character <- function(x){
  # Count characters
  out <- nchar(x)
  out/max(out)
}

#' @export
precision.default <- function(x){
  # Count characters
  out <- nchar(x)
  out/max(out)
}
