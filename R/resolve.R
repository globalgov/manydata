#' Resolving multiple observations of the same variable into one
#' @param .data A data frame or tibble containing the variables.
#' @param vars A vector of variables from `.data` to be resolved or converged.
#' @name resolving
NULL

#' @rdname resolving
#' @examples
#' test <- data.frame(bloop.x = c(1,6,NA), 
#'                    bloop.y = c(2,NA,3), bloop = c(NA,3,4))
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
#' @section Unite: 
#'   Note that uniting always returns a character/string vector.
#'   Values are separated by commas and a set is contained within braces.
#' @examples
#' resolve_unite(test)
#' @export
resolve_unite <- function(.data, vars){
  if(missing(vars)) vars <- names(.data)
  toRes <- dplyr::select(.data, dplyr::all_of(vars))
  apply(toRes, 1, function(x) paste0("{", paste(unique(na.omit(x)), 
                                                  collapse = ","),
                                       "}"))
}

#' @rdname resolving
#' @examples
#' resolve_min(test)
#' @export
resolve_min <- function(.data, vars){
  if(missing(vars)) vars <- names(.data)
  .data <- as.data.frame(.data)
  toRes <- dplyr::select(.data, dplyr::all_of(vars))
  out <- apply(toRes, 1, function(x) min(x, na.rm = TRUE))
  if(class(out) != class(.data[,vars[1]]))
    class(out) <- class(.data[,vars[1]])
  out
}

#' @rdname resolving
#' @examples
#' resolve_max(test)
#' @export
resolve_max <- function(.data, vars){
  if(missing(vars)) vars <- names(.data)
  .data <- as.data.frame(.data)
  toRes <- dplyr::select(.data, dplyr::all_of(vars))
  out <- apply(toRes, 1, function(x) max(x, na.rm = TRUE))
  if(class(out) != class(.data[,vars[1]]))
    class(out) <- class(.data[,vars[1]])
  out
}

#' @rdname resolving
#' @examples
#' resolve_random(test)
#' @export
resolve_random <- function(.data, vars){
  if(missing(vars)) vars <- names(.data)
  toRes <- dplyr::select(.data, dplyr::all_of(vars))
  data.frame(toRes)[matrix(c(seq.int(nrow(toRes)), 
                             sample.int(ncol(toRes), nrow(toRes), 
                                        replace = TRUE)),
                           nrow = nrow(toRes))]
}

#' @rdname resolving
#' @examples
#' resolve_precision(test)
#' @export
resolve_precision <- function(.data, vars){
  if(missing(vars)) vars <- names(.data)
  toRes <- dplyr::select(.data, dplyr::all_of(vars))
  apply(toRes, 1, function(x) x[which.max(precision(x))])
}

#' @rdname resolving
#' @examples
#' resolve_mean(test)
#' @export
resolve_mean <- function(.data, vars, na.rm = FALSE) {
  if (missing(vars)) vars <- names(.data)
  toRes <- dplyr::select(.data, dplyr::all_of(vars))
  
  mat <- as.matrix(toRes)
  rowMeans(mat, na.rm = na.rm)
}

#' @rdname resolving
#' @examples
#' test2 <- cbind(test, bloopy = c(2,NA,3))
#' resolve_mode(test2)
#' @export
resolve_mode <- function(.data, vars, na.rm = FALSE) {
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
#' resolve_median(test2)
#' @export
resolve_median <- function(.data, vars, na.rm = FALSE) {
  if (missing(vars)) vars <- names(.data)
  toRes <- dplyr::select(.data, dplyr::all_of(vars))
  
  mat <- as.matrix(toRes)
  apply(mat, 1, median, na.rm = na.rm)
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
