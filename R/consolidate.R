#' Consolidate database into a single dataset
#'
#' This function collapses a set or database of (q)datasets
#' into a single dataset with some combination of the
#' rows, columns, and observations of the parent datasets.
#' The function includes separate arguments for the rows and columns,
#' as well as for how to resolve conflicts in observations across datasets.
#' This provides users with considerable flexibility in how they combine qData.
#' For example, users may wish to stick to units that appear in every dataset
#' but include variables coded in any dataset,
#' or units that appear in any dataset
#' but only those variables that appear in every dataset.
#' Even then there may be conflicts, as the actual unit-variable observations
#' may differ from dataset to dataset.
#' Here we (will) offer a number of resolve methods that enable users to choose
#' how conflicts between observations are resolved.
#' @param database A qPackage database object
#' @param rows Which rows or units to retain.
#' By default "any" (or all) units are retained,
#' but another option is "every",
#' which retains only those units that appear in all parent datasets.
#' @param cols Which columns or variables to retain.
#' By default "any" (or all) variables are retained,
#' but another option is "every",
#' which retains only those variables that appear in all parent datasets.
#' @param resolve How conflicts between observations should be resolved?
#' Currently "coalesce", "min" and "max" are offered.
#' "coalesce" takes the first non-NA value.
#' "max" takes the largest value,
#' while "min" takes the smallest value.
#' Other options should soon include
#' "mean", "mode" and "median", as well as "append".
#' @param key An ID column to collapse by. By default "qID".
#' @seealso [pluck()] for selecting a single dataset from a database
#' @return A single tibble/data frame.
#' @name consolidate
NULL

#' @importFrom purrr pluck
#' @export
purrr::pluck

#' @rdname consolidate
#' @examples
#' pluck(emperors, "UNRV")
#' consolidate(emperors, "any", "any", resolve = "coalesce", key = "ID")
#' consolidate(emperors, "every", "every", resolve = "coalesce", key = "ID")
#' consolidate(emperors, "any", "every", resolve = "min", key = "ID")
#' consolidate(emperors, "any", "every", resolve = "max", key = "ID")
#' consolidate(emperors, "any", "every", resolve = "median", key = "ID")
#' consolidate(emperors, "every", "every", resolve = c(Beg = "min", End = "max"), key = "ID")
#' @export
consolidate <- function(database,
                        rows = c("any", "every"),
                        cols = c("any", "every"),
                        resolve = c("coalesce", "min", "max", "median"),
                        key = "qID") {

  # Step 1: Join datasets by ID
  rows <- match.arg(rows)
  if (rows == "any") {
    out <- purrr::reduce(database, dplyr::full_join, by = key)
  } else if (rows == "every") {
    out <- purrr::reduce(database, dplyr::inner_join, by = key)
  }

  # Step 2: Drop any unwanted variables
  cols <- match.arg(cols)
  all_variables <- unname(unlist(purrr::map(database, names)))
  if (cols == "every") {
    all_variables <- names(table(all_variables)[table(all_variables) == length(database)])
    out <- out %>% dplyr::select(all_of(key), starts_with(all_variables))
  }

  # Step 3: Resolve conflicts
  if (length(resolve) < 2) {
  resolve <- match.arg(resolve)
  other_variables <- unname(all_variables[!key == all_variables])
  if (resolve == "coalesce") {
    out <- r_coalesce(other_variables, out, key)
  }
  if (resolve == "min") {
    out <- r_min(other_variables, out, key)
  }
  if (resolve == "max") {
    out <- r_max(other_variables, out, key)
  }
  if (resolve == "median") {
    out <- r_median(other_variables, out, key)
  }
  # if (resolve == "mean") {
  # out <- r_mean(other_variables, out, key)
  # }
  } else {
    resolve <- data.frame(var = names(resolve), resolve = resolve)
    for (k in seq_len(nrow(resolve))) {
      if (resolve$resolve[k] == "coalesce") {
        rc <- r_coalesce(resolve$var[k], out, key)
      }
      if (resolve$resolve[k] == "min") {
        rmin <- r_min(resolve$var[k], out, key)
      }
      if (resolve$resolve[k] == "max") {
        rmax <- r_max(resolve$var[k], out, key)
      }
      if (resolve$resolve[k] == "median") {
        rmd <- r_median(resolve$var[k], out, key)
      }
      # if (resolve$resolve[k] == "mean") {
      #   rm <- r_mean(resolve$var[k])
      # }
    }
    if (exists("rc")) {
      out <- rc
    } else {
      out <- dplyr::select(out, key)
    }
    if (exists("rmin")) {
      out <- dplyr::full_join(out, rmin, by = key)
    }
    if (exists("rmax")) {
      out <- dplyr::full_join(out, rmax, by = key)
    }
    if (exists("rmd")) {
      out <- dplyr::full_join(out, rmd, by = key)
    }
  }
  out <- dplyr::distinct(out)
  if (any(duplicated(out[, 1]))) out <- coalesce_compatible(out)
  out
}

#' Coalesce all compatible rows of a data frame
#'
#' This function identifies and coalesces
#' all compatible rows in a data frame.
#' Compatible rows are defined as those rows where
#' all present elements are equal,
#' allowing for equality where one row has an element present
#' and the other is missing the observation.
#' @param .data data frame to consolidate
#' @importFrom utils combn
#' @importFrom dplyr coalesce bind_rows slice
#' @importFrom progress progress_bar
#' @examples
#' eg1 <- tribble(
#' ~x, ~y, ~z,
#' "a", "b", NA,
#' "a", "b", "c",
#' "j", "k", NA,
#' NA, "k", "l")
#' coalesce_compatible(eg1)
#' @export
coalesce_compatible <- function(.data) {

  pairs <- compatible_rows(.data)

  if (length(pairs) > 0) {
    merged <- apply(pairs, 1, function(x) {
      dplyr::coalesce(.data[x[1], ], .data[x[2], ])
    })
    merged <- dplyr::bind_rows(merged)

    dplyr::bind_rows(dplyr::slice(.data, -unique(c(pairs))),
                     merged)
  } else .data
}

compatible_rows <- function(x) {

  complete_vars <- x[, apply(x, 2, function(y) !any(is.na(y)))]
  compat_candidates <- which(duplicated(complete_vars) | duplicated(complete_vars, fromLast = TRUE))
  if (length(compat_candidates) == 0) {
    pairs <- vector(mode = "numeric", length = 0)
  } else {
    pairs <- t(utils::combn(compat_candidates, 2))
    pb <- progress::progress_bar$new(format = "identifying compatible pairs [:bar] :percent eta: :eta",
                                     total = nrow(pairs))

    compatico <- apply(pairs, 1, function(y) {
      pb$tick()
      o <- x[y[1], ] == x[y[2], ]
      o[is.na(o)] <- TRUE
      o
    })
    pairs[apply(t(compatico), 1, function(y) all(y)), ]
  }
}

#' Resolve Coalesce
#'
#' Helper function for resolving a database into a dataframe with coalesce.
#' "coalesce" takes the first non-NA value
#' @param other_variables A list of variables to be resolved
#' @param out A dataframe
#' @param key The ID column to collapse by. By default "qID"
#' @return The resolved dataframed or variable
r_coalesce <- function(other_variables, out, key) {
  for (var in other_variables) {
    vars_to_combine <- startsWith(names(out), var)
    new_var <- dplyr::coalesce(!!!out[vars_to_combine])
    out <- out %>% dplyr::select(-dplyr::starts_with(var))
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, key, other_variables)
  }
  out
}

#' Resolve Minimum
#'
#' Helper function for resolving a database into a dataframe with coalesce.
#' "Minimum" takes the smallest non-NA value
#' @param other_variables A list of variables to be resolved
#' @param out A dataframe
#' @param key The ID column to collapse by. By default "qID"
#' @return The resolved dataframed or variable
r_min <- function(other_variables, out, key) {
  for (var in other_variables) {
    vars_to_combine <- startsWith(names(out), var)
    new_var <- out[vars_to_combine]
    cl <- lapply(new_var, class)
    if (cl[[1]] == "messydt") {
      new_var <- apply(new_var, 1, function(x) as.character(min(x, na.rm = TRUE)))
    } else {
      new_var <- apply(new_var, 1, function(x) min(x, na.rm = TRUE))
    }
    out <- out %>% dplyr::select(-dplyr::starts_with(var))
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, key, other_variables)
  }
  out
}

#' Resolve Maximum
#'
#' Helper function for resolving a database into a dataframe with coalesce.
#' "maximum" takes the maximum non-NA value
#' @param other_variables A list of variables to be resolved
#' @param out A dataframe
#' @param key The ID column to collapse by. By default "qID"
#' @return The resolved dataframed or variable
r_max <- function(other_variables, out, key) {
  for (var in other_variables) {
    vars_to_combine <- startsWith(names(out), var)
    new_var <- out[vars_to_combine]
    cl <- lapply(new_var, class)
    if (cl[[1]] == "messydt") {
      new_var <- apply(new_var, 1, function(x) as.character(max(x, na.rm = TRUE)))
    } else {
      new_var <- apply(new_var, 1, function(x) max(x, na.rm = TRUE))
    }
    out <- out %>% dplyr::select(-dplyr::starts_with(var))
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, key, other_variables)
  }
  out
}

#' Resolve Median
#'
#' Helper function for resolving a database into a dataframe with coalesce.
#' "Median" takes the median value
#' @param other_variables A list of variables to be resolved
#' @param out A dataframe
#' @param key The ID column to collapse by. By default "qID"
#' @return The resolved dataframed or variable
r_median <- function(other_variables, out, key) {
  for (var in other_variables) {
    vars_to_combine <- startsWith(names(out), var)
    new_var <- out[vars_to_combine]
    cl <- lapply(new_var, class)
    if (cl[[1]] == "messydt") {
      new_var <- suppressWarnings(apply(new_var, 1,
                                        function(x) as.character(stats::median(x, na.rm = TRUE))))
    } else {
      new_var <- suppressWarnings(apply(new_var, 1, function(x) stats::median(x, na.rm = TRUE)))
    }
    # Sub NAs for first non NA value
    a <- dplyr::coalesce(!!!out[vars_to_combine])
    new_var <- ifelse(is.na(new_var), a, new_var)
    out <- out %>% dplyr::select(-dplyr::starts_with(var))
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, key, other_variables)
  }
  out
}

#' Resolve Mean
#'
#' Helper function for resolving a database into a dataframe with coalesce.
#' "mean" takes the average value
#' @param other_variables A list of variables to be resolved
#' @param out A dataframe
#' @param key The ID column to collapse by. By default "qID"
#' @return The resolved dataframed or variable
r_mean <- function(other_variables, out, key) {
  for (var in other_variables) {
    vars_to_combine <- startsWith(names(out), var)
    new_var <- out[vars_to_combine]
    cl <- lapply(new_var, class)
    if (cl[[1]] == "messydt") {
      new_var <- sapply(new_var, function(x) {
        if (length(x) > 1) x <- as.character(mean(as.Date(x),
                                                  trim = 0, na.rm = TRUE))
        x
      })
    } else {
      new_var <- apply(new_var, 1, function(x) mean(x, trim = 0, na.rm = TRUE))
    }
    # Sub NAs for first non NA value
    a <- dplyr::coalesce(!!!out[vars_to_combine])
    new_var <- ifelse(is.na(new_var), a, new_var)
    out <- out %>% dplyr::select(-dplyr::starts_with(var))
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, key, other_variables)
  }
  out
}
