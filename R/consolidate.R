#' Consolidate database into a single dataset
#'
#' This function collapses a set or database of many datasets into a single
#' dataset with some combination of the rows, columns, and observations
#' of the datasets in a database.
#' The function includes separate arguments for the rows and columns,
#' as well as for how to resolve conflicts in observations across datasets.
#' This provides users with considerable flexibility in how they combine data.
#' For example, users may wish to stick to units that appear in
#' every dataset but include variables coded in any dataset,
#' or units that appear in any dataset
#' but only those variables that appear in every dataset.
#' Even then there may be conflicts, as the actual unit-variable
#' observations may differ from dataset to dataset.
#' Here we (will) offer a number of resolve methods that enable
#' users to choose how conflicts between observations are resolved.
#' @param database A database object from one of the many packages
#' @param rows Which rows or units to retain.
#' By default "any" (or all) units are retained,
#' but another option is "every",
#' which retains only those units that appear in all parent datasets.
#' @param cols Which columns or variables to retain.
#' By default "any" (or all) variables are retained,
#' but another option is "every",
#' which retains only those variables that appear
#' in all parent datasets.
#' @param resolve How should conflicts between observations be resolved?
#' Currently "coalesce", "min", "max", "mean", "median",
#' and "random" are offered.
#' "coalesce" takes the first non-NA value.
#' "max" takes the largest value,
#' while "min" takes the smallest value.
#' "mean" takes the average value.
#' "median" takes the median value.
#' "random" takes a random value.
#' For different variables to be resolved differently,
#' you can specify the variables' names alongside
#' how each is to be resolved in a list.
#' In this case, only the variables named will be resolved and returned.
#' @param key An ID column to collapse by.
#' By default "many_ID".
#' @importFrom purrr reduce map
#' @importFrom dplyr select full_join inner_join distinct all_of starts_with
#' @import messydates
#' @return A single tibble/data frame.
#' @examples
#' \donttest{
#' consolidate(emperors, "any", "any", resolve = "coalesce", key = "ID")
#' consolidate(favour(emperors, "UNRV"), "every", "every",
#' resolve = "coalesce", key = "ID")
#' consolidate(emperors, "any", "every", resolve = "min", key = "ID")
#' consolidate(emperors, "every", "any", resolve = "max", key = "ID")
#' consolidate(emperors, "every", "every", resolve = "median", key = "ID")
#' consolidate(emperors, "every", "every", resolve = "mean", key = "ID")
#' consolidate(emperors, "every", "every", resolve = "random", key = "ID")
#' consolidate(emperors, "every", "every",
#' resolve = c(Beg = "min", End = "max"), key = "ID")
#' }
#' @export
consolidate <- function(database,
                        rows = c("any", "every"),
                        cols = c("any", "every"),
                        resolve = c("coalesce", "min", "max", "median", "mean", "random"),
                        key = "manyID") {

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
    out <- resolve_coalesce(other_variables, out, key)
  }
  if (resolve == "min") {
    out <- resolve_min(other_variables, out, key)
  }
  if (resolve == "max") {
    out <- resolve_max(other_variables, out, key)
  }
  if (resolve == "median") {
    out <- resolve_median(other_variables, out, key)
  }
  if (resolve == "mean") {
  out <- resolve_mean(other_variables, out, key)
  }
  if (resolve == "random") {
    out <- resolve_random(other_variables, out, key)
  }
  } else {
    resolve <- data.frame(var = names(resolve), resolve = resolve)
    for (k in seq_len(nrow(resolve))) {
      if (resolve$resolve[k] == "coalesce") {
        rco <- resolve_coalesce(resolve$var[k], out, key)
      }
      if (resolve$resolve[k] == "min") {
        rmin <- resolve_min(resolve$var[k], out, key)
      }
      if (resolve$resolve[k] == "max") {
        rmax <- resolve_max(resolve$var[k], out, key)
      }
      if (resolve$resolve[k] == "median") {
        rmd <- resolve_median(resolve$var[k], out, key)
      }
      if (resolve$resolve[k] == "mean") {
        rme <- resolve_mean(resolve$var[k], out, key)
      }
      if (resolve$resolve[k] == "random") {
        rra <- resolve_random(resolve$var[k], out, key)
      }
    }
    if (exists("rco")) {
      out <- rco
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
    if (exists("rme")) {
      out <- dplyr::full_join(out, rme, by = key)
    }
    if (exists("rra")) {
      out <- dplyr::full_join(out, rra, by = key)
    }
  }
  out <- dplyr::distinct(out)
  if (any(duplicated(out[, 1]))) out <- coalesce_compatible(out)
  out
}

resolve_coalesce <- function(other_variables, out, key) {
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

resolve_min <- function(other_variables, out, key) {
  for (var in other_variables) {
    vars_to_combine <- startsWith(names(out), var)
    new_var <- out[vars_to_combine]
    for (k in names(new_var)) {
      dates <- dplyr::pull(new_var, k)
      if(class(dates) == "messydt") {
        dates <- suppressWarnings(as.Date(dates, min))
        new_var[k] <- dates
        }
      }
    new_var <- apply(new_var, 1, min)
    # Sub NAs for first non NA value
    a <- dplyr::coalesce(!!!out[vars_to_combine])
    new_var <- ifelse(is.na(new_var), a, new_var)
    out <- dplyr::select(out, -dplyr::starts_with(var))
    new_var
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, key, other_variables)
  }
  out
}

resolve_max <- function(other_variables, out, key) {
  for (var in other_variables) {
    vars_to_combine <- startsWith(names(out), var)
    new_var <- out[vars_to_combine]
    for (k in names(new_var)) {
      dates <- dplyr::pull(new_var, k)
      if(class(dates) == "messydt") {
        dates <- suppressWarnings(as.Date(dates, max))
        new_var[k] <- dates
      }
    }
    new_var <- apply(new_var, 1, max)
    # Sub NAs for first non NA value
    a <- dplyr::coalesce(!!!out[vars_to_combine])
    new_var <- ifelse(is.na(new_var), a, new_var)
    out <- dplyr::select(out, -dplyr::starts_with(var))
    new_var
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, key, other_variables)
  }
  out
}

resolve_median <- function(other_variables, out, key) {
  for (var in other_variables) {
    vars_to_combine <- startsWith(names(out), var)
    new_var <- out[vars_to_combine]
    for (k in names(new_var)) {
      dates <- dplyr::pull(new_var, k)
      if(class(dates) == "messydt") {
        dates <- suppressWarnings(as.Date(dates, max))
        new_var[k] <- dates
      }
    }
    new_var <- suppressWarnings(apply(new_var, 1, stats::median))
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

resolve_mean <- function(other_variables, out, key) {
  for (var in other_variables) {
    vars_to_combine <- startsWith(names(out), var)
    new_var <- out[vars_to_combine]
    for (k in names(new_var)) {
      dates <- dplyr::pull(new_var, k)
      if(class(dates) == "messydt") {
        dates <- suppressWarnings(as.Date(dates, max))
        new_var[k] <- dates
      }
    }
    new_var <- suppressWarnings(apply(new_var, 1, mean))
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

resolve_random <- function(other_variables, out, key) {
  for (var in other_variables) {
    vars_to_combine <- startsWith(names(out), var)
    new_var <- out[vars_to_combine]
    for (k in names(new_var)) {
      dates <- dplyr::pull(new_var, k)
      if(class(dates) == "messydt") {
        dates <- suppressWarnings(as.Date(dates, max))
        new_var[k] <- dates
      }
    }
    new_var <- apply(new_var, 1, function(x) sample(x, size = 1))
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

#' Selects a single dataset from a database
#' 
#' @importFrom purrr pluck
#' @return The selected dataset
#' @details This function is reexported from the purrr package.
#' It allows users to select a single dataset from one
#' of the databases available across the 'many* packages'.
#' @examples
#' \donttest{
#' pluck(emperors, "UNRV")
#' }
#' @export
purrr::pluck

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
#' eg1 <- tibble::tribble(
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

#' Favour datasets in a database
#' 
#' @name favour
#' @param database A many database
#' @param dataset The name of one, or more, datasets within the database
#' to be favoured over others.
#' @details The dataset declared becomes the reference for
#' the first non NA value.
#' If more than one dataset is declared,
#' please add datasets increasing order of importance
#' (.i.e. last dataset should be favoured over previous).
#' @return The database with datasets re-ordered accordingly
#' @aliases favor
#' @examples
#' favour(emperors, "UNRV")
#' favour(emperors, c("wikipedia", "UNRV", "britannica"))
#' @export
favour <- function(database, dataset) {
  if (length(dataset) > 1) {
    for (n in unlist(dataset)) {
      fav <- database[n]
      database[n] <- NULL
      database <- append(fav, database)
    }
  } else {
    fav <- database[dataset]
    database[dataset] <- NULL
    database <- append(fav, database)
  }
  database
}

#' @rdname favour
#' @export
favor <- favour
