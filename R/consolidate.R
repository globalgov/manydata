#' Consolidate database into a single dataset
#'
#' This function consolidates a set of datasets in a 'many* package' database
#' into a single dataset with some combination of the rows, columns,
#' and observations of the datasets in the database.
#' The function includes separate arguments for the rows and columns,
#' as well as for how to resolve conflicts for observations across datasets.
#' This provides users with considerable flexibility in how they combine data.
#' For example, users may wish to stick to units that appear in
#' every dataset but include variables coded in any dataset,
#' or units that appear in any dataset
#' but only those variables that appear in every dataset.
#' Even then there may be conflicts, as the actual unit-variable
#' observations may differ from dataset to dataset.
#' We offer a number of resolve methods that enable
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
#' By default "coalesce",
#' but other options include: "min", "max", "mean", "median", and "random".
#' "coalesce" takes the first non-NA value.
#' "max" takes the largest value.
#' "min" takes the smallest value.
#' "mean" takes the average value.
#' "median" takes the median value.
#' "random" takes a random value.
#' For different variables to be resolved differently,
#' you can specify the variables' names alongside
#' how each is to be resolved in a list
#' (e.g. `resolve = c(var1 = "min", var2 = "max")`).
#' In this case, only the variables named will be resolved and returned.
#' @param key An ID column to collapse by.
#' By default "many_ID".
#' Users can also specify multiple key variables in a list.
#' For multiple key variables, the key variables must be present in
#' all the datasets in the database (e.g. `key = c("key1", "key2")`).
#' For equivalent key columns with different names across datasets,
#' matching is possible if keys are declared (e.g. `key = c("key1" = "key2")`).
#' @details Text variables are dropped for more efficient consolidation.
#' @importFrom purrr reduce map
#' @importFrom dplyr select full_join inner_join distinct all_of
#' @import messydates
#' @return A single tibble/data frame.
#' @examples
#' \donttest{
#' consolidate(database = emperors, key = "ID")
#' consolidate(database = favour(emperors, "UNRV"), rows = "every",
#' cols = "every", resolve = "coalesce", key = "ID")
#' consolidate(database = emperors, rows = "any", cols = "every",
#' resolve = "min", key = "ID")
#' consolidate(database = emperors, rows = "every", cols = "any",
#' resolve = "max", key = "ID")
#' consolidate(database = emperors, rows = "every", cols = "every",
#' resolve = "median", key = "ID")
#' consolidate(database = emperors, rows = "every", cols = "every",
#' resolve = "mean", key = "ID")
#' consolidate(database = emperors, rows = "every", cols = "every",
#' resolve = "random", key = "ID")
#' consolidate(database = emperors, rows = "every", cols = "every",
#' resolve = c(Beg = "min", End = "max"), key = "ID")
#' consolidate(database = emperors, rows = "any", cols = "any",
#' resolve = c(Death = "max", Cause = "coalesce"),
#' key = c("ID", "Beg"))
#' }
#' @export
consolidate <- function(database, rows = "any", cols = "any",
                        resolve = "coalesce", key = "manyID") {
  # Check that database has multiple datasets
  if (length(database) == 1) {
    dataset <- names(database)
    database <- deparse(substitute(database))
    stop(paste0(database, " contains only the ", dataset,
                " dataset and cannot be consolidated."))
  }
  # Step 1: Join datasets by ID and keep pertinent rows
  if (rows == "any") {
    out <- purrr::reduce(database, dplyr::full_join, by = key)
  } else if (rows == "every") {
    out <- purrr::reduce(database, dplyr::inner_join, by = key)
  }
  # Step 2: Drop any unwanted columns (including text variables)
  all_variables <- grep("text", unname(unlist(purrr::map(database, names))),
                        ignore.case = TRUE, value = TRUE, invert = TRUE)
  out <- dplyr::select(out, -dplyr::all_of(grep("text", names(out),
                                                ignore.case = TRUE,
                                                value = TRUE)))
  if (cols == "every") {
    all_variables <- names(table(all_variables)[table(all_variables) ==
                                                  length(database)])
    out <- dplyr::select(out, dplyr::all_of(key),
                                 dplyr::starts_with(all_variables))
  }
  # Step 3: Resolve conflicts
  if (length(resolve) < 2) {
  other_variables <- all_variables[!all_variables %in% key]
  out <- resolve_unique(resolve, other_variables, out, key)
  } else {
    resolve <- data.frame(var = names(resolve), resolve = resolve)
    out <- resolve_multiple(resolve, out, key)
  }
  # Step 4: Remove duplicates
  out <- dplyr::distinct(out)
  if (any(duplicated(out[, 1]))) out <- coalesce_compatible(out)
  out
}

resolve_unique <- function(resolve, other_variables, out, key) {
  if (resolve == "coalesce") {
    out <- resolve_coalesce(other_variables, out, key)
  } else if (resolve == "min") {
    out <- resolve_min(other_variables, out, key)
  } else if (resolve == "max") {
    out <- resolve_max(other_variables, out, key)
  } else if (resolve == "median") {
    out <- resolve_median(other_variables, out, key)
  } else if (resolve == "mean") {
    out <- resolve_mean(other_variables, out, key)
  } else if (resolve == "random") {
    out <- resolve_random(other_variables, out, key)
  }
  out
}

resolve_multiple <- function(resolve, out, key) {
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
  out
}

resolve_coalesce <- function(other_variables, out, key) {
  for (var in other_variables) {
    vvars <- paste0("^", var, "$|^", var, "\\.")
    vars_to_combine <- grep(vvars, names(out), value = TRUE)
    new_var <- dplyr::coalesce(!!!out[vars_to_combine])
    out <- dplyr::select(out, -dplyr::all_of(vars_to_combine))
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, dplyr::all_of(key), other_variables)
  }
  out
}

resolve_min <- function(other_variables, out, key) {
  for (var in other_variables) {
    vvars <- paste0("^", var, "$|^", var, "\\.")
    vars_to_combine <- grep(vvars, names(out), value = TRUE)
    new_var <- out[vars_to_combine]
    for (k in names(new_var)) {
      dates <- dplyr::pull(new_var, k)
      if (inherits(dates, "messydt")) {
        dates <- suppressWarnings(as.Date(dates, min))
        new_var[k] <- dates
        }
      }
    new_var <- apply(new_var, 1, min)
    # Sub NAs for first non NA value
    a <- dplyr::coalesce(!!!out[vars_to_combine])
    new_var <- ifelse(is.na(new_var), a, new_var)
    out <- dplyr::select(out, -dplyr::all_of(vars_to_combine))
    new_var
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, dplyr::all_of(key), other_variables)
  }
  out
}

resolve_max <- function(other_variables, out, key) {
  for (var in other_variables) {
    vvars <- paste0("^", var, "$|^", var, "\\.")
    vars_to_combine <- grep(vvars, names(out), value = TRUE)
    new_var <- out[vars_to_combine]
    for (k in names(new_var)) {
      dates <- dplyr::pull(new_var, k)
      if (inherits(dates, "messydt")) {
        dates <- suppressWarnings(as.Date(dates, max))
        new_var[k] <- dates
      }
    }
    new_var <- apply(new_var, 1, max)
    # Sub NAs for first non NA value
    a <- dplyr::coalesce(!!!out[vars_to_combine])
    new_var <- ifelse(is.na(new_var), a, new_var)
    out <- dplyr::select(out, -dplyr::all_of(vars_to_combine))
    new_var
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, dplyr::all_of(key), other_variables)
  }
  out
}

resolve_median <- function(other_variables, out, key) {
  for (var in other_variables) {
    vvars <- paste0("^", var, "$|^", var, "\\.")
    vars_to_combine <- grep(vvars, names(out), value = TRUE)
    new_var <- out[vars_to_combine]
    for (k in names(new_var)) {
      dates <- dplyr::pull(new_var, k)
      if (inherits(dates, "messydt")) {
        dates <- suppressWarnings(as.Date(dates, max))
        new_var[k] <- dates
      }
    }
    new_var <- suppressWarnings(apply(new_var, 1, stats::median))
    # Sub NAs for first non NA value
    a <- dplyr::coalesce(!!!out[vars_to_combine])
    new_var <- ifelse(is.na(new_var), a, new_var)
    out <- dplyr::select(out, -dplyr::all_of(vars_to_combine))
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, dplyr::all_of(key), other_variables)
  }
  out
}

resolve_mean <- function(other_variables, out, key) {
  for (var in other_variables) {
    vvars <- paste0("^", var, "$|^", var, "\\.")
    vars_to_combine <- grep(vvars, names(out), value = TRUE)
    new_var <- out[vars_to_combine]
    for (k in names(new_var)) {
      dates <- dplyr::pull(new_var, k)
      if (inherits(dates, "messydt")) {
        dates <- suppressWarnings(as.Date(dates, max))
        new_var[k] <- dates
      }
    }
    new_var <- suppressWarnings(apply(new_var, 1, mean))
    # Sub NAs for first non NA value
    a <- dplyr::coalesce(!!!out[vars_to_combine])
    new_var <- ifelse(is.na(new_var), a, new_var)
    out <- dplyr::select(out, -dplyr::all_of(vars_to_combine))
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, dplyr::all_of(key), other_variables)
  }
  out
}

resolve_random <- function(other_variables, out, key) {
  for (var in other_variables) {
    vvars <- paste0("^", var, "$|^", var, "\\.")
    vars_to_combine <- grep(vvars, names(out), value = TRUE)
    new_var <- out[vars_to_combine]
    for (k in names(new_var)) {
      dates <- dplyr::pull(new_var, k)
      if (inherits(dates, "messydt")) {
        dates <- suppressWarnings(as.Date(dates, max))
        new_var[k] <- dates
      }
    }
    new_var <- apply(new_var, 1, function(x) sample(x, size = 1))
    # Sub NAs for first non NA value
    a <- dplyr::coalesce(!!!out[vars_to_combine])
    new_var <- ifelse(is.na(new_var), a, new_var)
    out <- dplyr::select(out, -dplyr::all_of(vars_to_combine))
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, dplyr::all_of(key), other_variables)
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
#' @return A tibble with the missing observations coalesced for compatible rows
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
    if (length(pairs) == 2) {
      merged <- dplyr::coalesce(.data[pairs[1], ], .data[pairs[2], ])
    } else {
      merged <- apply(pairs, 1, function(x) {
        dplyr::coalesce(.data[x[1], ], .data[x[2], ])
    })
    }
    merged <- dplyr::bind_rows(merged)
    dplyr::bind_rows(dplyr::slice(.data, -unique(c(pairs))), merged)
  } else .data
}

compatible_rows <- function(x) {
  complete_vars <- x[, apply(x, 2, function(y) !any(is.na(y)))]
  compat_candidates <- which(duplicated(complete_vars) |
                               duplicated(complete_vars, fromLast = TRUE))
  if (length(compat_candidates) == 0) {
    pairs <- vector(mode = "numeric", length = 0)
  } else {
    pairs <- t(utils::combn(compat_candidates, 2))
    pb <- progress::progress_bar$new(
      format = "identifying compatible pairs [:bar] :percent eta: :eta",
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
#' please list datasets in increasing order of importance
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
