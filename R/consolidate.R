#' Consolidate datacube into a single dataset
#' @description
#'   This function consolidates a set of datasets in a 'many*' package datacube
#'   into a single dataset with some combination of the rows, columns,
#'   and observations of the datasets in the datacube.
#' @details
#'   The function includes separate arguments for the rows and columns,
#'   as well as for how to resolve conflicts for observations across datasets.
#'   This provides users with considerable flexibility in how they combine data.
#'   For example, users may wish to stick to units that appear in
#'   every dataset but include variables coded in any dataset,
#'   or units that appear in any dataset
#'   but only those variables that appear in every dataset.
#'   Even then there may be conflicts, as the actual unit-variable
#'   observations may differ from dataset to dataset.
#'   We offer a number of resolve methods that enable
#'   users to choose how conflicts between observations are resolved.
#' @param datacube A datacube from one of the many packages
#' @param rows Which rows or units to retain.
#'   By default "any" (or all) units are retained,
#'   but another option is "every",
#'   which retains only those units that appear in all parent datasets.
#' @param cols Which columns or variables to retain.
#'   By default "any" (or all) variables are retained,
#'   but another option is "every",
#'   which retains only those variables that appear
#'   in all parent datasets.
#' @param resolve How should conflicts between observations be resolved?
#'   By default "coalesce",
#'   but other options include: "min", "max", "mean", "median", and "random".
#'   "coalesce" takes the first non-NA value.
#'   "max" takes the largest value.
#'   "min" takes the smallest value.
#'   "mean" takes the average value.
#'   "median" takes the median value.
#'   "random" takes a random value.
#'   For different variables to be resolved differently,
#'   you can specify the variables' names alongside
#'   how each is to be resolved in a list
#'   (e.g. `resolve = c(var1 = "min", var2 = "max")`).
#'   In this case, only the variables named will be resolved and returned.
#' @param key An ID column to collapse by.
#'   By default "manyID".
#'   Users can also specify multiple key variables in a list.
#'   For multiple key variables, the key variables must be present in
#'   all the datasets in the datacube (e.g. `key = c("key1", "key2")`).
#'   For equivalent key columns with different names across datasets,
#'   matching is possible if keys are declared (e.g. `key = c("key1" = "key2")`).
#'   Missing observations in the key variable are removed.
#' @details Text variables are dropped for more efficient consolidation.
#' @importFrom purrr reduce map pluck
#' @importFrom dplyr select full_join inner_join distinct all_of
#' @importFrom dplyr group_by %>% mutate_at as_tibble
#' @importFrom tidyr drop_na
#' @importFrom plyr ddply
#' @importFrom zoo na.locf
#' @importFrom usethis ui_info
#' @importFrom messydates as_messydate
#' @return A single tibble/data frame.
#' @examples
#' \donttest{
#' consolidate(datacube = emperors, key = "ID")
#' consolidate(datacube = favour(emperors, "UNRV"), rows = "every",
#' cols = "every", resolve = "coalesce", key = "ID")
#' consolidate(datacube = emperors, rows = "any", cols = "every",
#' resolve = "min", key = "ID")
#' consolidate(datacube = emperors, rows = "every", cols = "any",
#' resolve = "max", key = "ID")
#' consolidate(datacube = emperors, rows = "every", cols = "every",
#' resolve = "median", key = "ID")
#' consolidate(datacube = emperors, rows = "every", cols = "every",
#' resolve = "mean", key = "ID")
#' consolidate(datacube = emperors, rows = "every", cols = "every",
#' resolve = "random", key = "ID")
#' consolidate(datacube = emperors, rows = "every", cols = "every",
#' resolve = c(Begin = "min", End = "max"), key = "ID")
#' consolidate(datacube = emperors, rows = "any", cols = "any",
#' resolve = c(Death = "max", Cause = "coalesce"),
#' key = c("ID", "Begin"))
#' }
#' @export
consolidate <- function(datacube, rows = "any", cols = "any",
                        resolve = "coalesce", key = NULL) {
  
  # Step 1: check that datacube has multiple datasets
  if (length(datacube) == 1) {
  
  # Step 1: Check datacube is not already consolidated ####
  if (!inherits(datacube, "list")) {
    dat <- deparse(substitute(datacube))
    cli::cli_alert_warning(paste(dat,"already consolidated."))
    cli::cli_alert_success(paste("Returning", dat))
    return(datacube)
  }
  if (inherits(datacube, "list") & length(datacube)==1) {
    dataset <- names(datacube)
    dat <- deparse(substitute(datacube))
    cli::cli_alert_warning(paste0(dat, " contains only the ", dataset,
                " dataset and cannot be consolidated further."))
    cli::cli_alert_success(paste("Returning", dataset))
    return(purrr::pluck(datacube, dataset))
  }
  
  # Step 2: check keys are correct ####
  if(is.null(key)){
    recog_keys <- c("stateID","stateID1","stateID2","manyID","leaderID","igoID")
    key <- recog_keys[recog_keys %in% names(datacube[[1]])]
    if(length(key)==0) cli::cli_abort("Please specify a {.var key} variable.")
  }
  if (grepl("membership", deparse(substitute(datacube)), ignore.case = TRUE) &
      length(key) == 1) {
    cli::cli_abort("For memberships datacube please indicate two keys, one identifying the
    agreements and one identifying the actors (e.g. {.var key = c('stateID', 'manyID')}).")
  }
  
  # Step 3: inform users about duplicates ####
  cli::cli_progress_message("Using {.var {key}} for matching observations across datasets...")
  if (length(key) == 1) {
    cli::cli_alert_success(paste("Matched",
                              prettyNum(sum(duplicated(unname(unlist(purrr::map(datacube, key))))), big.mark = ','), 
                              "observations by {.var {key}} variable{?s}",
                              "in {.var {deparse(substitute(datacube))}} datasets.\n"))
  }
  
  # Step 4: drop any unwanted columns (including text variables) ####
  cli::cli_progress_message("Dropping text variables...")
  all_variables <- grep("text", unname(unlist(purrr::map(datacube, names))),
                        ignore.case = TRUE, value = TRUE, invert = TRUE)
  vars_subset <- c(unique(all_variables), key)
  out <- purrr::map(datacube, .extract_if_present, vars_subset)
  
  if (cols == "every") {
    cli::cli_progress_message("Dropping unique variables...")
    shared_variables <- names(table(all_variables)[table(all_variables) ==
                                                  length(datacube)])
    out <- dplyr::select(out, dplyr::all_of(key),
                         dplyr::starts_with(shared_variables))
  }
  
  # Step 6: resolve conflicts ####
  cli::cli_progress_message("Resolving conflicts by {.var {resolve}}...")
  old_cols <- ncol(out)
  if (length(resolve) < 2) {
    other_variables <- unique(all_variables[!all_variables %in% key])
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
  } else {
    resolve <- data.frame(var = names(resolve), resolve = resolve)
    out <- resolve_multiple(resolve, out, key)
  }
  cli::cli_alert_success("Resolved {old_cols - ncol(out)} columns.")
  
  # Step 7: remove duplicates and fill NA values ####
  cli::cli_progress_message("Coalescing compatible rows...")
  old_rows <- nrow(out)
  out <- plyr::ddply(out, key, zoo::na.locf, na.rm = FALSE) %>%
    dplyr::as_tibble() %>%
    dplyr::select(-dplyr::starts_with("dplyr")) %>%
    dplyr::distinct()
  cli::cli_alert_success("Coalesced {old_rows - nrow(out)} rows.")
  
  out
}

.extract_if_present <- function(x, y) {
  x[intersect(y, names(x))]
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
    out <- dplyr::select(out, dplyr::all_of(key))
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
    vars_to_combine <- grep(paste0("^", var, "$|^", var, "\\."),
                            names(out), value = TRUE)
    new_var <- if (any(lapply(out[vars_to_combine], class) == "mdate")) {
      apply(out[vars_to_combine], 2, as.character)
    } else out[vars_to_combine]
    new_var <- dplyr::coalesce(!!!data.frame(new_var))
    out <- dplyr::select(out, -dplyr::all_of(vars_to_combine))
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, dplyr::all_of(key), dplyr::all_of(other_variables))
  }
  out
}

resolve_min <- function(other_variables, out, key) {
  for (var in other_variables) {
    vars_to_combine <- grep(paste0("^", var, "$|^", var, "\\."),
                            names(out), value = TRUE)
    new_var <- purrr::map_df(out[vars_to_combine], function(x) {
        if (messydates::is_messydate(x)) as.Date(x, min) else x
      })
    new_var <- suppressWarnings(do.call("c", purrr::pmap(
      new_var, ~ min(c(...), na.rm = TRUE))))
    if (any(grepl("^Inf$|^NaN$", new_var))) {
      new_var <- gsub("^Inf$|^NaN$", NA, new_var)
    }
    out <- dplyr::select(out, -dplyr::all_of(vars_to_combine))
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, dplyr::all_of(key), dplyr::all_of(other_variables))
  }
  out
}

resolve_max <- function(other_variables, out, key) {
  for (var in other_variables) {
    vars_to_combine <- grep(paste0("^", var, "$|^", var, "\\."),
                            names(out), value = TRUE)
    new_var <- purrr::map_df(out[vars_to_combine], function(x) {
      if (messydates::is_messydate(x)) as.Date(x, max) else x
      })
    new_var <- suppressWarnings(do.call("c", purrr::pmap(
      new_var, ~ max(c(...), na.rm = TRUE))))
    if (any(grepl("^Inf$|^NaN$", new_var))) {
      new_var <- gsub("^Inf$|^NaN$", NA, new_var)
    }
    out <- dplyr::select(out, -dplyr::all_of(vars_to_combine))
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, dplyr::all_of(key), dplyr::all_of(other_variables))
  }
  out
}

resolve_median <- function(other_variables, out, key) {
  for (var in other_variables) {
    vars_to_combine <- grep(paste0("^", var, "$|^", var, "\\."),
                            names(out), value = TRUE)
    new_var <- purrr::map_df(out[vars_to_combine], function(x) {
      if (messydates::is_messydate(x)) as.Date(x, median) else x
      })
    new_var <- suppressWarnings(do.call("c", purrr::pmap(
      new_var, ~ stats::median(c(...), na.rm = TRUE))))
    if (any(grepl("^Inf$|^NaN$", new_var))) {
      new_var <- gsub("^Inf$|^NaN$", NA, new_var)
    }
    out <- dplyr::select(out, -dplyr::all_of(vars_to_combine))
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, dplyr::all_of(key), dplyr::all_of(other_variables))
  }
  out
}

resolve_mean <- function(other_variables, out, key) {
  for (var in other_variables) {
    vars_to_combine <- grep(paste0("^", var, "$|^", var, "\\."),
                            names(out), value = TRUE)
    new_var <- purrr::map_df(out[vars_to_combine], function(x) {
      if (messydates::is_messydate(x)) as.Date(x, mean) else x
      })
    if (any(lapply(new_var, class) == "character")) {
      message("Calculating the mean is not possible for character(s) variables.
              Returning first non-missing value instead.")
      new_var <- dplyr::coalesce(!!!out[vars_to_combine])
    } else {
      new_var <- suppressWarnings(do.call("c", purrr::pmap(
        new_var, ~ mean(c(...), na.rm = TRUE))))
    }
    if (any(grepl("^Inf$|^NaN$", new_var))) {
      new_var <- gsub("^Inf$|^NaN$", NA, new_var)
    }
    out <- dplyr::select(out, -dplyr::all_of(vars_to_combine))
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, dplyr::all_of(key), dplyr::all_of(other_variables))
  }
  out
}

resolve_random <- function(other_variables, out, key) {
  for (var in other_variables) {
    vars_to_combine <- grep(paste0("^", var, "$|^", var, "\\."),
                            names(out), value = TRUE)
    new_var <- purrr::map_df(out[vars_to_combine], function(x) {
      if (messydates::is_messydate(x)) as.Date(x, random) else x
      })
    new_var <- apply(new_var, 1, function(x) sample(x, size = 1))
    out <- dplyr::select(out, -dplyr::all_of(vars_to_combine))
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, dplyr::all_of(key), dplyr::all_of(other_variables))
  }
  out
}

#' Selects a single dataset from a datacube
#'
#' @importFrom purrr pluck
#' @return The selected dataset
#' @details This function is reexported from the purrr package.
#' It allows users to select a single dataset from one
#' of the datacubes available across the 'many* packages'.
#' @examples
#' \donttest{
#' pluck(emperors, "UNRV")
#' }
#' @export
purrr::pluck

#' Favour datasets in a datacube
#'
#' @name favour
#' @param datacube A many datacube
#' @param dataset The name of one, or more, datasets within the datacube
#' to be favoured over others.
#' @details The dataset declared becomes the reference for
#' the first non NA value.
#' If more than one dataset is declared,
#' please list datasets in increasing order of importance
#' (.i.e. last dataset should be favoured over previous).
#' @return The datacube with datasets re-ordered accordingly
#' @aliases favor
#' @examples
#' \donttest{
#' favour(emperors, "UNRV")
#' favour(emperors, c("wikipedia", "UNRV", "britannica"))
#' }
#' @export
favour <- function(datacube, dataset) {
  if (length(dataset) > 1) {
    for (n in unlist(dataset)) {
      fav <- datacube[n]
      datacube[n] <- NULL
      datacube <- append(fav, datacube)
    }
  } else {
    fav <- datacube[dataset]
    datacube[dataset] <- NULL
    datacube <- append(fav, datacube)
  }
  datacube
}

#' @rdname favour
#' @export
favor <- favour
