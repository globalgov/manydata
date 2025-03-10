# Consolidate ####

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
#' @param join Which join procedure to use.
#'   By default "full" so that all observations are retained,
#'   but other options include "left" for basing the consolidated dataset
#'   on observations present in the first dataset 
#'   (reorder the datasets to favour another dataset),
#'   and "inner" for a consolidated dataset that includes only observations
#'   that are present in all datasets.
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
#' @importFrom dtplyr lazy_dt
#' @importFrom messydates as_messydate
#' @return A single tibble/data frame.
#' @examples
#' \donttest{
#' consolidate(emperors, join = "full", resolve = "coalesce", key = "ID")
#' consolidate(emperors, join = "inner", resolve = "min", key = "ID")
#' consolidate(emperors, join = "left", resolve = "max", key = "ID")
#' }
#' @export
consolidate <- function(datacube, 
                        join = c("full", "inner", "left"),
                        # cols = "any",
                        resolve = "coalesce", key = NULL) {
  
  join <- match.arg(join)
  
  # Step 1: Check datacube not already consolidated ####
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
  
  # Step 2: Check keys correct ####
  if(is.null(key)){
    recog_keys <- c("stateID","stateID1","stateID2","manyID","leaderID","igoID","treatyID","ID")
    key <- recog_keys[recog_keys %in% names(datacube[[1]])]
    if(length(key)==0) cli::cli_abort("Please specify a {.var key} variable.")
  }
  if (grepl("membership", deparse(substitute(datacube)), ignore.case = TRUE) &
      length(key) == 1) {
    cli::cli_abort("For memberships datacube please indicate two keys, one identifying the
    agreements and one identifying the actors (e.g. {.var key = c('stateID', 'manyID')}).")
  }
  
  # Step 3: Inform about duplicates ####
  cli::cli_progress_message("Using {.var {key}} for matching observations across datasets...")
  if (length(key) == 1) {
    cli::cli_alert_success(paste("Matched",
                              prettyNum(sum(duplicated(unname(unlist(purrr::map(datacube, key))))), big.mark = ','), 
                              "observations by {.var {key}} variable{?s}",
                              "in {.var {deparse(substitute(datacube))}} datasets.\n"))
  }
  
  # # Step 4: Drop unwanted columns (including text variables) ####
  # cli::cli_progress_message("Dropping text variables...")
  # all_variables <- grep("text", unname(unlist(purrr::map(datacube, names))),
  #                       ignore.case = TRUE, value = TRUE, invert = TRUE)
  # out <- purrr::map(datacube, .extract_if_present, vars_subset)
  all_variables <- unname(unlist(purrr::map(datacube, names)))
  vars_subset <- c(unique(all_variables), key)
  out <- datacube
  
  # Step 5: Join datasets by ID ####
  cli::cli_progress_message("Joining {length(datacube)} datasets using a {join} join...")
  # out <- purrr::reduce(out, dplyr::full_join, by = key)
  out <- suppressWarnings(switch(join,
                full = purrr::reduce(purrr::map(out, dtplyr::lazy_dt, key_by = key),
                                     dplyr::full_join, by = key) %>% as_tibble(),
                inner = purrr::reduce(out,
                                      dplyr::inner_join, by = key) %>% as_tibble(),
                left = purrr::reduce(out,
                                     dplyr::left_join, by = key) %>% as_tibble()))
  # out <- purrr::reduce(purrr::map(out, dtplyr::lazy_dt, key_by = key),
  #                      dplyr::full_join, by = key) %>% as_tibble()
  # out <- purrr::reduce(out, collapse::join, on = key, how = join,
  #                      multiple = FALSE, verbose = 0)
  cli::cli_alert_success("Joined {length(datacube)} datasets using a {join} join.")
  if(interactive())
    call_citations(deparse(substitute(datacube)), output = "console")
  
  # if (rows == "any") {
  #   cli::cli_alert_info("Joining datasets to observations in any dataset.")
  #   out <- purrr::map(out, tidyr::drop_na, dplyr::all_of(key)) %>%
  #     purrr::reduce(dplyr::full_join, by = key)
  # } else if (rows == "every") {
  #   cli::cli_alert_info("Joining datasets to observations shared by every dataset.")
  #   out <- purrr::reduce(out, dplyr::inner_join, by = key)
  # }
  # if (cols == "every") {
  #   cli::cli_progress_message("Dropping unique variables...")
  #   shared_variables <- names(table(all_variables)[table(all_variables) ==
  #                                                 length(datacube)])
  #   out <- dplyr::select(out, dplyr::all_of(key),
  #                        dplyr::starts_with(shared_variables))
  # }
  
  # Step 6: resolve conflicts ####
  cli::cli_progress_message("Resolving conflicts by {.var {resolve}}...")
  old_cols <- ncol(out)
  if (length(resolve) < 2) {
    other_variables <- unique(all_variables[!all_variables %in% key])
    if (resolve == "coalesce") {
      out <- resolve_coalesce(out, key, other_variables)
    } else if (resolve == "min") {
      out <- resolve_min(out, key, other_variables)
    } else if (resolve == "max") {
      out <- resolve_max(out, key, other_variables)
    } else if (resolve == "median") {
      out <- resolve_median(out, key, other_variables)
    } else if (resolve == "mean") {
      out <- resolve_mean(out, key, other_variables)
    } else if (resolve == "random") {
      out <- resolve_random(out, key, other_variables)
    }
  } else {
    resolve <- data.frame(var = names(resolve), resolve = resolve)
    out <- resolve_multiple(out, key, resolve)
  }
  cli::cli_alert_success("Resolved {old_cols - ncol(out)} shared variables.")
  
  # Step 7: remove duplicates and fill NA values ####
  # cli::cli_progress_message("Coalescing compatible observations...")
  # old_rows <- nrow(out)
  # out <- plyr::ddply(out, key, zoo::na.locf, na.rm = FALSE) %>%
  #   dplyr::as_tibble() %>%
  #   dplyr::select(-dplyr::starts_with("dplyr")) %>%
  #   dplyr::distinct()
  # cli::cli_alert_success("Coalesced {old_rows - nrow(out)} observations")
  
  out
}

.extract_if_present <- function(x, y) {
  x[intersect(y, names(x))]
}

# Pluck ####

#' Selects a single dataset from a datacube
#' @description This function is reexported from the `{purrr}` package.
#'   It allows users to select a single dataset from one
#'   of the datacubes available across the 'many* packages'.
#'.  It additionally invites users to cite the 
#' @importFrom purrr pluck
#' @return The selected dataset
#' @examples
#' \donttest{
#' pluck(emperors, "UNRV")
#' }
#' @export
pluck <- function(.x, ..., .default = NULL){
  call_citations(paste0(deparse(substitute(.x)), "$", list(...)[[1]]))
  purrr::pluck(.x = .x, ..., .default = .default)
}

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

