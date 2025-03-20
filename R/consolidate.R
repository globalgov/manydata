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
#' @param resolve Choice how (potentially conflicting) values from shared
#'   variables should be resolved. Options include:
#'   
#'   - "coalesce" (default): uses first non-NA value (if available) for
#'   each observation, essentially favouring the order the datasets are in
#'   in the datacube.
#'   - "unite": combines the unique values for each observation across datasets
#'   as a set (separated by commas and surrounded by braces), which can be
#'   useful for retaining information.
#'   - "random": selects values at random from among the observations from each
#'   dataset that observed that variable, of particular use for exploring the
#'   implications of dataset-related variation.
#'   - "precise": selects the value that has the highest precision from among 
#'   the observations from each dataset (see `resolving_precision()`),
#'   which favours more precise data.
#'   - "min", "max": these options return the minimum or maximum values
#'   respectively, which can be useful for conservative temporal fixing.
#'   
#'   To resolve variables by different functions, 
#'   pass the argument a vector
#'   (e.g. `resolve = c(var1 = "min", var2 = "max")`).
#'   Unnamed variables will be resolved according to the default ("coalesce").
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
                        resolve = "coalesce", 
                        key = NULL) {
  
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
  
  # Step 4: Join datasets by ID ####
  cli::cli_progress_message("Joining {length(datacube)} datasets using a {join} join...")
  all_variables <- unname(unlist(purrr::map(datacube, names)))
  vars_subset <- c(unique(all_variables), key)
  out <- datacube
  out <- suppressWarnings(switch(join,
                full = purrr::reduce(ifelse(length(key)>1, 
                                            purrr::map(out, dtplyr::lazy_dt, key_by = key),
                                            out),
                                     dplyr::full_join, by = key) %>% as_tibble(),
                inner = purrr::reduce(out,
                                      dplyr::inner_join, by = key) %>% as_tibble(),
                left = purrr::reduce(out,
                                     dplyr::left_join, by = key) %>% as_tibble()))
  # out <- purrr::reduce(out, collapse::join, on = key, how = join,
  #                      multiple = FALSE, verbose = 0)
  # duckplyr considered too, but difficulty in converting mdate class columns
  cli::cli_alert_success("Joined {length(datacube)} datasets using a {join} join.")
  if(interactive())
    call_citations(deparse(substitute(datacube)), output = "console")
  
  # Step 5: Resolve conflicts ####
  dupes <- grepl("\\.x|\\.y", names(out))
  if(any(dupes)){
    vars_to_resolve <- unique(gsub("\\.x|\\.y", "", names(out)[dupes]))
    if(length(resolve)==1){
      resolve <- data.frame(resolve, var = vars_to_resolve)
    } else {
      resolve <- dplyr::full_join(as_tibble(as.data.frame(resolve), 
                                            rownames = "var"), 
                                  data.frame(var = vars_to_resolve), by = "var")
      resolve$resolve[is.na(resolve$resolve)] <- "coalesce" # the default
    }
    cli::cli_progress_message("Found {sum(dupes)} conflicts to resolve by {.fn {unique(resolve$resolve)}}...")
    # old_cols <- ncol(out)
    other_variables <- unique(all_variables[!all_variables %in% key])
    for(conf in 1:nrow(resolve)){
      funny <- paste0("resolve_",resolve$resolve[conf])
      vars <- grep(paste0("^", resolve$var[conf], "$|^", resolve$var[conf], "\\."),
                   names(out), value = TRUE)
      var <- resolve$var[conf]
      out <- out %>% mutate("{var}" := get(funny)(out, all_of(vars)), .keep = "unused") %>% 
        select(-all_of(setdiff(vars,var)))
      cli::cli_alert_success("Resolved {length(vars)} related variables into {.var {var}}.")
    }
    out <- out[, all_variables[!duplicated(all_variables)]]
  }
  # cli::cli_alert_success("Resolved {old_cols - ncol(out)} shared variables.")
  
  out
}

# .extract_if_present <- function(x, y) {
#   x[intersect(y, names(x))]
# }

# Pluck ####

#' Selects a single dataset from a datacube
#' @description This function is reexported/wrapped from the `{purrr}` package.
#'   It allows users to select a single dataset from one
#'   of the datacubes available across the 'many* packages'.
#'   It additionally invites users to cite the selected dataset.
#' @importFrom purrr pluck
#' @param .x The datacube
#' @param ... The name of the dataset in the datacube
#' @param .default Value to use if target is `NULL` or absent.
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
