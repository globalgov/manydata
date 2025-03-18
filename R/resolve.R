#' Resolving multiple observations of the same variable into one
#' @name resolving
NULL

#' @rdname resolving
#' @examples
#' test <- data.frame(bloop.x = c(1,6,NA), 
#'                    bloop.y = c(2,NA,3), bloop = c(NA,3,4))
#' resolving_coalesce(test)
#' @export
resolving_coalesce <- function(.data, vars){
  if(missing(vars)) vars <- names(.data)
  toCoal <- dplyr::select(.data, vars)
  .data %>% dplyr::mutate(dplyr::coalesce(!!!as.data.frame(toCoal))) %>% 
    dplyr::pull(var = -1)
}

#' @rdname resolving
#' @section Unite: 
#'   Note that uniting always returns a character/string vector.
#'   Values are separated by commas and a set is contained within braces.
#' @examples
#' resolving_unite(test)
#' @export
resolving_unite <- function(.data, vars){
  if(missing(vars)) vars <- names(.data)
  toRes <- dplyr::select(.data, dplyr::all_of(vars))
  apply(toRes, 1, function(x) paste0("{", paste(unique(na.omit(x)), 
                                                  collapse = ","),
                                       "}"))
}

#' @rdname resolving
#' @examples
#' resolving_min(test)
#' @export
resolving_min <- function(.data, vars){
  if(missing(vars)) vars <- names(.data)
  toRes <- dplyr::select(.data, dplyr::all_of(vars))
  apply(toRes, 1, function(x) min(x, na.rm = TRUE))
}

#' @rdname resolving
#' @examples
#' resolving_max(test)
#' @export
resolving_max <- function(.data, vars){
  if(missing(vars)) vars <- names(.data)
  toRes <- dplyr::select(.data, dplyr::all_of(vars))
  apply(toRes, 1, function(x) max(x, na.rm = TRUE))
}

resolve_coalesce <- function(out, key, other_variables) {
  for (var in other_variables) {
    vars_to_combine <- grep(paste0("^", var, "$|^", var, "\\."),
                            names(out), value = TRUE)
    # vars_to_combine <- grep(paste0("^", var, "$|^", var, "\\_"),
    #                         names(out), value = TRUE)
    mdatable <- any(lapply(out[vars_to_combine], class) == "mdate")
    # if(mdatable) out <- mutate(out, across(all_of(vars_to_combine), mdate))
    new_var <- if (mdatable) {
      apply(out[vars_to_combine], 2, as.character)
    } else out[vars_to_combine]
    new_var <- dplyr::coalesce(!!!data.frame(new_var))
    out[, names(out) %in% vars_to_combine] <- NULL
    # out <- dplyr::select(out, -dplyr::all_of(vars_to_combine))
    if(mdatable) new_var <- messydates::as_messydate(new_var)
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, dplyr::all_of(key), dplyr::all_of(other_variables))
  }
  out
}

resolve_min <- function(out, key, other_variables) {
  for (var in other_variables) {
    vars_to_combine <- grep(paste0("^", var, "$|^", var, "\\."),
                            names(out), value = TRUE)
    mdatable <- any(lapply(out[vars_to_combine], class) == "mdate")
    if(mdatable) out <- mutate(out, across(all_of(vars_to_combine), mdate))
    # out <- out %>% rowwise() %>% 
    #   mutate(var = min(!!c(vars_to_combine), na.rm = TRUE), 
    #          .keep = "unused") %>% select(ID, var)
    
    new_var <- if (mdatable) {
      mdate(apply(out[vars_to_combine], 1, function(x) min(x, na.rm = TRUE)))
    } else apply(out[vars_to_combine], 1, min, na.rm = TRUE)
    # new_var <- purrr::map_df(out[vars_to_combine], function(x) {
    #   if (messydates::is_messydate(x)) as.Date(x, min) else x
    # })
    # new_var <- suppressWarnings(do.call("c", purrr::pmap(
    #   new_var, ~ min(c(...), na.rm = TRUE))))
    # new_var <- new_var %>% rowwise() %>% 
    #   mutate(bloop = min(out[vars_to_combine], na.rm = TRUE), 
    #                        .keep = "unused") %>% 
    #   pull(bloop)
    if (any(grepl("^Inf$|^NaN$", new_var))) {
      new_var <- gsub("^Inf$|^NaN$", NA, new_var)
    }
    # out <- dplyr::select(out, -dplyr::all_of(vars_to_combine))
    out[, names(out) %in% vars_to_combine] <- NULL
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, dplyr::all_of(key), dplyr::all_of(other_variables))
  }
  out
}

resolve_max <- function(out, key, other_variables) {
  for (var in other_variables) {
    vars_to_combine <- grep(paste0("^", var, "$|^", var, "\\."),
                            names(out), value = TRUE)
    mdatable <- any(lapply(out[vars_to_combine], class) == "mdate")
    if(mdatable) out <- mutate(out, across(all_of(vars_to_combine), mdate))
    new_var <- if (mdatable) {
      mdate(apply(out[vars_to_combine], 1, function(x) max(x, na.rm = TRUE)))
    } else apply(out[vars_to_combine], 1, max, na.rm = TRUE)
    if (any(grepl("^Inf$|^NaN$", new_var))) {
      new_var <- gsub("^Inf$|^NaN$", NA, new_var)
    }
    # out <- dplyr::select(out, -dplyr::all_of(vars_to_combine))
    out[, names(out) %in% vars_to_combine] <- NULL
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, dplyr::all_of(key), dplyr::all_of(other_variables))
  }
  out
}

# resolve_median <- function(out, key, other_variables) {
#   for (var in other_variables) {
#     vars_to_combine <- grep(paste0("^", var, "$|^", var, "\\_"),
#                             names(out), value = TRUE)
#     new_var <- purrr::map_df(out[vars_to_combine], function(x) {
#       if (messydates::is_messydate(x)) as.Date(x, median) else x
#     })
#     new_var <- suppressWarnings(do.call("c", purrr::pmap(
#       new_var, ~ stats::median(c(...), na.rm = TRUE))))
#     if (any(grepl("^Inf$|^NaN$", new_var))) {
#       new_var <- gsub("^Inf$|^NaN$", NA, new_var)
#     }
#     # out <- dplyr::select(out, -dplyr::all_of(vars_to_combine))
#     out[, names(out) %in% vars_to_combine] <- NULL
#     out[, var] <- new_var
#   }
#   if (length(other_variables) == 1) {
#     out <- dplyr::select(out, dplyr::all_of(key), dplyr::all_of(other_variables))
#   }
#   out
# }
# 
# resolve_mean <- function(out, key, other_variables) {
#   for (var in other_variables) {
#     vars_to_combine <- grep(paste0("^", var, "$|^", var, "\\_"),
#                             names(out), value = TRUE)
#     new_var <- purrr::map_df(out[vars_to_combine], function(x) {
#       if (messydates::is_messydate(x)) as.Date(x, mean) else x
#     })
#     if (any(lapply(new_var, class) == "character")) {
#       message("Calculating the mean is not possible for character(s) variables.
#               Returning first non-missing value instead.")
#       new_var <- dplyr::coalesce(!!!out[vars_to_combine])
#     } else {
#       new_var <- suppressWarnings(do.call("c", purrr::pmap(
#         new_var, ~ mean(c(...), na.rm = TRUE))))
#     }
#     if (any(grepl("^Inf$|^NaN$", new_var))) {
#       new_var <- gsub("^Inf$|^NaN$", NA, new_var)
#     }
#     # out <- dplyr::select(out, -dplyr::all_of(vars_to_combine))
#     out[, names(out) %in% vars_to_combine] <- NULL
#     out[, var] <- new_var
#   }
#   if (length(other_variables) == 1) {
#     out <- dplyr::select(out, dplyr::all_of(key), dplyr::all_of(other_variables))
#   }
#   out
# }

resolve_random <- function(out, key, other_variables) {
  for (var in other_variables) {
    vars_to_combine <- grep(paste0("^", var, "$|^", var, "\\."),
                            names(out), value = TRUE)
    mdatable <- any(lapply(out[vars_to_combine], class) == "mdate")
    if(mdatable) out <- mutate(out, across(all_of(vars_to_combine), mdate))
    new_var <- if (mdatable) {
      mdate(apply(out[vars_to_combine], 1, function(x) sample(x[!is.na(x)], size = 1)))
    } else apply(out[vars_to_combine], 1, sample, size = 1)
    # new_var <- purrr::map_df(out[vars_to_combine], function(x) {
    #   if (messydates::is_messydate(x)) as.Date(x, random) else x
    # })
    # new_var <- apply(new_var, 1, function(x) sample(x, size = 1))
    # out <- dplyr::select(out, -dplyr::all_of(vars_to_combine))
    out[, names(out) %in% vars_to_combine] <- NULL
    out[, var] <- new_var
  }
  if (length(other_variables) == 1) {
    out <- dplyr::select(out, dplyr::all_of(key), dplyr::all_of(other_variables))
  }
  out
}

resolve_multiple <- function(out, key, resolve) {
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
    # if (resolve$resolve[k] == "median") {
    #   rmd <- resolve_median(resolve$var[k], out, key)
    # }
    # if (resolve$resolve[k] == "mean") {
    #   rme <- resolve_mean(resolve$var[k], out, key)
    # }
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
  # if (exists("rmd")) {
  #   out <- dplyr::full_join(out, rmd, by = key)
  # }
  # if (exists("rme")) {
  #   out <- dplyr::full_join(out, rme, by = key)
  # }
  if (exists("rra")) {
    out <- dplyr::full_join(out, rra, by = key)
  }
  out
}

