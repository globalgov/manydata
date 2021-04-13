#' Collapse a database into a single data frame/tibble
#' 
#' @description The collapse family of functions collapses
#' lists of datasets that form a database in qPackages
#' according to user preferences.
#' @name collapse
#' @param dbase A qPackage database object
#' @param dset A dataset label from within that database
#' @param key An ID column to collapse by
#' @param resolve How do you want differences to be resolved?
#' It takes "max", "min", "mean", "mode" and "median" options.
#'
NULL

#' @rdname collapse
#' @importFrom purrr pluck
#' @example collapse_select(qStates::states, "COW")
#' @export
collapse_select <- function(dbase, dset){
  purrr::pluck(dbase, dset)
}

#' @rdname collapse
#' @importFrom purrr reduce
#' @importFrom dplyr full_join
#' @example collapse_full(qStates::states, "ID")
#' @export
collapse_full <- function(dbase, key, resolve = NULL){
  
  if(is.null(resolve)) {
  key <- resolve_mean(key)
  } else if(resolve == "max") {
  key <- resolve_max(key)
  } else if (resolve == "min") {
  key <- resolve_min(key)
  } else if (resolve == "median") {
  key <- resolve_median(key)
  } else if (resolve == "mode") {
  key <- resolve_mode(key)
  }
    
  purrr::reduce(dbase, function(x, y) dplyr::full_join(x, y, by = key))
}

#' @rdname collapse
#' @importFrom purrr reduce
#' @importFrom dplyr inner_join
#' @example collapse_consensus(qStates::states, "ID")
#' @export
collapse_consensus <- function(dbase, key, resolve = NULL){
# Let's rename this one collapse_inner for consistency ?
  if(is.null(resolve) || resolve == "mean") {
    for (i in c(1:length(dbase))) {
      dbase[[i]][key] <- resolve_mean(pull(dbase[[i]], key))
    }
  } else if(resolve == "max") {
    for (i in c(1:length(dbase))) {
      dbase[[i]][key] <- resolve_max(pull(dbase[[i]], key))
    }
  } else if (resolve == "min") {
    for (i in c(1:length(dbase))) {
      dbase[[i]][key] <- resolve_min(pull(dbase[[i]], key))
    }
  } else if (resolve == "median") {
    for (i in c(1:length(dbase))) {
      dbase[[i]][key] <- resolve_median(pull(dbase[[i]], key))
    }
  } else if (resolve == "mode") {
    for (i in c(1:length(dbase))) {
      dbase[[i]][key] <- resolve_mode(pull(dbase[[i]], key))
    }
  }
  
  purrr::reduce(dbase, function(x, y) dplyr::inner_join(x, y, by = key))
}

# collapse_left

#' @rdname collapse
#' @importFrom purrr reduce
#' @importFrom dplyr left_join
#' @example collapse_left(qStates::states, "ID")
#' @export
collapse_left <- function(dbase, key, resolve = NULL){
  
  if(is.null(resolve) || resolve == "mean") {
    for (i in c(1:length(dbase))) {
      dbase[[i]][key] <- resolve_mean(pull(dbase[[i]], key))
    }
  } else if(resolve == "max") {
    for (i in c(1:length(dbase))) {
      dbase[[i]][key] <- resolve_max(pull(dbase[[i]], key))
    }
  } else if (resolve == "min") {
    for (i in c(1:length(dbase))) {
      dbase[[i]][key] <- resolve_min(pull(dbase[[i]], key))
    }
  } else if (resolve == "median") {
    for (i in c(1:length(dbase))) {
      dbase[[i]][key] <- resolve_median(pull(dbase[[i]], key))
    }
  } else if (resolve == "mode") {
    for (i in c(1:length(dbase))) {
      dbase[[i]][key] <- resolve_mode(pull(dbase[[i]], key))
    }
  }
  
  purrr::reduce(dbase, function(x, y) dplyr::left_join(x, y, by = key))
}

# Collapse_right

#' @rdname collapse
#' @importFrom purrr reduce
#' @importFrom dplyr right_join
#' @example collapse_right(qStates::states, "ID")
#' @export
collapse_right <- function(dbase, key, resolve = NULL){
  
  if(is.null(resolve) || resolve == "mean") {
    for (i in c(1:length(dbase))) {
      dbase[[i]][key] <- resolve_mean(pull(dbase[[i]], key))
    }
  } else if(resolve == "max") {
    for (i in c(1:length(dbase))) {
      dbase[[i]][key] <- resolve_max(pull(dbase[[i]], key))
    }
  } else if (resolve == "min") {
    for (i in c(1:length(dbase))) {
      dbase[[i]][key] <- resolve_min(pull(dbase[[i]], key))
    }
  } else if (resolve == "median") {
    for (i in c(1:length(dbase))) {
      dbase[[i]][key] <- resolve_median(pull(dbase[[i]], key))
    }
  } else if (resolve == "mode") {
    for (i in c(1:length(dbase))) {
      dbase[[i]][key] <- resolve_mode(pull(dbase[[i]], key))
    }
  }
  
  purrr::reduce(dbase, function(x, y) dplyr::right_join(x, y, by = key))
}

