#' Consolidate database into a single dataset
#'
#' This function collapses a set or database of (q)datasets
#' into a single dataset with some combination of the
#' rows, columns, and observations of the parent datasets.
#' The function includes separate arguments for the rows and columns,
#' as well as for how to resolve conflicts in observations across datasets.
#' This provides users with considerable flexibility in how they combine qData.
#' For example, users may wish to stick to units that appear in every dataset
#' but include variables coded in any dataset, or units that appear in any dataset
#' but only those variables that appear in every dataset.
#' Even then there may be conflicts, as the actual unit-variable observations
#' may differ from dataset to dataset.
#' Here we (will) offer a number of resolve methods that enable users to choose
#' how conflicts between observations are resolved.
#' @param .data A qPackage database object
#' @param rows Which rows or units to retain.
#' By default "any" (or all) units are retained,
#' but another option is "every",
#' which retains only those units that appear in all parent datasets.
#' @param cols Which columns or variables to retain.
#' By default "any" (or all) variables are retained,
#' but another option is "every",
#' which retains only those variables that appear in all parent datasets.
#' @param resolve How conflicts between observations should be resolved.
#' Currently only "coalesce" is offered, which takes the first non-NA value,
#' but options should soon include
#' "max", "min", "mean", "mode" and "median", as well as "append".
#' @param key An ID column to collapse by. By default "qID".
#' @return A single tibble/data frame.
#' @examples
#' \donttest{
#' data1 <- dplyr::tibble(qID = c("NZL", "BRA", "CHF"),
#'                        date = c("1990-01-01","1990-01-02","1990-01-01:1990-01-31"),
#'                        number = c(100, 1000, 10000))
#' data2 <- dplyr::tibble(qID = c("NZL", "BRA"),
#'                        date = c("1990-01-01","1990-01-03"))
#' test <- tibble::lst(a = data1, b = data2)
#' consolidate(test, "every", "every")
#' consolidate(test, "any", "any")
#' }
#' @export
consolidate <- function(.data,
                        rows = c("any","every"),
                        cols = c("any","every"),
                        resolve = "coalesce",
                        key = "qID"){

  # Step 1: Join datasets by ID
  rows <- match.arg(rows)
  if(rows == "any"){
    out <- purrr::reduce(.data, dplyr::full_join, by = key)
  } else if(rows == "every"){
    out <- purrr::reduce(.data, dplyr::inner_join, by = key)
  }

  # Step 2: Drop any unwanted variables
  cols <- match.arg(cols)
  all_variables <- unlist(purrr::map(.data, names))
  if(cols == "every"){
    all_variables <- names(table(all_variables)[table(all_variables) == length(.data)])
    out <- out %>% dplyr::select(all_of(key), starts_with(all_variables))
  }

  # Step 3: Resolve conflicts
  resolve <- match.arg(resolve)
  other_variables <- all_variables[!key == all_variables]
  if(resolve == "coalesce"){
    for(var in other_variables){
      vars_to_combine <- startsWith(names(out), var)
      new_var <- dplyr::coalesce(!!!out[vars_to_combine])
      out <- out %>% dplyr::select(-dplyr::starts_with(var))
      out[,var] <- new_var
      out
    }
  }

  out <- dplyr::distinct(out)
  if(any(duplicated(out[,1]))) out <- coalesce_compatible(out)
  out
}

#' @importFrom purrr pluck
#' @export
purrr::pluck

#' Coalesce all compatible rows of a data frame
#'
#' This function identifies and coalesces
#' all compatible rows in a data frame.
#' Compatible rows are defined as those rows where all present elements are equal,
#' allowing for equality where one row has an element present
#' and the other is missing the observation.
#' @param .data data frame to consolidate
#' @importFrom utils combn
#' @importFrom dplyr coalesce bind_rows slice
#' @examples
#' eg1 <- tribble(
#' ~x, ~y, ~z,
#' "a", "b", NA,
#' "a", "b", "c",
#' "j", "k", NA,
#' NA, "k", "l")
#' coalesce_compatible(eg1)
#' @export
coalesce_compatible <- function(.data){

  pairs <- compatible_rows(.data)

  if(length(pairs)>0){
    merged <- apply(pairs, 1, function(x){
      dplyr::coalesce(.data[x[1],], .data[x[2],])
    })
    merged <- dplyr::bind_rows(merged)

    dplyr::bind_rows(dplyr::slice(.data, -unique(c(pairs))),
                     merged)
  } else .data

}

compatible_rows <- function(x){

  complete_vars <- x[,apply(x, 2, function(y) !any(is.na(y)))]
  compat_candidates <- which(duplicated(complete_vars) | duplicated(complete_vars, fromLast = TRUE))

  pairs <- t(utils::combn(compat_candidates,2))
  pb <- progress::progress_bar$new(format = "identifying compatible pairs [:bar] :percent eta: :eta",
                                   total = nrow(pairs))

  compatico <- apply(pairs, 1, function(y){
    pb$tick()
    o <- x[y[1],]==x[y[2],]
    o[is.na(o)] <- TRUE
    o
  })
  pairs[apply(t(compatico), 1, function(y) all(y)),]
}
