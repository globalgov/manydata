#' Drop only columns used in formula
#'
#' A function between dplyr's transmute and mutate
#' @param .data Data frame to pass to the function
#' @param ... Variables to pass to the function
#' @return Data frame with mutated variables
#' and none of the variables used in the mutations,
#' but, unlike `dplyr::transmute()`, all other unnamed variables.
#' @importFrom purrr map
#' @import rlang
#' @import dplyr
#' @source https://stackoverflow.com/questions/51428156/dplyr-mutate-transmute-drop-only-the-columns-used-in-the-formula
#' @examples
#' transmutate(mtcars, X = ifelse( vs, drat, wt ), Y = mpg*cyl)
#' @export
transmutate <- function(.data, ...) {

  # Helper functions
  getAST <- function(ee) { as.list(ee) %>% purrr::map_if(is.call, getAST) }
  getSyms <- function(ee) { getAST(ee) %>% unlist %>% purrr::map_chr(deparse) }

  ## Capture the provided expressions and retrieve their symbols
  vSyms <- rlang::enquos(...) %>% purrr::map(~getSyms(rlang::get_expr(.x)))

  ## Identify symbols that are in common with the provided dataset
  ## These columns are to be removed
  vToRemove <- intersect(colnames(.data), unlist(vSyms))

  ## Pass on the expressions to mutate to do the work
  ## Remove the identified columns from the result
  dplyr::mutate(.data, ...) %>% dplyr::select(-dplyr::one_of(vToRemove))
}

#' Pastes unique string vectors
#'
#' A vectorised function for use with dplyr's mutate, etc
#' @param ... Variables to pass to the function,
#' currently only two at a time
#' @param sep Separator when vectors reunited, by default "_"
#' @return A single vector with unique non-missing information
#' @examples
#' data <- data.frame(fir=c(NA, "two", "three", NA),
#'                    sec=c("one", NA, "three", NA), stringsAsFactors = FALSE)
#' transmutate(data, single = reunite(fir, sec))
#' @export
reunite <- function(..., sep = "_") {
  out <- cbind(...)
  out[out[, 1] == out[, 2], 2] <- NA
  out <- na_if(
    gsub(paste0("NA", sep), "",
         gsub(paste0(sep, "NA"), "",
    apply(out, 1, paste, collapse = sep))), "NA")
  out
}

#' Moving variables relative to others
#'
#' @param data First variable to be used, required.
#' @param tomove Variable(s) to be moved
#' @param where String that dictates position in relation to reference variable.
#' Can be one of: "last", "first", "before", or "after".
#' @param ref Optional string identifying reference variable
#' @details Moves variables (columns) of a data frame to positions
#' relative to other variables in the data frame.
#' @return The data frame given by 'data' with the variables repositioned
#' @export
rearrange <- function(data, tomove, where = "last", ref = NULL) {
  .Deprecated("dplyr::relocate")
}

#' Pastes unique string vectors
#'
#' For use with dplyr::summarise, for example
#' @param x A vector
#' @param collapse String indicating how elements separated
#' @return A single value
#' @details This function operates similarly to reunite,
#' but instead of operating on columns/observations,
#' it pastes together unique rows/observations.
#' @importFrom stats na.omit
#' @examples
#' data <- data.frame(ID = c(1,2,3,3,2,1))
#' data1 <- data.frame(ID = c(1,2,3,3,2,1), One = c(1,NA,3,NA,2,NA))
#' recollect(data$ID)
#' recollect(data1$One)
#' @export
recollect <- function(x, collapse = "_") {
  na_if(paste(unique(na.omit(x)), collapse = collapse), "")
}

#' Get first non-missing
#'
#' For use with dplyr::summarise, for example
#' @param x A vector
#' @return A single value
#' @details This function operates similarly to coalesce for columns,
#' that is picking the first non-missing observation,
#' but on observations rather than variables.
#' @source https://stackoverflow.com/questions/40515180/dplyr-how-to-find-the-first-non-missing-string-by-groups
#' @examples
#' dplyr::summarise(mtcars, coalesce_rows(mtcars))
#' coalesce_rows(mtcars$wt)
#' @export
coalesce_rows <- function(x) {
  x[which(!is.na(x))[1]]
}

#' Fills missing data by lookup
#'
#' Fills missing data where known by other observations with the same id/index
#' @param df a dataframe
#' @param id a string identifying a column in the dataframe for indexing
#' @param var a string identifying a column or columns in the dataframe
#' to be filled
#' @return a dataframe
#' @examples
#' data <- data.frame(ID = c(1,2,3,3,2,1),
#'                     One = c(1,NA,3,NA,2,NA),
#'                     Two = c(NA,"B",NA,"C",NA,"A"))
#' repaint(data, "ID", c("One","Two"))
#' @export
repaint <- function(df, id, var) {
  for (co in var) {
    for (ea in unique(df[, id])) {
      if (any(!is.na(df[df[, id] == ea, co])) &
          any(is.na(df[df[, id] == ea, co]))) {
        df[df[, id] == ea &
             is.na(df[, co]), co] <- df[df[, id] == ea &
                                          !is.na(df[, co]), co][1]
      }
    }
  }
  df
}
