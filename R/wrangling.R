#' Drop only columns used in formula
#'
#' A function between dplyr's transmute and mutate
#' @param .data Data frame to pass to the function
#' @param ... Variables to pass to the function
#' @return Data frame with mutated variables
#' and none of the variables used in the mutations,
#' but (unlike the transmute() function in dplyr) all other unnamed variables.
#' @importFrom purrr map
#' @import rlang
#' @import dplyr
#' @source https://stackoverflow.com/questions/51428156/dplyr-mutate-transmute-drop-only-the-columns-used-in-the-formula
#' @examples
#' \dontrun{
#' transmutate( mtcars, X = ifelse( vs, drat, wt ), Y = mpg*cyl )
#' }
#' @export
transmutate <- function( .data, ... ){

  # Helper functions
  # require(tidyverse)
  getAST <- function( ee ) { as.list(ee) %>% purrr::map_if(is.call, getAST) }
  getSyms <- function( ee ) { getAST(ee) %>% unlist %>% purrr::map_chr(deparse) }

  ## Capture the provided expressions and retrieve their symbols
  vSyms <- rlang::enquos(...) %>% purrr::map( ~getSyms(rlang::get_expr(.x)) )

  ## Identify symbols that are in common with the provided dataset
  ## These columns are to be removed
  vToRemove <- intersect( colnames(.data), unlist(vSyms) )

  ## Pass on the expressions to mutate to do the work
  ## Remove the identified columns from the result
  dplyr::mutate( .data, ... ) %>% dplyr::select( -dplyr::one_of(vToRemove) )
}

#' Pastes unique string vectors
#'
#' A vectorised function for use with dplyr's mutate, etc
#' @param ... Variables to pass to the function,
#' currently only two at a time
#' @param sep Separator when vectors reunited, by default "_"
#' @return A single vector with unique non-missing information
#' @examples
#' \dontrun{
#' data <- data.frame(fir=c(NA, "two", "three", NA),
#'   sec=c("one", NA, "three", NA), stringsAsFactors = F)
#' transmutate(data, single = reunite(fir, sec))
#' }
#' @export
reunite <- function(..., sep = "_"){
  out <- cbind(...)
  out[out[,1]==out[,2], 2] <- NA
  out <- na_if(
    gsub(paste0("NA", sep), "",
         gsub(paste0(sep, "NA"), "",
    apply(out, 1, paste, collapse = sep))), "NA")
  out
}


#' Moving variables relative to others
#'
#' Moves variables (columns) of a data frame to positions
#' relative to other variables in the data frame.
#' @param data First variable to be used, required.
#' @param tomove Variable(s) to be moved
#' @param where String that dictates position in relation to
#' reference variable. Can be one of: "last", "first", "before", or "after".
#' @param ba Optional string identifying reference variable
#' By default this is the system date, but can be specified.
#' @return The data frame given by 'data' with the variables repositioned
#' @examples
#' \dontrun{
#' gneva.treat <- rearrange(gneva.treat, "L", "after", "X")
#' gneva.treat <- rearrange(gneva.treat, c("Cites","Amends","Supersedes"), "before", "Amended.by")
#' }
#' @export
rearrange <- function(data, tomove, where = "last", ba = NULL) {
  temp <- setdiff(names(data), tomove)
  x <- switch(
    where,
    first = data[c(tomove, temp)],
    last = data[c(temp, tomove)],
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)-1))]
    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)))]
    })
  x
}
