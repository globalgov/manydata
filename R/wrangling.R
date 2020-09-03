#' Drop only columns used in formula
#'
#' A function between dplyr's transmute and mutate
#' @param .data Data frame to pass to the function
#' @param ... Variables to pass to the function
#' @return Data frame with mutated variables
#' and none of the variables used in the mutations,
#' but (unlike the transmute() function in dplyr) all other unnamed variables.
#' @importFrom purrr map
#' @importFrom rlang get_expr
#' @source https://stackoverflow.com/questions/51428156/dplyr-mutate-transmute-drop-only-the-columns-used-in-the-formula
#' @examples
#' \dontrun{
#' transmutate( mtcars, X = ifelse( vs, drat, wt ), Y = mpg*cyl )
#' }
#' @export
transmutate <- function( .data, ... ){

  # Helper functions
  require(tidyverse)
  getAST <- function( ee ) { as.list(ee) %>% map_if(is.call, getAST) }
  getSyms <- function( ee ) { getAST(ee) %>% unlist %>% map_chr(deparse) }

  ## Capture the provided expressions and retrieve their symbols
  vSyms <- enquos(...) %>% purrr::map( ~getSyms(rlang::get_expr(.x)) )

  ## Identify symbols that are in common with the provided dataset
  ## These columns are to be removed
  vToRemove <- intersect( colnames(.data), unlist(vSyms) )

  ## Pass on the expressions to mutate to do the work
  ## Remove the identified columns from the result
  mutate( .data, ... ) %>% select( -one_of(vToRemove) )
}

