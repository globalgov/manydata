#' @export
new_messydate <- function(x = character()){
  stopifnot(is.character(x))
  structure(x, class = c("messydt"))
}

#' @export
validate_messydate <- function(x){
  values <- unclass(x)
  
  if (any(grepl("[A-WYZa-z]", values))) {
    stop(
      "The only alpha character allowed in messy dates is 'X' for unspecified time components",
      call. = FALSE
    )
  }

    if (!all(grepl("[0-9]", values))) {
    stop(
      "Messy dates require at least one specified date component.",
      call. = FALSE
    )
    }
  
  # can only consist of numbers and some special symbols: []{}..X%?~"
  
  x
}

