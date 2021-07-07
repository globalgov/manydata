#' @export
new_messydate <- function(x = character()){
  stopifnot(is.character(x))
  structure(x, class = c("messydt"))
}

