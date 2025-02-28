#' @export
describe_datacube <- function(datacube){
  paste0("The `", deparse(substitute(datacube)), 
         "` datacube is a list containing ", length(datacube),
         " datasets: ", cli::pluralize("{names(datacube)}"))
}