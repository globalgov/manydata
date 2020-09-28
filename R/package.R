#' Create a new package in the qDatr ecosystem
#'
#' Creates a new package in and consistent with the qDatr ecosystem
#' @param packageName A string giving the desired name of the package, must start with "q"
#' @param packageAuthor A string, list or vector giving the package author(s), required
#' @details The function establishes many of the required files and folder structures
#' required for a qDatr-consistent data package.
#' @return A new package structure 
#' @examples
#' \dontrun{
#' qpackage_create("qStates", c("James Hollway", "Henrique Sposito"))
#' }
#' @export
qpackage_create <- function(packageName, packageAuthor){
  
  if(!startsWith(packageName, "q")) stop("Package name must start with a 'q'")
  
}