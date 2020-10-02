#' Create a new package in the qDatr ecosystem
#'
#' Creates a new package in and consistent with the qDatr ecosystem
#' @param packageName A string giving the desired name of the package, must start with "q"
#' @param packageAuthor A string, list or vector giving the package author(s), required
#' @param update A logical indicating whether existing files should be overwritten, by default TRUE.
#' TODO: do these checks internally and smartly...
#' @details The function establishes many of the required files and folder structures
#' required for a qDatr-consistent data package.
#' @return A new package structure 
#' @importFrom usethis create_tidy_package
#' @examples
#' \dontrun{
#' qpackage_create("qStates", c("James Hollway", "Henrique Sposito"))
#' }
#' @export
create_qpackage <- function(packageName, 
                            packageAuthor,
                            update = TRUE){
  
  if(is.null(packageName)) stop("Please declare a package name")
  if(!startsWith(packageName, "q")) stop("Package name must start with a 'q'")
  if(is.null(packageAuthor)) stop("Please declare at least one author")
  
  usethis::create_tidy_package(getwd(), name = packageName)
  # usethis::use_code_of_conduct()
  # usethis::use_ccby_license()
  # usethis::use_readme_rmd()
  # usethis::use_description()
  # usethis::use_github_actions()
  # usethis::use_github_actions_badge()
  # usethis::use_namespace()
  # usethis::use_news_md()
  # usethis::use_readme_rmd()
  # usethis::use_spell_check()
  # usethis::use_github_action_check_standard()
  # usethis::use_git()
  # usethis::use_tidy_github_actions()
  # usethis::use_tibble()
  
}

