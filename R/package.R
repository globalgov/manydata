#' Create a new package in the qDatr ecosystem
#'
#' Creates a new package in and consistent with the qDatr ecosystem
#' @param packageName A string giving the desired name of the package,
#' must start with "q"
#' @param packageAuthor A string, list or vector giving the package
#' author(s), required
#' @param update A logical indicating whether existing files should be
#' overwritten, by default TRUE.
#' TODO: do these checks internally and smartly...
#' @details The function establishes many of the required files and
#' folder structures required for a qDatr-consistent data package.
#' @return A new package structure
#' @importFrom usethis create_tidy_package
#' @import pkgdown
#' @examples
#' \dontrun{
#' qpackage_create("qStates",
#' c("James Hollway", "Henrique Sposito"))
#' }
#' @export
create_qpackage <- function(packageName,
                            packageAuthor,
                            update = TRUE) {
  if (is.null(packageName)) stop("Please declare a package name")
  if (!startsWith(packageName, "q")) stop("Package name must start with a 'q'")
  if (is.null(packageAuthor)) stop("Please declare at least one author")
  
  usethis::create_tidy_package(getwd(), name = packageName)
  
  if (!is.null(path)) {
      use_directory(path, ignore = is_package())
    }
    save_as <- pkgdown::path_join(c(path, "CODE_OF_CONDUCT.md"))
    
    new <- use_template(
      "CODE_OF_CONDUCT.md",
      save_as = save_as,
      ignore = is_package() && is.null(path)
    )
    
    href <- pkgdown_url(pedantic = TRUE) %||%
      "https://raw.githubusercontent.com/globalgov/qDatr/main/.github/CODE_OF_CONDUCT.md"
    href <- paste0(href, "/CODE_OF_CONDUCT.txt")
    
    ui_todo("Don't forget to describe the code of conduct in your README:")
    ui_code_block("
    ## Code of Conduct
    Please note that the {project_name()} project is released with a \\
    [Contributor Code of Conduct]({href}). By contributing to this project, \\
    you agree to abide by its terms."
    )
    
    invisible(new)
    
    usethis::use_ccby_license()
  
#  if (!is.null(path)) { 
#  use_directory(path, ignore = is_package()) 
#  }
#   save_as <- pkgdown::path_join(c(path, "README.rmd"))
#    (open = rlang::is_interactive())
#   librarian::check_installed("rmarkdown")
#   read <- pkgdown_url(pedantic = TRUE) %||%
#            "https://raw.githubusercontent.com/globalgov/qDatr/main/README.md"
#   read <- paste0(read, "/README.txt")
#   data = read
#   data$Rmd <- TRUE
#    
#   new <- usethis::use_template(
#   if (!is.null(path)) "package-README" else "project-README",
#     "README.Rmd",
#     data = read,
#      open = open,
#      save_as = save_as
#    )
#    if (!new) {
#      return(invisible(FALSE))
#    }
#    invisible(TRUE)
    
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

