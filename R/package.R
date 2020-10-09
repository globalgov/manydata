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
#' @examples
#' \dontrun{
#' qpackage_create("qStates",
#' c("James Hollway", "Henrique Sposito"))
#' }
#' @export
create_qPackage <- function(packageName,
                            packageAuthor,
                            update = TRUE) {
  # Checks to see whether inputs are correct
  if (is.null(packageName)) stop("Please declare a package name")
  if (!startsWith(packageName, "q")) stop("Package name must start with a 'q'")
  if (is.null(packageAuthor)) stop("Please declare at least one author")
  # Checks to see whether path already contains files or is empty
  path <- usethis::create_package(path, rstudio = TRUE, open = FALSE)
  old_project <- usethis::proj_set(path)
  on.exit(usethis::proj_set(old_project), add = TRUE)
  # Step one: ensure/create package/project structure
  qreadme() # Similar to usethis::use_readme_rmd(open = FALSE) but sets README.rmd from our template.
  # README.md is still missing.
  # Step two: ensure/create core package files
  usethis::use_ccby_license()
  usethis::use_tidy_description()
  usethis::use_namespace()
  usethis::use_news_md()
  # Step three: ensure/create github files 
  qgithub() # Similar to usethis::use_tidy_github(). This creates a COC, contributing
  # and other documentation from our templates.
  usethis::use_github_actions_badge()
  usethis::use_github_action_check_standard() #May be redundant...
  qprchecks() # Sets our own prcheck into the workflows folder from our template.
  qprcommands() # Sets our own prcommands into the workflow folder from our templetes.  
  # If we want to use tidy templates for actions, and not ours, the usethis::use_tidy_github_actions() can be used.
  usethis::ui_todo("In the new package, remember to do:")
  usethis::ui_todo("{ui_code('use_git()')}")
  usethis::ui_todo("{ui_code('use_github()')}")
  usethis::ui_todo("{ui_code('use_tidy_github_actions()')}")
  usethis::ui_todo("{ui_code('use_pkgdown()')}")
  # Step four: ensure/create additional files
  usethis::use_testthat()
  # Step five: checks package (?) 
  usethis::use_spell_check()
  usethis::use_tibble()
  # Step 6: create GitHub repository (?)
  usethis::use_git() # The usethis::use_github() may also be an interesting option to explore here. 
  usethis::proj_activate(path)
}


## Functions to add our GitHub actions checks templates to qpackage.

qchecks <- function() {
  usethis::use_directory(urltools::path(".github", "workflows"))
  usethis::use_git_ignore("*.html", directory = ".github")
}

qprchecks <- function() {
  qchecks()
  qtemplate(
    "prchecks.yml",
    urltools::path("workflows", "prchecks.yml"),
    data = usethis:::project_data()
  )
}

qprcommands <- function() {
  qchecks()
  qtemplate(
    "prcommands.yml",
    urltools::path("workflows", "prcommands.yml"),
    data = usethis:::project_data()
  )
}


# With pushrelease it is a little more complicated as it requires the package names to 
# be changed, but I am trying to work around it. 
# qpushrelease <- function() {
#qchecks()
#qtemplate("prcommands.yml",
#  urltools::path("workflows", "prcommands.yml"),
#  data = usethis:::project_data()) {
# out <- textclean::mgsub("qdatr", basename(packageName), out)
#}
#out
#}

## Funtions to add our own README, COC, contributing and issue PR templates. 

qreadme <- function(open = rlang::is_interactive()) {
  librarian::check_installed("rmarkdown")
  data <- usethis:::project_data()
  data$Rmd <- TRUE
  new <- qtemplate("README.Rmd",
                   data = data,
                   open = open)
  invisible(TRUE)
}

qgithub <- function () {
  usethis:::use_dot_github()
  qcoc()
  qprtemplate()
  qcontributing()
}

# We do not have a REAMDME.md file template yet.

qcoc <- function() {
  usethis:::use_dot_github()
  usethis::use_directory(urltools::path(".github"))
  qtemplate("CODE_OF_CONDUCT.md",
            urltools::path(".github", "CODE_OF_CONDUCT.md"))
}

qprtemplate <- function() {
  usethis:::use_dot_github()
  usethis::use_directory(urltools::path(".github"))
  qtemplate("pull_request_template.md",
            urltools::path(".github", "pull_request_template.md"))
}

qcontributing <- function() {
  usethis:::use_dot_github()
  usethis::use_directory(urltools::path(".github"))
  qtemplate("CONTRIBUTING_GGO.md",
            urltools::path(".github", "CONTRIBUTING_GGO.md"))
}
