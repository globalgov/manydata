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
    data = project_data()
  )
}

qprcommands <- function() {
  qchecks()
  qtemplate(
    "prcommands.yml",
    urltools::path("workflows", "prcommands.yml"),
    data = project_data()
  )
}


# With pushrelease it is a little more complicated as it requires the package names to 
# be changed, but I am trying to work around it. 

## Funtions to add our own README, COC, contributing and issue PR templates. 

qreadme <- function(open = rlang::is_interactive()) {
  librarian::check_installed("rmarkdown")
  data <- project_data()
  data$Rmd <- TRUE
  new <- qtemplate("README.Rmd",
                   data = data,
                   open = open)
  invisible(TRUE)
}

qgithub <- function () {
  use_dot_github()
  qcoc()
  qprtemplate()
  qcontributing()
}

# We do not have a REAMDME.md file template yet.

qcoc <- function() {
  use_dot_github()
  usethis::use_directory(urltools::path(".github"))
  qtemplate("CODE_OF_CONDUCT.md",
            urltools::path(".github", "CODE_OF_CONDUCT.md"))
}

qprtemplate <- function() {
  use_dot_github()
  usethis::use_directory(urltools::path(".github"))
  qtemplate("pull_request_template.md",
            urltools::path(".github", "pull_request_template.md"))
}

qcontributing <- function() {
  use_dot_github()
  usethis::use_directory(urltools::path(".github"))
  qtemplate("CONTRIBUTING_GGO.md",
            urltools::path(".github", "CONTRIBUTING_GGO.md"))
}

#' Createa a dataraw file the new q package
#'
#' Creates a dataraw file and provides templates that make it consistent with the qDatr ecosystem
#' @param name name of dataset saved
#' @param open load data in environemnt
#' @details The function loads raw data into a q package
#' @return A dataraw folder
#' @importFrom usethis create_tidy_package
#' @examples
#' \dontrun{
#' TODO
#' }
#' @export
use_qData_raw <- function(name = "DATASET", open = rlang::is_interactive()) {
  # Step one: take a data file from anywhere on your harddrive and move/copy it to a new data-raw folder
  usethis::use_data_raw()
  # Step two: open up a script containing a template for how to convert raw data to 
  # qDatr consistent (hopefully) data objects
  qtemplate("qdataraw")
}

#' Createa a data file the new package for the qDatr ecosystem
#'
#' Creates a data file with templates that make it consistent with the qDatr ecosystem
#' @param ... Unquoted names of existing objects to save.
#' @param internal If `FALSE`, saves each object in its own `.rda`
#'   file in the `data/` directory. These data files bypass the usual
#'   export mechanism and are available whenever the package is loaded
#'   (or via [data()] if `LazyData` is not true).
#'   If `TRUE`, stores all objects in a single `R/sysdata.rda` file.
#'   Objects in this file follow the usual export rules. Note that this means
#'   they will be exported if you are using the common `exportPattern()`
#'   rule which exports all objects except for those that start with `.`.
#' @param overwrite By default, `use_data()` will not overwrite existing
#'   files. If you really want to do so, set this to `TRUE`.
#' @param compress Choose the type of compression used by [save()].
#'   Should be one of "gzip", "bzip2", or "xz".
#' @param version The serialization format version to use. The default, 2, was
#'   the default format from R 1.4.0 to 3.5.3. Version 3 became the default from
#'   R 3.6.0 and can only be read by R versions 3.5.0 and higher.
#' @examples
#' \dontrun{
#' TODO
#' }
#' @export
use_qData <- function(..., internal = FALSE,
                      overwrite = FALSE,
                      compress = "bzip2",
                      version = 2) {
  # Step one: take object created from raw-data and save as data to be lazy loaded in the package
  usethis::use_data(...)
  # Step one: make sure that testthat is set up correctly for the package
  usethis::use_testthat()
  usethis::use_package("pointblank")
  # Step three: create the right kind of test script for the type of object it is
  # TODO: decide on what kinds of objects can be contained in qDatr 
  # packagess (actors, agreements, relations, etc)
  qtemplate("qData-document")
  qtemplate("qData-test")
}

# set use_template to qDatr package template files and not usthis ...

qtemplate <- function(template,
                         save_as = template,
                         data = list(),
                         ignore = FALSE,
                         open = FALSE,
                         package = "qDatr") {
  template_contents <- render_template(template, data, package = package)
  new <- usethis::write_over(usethis::proj_path(save_as), template_contents)
  if (ignore) {
    usethis::use_build_ignore(save_as)
  }
  if (open && new) {
    usethis::edit_file(usethis::proj_path(save_as))
  }
  invisible(new)
}

render_template <- function(template, data = list(), package = "qDatr") {
  template_path <- find_template(template, package = package)
  strsplit(whisker::whisker.render(xfun::read_utf8(template_path), data), "\n")[[1]]
}

find_template <- function(template_name, package = "qDatr") {
  path <- tryCatch(fs::path_package(package = package, "templates", template_name),
    error = function(e) ""
  )
  if (identical(path, "")) {
    usethis::ui_stop(
      "Could not find template {ui_value(template_name)} \\
      in package {ui_value(package)}."
    )
  }
  path
}

# We just have to make sure that the templates exist in the qDatr templates file and that the names match.
# We can also add COC, contributing, GitHub actions checks and other templates to the file as well.

# Helper functions taken from the usethis package.These functions are in the usthis code
# but are not exported. That is why they have to be replicated here. This is necessary for 
# adapting/creating the paths to use our own templates.

project_data <- function(base_path = usethis::proj_get()) {
  if (!possibly_in_proj(base_path)) {
    usethis::ui_stop(c(
      "{ui_path(base_path)} doesn't meet the usethis criteria for a project.",
      "Read more in the help for {ui_code(\"proj_get()\")}."
    ))
  }
  if (is_package(base_path)) {
    data <- package_data(base_path)
  } else {
    data <- list(Project = fs::path_file(base_path))
  }
  data
}

package_data <- function(base_path = usethis::proj_get()) {
  desc <- desc::description$new(base_path)
  as.list(desc$get(desc$fields()))
}

possibly_in_proj <- function(path = ".") !is.null(proj_find(path))

is_package <- function(base_path = usethis::proj_get()) {
  res <- tryCatch(
    rprojroot::find_package_root_file(path = base_path),
    error = function(e) NULL
  )
  !is.null(res)
}

is_package <- function(base_path = usethis::proj_get()) {
  res <- tryCatch(
    rprojroot::find_package_root_file(path = base_path),
    error = function(e) NULL
  )
  !is.null(res)
}

proj_find <- function(path = ".") {
  tryCatch(
    rprojroot::find_root(proj_crit(), path = path),
    error = function(e) NULL
  )
}

proj_crit <- function() {
  rprojroot::has_file(".here") |
    rprojroot::is_rstudio_project |
    rprojroot::is_r_package |
    rprojroot::is_git_root |
    rprojroot::is_remake_project |
    rprojroot::is_projectile_project
}

use_dot_github <- function(ignore = TRUE) {
  usethis::use_directory(".github", ignore = ignore)
  usethis::use_git_ignore("*.html", directory = ".github")
}