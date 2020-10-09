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
  usethis::use_readme_rmd(open = FALSE) # There is no need for usethis::use_read_md (open = FALSE), 
  #read.md is created automatically.
  # Step two: ensure/create core package files
  usethis::use_ccby_license()
  usethis::use_tidy_description()
  usethis::use_namespace()
  usethis::use_news_md()
  # Step three: ensure/create github files 
  usethis::use_tidy_github() # This saves a tidy COC, contributing and issue template
  # to the .github file automatically.However, if we want to our own templates here,
  # use_coc() creates a COC based on our template.
  # As well, we can use our contributing with use_contributing() and our PR template with use_pr_template().
  # To do so we also have to modify what is included in the use_tidy_github() call.
  usethis::use_github_actions_badge()
  usethis::use_github_action_check_standard() #May be redundant...
  usethis::use_github_action("prchecks",
                             url = "https://raw.githubusercontent.com/globalgov/qDatr/main/.github/workflows/prchecks.yml",
                             save_as = NULL,
                             ignore = TRUE,
                             open = FALSE)
  usethis::use_github_action("pushrelease",
                             url = "https://raw.githubusercontent.com/globalgov/qDatr/main/.github/workflows/pushrelease.yml",
                             save_as = NULL,
                             ignore = TRUE,
                             open = FALSE)
  usethis::use_github_action("prcommands",
                             url = "https://raw.githubusercontent.com/globalgov/qDatr/main/.github/workflows/prcommands.yml",
                             save_as = NULL,
                             ignore = TRUE,
                             open = FALSE)
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

## Path to our COC template in qDatr

#use_coc <- function(path = NULL) {
#  if (!is.null(path)) {
#    use_directory(path, ignore = is_package())
#  }
#  save_as <- pkgdown::path_join(c(path, "CODE_OF_CONDUCT.md"))
#  
#  new <- usethis::use_template(
#    "CODE_OF_CONDUCT.md",
#    save_as = save_as,
#    ignore = is_package() && is.null(path)
#  )
#  
#  href <- pkgdown_url(pedantic = TRUE) %||%
#    "https://raw.githubusercontent.com/globalgov/qDatr/main/.github/CODE_OF_CONDUCT.md"
#  href <- paste0(href, "/CODE_OF_CONDUCT.txt")
#  
#  ui_todo("Don't forget to describe the code of conduct in your README:")
#  ui_code_block("
#    ## Code of Conduct
#    Please note that the {project_name()} project is released with a \\
#    [Contributor Code of Conduct]({href}). By contributing to this project, \\
#    you agree to abide by its terms."
#  )
#  
#  invisible(new)
#}

## Path to our Contributing template in qDatr

#use_contributing <- function(path = NULL) {
#  if (!is.null(path)) {
#    use_directory(path, ignore = is_package())
#  }
#  save_as <- pkgdown::path_join(c(path, "Contributing.md"))
#  
#  newc <- usethis::use_template(
#    "Contributing.md",
#    save_as = save_as,
#    ignore = is_package() && is.null(path)
#  )
#  
#  cref <- pkgdown_url(pedantic = TRUE) %||%
#    "https://raw.githubusercontent.com/globalgov/qDatr/main/.github/CONTRIBUTING.md"
#  cref <- paste0(href, "/Contributing.txt")
#  invisible(newc)
#}

## Path to our PR template in qDatr

#use_pr_template <-  function(path = NULL) {
#  if (!is.null(path)) {
#    use_directory(path, ignore = is_package())
#  }
#  save_as <- pkgdown::path_join(c(path, "pull_request_template.md"))
#  
#  newpr <- usethis::use_template(
#    "pull_request_template.md",
#    save_as = save_as,
#    ignore = is_package() && is.null(path)
#  )
#  
#  cref <- pkgdown_url(pedantic = TRUE) %||%
#    "https://raw.githubusercontent.com/globalgov/qDatr/main/.github/pull_request_template.md"
#  cref <- paste0(href, "/pull_request_template.txt")
#  invisible(newpr)
#}

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
# We can also add COC, contributing, GitHub actions checks and other tenplates to the file as well.  