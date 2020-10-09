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
  qdataraw()
}

qdataraw <- function() {
  qtemplate(
    "qdataraw.R",
    urltools::path("data-raw", "qdataraw.R"),
    data = usethis:::project_data())
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
use_qData <- function(...) {
  # Step one: take object created from raw-data and save as data to be lazy loaded in the package
  usethis::use_data(...)
  # Step one: make sure that testthat is set up correctly for the package
  usethis::use_testthat()
  usethis::use_package("pointblank")
  # Step three: create the right kind of test script for the type of object it is
  # TODO: decide on what kinds of objects can be contained in qDatr 
  # packagess (actors, agreements, relations, etc)
  qdatadoc()
  qdatatest()
}

qdatadoc <- function() {
  qtemplate("qData-document.R",
            urltools::path("data", "qData-document.R"),
            data = usethis:::project_data())
}

qdatatest <- function() {
  qtemplate("qData-test.R",
            urltools::path("data", "qData-test.R"),
            data = usethis:::project_data())
}

# set use_template to qDatr package template files and not usethis ...

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