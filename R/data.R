#' Creates a dataraw file the new q package
#'
#' Creates a dataraw file and provides templates that make it consistent with the qDatr ecosystem
#'
#'
#' @details The function loads raw data into a q package
#' @return A dataraw folder
#' @examples
#' \dontrun{
#' TODO
#' }
#' @export
use_qData_raw <- function(name = "DATASET", open = rlang::is_interactive()) {
  
  # Step one: checks and setup
  stopifnot(is_string(name))
  usethis::use_directory("data-raw", ignore = TRUE)
  
  # Step two: create preparation template
  
  # Step three: inform user what to do next
  usethis::ui_todo("Finish the opened data preparation script")
  usethis::ui_todo("Use {usethis::ui_code('qDatr::use_qData()')} to add prepared data to package")

}

#' Creates a data file the new package for the qDatr ecosystem
#'
#' Creates a data file with templates that make it consistent with the qDatr ecosystem
#'
#' @param ... Unquoted names of existing objects to save.
#'
#' @examples
#' \dontrun{
#' TODO
#' }
#' @export
use_qData <- function(...) {

  object <- as.list(substitute(list(...)))[-1L]

  # Step one: take object created from raw-data and save as data to be lazy loaded in the package
  usethis::use_data(...)

  # Step two: make sure that testthat is set up correctly for the package
  usethis::use_testthat()
  usethis::use_package("pointblank")

  # Step three: create the right kind of test script for the type of object it is
  # TODO: decide on what kinds of objects can be contained in qDatr packages 
  # (actors, agreements, relations, etc)
  qtemplate("qData-test.R",
            fs::path("tests", "testthat", paste0("qTest-", object[[1]], ".R")),
            data = usethis:::project_data())
  
  # Step four: create and open a documentation script
  qtemplate("qData-document.R",
            fs::path("R", paste0("qData-", object[[1]], ".R")),
            data = usethis:::project_data())
}

# set use_template to qDatr package template files and not usethis ...
qtemplate <- function(template,
                      save_as = path,
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