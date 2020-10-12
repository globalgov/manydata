#' Creates a dataraw file the new q package
#'
#' Creates a dataraw file and provides templates that make it consistent with the qDatr ecosystem
#'
#' @param name Intended (short)name of the dataset
#' @param open Whether the resulting preparation script will be opened
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
  stopifnot(is_string(name)) # Could also check if ASCII
  usethis::use_directory("data-raw", ignore = TRUE)
  
  # Step two: create preparation template
  qtemplate(
    "qData-raw.R",
    save_as = fs::path("data-raw", paste0("prepare-", name), ext = "R"),
    data = list(name = name),
    ignore = FALSE,
    open = open
  )
  
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

#' Helper function for finding and rendering templates
#'
#' Helper function for finding and rendering templates from the qDatr package
#'
#' @param template Template called
#' @param save_as Path to where the rendered template should be saved
#' @param data Any elements to be entered into the template via Whisker
#' @param ignore For use with usethis::use_build_ignore()
#' @param open Whether the resulting template will be opened
#' @param package Package called
#'
#' @details This function is an adaptation of the usethis variant
#' for use in the qDatr ecosystem.
#' @return A rendered template, saved into the correct folder
#' @importFrom whisker whisker.render
#' @examples
#' \dontrun{
#' TODO
#' }
#' @export
qtemplate <- function(template,
                      save_as = template,
                      data = list(),
                      ignore = FALSE,
                      open = FALSE,
                      package = "qDatr") {
  
  # Set up find_template() helper function
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
  
  # Set up render_template() helper function
  render_template <- function(template, data = list(), package = "qDatr") {
    template_path <- find_template(template, package = package)
    strsplit(whisker::whisker.render(xfun::read_utf8(template_path), data), "\n")[[1]]
  }
  
  # Render and save the template as correct file
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
