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
use_qData_raw <- function(...) {
  
  object <- as.list(substitute(list(...)))[-1L]
  
  dataraw(...)

  qtemplate("qdataraw.R",
            fs::path("data-raw", paste0("qDataraw-", object[[1]], ".R")),
            data = usethis:::project_data())
}
    

dataraw <- function(...,
                    internal = FALSE,
                    overwrite = FALSE,
                    compress = "bzip2",
                    version = 2) {
    
    objs <- usethis:::get_objs_from_dots(base::dots(...))
    
    usethis:::use_dependency("R", "depends", "2.10")
    if (internal) {
      usethis::use_directory("R")
      paths <- fs::path("R", "sysdata.rda")
      objs <- list(objs)
    } else {
      usethis::use_directory("data-raw")
      paths <- fs::path("data", objs, ext = "rda")
    }
    usethis:::check_files_absent(usethis::proj_path(paths), overwrite = overwrite)
    
    usethis::ui_done("Saving {ui_value(unlist(objs))} to {ui_value(paths)}")
    if (!internal) usethis::ui_todo("Document your data (see {ui_value('https://r-pkgs.org/data.html')})")
    
    envir <- parent.frame()
    mapply(
      save,
      list = objs,
      file = usethis::proj_path(paths),
      MoreArgs = list(envir = envir, compress = compress, version = version)
    )
    
    invisible()
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