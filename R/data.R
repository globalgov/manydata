#' Imports and establishes preparation of raw data
#'
#' Create a data-raw folder and provide templates that make easier for setting up the data cleaning 
#' and wrangling, consistent with the qDatr ecosystem
#' @param dataset Intended (short)name of the dataset
#' @param database Intended (short)name of the dataset
#' @param path Path to raw data file. If left unspecified, a dialog box is raised to select the file via the system
#' @param delete_original Does not delete original files by default.
#' @param open Whether the resulting preparation script will be opened
#' @importFrom fs path
#' @importFrom fs path_file
#' @details The function helps importing raw data into q package while providing a template that facilitates 
#' data cleaning and wrangling, consistent with the qDatr ecosystem. The function can be used without specifying
#' a path to the file. In that case an interactive dialog box will be openend and the data file can be manually selected.
#' The script provided to help with data cleaning and wrangling contain suggestions on how to properly load the data into 
#' the environment. 
#' @return This function returns a data-raw folder containing the data imported as well as a script in the R directory 
#' to guide preparation of data using qDatr.   
#' @examples
#' \dontrun{
#' qDatr::import_data(dataset = "cow", database = "states")
#' }
#' @export
import_data <- function(dataset = NULL,
                        database = NULL,
                        # type = NULL,
                        path = NULL,
                        delete_original = FALSE,
                        open = rlang::is_interactive()) {

  # Step one: checks and setup
  if(is.null(dataset)) stop("You need to name the dataset. We suggest a short name, all small letters, such as 'cow'.")
  if(is.null(database)) stop("You need to name the database to which the dataset would belong. We suggest a descriptive short name, all small letters, such as 'states'.")
  stopifnot(rlang::is_string(dataset)) # Could also check if ASCII
  stopifnot(rlang::is_string(database)) # Could also check if ASCII
  usethis::use_directory("data-raw", ignore = TRUE)
  usethis::use_directory(paste("data-raw", database, sep = "/"), ignore = TRUE)
  usethis::use_directory(paste(paste("data-raw", database, sep = "/"), dataset, sep = "/"), ignore = TRUE)
  usethis::ui_done("Made sure data folder hierarchy exists.") 
  # This step may not be necessary if create_package() already creates this folder too...
  
  # Step two: move raw data file to correct location
  if (is.null(path)) path <- file.choose()
  new_path <- fs::path("data-raw", database, dataset, fs::path_file(path))
  file.copy(path, new_path)
  usethis::ui_done("Copied data to data-raw/ folder.")
  if (delete_original) file.remove(path)
  
  # Step three: create preparation template
  # Get data type
  if (grepl("csv$", path)) import_type <- "readr::read_csv"
  if (grepl("xlsx$|xls$", path)) import_type <- "readxl::read_excel"
  if (grepl("dta$", path)) import_type <- "haven::read_dta"
  # TODO: Add these packages as suggests or maybe have the function install them if necessary
  # Create preparation template
  qtemplate(
    "qData-prep.R",
    save_as = fs::path("data-raw", database, dataset, paste0("prepare-", dataset), ext = "R"),
    data = list(dataset = dataset,
                database = database,
                import_type = import_type,
                path = new_path),
    ignore = FALSE,
    open = open
  )

  # Step four: inform user what to do next
  usethis::ui_todo("Finish the opened data preparation script")
  usethis::ui_todo("Use {usethis::ui_code('qDatr::export_data()')} to add prepared data to package")

}

#' Save a cleaned data object in the new q package
#'
#' Save a cleaned data object, consistent with the qDatr ecosystem, ready to be lazy-loaded 
#' and create scripts for documenting and testing that object within the new q package
#' @param ... Unquoted names of existing objects to save
#' @param overwrite Whether to overwrite any existing objects saved
#' @param compress Compression formula
#' @details The function creates a data directory, if inexistent, and save cleaned data. 
#' The functions also cretes a script for testing the cleaned data and make sure it complies with qDatr requirements. 
#' As well, it creates a documentation script to help documenting data sources and describing variables.     
#' @return This function returns a data folder containing the cleaned data as well as scripts in the R directory 
#' to test and document cleaned data.    
#' @examples
#' \dontrun{
#' data("mtcars")
#' qDatr::export_data("mtcars")
#' }
#' @export
export_data <- function(..., 
                        overwrite = FALSE, 
                        compress = "bzip2") {
  
  dat <- deparse(substitute(...))

  # Step one: take object created from raw-data and save as tibble to be lazy loaded in the package
  if (!tibble::is_tibble(..., FALSE)){
    tibble::as_tibble(...)
  } 
  save(..., 
       file = fs::path("data", dat, ext = "rda"), 
       envir = parent.frame(), compress = compress)
  ui_done("Saved {usethis::ui_value(dat)} to the package data folder.")
  
  # Step two: create the right kind of test script for the type of object it is
  # TODO: decide on what kinds of objects can be contained in qDatr packages 
  # (actors, agreements, relations, etc)
  qtemplate("qData-test.R",
            fs::path("tests", "testthat", paste0("qTest-", dat, ".R")),
            data = list(dat = dat),
            open = FALSE)
  ui_done("A test script has been created for this data.")
  ui_todo("Press Cmd/Ctrl-Shift-T to run all tests.")
  
  # Step three: create and open a documentation script
  nr <- nrow(...)
  nc <- ncol(...)
  nm <- names(...)
  print(nm)
  describe <- paste0("#' \\describe{\n", paste0("#'   \\item{",nm,"}{Decribe variable here}\n", collapse = ""), "#' }")
  qtemplate("qData-doc.R",
            fs::path("R", paste0("qData-", dat, ".R")),
            data = list(dat = dat,
                        nr = nr,
                        nc = nc,
                        describe = describe))
}

#' Helper function for finding and rendering templates
#'
#' Helper function for finding and rendering templates from the qDatr package
#' @param template Template called
#' @param save_as Path to where the rendered template should be saved
#' @param data Any elements to be entered into the template via Whisker
#' @param ignore For use with usethis::use_build_ignore()
#' @param open Whether the resulting template will be opened
#' @param package Package called
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
                      open = rlang::is_interactive(),
                      package = "qDatr") {
  
  # Set up find_template() helper function
  find_template <- function(template_name, package = "qDatr") {
    path <- tryCatch(fs::path_package(package = package, "templates", template_name),
                     error = function(e) ""
    )
    if (identical(path, "")) {
      usethis::ui_stop(
        "Could not find template {usethis::ui_value(template_name)} \\
      in package {usethis::ui_value(package)}."
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
