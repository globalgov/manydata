#' Imports and establishes preparation of raw data
#'
#' Create a data-raw folder and provide templates that make easier for setting up the data cleaning 
#' and wrangling, consistent with the qDatr ecosystem
#' @param dataset Intended (short) name of the dataset. This refers to the two-diemnsional sheet of data in the 
#' form of a dataset which will be connected to other datasets.  
#' @param database Intended name of the database. This refers to a domain-issue which the dataset(s) to be 
#' corrected and connected. That is, what is the domain-issue of the several datasets to be connected belong to. 
#' By connecting two-dimensional datasets, we can form a a three-dimensional database that resembles a data cube.
#' @param path Path to raw data file. If left unspecified, a dialog box is raised to select the file via the system
#' @param delete_original Does not delete original files by default
#' @param open Whether the resulting preparation script will be opened
#' @importFrom fs path
#' @importFrom fs path_file
#' @importFrom usethis use_directory
#' @importFrom usethis ui_done
#' @importFrom rlang is_string
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
#' @importFrom fs path  
#' @examples
#' \dontrun{
#' export_data("cow")
#' }
#' @export
export_data <- function(..., 
                        overwrite = FALSE, 
                        compress = "bzip2") {
  
  dat <- deparse(substitute(...))

  # Step one: take object created from raw-data and save as tibble to be lazy loaded in the new data folder in package
  if (!tibble::is_tibble(...)){
    tibble::as_tibble(...)
  } 
  usethis::use_directory("data", ignore = TRUE)
  usethis::use_directory(paste("data", dat, sep = "/"), ignore = TRUE)
  save(..., 
       file = fs::path("data", dat, ext = "rda"), 
       envir = parent.frame(), compress = compress)
  ui_done("Saved {usethis::ui_value(dat)} to the package data folder.")
  
  # Step two: create the right kind of test script for the type of object it is
  # TODO: decide on what kinds of objects can be contained in qDatr packages 
  # (actors, agreements, relations, etc)
  qtemplate("qData-test.R",
            save_as = fs::path("tests", "testthat", paste0("qTest-", dat, ".R")),
            data = list(dat = dat),
            open = FALSE,
            ignore = FALSE,
            path = getwd())
  ui_done("A test script has been created for this data.")
  ui_todo("Press Cmd/Ctrl-Shift-T to run all tests.")

  # Step three: create and open a documentation script
  nr <- nrow(...)
  nc <- ncol(...)
  nm <- names(...)
  print(nm)
  describe <- paste0("#' \\describe{\n", paste0("#'   \\item{",nm,"}{Decribe variable here}\n", collapse = ""), "#' }")
  qtemplate("qData-doc.R",
            save_as = fs::path("R", paste0("qData-", dat, ".R")),
            data = list(dat = dat,
                        nr = nr,
                        nc = nc,
                        describe = describe),
            open = TRUE,
            ignore = FALSE,
            path = getwd())
}
