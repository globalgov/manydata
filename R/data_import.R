#' Imports and establishes preparation of raw data
#'
#' Create a data-raw folder and provide templates that make easier for setting
#' up the data cleaning and wrangling, consistent with the qData ecosystem
#' @param dataset Intended (short) name of the dataset. This refers
#' to the two-dimensional sheet of data in the form of a dataset which
#' will be connected to other datasets.
#' @param database Intended name of the database. This refers to a domain
#' issue which the dataset(s) to be corrected and connected. That is, what
#' is the domain-issue of the several datasets to be connected belong to.
#' By connecting two-dimensional datasets, we can form a a three-dimensional
#' database that resembles a data cube.
#' @param path Path to raw data file. If left unspecified, a dialog box is
#' raised to select the file via the system
#' @param delete_original Does not delete original files by default
#' @param open Whether the resulting preparation script will be opened
#' @importFrom fs path
#' @importFrom fs path_file
#' @importFrom usethis use_directory
#' @importFrom usethis ui_done
#' @importFrom rlang is_string
#' @details The function helps importing raw data into q package while
#' providing a template that facilitates data cleaning and wrangling,
#' consistent with the qData ecosystem. The function can be used without
#' specifying a path to the file. In that case an interactive dialog
#' box will be openend and the data file can be manually selected.
#' The script provided to help with data cleaning and wrangling contain
#' suggestions on how to properly load the data into the environment.
#' @return This function returns a data-raw folder containing the data
#' imported as well as a script in the R directory to guide
#' preparation of data using qData.
#' @examples
#' \dontrun{
#' qData::import_data(dataset = "cow", database = "states")
#' }
#' @export
import_data <- function(dataset = NULL,
                        database = NULL,
                        path = NULL,
                        delete_original = FALSE,
                        open = rlang::is_interactive()) {
  
  # Step one: checks and setup
  if (is.null(dataset)) stop("You need to name the dataset. We suggest a short name, all small letters, such as 'cow'.")
  if (is.null(database)) stop("You need to name the database to which the dataset would belong. We suggest a descriptive short name, all small letters, such as 'states'.")
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
                import_type = import_type),
    ignore = FALSE,
    open = open,
    path = getwd())
  
  # Step four: inform user what to do next
  usethis::ui_todo("Finish the opened data preparation script")
  usethis::ui_todo("Use {usethis::ui_code('qData::export_data()')} to add prepared data to package")
  
}