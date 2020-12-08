#' Imports and establishes preparation of raw data
#'
#' Creates a data-raw folder, moves raw data files to a consistent location,
#' and provides a script that makes it easy to clean and wrangle the data
#' into a format consistent with the qData ecosystem.
#' @param dataset Intended (short) name of the dataset. 
#' That is, the name of the two-dimensional tabular data format.
#' For consistency reasons, this should be a unique name in all capitals.
#' Abbreviations make good dataset names, such as "COW" or "DESTA".
#' @param database Intended name of the database or datacube. 
#' That is, the name of the population or phenomenon to which the dataset relates.
#' For consistency reasons, this should be a unique name in small letters.
#' Concepts make good database names, such as "states" or "colonial_relations".
#' @param path Path to raw data file. 
#' If left unspecified, a dialogue box is raised to select the file via the system.
#' @param delete_original Whether the original file is moved (TRUE) or copied (FALSE).
#' By default FALSE.
#' @param open Whether the resulting preparation script will be opened.
#' By default TRUE.
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
  if (is.null(dataset)) stop("You need to name the dataset. We suggest a short, unique name, all capital letters, such as 'COW'.")
  if (is.null(database)) stop("You need to name the database to which the dataset would belong. We suggest a short, descriptive name, all small letters, such as 'states'.")
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
  usethis::ui_done("Copied data to {new_path}.")
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
                path = new_path,
                import_type = import_type),
    ignore = FALSE,
    open = open,
    path = getwd())
  
  # Step four: inform user what to do next
  usethis::ui_todo("Finish the opened data preparation script")
  usethis::ui_todo("Use {usethis::ui_code('qData::export_data()')} to add prepared data to package")
  
}