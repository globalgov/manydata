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
#' @details The function assists with importing existing raw data into a qPackage.
#' The function does two main things.
#' 
#' First, it moves or copies a chosen file into the "data-raw/" folder of the current qPackage.
#' A hierarchy to this folder is established.
#' It first checks whether there is already a folder under "data-raw/" on the harddrive
#' that is the same as the name of the database and, if there is no such folder, it creates one.
#' It then also checks whether there is already a folder under that that is consistent with
#' the name of the dataset. If there is no such folder, it creates one.
#' Finally, it places the chosen file into that dataset folder.
#' If the argument `delete_original = TRUE` then the original file will be deleted.
#' This can be useful if, for example, the file had just been downloaded to your "Downloads" folder.
#' 
#' Second, the function creates a new script in the dataset-level folder,
#' alongside the raw data file.
#' By default, it also opens this script in RStudio or equivalent.
#' The purpose of this script is for reading the file into R, 
#' cleaning the data and wrangling it into a qData-consistent format,
#' and then exporting it for use in the package.
#' Quite a bit of this is pre-populated either using information given to `import_data()`,
#' or inferring what is required from the name or format of the file.
#' 
#' @return Places the chosen file into a folder hierarchy within the package
#' such as "data-raw/\{database\}/\{dataset\}/" and creates and opens
#' a script in the same folder for preparing the data for use in the qPackage.
#' @examples
#' \dontrun{
#' import_data(dataset = "COW", database = "states")
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
  if (grepl("csv$", path)) { 
    import_type <- "readr::read_csv" 
    } else if (grepl("xlsx$|xls$", path)) { 
      import_type <- "readxl::read_excel"
    } else if (grepl("dta$", path)) {
      import_type <- "haven::read_dta"
    } else if (grepl("RData$", path)) {
      import_type <- "load"
    } else stop("File type not recognised")
  
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
