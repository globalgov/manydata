#' Save a cleaned data object in the new q package
#'
#' Save a cleaned data object, consistent with the qData ecosystem, ready to be lazy-loaded
#' and create scripts for documenting and testing that object within the new q package
#' @param ... Unquoted names of existing objects to save
#' @param overwrite Whether to overwrite any existing objects saved
#' @param compress Compression formula
#' @details The function creates a data directory, if inexistent, and save cleaned data.
#' The functions also cretes a script for testing the cleaned data and make sure it
#' complies with qData requirements. As well, it creates a documentation script to help
#' documenting data sources and describing variables.
#' @return This function returns a data folder containing the cleaned data as well as scripts
#' in the R directory to test and document cleaned data.
#' @importFrom fs path
#' @examples
#' \dontrun{
#' export_data("cow")
#' }
#' @export
export_data <- function(...,
                        overwrite = FALSE,
                        compress = "bzip2") {
  
  dataset_name <- deparse(substitute(...))
  dataset <- get(dataset_name)
  
  # Step one: coerce dataset into correct format if not already
  # if(!"Beg" %in% colnames(dataset)) stop("Please ensure there is at least one date column named 'Beg' for beginning")
  # if(!"ID" %in% colnames(dataset)) stop("Please ensure there is at least one identification column named 'ID'")
  # dataset <- as_tibble(dataset) %>% dplyr::arrange(.data$Beg, .data$ID)
  # dataset
  
  # Step two: join dataset to any related datasets in a database
  if(file.exists(paste0("data/", database, ".rda"))){
    usethis::ui_info("Found an existing {usethis::ui_value(database)} database. Imported it ready to update.")
    env <- new.env()
    load(paste0("data/", database, ".rda"), envir = env)
    dataset_exists <- exists(dataset_name, envir = env)
    if(dataset_exists){
      usethis::ui_info("Found an existing {usethis::ui_value(dataset_name)} dataset. This will be overwritten.")
    } else {
      usethis::ui_info("The {usethis::ui_value(dataset_name)} dataset does not yet exist in {usethis::ui_value(database)}. It will be added.")
    }
    env[[database]][[dataset_name]] <- get(dataset_name)
    save(list = database, envir = env, 
         file = fs::path("data", database, ext = "rda"),
         compress = "bzip2")
    if(dataset_exists){
      usethis::ui_info("Saved a new version of the {usethis::ui_value(database)} database with an updated version of the {usethis::ui_value(dataset_name)} dataset.")
    } else {
      usethis::ui_info("Saved a new version of the {usethis::ui_value(database)} database that includes the {usethis::ui_value(dataset_name)} dataset.")
    }
  } else {
    usethis::ui_info("Didn't find an existing {usethis::ui_value(database)} database.")
    env <- new.env()
    env[[database]] <- tibble::lst(...)
    save(list = database, envir = env, 
         file = fs::path("data", database, ext = "rda"),
         compress = "bzip2")
    usethis::ui_done("Saved a {usethis::ui_value(database)} database that includes the {usethis::ui_value(deparse(substitute(...)))} dataset.")
  }
  
  # Step three: create the right kind of test script for the type of object it is
  # TODO: decide on what kinds of objects can be contained in qData packages
  # (actors, agreements, relations, etc)
  qtemplate("qData-test.R",
            save_as = fs::path("tests", "testthat", paste0("qTest-", dataset_name, ".R")),
            data = list(dat = dataset_name),
            open = FALSE,
            ignore = FALSE,
            path = getwd())
  ui_done("A test script has been created for this data.")
  ui_todo("Press Cmd/Ctrl-Shift-T to run all tests.")

  # Step four: create and open a documentation script
  nr <- nrow(dataset)
  nc <- ncol(dataset)
  nm <- names(dataset)
  # print(nm)
  describe <- paste0("#' \\describe{\n", paste0("#'   \\item{",nm,"}{Decribe variable here}\n", collapse = ""), "#' }")
  qtemplate("qData-doc.R",
            save_as = fs::path("R", paste0("qData-", dataset_name, ".R")),
            data = list(dat = dataset_name,
                        nr = nr,
                        nc = nc,
                        describe = describe),
            open = TRUE,
            ignore = FALSE,
            path = getwd())
}
