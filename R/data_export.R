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
  
  }
  usethis::use_directory("data", ignore = TRUE)
  usethis::use_directory(paste("data", dat, sep = "/"), ignore = TRUE)
  save(...,
       file = fs::path("data", dat, ext = "rda"),
       envir = parent.frame(), compress = compress)
  ui_done("Saved {usethis::ui_value(dat)} to the package data folder.")
  
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
