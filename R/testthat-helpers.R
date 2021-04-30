#' #' Test Helpers for Testthat
#' #'
#' #' Interactive tests for developer function
#' #' @param qTest test package
#' #' @param env new temporary environment
#' #' @importFrom fs file_temp
#' #' @importFrom fs dir_delete
#' #' @importFrom usethis proj_set
#' #' @return No objects are returned.
#' #' Temporary package is delected after tests.
#' #' @source https://www.tidyverse.org/blog/2020/04/self-cleaning-test-fixtures/#usethis-and-create_local_package
#' create_test_package <- function(qTest = fs::file_temp(pattern = "testpkg"),
#'                                 env = parent.frame(),
#'                                 rstudio = FALSE) {
#' 
#'  if (fs::dir_exists(qTest)) {
#'    usethis::ui_stop("Target {usethis::ui_code('dir')} {usethis::ui_path(dir)} already exists.")
#'  }
#'
#'  old_wd <- getwd() # not necessarily same as `old_project`
#'
#'  withr::defer(
#'    {
#'      usethis::ui_done("Deleting temporary project: {usethis::ui_path(qTest)}") # Delete test package
#'      usethis::proj_set(old_wd, force = TRUE)
#'      setwd(old_wd)
#'      fs::dir_delete(qTest)
#'    },
#'    envir = env
#'  )
#'  invisible(qData::setup_package("qTest", AuthorName = "James", AuthorSurname = "Hollway", path = qTest))
#'  # Create a test package
#'  usethis::proj_set(qTest)
#'  invisible(qTest)
#' }
