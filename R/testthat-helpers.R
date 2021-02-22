#' Test Helpers for Testthat
#' 
#' Interactive tests for developer function
#' @param qTest test package 
#' @param env new temporary environment
#' @importFrom fs file_temp
#' @importFrom fs dir_delete
#' @importFrom usethis proj_set
#' @return No objects are returned. Temporary package is delected after tests. 
#' @source https://www.tidyverse.org/blog/2020/04/self-cleaning-test-fixtures/#usethis-and-create_local_package
create_local_package <- function(qTest = fs::file_temp(), env = parent.frame()) {
  
  old_project <- getwd()        
  
  withr::defer({                       
    usethis::proj_set(old_project, force = TRUE) 
    setwd(old_project)                  
    fs::dir_delete(qTest)                 
  }, envir = env)
  
  qData::setup_package("qTest", AuthorName = "James", AuthorSurname = "Hollway")
  usethis::proj_set(qTest)
  invisible(qTest)
}

# Old Helper

#create_test_package <- function(dir = fs::file_temp(pattern = "testpkg"),
#                                 env = parent.frame(),
#                                 rstudio = FALSE) {
# 
#  if (fs::dir_exists(dir)) {
#    usethis::ui_stop("Target {usethis::ui_code('dir')} {usethis::ui_path(dir)} already exists.")
#  }
#  
#  old_wd <- getwd() # not necessarily same as `old_project`
#  
#  withr::defer(
#    {
#      usethis::ui_done("Deleting temporary project: {usethis::ui_path(dir)}")
#      fs::dir_delete(dir)
#    },
#    envir = env
#  )
#  usethis::ui_silence(qData::setup_package("qTest", "hs", path = dir)) # A
#  
#  withr::defer(
#    {
#      ui_done("Restoring original working directory: {usethis::ui_path(old_wd)}")
#      setwd(old_wd)
#    },
#    envir = env
#  )
#
#}
