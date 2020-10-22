# Helper

# Here I am trying to create a test package for testing purposes. 
# This is how the usethis package tests many of its functions. The function is not currently working.
# This approach appears to be the best way to test some of the package construction 
# functionalities of qDatr. 
# However, we may opt for other alternatives when it comes to testing
# if this generates too many issues or becomes to burdensome to develop. 

create_local_package <- function(dir = fs::file_temp(), env = parent.frame()) {
  old_project <- usethis:::proj_get_()
  
  # create new folder and package
  setup_package(dir, "hs") # A
  # TODO: address error that user input is required, but session is not interactive...
  withr::defer(fs::dir_delete(dir), envir = env) # -A
  usethis::ui_silence(open = FALSE, check_name = FALSE)
  
  # change working directory
  setwd(dir) # B
  withr::defer(setwd(old_project), envir = env) # -B
  
  # switch to new usethis project
  usethis::proj_set(dir) # C
  withr::defer(usethis::proj_set(old_project, force = TRUE), envir = env) # -C
  
  invisible(dir)
}

# Tests

test_that("create_qPackage() creates a package", {
  create_local_package()
  expect_true(usethis:::is_package(dir))
})
