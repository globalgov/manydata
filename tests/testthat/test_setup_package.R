# Helper

# Here I am trying to create a test package for testing purposes. 
# This is how the usethis package tests many of its functions. The function is not currently working.
# This approach appears to be the best way to test some of the package construction 
# functionalities of qDatr. 
# It appears that the interactive error is gone for now and the new error relates to "usethis::proj_set(".") -
# "usethis::ui_stop("Path {ui_path(path)} does not appear to be inside a project or package.")" I am trying to overwrite 
# this error message...

create_local_package <- function(dir = fs::file_temp(), env = parent.frame()) {
  old_project <- usethis:::proj_get_()
  
  # create new folder and package
  setup_package(dir, "hs") # A
  # TODO: address the error that path does n
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

# TODO: test to see if the expected folders inside package were created
 