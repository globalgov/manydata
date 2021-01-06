test_that("dataset name is declared", {
  expect_error(import_data(), "You need to name the dataset. We suggest a short, unique name, all capital letters, such as 'COW'.")
})
test_that("database must be declared", {
  expect_error(import_data("test"), "You need to name the database to which the dataset would belong. We suggest a short, descriptive name, all small letters, such as 'states'.")
})

# transp <- data.frame (bikes = c(5), skates = c(4))
# 
# create_local_package <- function(dir = fs::file_temp(), env = parent.frame()) {
#   old_project <- usethis:::proj_get_()
#   
#   # create new folder and package
#   setup_package(dir, "hs") # A
#   # TODO: address error that user input is required, but session is not interactive...
#   withr::defer(fs::dir_delete(dir), envir = env) # -A
#   usethis::ui_silence(open = FALSE, check_name = FALSE)
#   #
#   # change working directory
#   setwd(dir) # B
#   withr::defer(setwd(old_project), envir = env) # -B
#   
#   # switch to new usethis project
#   usethis::proj_set(dir) # C
#   withr::defer(usethis::proj_set(old_project, force = TRUE), envir = env) # -C
#   
#   invisible(dir)
# }
# 
# test_that("import_data() creates a data-raw folder and saves data in folder", {
#    create_local_package()
#    import_data("transp", path = "transp")
#    usethis:::expect_proj_file(path("data-raw", "transp"))
# })
# 
# # Here I am trying to write a simple test for import_data() to check if it does create a folder and saves raw data.
# # I run into some issues here, but the fact that import_data() is interactive if path is not specified is the main one.
