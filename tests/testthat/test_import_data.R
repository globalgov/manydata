# transp <- data.frame (bikes = c(5), skates = c(4))

test_that("import_data() creates a data-raw foolder and saves data in folder", {
  create_local_package()
  import_data("transp", path = "transp")
  usethis:::expect_proj_file(path("data-raw", "transp"))
 })

# Here I am trying to write a simple test for import_data() to check if it does create a folder and saves raw data. 
# I run into some issues here, but the fact that import_data() is interactive if path is not specified is the main one. 
