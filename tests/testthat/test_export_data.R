transp <- data.frame (bikes = c(5), skates = c(4))

test_that("export_data() stores create data folder and tests folder", {
  create_local_package()
  export_data("transp")
  usethis:::expect_proj_file(path("data"))
  usethis:::expect_proj_file(path("tests", "testhat"))
})

# Here the issue appears to be related to the fact that the create_local_package() function
# is not currently working. 
