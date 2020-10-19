transp <- data.frame (bikes = c(5), skates = c(4))

test_that("import_data() creates a data-raw foolder", {
  create_local_package()
  import_data()
  usethis:::expect_proj_file(path("data-raw", "DATaSET"))
})

test_that("import_data() saves dataset in data-raw folder", {
  create_local_package()
  import_data("transp")
  usethis:::expect_proj_file(path("data-raw", "transp"))
})
