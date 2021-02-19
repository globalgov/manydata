test_that("url is declared", {
  expect_error(export_data("COW", databse = "states"), "Please use the URL argument to provide a direct webURL to the source of your dataset.")
})
test_that("url is valid", {
  expect_error(export_data("COW", databse = "states", URL = 5), "Please provide a valid URL argument.")
})

test_that("export_data() stores create data folder", {
   create_local_package()
   export_data("T")
   expect_true(file.exists("data"))
   expect_true(file.exists("data/T"))
})
