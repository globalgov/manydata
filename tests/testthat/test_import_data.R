test_that("dataset name is declared", {
  expect_error(import_data(), "You need to name the dataset. We suggest a short, unique name, all capital letters, such as 'COW'.")
})
test_that("database must be declared", {
  expect_error(import_data("test"), "You need to name the database to which the dataset would belong. We suggest a short, descriptive name, all small letters, such as 'states'.")
})

T <- data.frame (bikes = c(5), skates = c(4))

test_that("import_data() creates a data-raw folder and saves data in folder", {
  create_local_package()
  import_data("T",  database = "transportation")
  expect_true(file.exists("data-raw"))
  expect_true(file.exists("dataraw/transportation"))
  expect_true(file.exists("dataraw/transportation/T"))
})
