test_that("package name is required", {
  expect_error(setup_package(), "Please declare a package name")
})
test_that("author names must be declared", {
  expect_error(setup_package("qtest"), "Please declare at least one author")
})

test_that("package name must start with the letter q", {
  expect_error(setup_package("test"), "Package name must start with a 'q'")
})

test_that("setup_package has expected file structure", {
  qTest <- create_local_package()
  expect_true(file.exists("R"))
  expect_true(file.exists("DESCRIPTION"))
  expect_true(file.exists("LICENSE"))
  expect_true(file.exists(".github"))
})
