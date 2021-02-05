test_that("package name is required", {
  expect_error(setup_package(), "Please declare a package name")
})
test_that("author names must be declared", {
  expect_error(setup_package("qtest"), "Please declare at least one author")
})

#test_that("package name must start with the letter q", {
#  expect_error(setup_package("test"), "Package name must start with a 'q'")
#})

# Helper

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

# Tests

#test_that("setup_package() creates a package", {
#  dir <- fs::file_temp(pattern = "testpkg")
#  create_test_package(dir = dir)
#  expect_true(file.exists(paste0(dir, "/DESCRIPTION")))
#  expect_true(file.exists(paste0(dir, "/NEWS.md")))
#  expect_true(file.exists(paste0(dir, "/README.Rmd")))
  # expect_true(file.exists(paste0(dir, "/NAMESPACE")))
#  expect_true(file.exists(paste0(dir, "/.github/CODE_OF_CONDUCT.md")))
#  expect_true(file.exists(paste0(dir, "/.github/CONTRIBUTING.md")))
#  expect_true(file.exists(paste0(dir, "/.github/pull_request_template.md")))
#  expect_true(file.exists(paste0(dir, "/.github/ISSUE_TEMPLATE/bug_report.md")))
#  expect_true(file.exists(paste0(dir, "/.github/ISSUE_TEMPLATE/feature_request.md")))
#})