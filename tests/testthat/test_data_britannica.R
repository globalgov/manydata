# Test if  meets the many packages universe requirements

test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", emperors[["Britannica"]])))
  expect_false(any(grepl("^N/A$", emperors[["Britannica"]])))
  expect_false(any(grepl("^\\s$", emperors[["Britannica"]])))
  expect_false(any(grepl("^\\.$", emperors[["Britannica"]])))
  expect_false(any(grepl("N\\.A\\.$", emperors[["Britannica"]])))
  expect_false(any(grepl("n\\.a\\.$", emperors[["Britannica"]])))
})

test_that("a column indicating an ID source exists", {
  expect_true(any(grepl("ID", colnames(emperors[["Britannica"]]))))
})

test_that("Columns with dates are standardized", {
  if (!is.null(emperors[["Britannica"]]$Begin)) {
    expect_equal(class(emperors[["Britannica"]]$Begin), "mdate")
    expect_false(any(grepl("/", emperors[["Britannica"]]$Begin)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["Britannica"]]$Begin)))
  }
  if (!is.null(emperors[["Britannica"]]$End)) {
    expect_equal(class(emperors[["Britannica"]]$End), "mdate")
    expect_false(any(grepl("/", emperors[["Britannica"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["Britannica"]]$End)))
  }
})

test_that("dataset is arranged by date variable", {
  skip_on_ci()
  skip_on_cran()
  if (!is.null(emperors[["Britannica"]]$Begin)) {
    expect_true(emperors[["Britannica"]]$Begin[10] <
                  emperors[["Britannica"]]$Begin[20])
  }
})
