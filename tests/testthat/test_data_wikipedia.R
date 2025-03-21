# Test if  meets the many packages universe requirements

test_that("missing observations are reported correctly", {
  expect_false(any(grepl("^n/a$", emperors[["Wikipedia"]])))
  expect_false(any(grepl("^N/A$", emperors[["Wikipedia"]])))
  expect_false(any(grepl("^\\s$", emperors[["Wikipedia"]])))
  expect_false(any(grepl("^\\.$", emperors[["Wikipedia"]])))
  expect_false(any(grepl("N\\.A\\.$", emperors[["Wikipedia"]])))
  expect_false(any(grepl("n\\.a\\.$", emperors[["Wikipedia"]])))
})

test_that("a column indicating an ID source exists", {
  expect_true(any(grepl("ID", colnames(emperors[["Wikipedia"]]))))
})

test_that("Columns with dates are standardized", {
  if (!is.null(emperors[["Wikipedia"]]$Begin)) {
    expect_equal(class(emperors[["Wikipedia"]]$Begin), "mdate")
    expect_false(any(grepl("/", emperors[["Wikipedia"]]$Begin)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["Wikipedia"]]$Begin)))
  }
  if (!is.null(emperors[["Wikipedia"]]$End)) {
    expect_equal(class(emperors[["Wikipedia"]]$End), "mdate")
    expect_false(any(grepl("/", emperors[["Wikipedia"]]$End)))
    expect_false(any(grepl("^[:alpha:]$",
                           emperors[["Wikipedia"]]$End)))
  }
})

test_that("dataset is arranged by date variable", {
  skip_on_ci()
  skip_on_cran()
  if (!is.null(emperors[["Wikipedia"]]$Begin)) {
    expect_true(emperors[["Wikipedia"]]$Begin[10] <
                  emperors[["Wikipedia"]]$Begin[20])
  }
})
