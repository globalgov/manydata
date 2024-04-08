# Test for compare_ family of functions

test_that("plot for compare_categories returns the correct output format", {
  db <- plot(compare_categories(datacube = emperors, key = "ID"))
  expect_type(db, "list")
  expect_length(db, 11)
  expect_true(ggplot2::is.ggplot(db))
  expect_named(db, names(ggplot2::ggplot()))
})

test_that("compare_categories() returns the correct output format", {
  db <- compare_categories(datacube = emperors, key = "ID")
  db1 <- compare_categories(datacube = emperors, key = "ID", variable = "Begin")
  db2 <- compare_categories(datacube = emperors, key = "ID",
                            variable = c("Begin", "End"), category = "conflict")
  expect_type(db, "list")
  expect_type(db1, "list")
  expect_type(db2, "list")
  expect_s3_class(db, "tbl")
  expect_s3_class(db1, "tbl")
  expect_s3_class(db2, "tbl")
  expect_length(db, 37)
  expect_length(db1, 5)
  expect_length(db2, 9)
  expect_equal(nrow(db), nrow(db1))
  expect_named(db1, c("ID", "wikipedia$Begin", "UNRV$Begin",
                      "britannica$Begin", "Begin (3)"))
  expect_true(db2[1,5] == db2[10,5])
})

test_that("compare_dimensions() returns the correct output format", {
  db <- compare_dimensions(emperors)
  expect_type(db, "list")
  expect_length(db, 5)
  expect_s3_class(db, "tbl_df")
  skip_on_ci()
  skip_on_cran()
  expect_equal(as.character(db$Earliest_Date[1]),
               as.character(messydates::as_messydate("-0026-01-16")))
  expect_equal(as.character(db$Latest_Date[1]),
               as.character(messydates::as_messydate("0395-01-17")))
})

test_that("compare_ranges() returns the correct output format", {
  expect_error(compare_ranges(emperors),
               "Please declare one or more variables.")
  db <- compare_ranges(emperors, variable = c("Begin", "End"))
  expect_type(db, "list")
  expect_length(db, 6)
  expect_s3_class(db, "tbl_df")
})

test_that("compare_overlap() and plot_overlap() returns the correct output format", {
  db <- compare_overlap(emperors, key = "ID")
  expect_type(db, "list")
  expect_length(db, 2)
  expect_s3_class(db, "tbl_df")
  pl <- plot(db)
  expect_type(pl, "list")
  expect_length(pl, length(ggplot2::ggplot()))
  expect_true(ggplot2::is.ggplot(pl))
  expect_named(pl, names(ggplot2::ggplot()))
})

test_that("compare_missing() and plot_missing() returns the correct output format", {
  db <- compare_missing(emperors)
  expect_type(db, "list")
  expect_length(db, 6)
  expect_s3_class(db, "tbl_df")
  pl <- plot(db)
  expect_type(pl, "list")
  expect_length(pl, length(ggplot2::ggplot()))
  expect_true(ggplot2::is.ggplot(pl))
  expect_named(pl, names(ggplot2::ggplot()))
})
