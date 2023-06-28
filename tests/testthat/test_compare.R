# Test for compare_ family of functions

test_that("plot for compare_categories returns the correct output format", {
  db <- plot(compare_categories(database = emperors, key = "ID"))
  expect_type(db, "list")
  expect_length(db, 9)
  expect_true(ggplot2::is.ggplot(db))
  expect_named(db, c("data", "layers", "scales", "mapping", "theme",
                     "coordinates", "facet", "plot_env", "labels"))
})

test_that("compare_categories() returns the correct output format", {
  db <- compare_categories(database = emperors, key = "ID")
  db1 <- compare_categories(database = emperors, key = "ID", variable = "Beg")
  db2 <- compare_categories(database = emperors, key = "ID",
                            variable = c("Beg", "End"), category = "conflict")
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
  expect_named(db1, c("ID", "wikipedia$Beg", "UNRV$Beg",
                      "britannica$Beg", "Beg (3)"))
  expect_true(db2[1,5] == db2[10,5])
})

test_that("compare_data() returns the correct output format", {
  db <- compare_data(emperors)
  expect_type(db, "list")
  expect_length(db, 5)
  expect_s3_class(db, "tbl_df")
})

test_that("compare_overlap() and plot_overlap() returns the correct output format", {
  db <- compare_overlap(emperors, key = "ID")
  expect_type(db, "list")
  expect_length(db, 2)
  expect_s3_class(db, "tbl_df")
  pl <- plot(db)
  expect_type(pl, "list")
  expect_length(pl, 9)
  expect_true(ggplot2::is.ggplot(pl))
  expect_named(pl, c("data", "layers", "scales", "mapping", "theme",
                     "coordinates", "facet", "plot_env", "labels"))
})

test_that("compare_missing() and plot_missing() returns the correct output format", {
  db <- compare_missing(emperors)
  expect_type(db, "list")
  expect_length(db, 6)
  expect_s3_class(db, "tbl_df")
  pl <- plot(db)
  expect_type(pl, "list")
  expect_length(pl, 9)
  expect_true(ggplot2::is.ggplot(pl))
  expect_named(pl, c("data", "layers", "scales", "mapping", "theme",
                     "coordinates", "facet", "plot_env", "labels"))
})
