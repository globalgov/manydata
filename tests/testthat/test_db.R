test_that("dbplot() returns the correct output format", {
  db <- db_plot(database = emperors, key = "ID")
  expect_type(db, "list")
  expect_length(db, 9)
  expect_true(ggplot2::is.ggplot(db))
  expect_named(db, c("data", "layers", "scales", "mapping", "theme",
                      "coordinates", "facet", "plot_env", "labels"))
})

test_that("dbcomp() returns the correct output format", {
  db <- db_comp(database = emperors, key = "ID")
  db1 <- db_comp(database = emperors, key = "ID", variable = "Beg")
  db2 <- db_comp(database = emperors, key = "ID", variable = c("Beg", "End"),
         category = "conflict")
  expect_type(db, "list")
  expect_type(db1, "list")
  expect_type(db2, "list")
  expect_true(tibble::is_tibble(db))
  expect_true(tibble::is_tibble(db1))
  expect_true(tibble::is_tibble(db2))
  expect_length(db, 37)
  expect_length(db1, 5)
  expect_length(db2, 9)
  expect_equal(nrow(db), nrow(db1))
  expect_named(db1, c("ID", "wikipedia$Beg", "UNRV$Beg",
                      "britannica$Beg", "Beg (3)"))
})
