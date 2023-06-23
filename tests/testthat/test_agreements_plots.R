# Test plot functions

test_that("agreements_plot() returns the correct output format", {
  agreements <- dplyr::filter(manyenviron::agreements$ECOLEX,
                              Beg > "2000-01-01" & Beg < "2000-12-12")
  p <- agreements_plot(agreements)
  expect_type(p, "list")
  expect_length(p, 9)
  expect_true(ggplot2::is.ggplot(p))
  expect_named(p, c("data", "layers", "scales", "mapping", "theme",
                     "coordinates", "facet", "plot_env", "labels"))
  expect_true(p[["plot_env"]][["layout"]] == "circle")
})

test_that("membership_plot() returns the correct output format", {
  memberships <- dplyr::filter(manyenviron::memberships$ECOLEX_MEM,
                               Beg > "2000-01-01" & Beg < "2000-06-12")
  p <- membership_plot(memberships)
  expect_type(p, "list")
  expect_length(p, 9)
  expect_true(ggplot2::is.ggplot(p))
  expect_named(p, c("data", "layers", "scales", "mapping", "theme",
                    "coordinates", "facet", "plot_env", "labels"))
  expect_true(p[["plot_env"]][["layout"]] == "circle")
})

