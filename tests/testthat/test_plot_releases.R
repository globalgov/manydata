test_that("plot_releases work properly", {
  expect_error(plot_releases(), "argument \"repo\" is missing, with no default")
  a <- plot_releases("globalgov/manydata")
  expect_type(a, "list")
  expect_length(a, 9)
})
