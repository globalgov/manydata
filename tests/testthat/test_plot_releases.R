testplot <- plot_releases("globalgov/qData")

test_that("Plotting function visualises historical milestones/releases of a repository", {
  skip_on_os("mac")
  expect_true(is.list(testplot))
  expect_length(testplot, 9)
  expect_named(testplot[1:3], c("data", "layers", "scales"))
})
