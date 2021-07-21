test_that("get_packages() works properly", {
  expect_equivalent(get_packages(1), get_packages("qCreate"))
  expect_equivalent(get_packages(4), get_packages("globalgov/qStates"))
  expect_type(get_packages(), "NULL")
})
