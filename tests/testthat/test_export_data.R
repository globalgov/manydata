transp <- data.frame (bikes = c(5), skates = c(4))

test_that("export_data() stores create data folder and stores data", {
  create_local_package()
  export_data("transp")
  usethis:::expect_proj_file(path("data", "transp"))
})