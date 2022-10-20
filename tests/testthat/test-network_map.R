# Test if network_map() meets the many universe requirements

membership  <-  membership <- migraph::as_igraph(data.frame(
  from = c("ETH", "ETH", "ETH", "ETH", "UKR", "UKR",
           "MOZ", "MOZ", "JPN", "JPN"),
  to = c("GNQ", "KEN", "TZA", "RWA", "CHN", "POL",
         "COL", "NZL", "MNE", "LKA")))
testlight   <-  network_map(membership, date = "2010-01-01", theme = "light")
testdark    <-  network_map(membership, date = "2010-01-01", theme = "dark")
testearth   <-  network_map(membership, date = "2010-01-01", theme = "earth")

test_that("Mapping networks works", {
  expect_equal(class(testlight$data), c("layout_tbl_graph",
                                        "layout_ggraph",
                                        "data.frame"))
  expect_equal(class(testdark$data), c("layout_tbl_graph",
                                        "layout_ggraph",
                                        "data.frame"))
  expect_equal(class(testdark$data), c("layout_tbl_graph",
                                        "layout_ggraph",
                                        "data.frame"))
  expect_equal(testlight[["theme"]][["plot.background"]][["fill"]], "#FFFAFA")
  expect_equal(testdark[["theme"]][["plot.background"]][["fill"]], "#596673")
  expect_equal(testearth[["theme"]][["plot.background"]][["fill"]], "#4259FD")
  expect_equal(testlight[["layers"]][[1]][["aes_params"]][["fill"]], "#596673")
  expect_equal(testdark[["layers"]][[1]][["aes_params"]][["fill"]], "#FFFAFA")
  expect_equal(testearth[["layers"]][[1]][["aes_params"]][["fill"]], "#79B52F")
})

test_that("Errors are thrown if input is not correct", {
  expect_error(network_map("membership", date = "2010-01-01", theme = "light"))
  expect_error(network_map(membership, date = "2010-01-01", theme = "purple"))
})
