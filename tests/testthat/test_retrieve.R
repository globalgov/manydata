membs <- tibble::tibble(StateID = c("ROU", "RUS", "DNK"),
                        manyID = c("ROU-RUS[RFP]_1901A", "ROU-RUS[RFP]_1901A", "GD16FI_1901A"),
                        Title = c("Convention Between Roumania And Russia Concerning Fishing",
                                  "Convention Between Roumania And Russia Concerning Fishing",
                                  "Convention Between The Governments Of Denmark And ..."),
                        Beg = c("1901-02-22", "1901-02-22", "1901-06-24"),
                        End = c(NA, NA, NA),
                        Text = c("treaty 1", "treaty 2", "treaty 3"))
bilats <- tibble::tibble(manyID = "ROU-RUS[RFP]_1901A", StateID1 = "ROU", StateID2 = "RUS",
                         Title = "Convention Between Roumania And Russia Concerning Fishing",
                         Beg = "1901-02-22")
multi <- tibble::tibble(manyID = "GD16FI_1901A",
                        Title = "Convention Between The Governments Of Denmark And ...",
                        Beg = "1901-06-24")
mlist <- tibble::tibble(manyID = c("ROU-RUS[RFP]_1901A", "GD16FI_1901A"),
                       Memberships = c("ROU, RUS", "DNK"))
db <- tibble::tibble(manyID = c("ROU-RUS[RFP]_1901A",
                                "ROU-RUS[RFP]_1901A:ROU-RUS[RFP]_1901A",
                                "GD16FI_1901A"))
links <- tibble::tibble(Agreement = "ROU-RUS[RFP]_1901A",
                        Link = "ROU-RUS[RFP]_1901A")

texts <- tibble::tibble(manyID = c("ROU-RUS[RFP]_1901A", "ROU-RUS[RFP]_1901A",
                                   "GD16FI_1901A"),
                        Text = c("treaty 1", "treaty 2", "treaty 3"))

test_that("retrieve functions work", {
  expect_equal(retrieve_bilaterals(membs), bilats)
  expect_equal(retrieve_multilaterals(membs), multi)
  expect_equal(retrieve_membership_list(membs), mlist)
  expect_equal(retrieve_links(dataset = db), links)
  expect_equal(retrieve_texts(dataset = membs), texts)
})
