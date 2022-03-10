#' Extracting international agreements
#' 
#' @description Some datasets in the membership databases across the
#' 'many* packages' (e.g. manyenviron) contain a myriad of information on
#' international agreements governing an international domain.
#' The extraction functions help researchers retrieve all bilateral
#' agreements, or multilateral agreements, from these datasets.
#' Researchers can use, for example, `extract_bileterals()` to retrieve
#' which countries have signed a specific international agreement or
#' several international agreements signed in a respective year.
#' Also, researchers can use `extract_multilaterals()` to retrieve
#' the titles of all multilateral agreements signed in the past 10 years.
#' Alternatively, to extract information from several datasets in a
#' memberships database, researchers can `consolidate()` the database
#' into one dataset with some combination of the rows, columns,
#' and observations of the datasets before extracting
#' bilateral or multilateral agreements.
#' @param membs A memberships dataset from one of the many packages
#' @import dplyr
#' @importFrom stringr str_detect
#' @name extraction
NULL

#' @rdname extraction
#' @return A tibble of bilateral agreements
#' @examples
#' membs <- tibble::tibble(CountryID = c("ROU", "RUS", "DNK"),
#' manyID = c("ROU-RUS[RFP]_1901A", "ROU-RUS[RFP]_1901A", "GD16FI_1901A"),
#' Title = c("Convention Between Roumania And Russia Concerning Fishing
#' In The Danube And The Pruth",
#' "Convention Between Roumania And Russia Concerning Fishing
#' In The Danube And The Pruth",
#' "Convention Between The Governments Of Denmark And
#' The United Kingdom Of Great Britain
#' And Northern Ireland For Regulating The Fisheries
#' Of Their Respective Subjects Outside
#' Territorial Waters In The Ocean Surrounding The Faroe Islands"),
#' Beg = c("1901-02-22", "1901-02-22", "1901-06-24"),
#' End = c(NA, NA, NA))
#' extract_bilaterals(membs)
#' @export
extract_bilaterals <- function(membs) {
  Beg <- CountryID <- CountryID1 <- CountryID2 <- End <- Title <- manyID <- NULL
  if (!any(colnames(membs) == "manyID")) {
    stop("manyID column not found, please declare a many packages dataset.")
  }
  bilats <- subset(membs, grepl("[A-Z]{3}-", manyID)) %>%
    dplyr::arrange(manyID, CountryID) %>%
    dplyr::select(CountryID, manyID, Title, Beg, End)
  bilats <- bilats %>%
    dplyr::filter(manyID %in%
                    names(table(bilats$manyID)[table(bilats$manyID) == 2]))
  bilats1 <- bilats %>%
    dplyr::filter(row_number() %% 2 == 0) %>%
    dplyr::rename(CountryID1 = "CountryID")
  bilats2 <- bilats %>%
    dplyr::filter(row_number() %% 2 == 1) %>%
    dplyr::rename(CountryID2 = "CountryID")
  bilats <- dplyr::full_join(bilats2, bilats1,
                             by = c("manyID", "Title", "Beg", "End")) %>%
    dplyr::select(CountryID1, CountryID2, Title, Beg, End)
  bilats
}

#' @rdname extraction
#' @return A tibble of multilateral agreements
#' @examples
#' membs <- tibble::tibble(CountryID = c("ROU", "RUS", "DNK"),
#' manyID = c("ROU-RUS[RFP]_1901A", "ROU-RUS[RFP]_1901A", "GD16FI_1901A"),
#' Title = c("Convention Between Roumania And Russia Concerning Fishing
#' In The Danube And The Pruth",
#' "Convention Between Roumania And Russia Concerning Fishing
#' In The Danube And The Pruth",
#' "Convention Between The Governments Of Denmark And
#' The United Kingdom Of Great Britain
#' And Northern Ireland For Regulating The Fisheries
#' Of Their Respective Subjects Outside
#' Territorial Waters In The Ocean Surrounding The Faroe Islands"),
#' Beg = c("1901-02-22", "1901-02-22", "1901-06-24"),
#' End = c(NA, NA, NA))
#' extract_multilaterals(membs)
#' @export
extract_multilaterals <- function(membs) {
  manyID <- Title <- Beg <- End <- NULL
  if (!any(colnames(membs) == "manyID")) {
    stop("manyID column not found, please declare a many packages dataset.")
  }
  multi <- subset(membs, stringr::str_detect(manyID, "\\-", negate = TRUE)) %>%
    dplyr::filter(manyID %in% names(table(manyID)[table(manyID) != 2])) %>%
    dplyr::select(manyID, Title, Beg, End)
  multi
}
