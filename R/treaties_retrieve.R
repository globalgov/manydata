#' Retrieve international treaties
#'
#' @description Some databases and datasets across the
#' 'many* packages' (e.g. manyenviron) contain a myriad of information on
#' international treaties governing an international domain.
#' Researchers can, for example, use `retrieve_bilaterals()` to retrieve
#' which countries have signed bilateral agreements in a respective year.
#' Alternatively, researchers can use `retrieve_multilaterals()` to retrieve
#' the titles of all multilateral agreements signed in the past 10 years.
#' Alternatively, researchers can retrieve treaties that modify,
#' amend, or expand other treaties with `retrieve_links()`.
#' Or, even, researchers can retrieve membership lists of countries part to a
#' certain treaty with `retrieve_membership_list()`.
#' Finally, researchers can retrieve treaty texts available in 'many' datasets
#' with `retrieve_texts()`.
#' To retrieve information from several datasets in a database,
#' researchers can `consolidate()` a database into one dataset
#' with some combination of the rows, columns, and observations
#' before getting the desired information.
#' @param dataset A dataset from one of the many packages.
#' @param treaty_type The type of treaties to be returned.
#' NULL, by default.
#' Other options are "bilateral" or "multilateral".
#' @param actor An actor variable.
#' "StateID", by default.
#' @import dplyr
#' @importFrom stringr str_detect str_extract str_remove_all
#' @importFrom purrr map map_chr
#' @importFrom tibble tibble
#' @name retrieve_treaty
NULL

#' @rdname retrieve_treaty
#' @return A tibble of bilateral agreements.
#' @examples
#' membs <- tibble::tibble(manyID = c("ROU-RUS[RFP]_1901A",
#' "ROU-RUS[RFP]_1901E", "GD16FI_1901A"),
#' Title = c("Convention Between Roumania And Russia Concerning Fishing
#' In The Danube And The Pruth",
#' "Convention Between Roumania And Russia Concerning Fishing
#' In The Danube And The Pruth",
#' "Convention Between The Governments Of Denmark And
#' The United Kingdom Of Great Britain
#' And Northern Ireland For Regulating The Fisheries
#' Of Their Respective Subjects Outside
#' Territorial Waters In The Ocean Surrounding The Faroe Islands"),
#' Beg = c("1901-02-22", "1901-02-22", "1901-06-24"))
#' retrieve_bilaterals(membs)
#' @export
retrieve_bilaterals <- function(dataset) {
  Beg <- StateID1 <- StateID2 <- Title <- manyID <- NULL
  if (!any(colnames(dataset) == "manyID")) {
    stop("manyID column not found, please declare a many packages dataset.")
  }
  subset(dataset, grepl("[A-Z]{3}-[A-Z]{3}", manyID)) %>%
    dplyr::mutate(StateID1 = stringr::str_extract(manyID, "^[A-Z]{3}"),
                  StateID2 = stringr::str_remove_all(stringr::str_extract(manyID, "-[A-Z]{3}"),
                                                     "-")) %>%
    dplyr::arrange(manyID) %>%
    dplyr::select(manyID, StateID1, StateID2, Title, Beg) %>%
    dplyr::distinct()
}

#' @rdname retrieve_treaty
#' @return A tibble of multilateral agreements.
#' @examples
#' membs <- tibble::tibble( manyID = c("ROU-RUS[RFP]_1901A",
#' "ROU-RUS[RFP]_1901A", "GD16FI_1901A"),
#' Title = c("Convention Between Roumania And Russia Concerning Fishing
#' In The Danube And The Pruth",
#' "Convention Between Roumania And Russia Concerning Fishing
#' In The Danube And The Pruth",
#' "Convention Between The Governments Of Denmark And
#' The United Kingdom Of Great Britain
#' And Northern Ireland For Regulating The Fisheries
#' Of Their Respective Subjects Outside
#' Territorial Waters In The Ocean Surrounding The Faroe Islands"),
#' Beg = c("1901-02-22", "1901-02-22", "1901-06-24"))
#' retrieve_multilaterals(membs)
#' @export
retrieve_multilaterals <- function(dataset) {
  manyID <- Title <- Beg <- NULL
  if (!any(colnames(dataset) == "manyID")) {
    stop("manyID column not found, please declare a many packages dataset.")
  }
  subset(dataset, stringr::str_detect(manyID, "\\-", negate = TRUE)) %>%
    dplyr::filter(manyID %in% names(table(manyID)[table(manyID) != 2])) %>%
    dplyr::select(manyID, Title, Beg)
}

#' @name retrieve_treaty
#' @return A tibble of manyIDs and countries part of the treaty.
#' @examples
#' membs <- tibble::tibble(StateID = c("ROU", "RUS", "DNK"),
#' manyID = c("ROU-RUS[RFP]_1901A", "ROU-RUS[RFP]_1901A", "GD16FI_1901A"))
#' retrieve_membership_list(dataset = membs)
#' @export
retrieve_membership_list <- function(dataset, actor = "StateID",
                                     treaty_type = NULL) {
  Actor <- Memberships <- manyID <- NULL
  if (!any(colnames(dataset) == "manyID")) {
    stop("manyID column not found, please declare a many packages dataset.")
  }
  membs_list <- dplyr::select(dataset, manyID, actor) %>%
    rename(Actor = 2)
  if (!is.null(treaty_type)) {
    if (treaty_type == "bilateral") {
      membs_list <- subset(membs_list, stringr::str_detect(manyID, "\\-"))
    }
    if (treaty_type == "multilateral") {
      membs_list <- subset(membs_list, stringr::str_detect(manyID, "\\-",
                                                           negate = TRUE))
      }
  }
  ml <- membs_list %>%
    dplyr::group_by(manyID) %>%
    dplyr::summarise(Memberships = toString(Actor)) %>%
    dplyr::ungroup()
  dplyr::left_join(membs_list, ml, by = "manyID") %>%
    dplyr::select(manyID, Memberships) %>%
    unique()
}

#' @rdname retrieve_treaty
#' @return A tibble of manyIDs and their links.
#' @examples
#' membs <- tibble::tibble(manyID = c("ROU-RUS[RFP]_1901A",
#' "ROU-RUS[RFP]_1901A:ROU-RUS[RFP]_1901A",
#' "GD16FI_1901A"))
#' retrieve_links(dataset = membs)
#' @export
retrieve_links <- function(dataset, treaty_type = NULL) {
  # Get manyID
  if (!any(colnames(dataset) == "manyID")) {
    stop("manyID column not found, please declare a many packages dataset.")
  }
  # Filter by links
  treatyID <- grep(":", dataset$manyID, value = TRUE)
  # Filter by treaty_type
  if (!is.null(treaty_type)) {
    if (treaty_type == "bilateral") {
    treatyID <- grep("-", treatyID, value = TRUE)
    }
    if (treaty_type == "multilateral") {
    treatyID <- grep("-", treatyID, value = TRUE, invert = TRUE)
    }
  }
  # Split treatyID
  Link <- purrr::map_chr(strsplit(treatyID, ":"), 2)
  Agreement <- purrr::map_chr(strsplit(treatyID, ":"), 1)
  # Return tibble
  tibble::tibble(Agreement, Link)
}

#' @rdname retrieve_treaty
#' @return A tibble of manyIDs and their texts.
#' @examples
#' membs <- tibble::tibble(manyID = c("ROU-RUS[RFP]_1901A",
#' "ROU-RUS[RFP]_1901A:ROU-RUS[RFP]_1901A",
#' "GD16FI_1901A"),
#' Text = c("treaty 1", "treaty 2", "treaty 3"))
#' retrieve_texts(dataset = membs)
#' #retrieve_texts(dataset = manyenviron::agreements$HUGGO)
#' @export
retrieve_texts <- function(dataset, treaty_type = NULL) {
  manyID <- NULL
  if (!any(colnames(dataset) == "manyID")) {
    stop("manyID column not found, please declare a many packages dataset.")
  }
  text_vars <- c("manyID", grep("text", names(dataset),
                                ignore.case = TRUE, value = TRUE))
  if (!is.null(treaty_type)) {
    if (treaty_type == "bilateral") {
      dataset <- subset(dataset, stringr::str_detect(manyID, "\\-"))
    }
    if (treaty_type == "multilateral") {
      dataset <- subset(dataset, stringr::str_detect(manyID, "\\-", negate = TRUE))
    }
  }
  tibble::tibble(dataset[, c(text_vars)]) %>%
    dplyr::filter(!dplyr::if_all(-manyID, is.na)) %>%
    dplyr::distinct()
}
