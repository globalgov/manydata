#' Code Agreement Titles
#'
#' Creates an ID column that contains information on the
#' parties to an agreement, the type of agreement, the date
#' and the linkage to other agreements in the dataset.
#' @param title title column variable.
#' Ideally, title variable should come from a qPackage database/dataset
#' for which strings were standardised with `standardise_titles()`
#' @param date date column variable
#' Ideally, date variable should come from a qPackage database/dataset
#' for which dates were standardised with `standardise_dates()`
#' @param dataset name of the dataset, optional. When provided with
#' dataset argument, the function will return qID as a column in the dataset.
#' If not provided, the function will return a character vector with qIDs.
#' @importFrom usethis ui_done
#' @importFrom stringr str_replace_all str_detect
#' @examples
#' IEADB <- dplyr::slice_sample(qEnviron::agreements$IEADB, n = 10)
#' code_agreements(IEADB$Title, IEADB$Signature, dataset = IEADB)
#' @export
code_agreements <- function(title, date, dataset = NULL) {

  if (missing(title)) {
    stop("Please declare a title column.")
  }
  if (missing(date)) {
    stop("Please declare a beginning date column.")
  }

  # Step one: create a new qID column
  qID <- purrr::map(title, as.character)
  # Eventually this will connect to a centralized GitHub repo or SQL file for
  # smarter, interactive and more consistent coding of agreement titles.

  # Step two: code parties if present
  parties <- code_parties(qID)
  usethis::ui_done("Coded agreement parties")

  # Step three: code agreement type
  type <- code_type(qID)
  usethis::ui_done("Coded agreement type")

  # Step four: code known agreements
  abbrev <- code_known_agreements(qID)
  usethis::ui_done("Coded known agreements")

  #step five: give the observation a unique ID by dates
  uID <- code_dates(title, date)
  usethis::ui_done("Coded agreement dates")

  # step six: detect treaties from the same 'family'
  line <- code_linkage(qID, date)
  usethis::ui_done("Coded agreement linkages")

  # Step seven: add items together correctly
  # The following coding assumes that any other
  # types than A (= Agreement) are linked to another treaty.
  out <- ifelse((!is.na(abbrev) & (type == "A")), paste0(abbrev),
                # if abrreviation is known and type is agreement
                (ifelse((!is.na(abbrev) & (type != "A")), paste0(uID, type, "_", line),
                        # if abrreviation is known and type is not agreement
                        (ifelse((is.na(parties) & (type == "A")), paste0(uID, type),
                                # if parties were not identified and type is agreement
                                (ifelse((is.na(parties) & (type != "A")), paste0(uID, type, "_", line),
                                        # if parties were not identified and type is not agreement
                                        (ifelse((!is.na(parties) & (type == "A")), paste0(uID, "_", parties),
                                                # if parties were identified and type is agreement
                                                (ifelse((!is.na(parties) & (type != "A")), paste0(uID, type, "_", line), NA)))))))))))
                                                        # if parties were identified and type is not agreement

  # When line is left empty, the last "_" of the qID should be deleted
  out <- stringr::str_remove_all(out, "_$")
  out <- stringr::str_replace_all(out, "NA_", NA_character_)

  cat(sum(is.na(out)), "entries were not matched at all.\n")
  cat("There were", sum(duplicated(out, incomparables = NA)), "duplicated IDs.\n")

  qID <- out

  usethis::ui_done("Please run `vignette('agreements')` for more information.")

  # Step eight: add new qID column to data if dataset argument is provided
  if (!is.null(dataset)) {
    dataset <- dataset %>%
      dplyr::mutate(qID = qID)
  } else {
    qID
  }
}

#' Code Agreement Parties
#'
#' Identify the countries that are part of an agreement.
#' @param title A character vector of treaty titles
#' @importFrom qStates code_states
#' @importFrom stringr str_replace_all
#' @return A character vector of parties that are mentioned in the treaty title
#' @examples
#' IEADB <- dplyr::slice_sample(qEnviron::agreements$IEADB, n = 10)
#' code_parties(IEADB$Title)
#' @export
code_parties <- function(title) {
  
  parties <- qStates::code_states(title)
  parties <- stringr::str_replace_all(parties, "_", "-")
  parties[!grepl("-", parties)] <- NA

  # Only considers bilateral agreements where two parties have been identified
  parties <- ifelse(stringr::str_detect(parties, "^[:alpha:]{3}-[:alpha:]{3}$"), parties,
                    ifelse(stringr::str_detect(parties, "^[:alpha:]{2}-[:alpha:]{3}$"), parties,
                           ifelse(stringr::str_detect(parties, "^[:alpha:]{3}-[:alpha:]{2}$"), parties, NA)))

  parties

}

#' Code Agreement Type
#'
#' Identify the type of international agreement from titles.
#' Agreements can be, for example, multilateral treaties or coventions (A),
#' protocols (P) or amendments (E), if they contain words in title.
#' @param title A character vector of treaty title
#' @return A character vector of the treaty type
#' @importFrom dplyr case_when
#' @examples
#' IEADB <- dplyr::slice_sample(qEnviron::agreements$IEADB, n = 10)
#' code_type(IEADB$Title)
#' @export
code_type <- function(title) {

  type <- dplyr::case_when(
    # When the title contains "Protocol amending..."
    grepl("^Protocol", title, ignore.case = T) ~ "P",
    # E stands for amendment
    grepl("amend|modify|extend|proces-verbal", title, ignore.case = T) ~ "E",
    # P stands for protocols
    grepl("protocol|additional|subsidiary|supplementary|
          complementaire|
          complementar|complementario|annex |annexes ",
          title, ignore.case = T) ~ "P",
    # Added annex in this category
    # A stands for agreements
    grepl("agreement|arrangement|accord|acuerdo|bilateral co|
          technical co|treat|trait|tratado|convention|convencion|
          convenio|constitution|charte|instrument|statute|estatuto|
          provisional understanding|provisions relating|ubereinkunft",
          title, ignore.case = T) ~ "A",
    grepl("Act|Declaration|Covenant|Scheme|Government Of|Law",
          title, ignore.case = T) ~ "A",
    # title stands for excahnges of notes
    grepl("Exchange|Letters|Notas", title, ignore.case = T) ~ "X",
    # Y stands for memorandum of understanding
    grepl("Memorandum|memorando|Principles of Conduct|Code of Conduct",
          title, ignore.case = T) ~ "Y",
    # W stands for resolutions
    grepl("Agreed Measures|Agreed Record|Consensus|Conclusions|
          Decision|Directive|Regulation|
          Reglamento|Resolution|Rules|Recommendation",
          title, ignore.case = T) ~ "W",
    # Q stands for minutes
    grepl("Minute|Adjustment|First Session Of|First Meeting Of|Commission|
          Committee|Center", title, ignore.case = T) ~ "Q",
    # V stands for declarations
    grepl("Statement|Communiq|Comminiq|Joint Declaration|Proclamation|
          Administrative Order", title, ignore.case = T) ~ "V",
    # S stands for strategies
    grepl("Strategy|Plan|Program|Improvement|Project|Study|Working Party|
          Working Group", title, ignore.case = T) ~ "S",
  )

  # Extracts meaningful ordering numbers for protocols and amendments
  number <- order_agreements(title)

  # When no type is found
  type <- stringr::str_replace_na(type, "O")

  type <- paste0(type, number)

  type

}

#' Creates Numerical IDs from Signature Dates
#'
#' Agreements should have a unique identification number that is meaningful,
#' we condense their signature dates to produce this number.
#' @param title A title variable
#' @param date A date variable
#' @return A character vector with condensed dates
#' @import stringr
#' @examples
#' IEADB <- dplyr::slice_sample(qEnviron::agreements$IEADB, n = 10)
#' code_dates(IEADB$Title, IEADB$Signature)
#' @export
code_dates <- function(title, date) {

  uID <- stringr::str_remove_all(date, "-")
  # For treaties without signature date
  uID[is.na(uID)] <- paste0("9999",
                            sample(1000:9999, sum(is.na(uID)), replace = TRUE))

  # When the date is a range, the uID is taking only
  # the first value of the dates range (temporary solution)
  A <- stringr::str_extract_all(title, "^[:alpha:]")
  A <- stringr::str_to_upper(A)
  B <- stringr::str_sub(title, start = 19, end = 19)
  B <- suppressWarnings(ifelse(stringr::str_detect(B, "\\s"), "L", B))
  B <- stringr::str_to_upper(B)
  C <- stringr::str_extract_all(title, "[:alpha:]$")
  C <- suppressWarnings(ifelse(!stringr::str_detect(C, "^[:alpha:]$"), "O", C))
  C <- stringr::str_to_upper(C)
  uID <- stringr::str_replace_all(uID, "[:digit:]{4}\\:[:digit:]{8}$",
                                  paste0(A, B, C, "01"))

  uID

}

#' Known agreements abbreviation
#'
#' Some agreements have known abbreviations that facilitate their identification.
#' @param title A character vector of treaty title
#' @return A character vector of abbreviation of known treaties
#' @importFrom dplyr case_when
#' @examples
#' IEADB <- dplyr::slice_sample(qEnviron::agreements$IEADB, n = 10)
#' code_known_agreements(IEADB$Title)
#' @export
code_known_agreements <- function(title) {

  abbreviations <- purrr::map(abbreviations, as.character)
  # Assign the specific abbreviation to the "known" treaties
  ab <- sapply(abbreviations$title, function(x) grepl(x, title, ignore.case = T, perl = T)*1)
  colnames(ab) <- paste0(abbreviations$abbreviation, as.character(stringr::str_remove_all(abbreviations$signature, "-")))
  rownames(ab) <- title
  out <- apply(ab, 1, function(x) paste(names(x[x==1])))
  out[out=="character(0)"] <- NA_character_
  out <- unname(out)
  out <- unlist(out)

  # If output is a list with no values, returns an empty list of the same lenght as title variable
  lt <- as.numeric(length(title))
  ifelse(length(out) == 0, out <- rep(NA_character_, lt), out)

  out

}

#' Code Agreement Linkages
#'
#' Identify the linkage between amendments and protocols to a main agreement.
#' @param title A character vector of treaty title
#' @param date A date variable
#' @importFrom textclean add_comma_space mgsub
#' @importFrom english ordinal
#' @importFrom stringr str_replace_all str_squish str_remove_all
#' @import dplyr
#' @return A character vector of the agreements that are linked
#' @details The function identifies duplicates via excluding a few
#' 'predictable' words from strings, this maintains key words that are
#' then used to identify and link duplicates.
#' This is a choice that considers errors should lie on the side of false
#' negatives rather than false positives.
#' @examples
#' IEADB <- dplyr::slice_sample(qEnviron::agreements$IEADB, n = 10)
#' code_linkage(IEADB$Title, IEADB$Signature)
#' @export
code_linkage <- function(title, date) {

  s <-  purrr::map(title, as.character)

  # Step one: find type, parties and known agreements
  type <- code_type(s)
  abbrev <- code_known_agreements(s)
  parties <- code_parties(s)

  # Step two: standardise words in title
  out <- standardise_titles(as.character(title))

  # Step three: and remove 'predictable words' in agreements
  predictable_words <- predictable_words$predictable_words
  predictable_words <- paste(predictable_words, collapse = '\\>|\\<')
  predictable_words <- paste0("\\<", predictable_words, "\\>")
  out <- gsub(predictable_words, "", out, ignore.case = TRUE)
  out <- stringr::str_replace_all(out, predictable_words, "")
  out <- gsub("\\s*\\([^\\)]+\\)", "", out, ignore.case = FALSE)
  out <- gsub("-", "", out, ignore.case = FALSE)
  out <- stringr::str_replace_all(out, ",|-", "")
  out <- stringr::str_replace_all(out, " [:digit:]{1} | [:digit:]{2} ", "")
  out <- trimws(out)
  out <- stringr::str_squish(out)
  out <- textclean::add_comma_space(out)
  out <- as.data.frame(out)

  # Step four: find duplicates
  dup <- duplicated(out)
  dates <- stringr::str_remove_all(date, "-")
  # When date is a range, remove the last
  # date (temporary solution to deal with date range)
  dates <- stringr::str_remove_all(dates, "\\:[:digit:]{8}$")

  id <- ifelse((!is.na(abbrev)), paste0(abbrev),
               (ifelse((is.na(parties)), paste0(dates, type),
                       (ifelse((!is.na(parties) & (type == "A")), paste0(dates, "_", parties),
                               (ifelse((!is.na(parties) & (type != "A")), paste0(dates, type), NA)))))))

  out <- cbind(out, dup, id)

  # Initialize variables to suppress CMD notes
  ref <- NULL

  # Step five: make sure duplicates have the same ID number
  out <- out %>%
    dplyr::group_by_at(dplyr::vars(out)) %>%
    dplyr::mutate(
      dup = dplyr::row_number() > 1,
      ref = ifelse(dup, paste0(dplyr::first(id)), as.character(id)))

  out <- out %>%
    dplyr::group_by(ref) %>%
    dplyr::mutate(n = dplyr::n()) %>%
    dplyr::mutate(line = dplyr::case_when(n != 1 ~ paste(ref), n == 1 ~ "1"))

  line <- out$line

  line <- stringr::str_replace_all(line, "^1$", "")

  line

}

#' Remove dates from agreement titles
#'
#' Dates can cause an issue when trying to extract meaningful
#' numbers from titles for ordering purposes, this function removes dates.
#' @param title A character vector of treaty title
#' @importFrom stringr str_remove
#' @return A character vector without dates
remove_dates <- function(title) {

  rd <- title
  rd <- stringr::str_remove(rd, "[:digit:]{2}\\s[:alpha:]{3}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{4}\\s[:digit:]{4}")
  rd <- stringr::str_remove(rd, "[:digit:]{2}\\s[:alpha:]{5}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{6}\\s[:digit:]{4}")
  rd <- stringr::str_remove(rd,  "[:digit:]{2}\\s[:alpha:]{7}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{8}\\s[:digit:]{4}")
  rd <- stringr::str_remove(rd, "[:digit:]{2}\\s[:alpha:]{9}\\s[:digit:]{4}| [:digit:]{1}\\s[:alpha:]{3}\\s[:digit:]{4}")
  rd <- stringr::str_remove(rd, "[:digit:]{1}\\s[:alpha:]{4}\\s[:digit:]{4}| [:digit:]{1}\\s[:alpha:]{5}\\s[:digit:]{4}")
  rd <- stringr::str_remove(rd, "[:digit:]{1}\\s[:alpha:]{6}\\s[:digit:]{4}| [:digit:]{1}\\s[:alpha:]{7}\\s[:digit:]{4}")
  rd <- stringr::str_remove(rd, "[:digit:]{1}\\s[:alpha:]{8}\\s[:digit:]{4}| [:digit:]{1}\\s[:alpha:]{9}\\s[:digit:]{4}")
  rd <- stringr::str_remove(rd, "[:digit:]{4}")

  rd

}

#' Extracts Orders from Agreements 
#'
#' Identifies and extracts meaningful numbers from agreements titles that
#' are important information about the order of the agreement.
#' @param title A character vector of treaty title
#' @import stringr
#' @return A character vector with meangniful numbers from titles
order_agreements <- function(title) {

  rd <- remove_dates(title)

  oa <- gsub("\\<one\\>|\\<first\\>", "1", rd)
  oa <- gsub("\\<two\\>|\\<second\\>", "2", oa)
  oa <- gsub("\\<three\\>|\\<third\\>", "3", oa)
  oa <- gsub("\\<four\\>|\\<fourth\\>", "4", oa)
  oa <- gsub("\\<five\\>|\\<fifth\\>", "5", oa)
  oa <- gsub("\\<six\\>|\\<sixth\\>", "6", oa)
  oa <- gsub("\\<seven\\>|\\<seventh\\>", "7", oa)
  oa <- gsub("\\<eight\\>|\\<eighth\\>", "8", oa)
  oa <- gsub("\\<nine\\>|\\<ninth\\>", "9", oa)
  oa <- gsub("\\<ten\\>|\\<tenth\\>", "10", oa)
  oa <- gsub("\\<eleven\\>|\\<eleventh\\>", "11", oa)
  oa <- gsub("\\<twelve\\>|\\<twelfth\\>", "12", oa)
  oa <- gsub("\\<thirteen\\>|\\<thirteenth\\>", "13", oa)
  oa <- gsub("\\<fourteen\\>|\\<fourteenth\\>", "14", oa)
  oa <- gsub("\\<fifteen\\>|\\<fifteenth\\>", "15", oa)
  oa <- gsub("\\<sixteen\\>|\\<sixteenth\\>", "16", oa)
  oa <- gsub("\\<seventeen\\>|\\<seventeenth\\>", "17", oa)
  oa <- gsub("\\<eighteen\\>|\\<eighteenth\\>", "18", oa)
  oa <- gsub("\\<nineteen\\>|\\<nineteenth\\>", "19", oa)
  oa <- gsub("\\<twenty\\>|\\<twentieth\\>", "20", oa)

  oa <- stringr::str_extract(oa, "[:digit:]{1}|[:digit:]{2}")
  oa <- stringr::str_replace_na(oa)
  oa <- stringr::str_remove_all(oa, "NA")

  oa

}
