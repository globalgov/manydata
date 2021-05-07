#' Standardise titles
#'
#' Standardises words in a character title variable to improve readability,
#' facilitate string matching and enable more accurate comparisons
#' for variables in different datatsets.
#' @param s A string
#' @param strict By default FALSE
#' @param api_key If google API key is provided, the function will
#' translate and return strings in english using google translator.
#' @details The function capitalises words in the strings passed to it.
#' It trims white spaces from the start, middle and end of the strings.
#' Removes ambiguous punctions and symbols from strings.
#' All the strings are transformed into to ASCII character encoding.
#' Written numbers in ordinal form are transformed into numerical form.  
#' If a google API key is provided as an argument, the function detects
#' strings in other languages and translates them to English.
#' @return A capitalised, trimmed and standardised string
#' @importFrom textclean add_comma_space mgsub
#' @importFrom english ordinal
#' @import stringr
#' @import dplyr
#' @importFrom utils as.roman
#' @importFrom stringi stri_trans_general
#' @examples
#' e <- standardise_titles("A treaty concerning things")
#' e==c("A Treaty Concerning Things")
#' @export
standardise_titles <- standardize_titles <- function(s, strict = FALSE, api_key = NULL) {

  cap <- function(s) paste(toupper(substring(s, 1, 1)), {
    s <- substring(s, 2)
    if (strict) tolower(s) else s
  }
  , sep = "", collapse = " ")
  out <- vapply(strsplit(s, split = " "), cap, "", USE.NAMES = !is.null(names(s)))

  if (!is.null(api_key)) {
    qData::depends("cld2", "translateR")

    # Initialize variables to suppress CMD notes
    . <- NULL

    # For titles in other languages than English, we need to detect language first
    lang <- out %>%
      vapply(., purrr::map_chr, "", cld2::detect_language) %>%
      data.frame(check.names = FALSE)
    out <- cbind(out, lang)
    
    # Translates only the titles not in English
    for (k in seq_len(nrow(out))) {
    if (is.na(out$.[k])) {
      out$out[k] == out$out[k]
    } else if (out$.[k] == "en") {
      out$out[k] == out$out[k]
    } else {
      out$out[k] <- suppressWarnings(translateR::translate(content.vec = out$out[k],
                                                           google.api.key = api_key,
                                                           source.lang = out$.[k],
                                                           target.lang = "en"))
    }
  }
  out <- out$out
  }
  # Transforms strings to ASCII character encoding
  out <- suppressWarnings(stringi::stri_trans_general(out, id = "Latin-ASCII"))
  out[out == "NANA"] <- NA
  out <- trimws(out)
  out <- gsub("\\.(?=\\.*$)", "", out, perl = TRUE)
  out <- gsub("U.K.", "UK", out)
  out <- gsub("U.S.S.R.", "USSR", out)
  out <- gsub("U.S. ", "USA", out)
  out <- gsub("Art\\.", "Article", out)
  out <- gsub("\\#", "Number ", out)
  out <- textclean::add_comma_space(out)
  out <- textclean::mgsub(out,
                          paste0("(?<!\\w)", as.roman(1:100), "(?!\\w)"),
                          as.numeric(1:100),
                          safe = TRUE, perl = TRUE)
  ords <- english::ordinal(1:100)
  ords <- paste0(ords,
                 dplyr::if_else(stringr::str_count(ords, "\\S+") == 2,
                         paste0("|", gsub(" ", "-", as.character(ords))),
                         ""))
  out <- textclean::mgsub(out,
                          paste0("(?<!\\w)", ords, "(?!\\w)"),
                          as.numeric(1:100),
                          safe = TRUE, perl = TRUE,
                          ignore.case = TRUE, fixed = FALSE)
  num <- english::words(1:100)
  num <- paste0(num,
                 dplyr::if_else(stringr::str_count(num, "\\S+") == 2,
                                paste0("|", gsub(" ", "-", as.character(num))),
                                ""))
  out <- textclean::mgsub(out,
                          paste0("(?<!\\w)", num, "(?!\\w)"),
                          as.numeric(1:100),
                          safe = TRUE, perl = TRUE,
                          ignore.case = TRUE, fixed = FALSE)
  out
}
