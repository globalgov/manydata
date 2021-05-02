#' Standardise titles
#'
#' Standardises words in a string to enable comparison
#' @param s A string
#' @param strict By default FALSE
#' @param api_key If google API key is provided, the function will translate and
#' return strings in english using google translator.
#' @details The function capitalises all words in the strings passed to it,
#' as well as trimming all white space from the start, middle and end
#' of the strings.If an API key is provided as an argument,
#' the function detects strings in other languages
#' and translates them to English.
#' @return A capitalised, trimmed string
#' @import textclean
#' @import english
#' @import stringr
#' @import dplyr
#' @importFrom utils as.roman
#' @importFrom stringi stri_trans_general
#' @examples
#' \dontrun{
#' e <- standardise_titles("A treaty concerning things")
#' e==c("A Treaty Concerning Things")
#' }
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

    # For titles in other languages than english, we need to dectct language first
    lang <- out %>%
      vapply(., purrr::map_chr, "", cld2::detect_language) %>%
      data.frame(check.names = FALSE)
    out <- cbind(out, lang)
    
    # Translates only the titles not in English
    for (k in 1:nrow(out)) {
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
  stringi::stri_trans_general(out, id = "Latin-ASCII")
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
                 if_else(stringr::str_count(ords, "\\S+") == 2,
                         paste0("|", gsub(" ", "-", as.character(ords))),
                         ""))
  out <- textclean::mgsub(out,
                          paste0("(?<!\\w)", ords, "(?!\\w)"),
                          as.numeric(1:100),
                          safe = TRUE, perl = TRUE,
                          ignore.case = TRUE, fixed = FALSE)
  out
}

#' Fills missing data by lookup
#'
#' Fills missing data where known by other observations with the same id/index
#' @param df a dataframe
#' @param id a string identifying a column in the dataframe for indexing
#' @param var a string identifying a column or columns in the dataframe
#' to be filled
#' @return a dataframe
#' @examples
#' \dontrun{
#' myData <- repaint(myData, id="StatID", var=c("Area","Region")
#' }
#' @export
repaint <- function(df, id, var) {
  for (co in var) {
    for (ea in unique(df[, id])) {
      if (any(!is.na(df[df[, id] == ea, co])) &
         any(is.na(df[df[, id] == ea, co]))) {
        df[df[,id] == ea &
             is.na(df[, co]), co] <- df[df[, id] == ea &
                                         !is.na(df[, co]), co][1]
      }
    }
  }
  df
}
