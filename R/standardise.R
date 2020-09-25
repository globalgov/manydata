#' Capitalises all words
#'
#' Capitalises all words in a string to enable comparison
#' @param s A string
#' @param strict By default FALSE
#' @details The function capitalises all words in the strings passed to it,
#' as well as trimming all white space from the starts and ends of the strings.
#' @return A capitalised, trimmed string
#' @import textclean
#' @import english
#' @import stringr
#' @importFrom utils as.roman
#' @importFrom dplyr if_else
#' @examples
#' \dontrun{
#' e <- entitle("A treaty concerning things")
#' e==c("A Treaty Concerning Things")
#' }
#' @export
entitle <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {
                            s <- substring(s, 2);
                            if (strict) tolower(s) else s
                            },
                           sep = "", collapse = " ")
  out <- sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
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
