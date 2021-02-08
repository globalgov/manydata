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
#' e <- standardise_titles("A treaty concerning things")
#' e==c("A Treaty Concerning Things")
#' }
#' @export
standardise_titles <- standardize_titles <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)), {
    s <- substring(s, 2)
    if (strict) tolower(s) else s
    }
    , sep = "", collapse = " ")
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
repaint <- function(df, id, var){
  for(co in var){
    for(ea in unique(df[,id])){
      if(any(!is.na(df[df[, id]==ea, co])) &
         any(is.na(df[df[, id]==ea, co]))){
        df[df[,id]==ea &
             is.na(df[,co]), co] <- df[df[,id]==ea &
                                         !is.na(df[,co]), co][1]
      }
    }
  }
  df
}
