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
    s <- substring(s, 2);
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

#' Resorting and filtering dates
#'
#' Resorting and filtering dates
#' @param data a dataframe
#' @param vars a character vector identifying columns in the dataframe to sequence
#' @param unity a string identifying how multiple entries may be glued together.
#' By default, tidyr::unite() glues using the underscore "_".
#' @return a dataframe/columns
#' @import lubridate
#' @importFrom stats na.omit
#' @examples
#' \dontrun{
#' data <- data.frame(Sign = c("2000-01-01", "2001-01-01", "2001-01-01_2000-01-01", "2000-01-01", NA),
#'                    Force = c("2001-01-01", "2000-01-01", "2001-01-01", NA, "2001-01-01"))
#' resequence(data, c("Sign","Force"))
#' }
#' @export
resequence <- function(data, vars, unity = "_"){

  len <- length(vars)

  out <- apply(data[,vars], 1, function(x) {
    dates <- sort(unlist(strsplit(unique(na.omit(x)),unity)))

    if (length(dates) < len){
      dates <- interleave(dates, which(is.na(x)))
    }

    if (length(dates) > len){
      if (sum((!grepl("-01-01", dates))*1)>=len) dates <- dates[!grepl("-01-01",dates)]
      if (sum((!grepl("9999", dates))*1)>=len) dates <- dates[!grepl("9999",dates)]

      dmax <- max(lubridate::as.duration(interval(dates[1:(length(dates)-1)],dates[2:(length(dates))])))
      dmax <- which(lubridate::as.duration(interval(dates[1:(length(dates)-1)],dates[2:(length(dates))]))==as.duration(dmax))
      dates <- dates[c(1,dmax+1)]
    }

    dates
  })

  t(out)

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

