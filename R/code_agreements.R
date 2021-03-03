#' @export
code_agreements <- function(title, beg){
  parties <- qStates::code_states(title)
  parties <- stringr::str_replace_all(parties, "_", "-")
  parties[!grepl("-", parties)] <- NA
  out <- paste(parties, beg, sep = "_")
  out <- stringr::str_replace_all(out, "NA_", NA_character_)
  cat(sum(is.na(out)), "entries were not matched at all.\n")
  # cat(sum(stringr::str_detect(out, "^[0-9]")), " entries were only coded by date.\n")
  cat("There were", sum(duplicated(out, incomparables = NA)), "duplicated IDs.\n")
  out
}