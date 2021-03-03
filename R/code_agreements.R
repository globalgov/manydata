#' @export
code_agreements <- function(title, beg){
  parties <- qStates::code_states(title)
  parties <- stringr::str_replace_all(parties, "_", "-")
  parties[!grepl("-", parties)] <- NA
  # Added topic abbreviations but other categories still need to be added
  topic <- case_when(
    grepl("Waste|waste|disposal|pollut|toxic|hazard", title, ignore.case = T) ~ "WAS",
    grepl("species|habitat|ecosystems|biological diversity|genetic resources|biosphere", title, ignore.case = T) ~ "SPE",
    grepl("air|atmos|climate|outer space|ozone|emissions", title, ignore.case = T) ~ "AIR",
    grepl("water|freshwater|river|rhine|hydro|basin|drought", title, ignore.case = T) ~ "WAT",
    grepl("soil|wetland|desert|erosion", title, ignore.case = T) ~ "SOI",
    grepl("nature|environment|biodiversity|flora|plant|fruit|vegetable|seed|forest|tree", title, ignore.case = T) ~ "BIO",
  )
  out <- paste(parties, beg, topic, sep = "_")
  out <- stringr::str_replace_all(out, "NA_", NA_character_)
  cat(sum(is.na(out)), "entries were not matched at all.\n")
  # cat(sum(stringr::str_detect(out, "^[0-9]")), " entries were only coded by date.\n")
  cat("There were", sum(duplicated(out, incomparables = NA)), "duplicated IDs.\n")
  out
}

