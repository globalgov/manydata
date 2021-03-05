#' @param dataset Name of the dataset
#' @param title Title collumn variable
#' @param parties Parties to an agreement
#' @param topic Abbreviated topic of an agreement 
#' @param type Agreement type
#' @param uID Unique agreement ID
#' @param lineage Whether agreement is part of a lineage of agreements
#' @export
code_agreements <- function(dataset, title, beg, parties, topic, type, uID, line) {
  
  if(missing(dataset)){
    stop("Please declare the dataset.")
  }
  if(missing(title)){
    stop("Please declare a title column.")
  }
  if(missing(beg)){
    stop("Please declare a beg column.")
  }
  
  # Step one: create a new qID column
  qID <- purrr::map(title, as.character)
  # Eventually this will connect to a centralized GitHub repo or SQL file for 
  # smarter, interactive and more consistent coding of agreement titles. 
  
  # Step two: code parties if present
  parties <- qStates::code_states(qID)
  parties <- stringr::str_replace_all(parties, "_", "-")
  parties[!grepl("-", parties)] <- NA
  
  # Step three: code agreement topic
  # Added some topic abbreviations but other categories still need to be added
  topic <- case_when(
    grepl("Waste|waste|disposal|pollut|toxic|hazard", qID, ignore.case = T) ~ "WAS",
    grepl("species|habitat|ecosystems|biological diversity|genetic resources|biosphere", qID, ignore.case = T) ~ "SPE",
    grepl("air|atmos|climate|outer space|ozone|emissions", qID, ignore.case = T) ~ "AIR",
    grepl("water|freshwater|river|rhine|hydro|basin|drought", qID, ignore.case = T) ~ "WAT",
    grepl("soil|wetland|desert|erosion", qID, ignore.case = T) ~ "SOI",
    grepl("nature|environment|biodiversity|flora|plant|fruit|vegetable|seed|forest|tree", qID, ignore.case = T) ~ "BIO",
  )
  
  # Step four: code agreement type 
  type <- case_when(
    # E stands for amendment
    grepl("amend|modify|extend|proces-verbal", qID, ignore.case = T) ~ "E",
    # P stands for protocols
    grepl("protocol|additional|subsidiary|supplementary|complÃ©mentaire|complementar|complementario", qID, ignore.case = T) ~ "P",
    # A stands for agreements
    grepl("agreement|arrangement|accord|acuerdo|bilateral co|technical co|treat|trait|tratado|convention|convencion|convenio|constitution|charte|instrument|statute|estatuto|provisional understanding|provisions relating|übereinkunft", v, ignore.case = T) ~ "A",
    grepl("Act|Declaration|Covenant|Scheme|Government Of|Law", qID, ignore.case = T) ~ "A",
  )
  
  #step five: give the observation a unique ID
  uID <- function(qID) {
# should detect similarities based and give same unique IDs to similar obs
#     
#    uID <- stringr::str_remove_all(beg, "-")
#     
#    if(maybe(qID) == TRUE) {
#       menu(c("Yes", "No"), title = "Are these the same treaty/agreement or amendments or protocols to the same treaty/agreement?")
#       if("Yes") {
#         qID <- same_agreements(qID) 
#         } else qID <- qID
#       }
#     
#     case_when(
#       # same agreements same uIDs
#       # obs with A will be reference
#     ) 
#     
#     case_when(
#       # user decided on ID
#     )
# }
#     
#   # adapted from: https://stackoverflow.com/questions/12999772/create-a-unique-id-by-fuzzy-matching-of-names-via-agrep-using-r
#     
# same_agreements <- function(x) {
#   x <- as.factor(x)
#   matches <- lapply(levels(x), agrep, x=levels(x),fixed=TRUE, value=FALSE)
#   levels(x) <- levels(x)[unlist(lapply(matches, function(x) x[0:10]))]
#   as.character(x)
# }
#     
# maybe <- function(x) {
#   x <- as.factor(x)
#   matches <- lapply(levels(x), agrep, x=levels(x),fixed=TRUE, value=FALSE)
#   levels(x) <- levels(x)[unlist(lapply(matches, function(x) x[10:20]))]
#   as.character(x)
# }
}
   
  #Step six: identify if agreement is part of a family
  line <- case_when(
    # should order observations with the same unique IDs as part of the same lineage
    # maybe this can be done according to date in beg
    # pay attention to agreements, like meetings, that have one number changed...
    # for lineage arguments beyond parts of the same treaty/agreement, 
    # a separate function may be warranted. 
  )
  
  # Step seven: add items together correctly   
  if(is.na == parties) {
    if(is.null(lineage)) {
      out <- paste0(topic, "_", uID, "-", type)  
    } else {
    out <- paste(topic, "_", uID, "-", type, line)
    }
  } else {
    if(is.null(lineage)) {
      out <- paste0(parties, "_", topic, "_", uID, "-", type)  
    } else {
    out <- paste0(parties, "_", topic, "_", uID, "-", type, line)
    }
  }
  
  out <- stringr::str_replace_all(out, "NA_", NA_character_)
  cat(sum(is.na(out)), "entries were not matched at all.\n")
  # cat(sum(stringr::str_detect(out, "^[0-9]")), " entries were only coded by date.\n")
  cat("There were", sum(duplicated(out, incomparables = NA)), "duplicated IDs.\n")
  qID <- out
  
  # Step eight: add new qID column to data
  cbind(dataset, qID)
  
  qID 

}
