#' @param dataset Name of the dataset
#' @param title Title collumn variable
#' @param parties Parties to an agreement
#' @param topic Abbreviated topic of an agreement 
#' @param type Agreement type
#' @param uID Unique agreement ID
#' @param lineage Whether agreement is part of a lineage of agreements
#' @export
code_agreements <- function(dataset, title, beg, parties, topic, type, uID) {
  
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
  # Categories and key words still need some adjustements
  topic <- case_when(
    grepl("Waste|disposal|pollut|toxic|hazard", qID, ignore.case = T) ~ "WAS",
    grepl("species|habitat|ecosystems|biological diversity|genetic resources|biosphere", qID, ignore.case = T) ~ "SPE",
    grepl("air|atmos|climate|outer space|ozone|emissions", qID, ignore.case = T) ~ "AIR",
    grepl("water|freshwater|river|rhine|hydro|basin|drought", qID, ignore.case = T) ~ "WAT",
    grepl("soil|wetland|desert|erosion", qID, ignore.case = T) ~ "SOI",
    grepl("nature|environment|biodiversity|flora|plant|fruit|vegetable|seed|forest|tree", qID, ignore.case = T) ~ "BIO",
    grepl("fish|salmon|herring|tuna|aquaculture|mariculture|molluscs", qID, ignore.case = T) ~ "FIS",
    grepl("agricultur|food|livestock|crop|irrigation|cattle|meat|farm|cultivate", qID, ignore.case = T) ~ "AGR",
    grepl("culture|scien|techno|trade|research|exploration|navigation|data|information", qID, ignore.case = T) ~ "BOT",
    grepl("energy|nuclear|oil|mining|gas|hydro|power", qID, ignore.case = T) ~ "NUC",
    grepl("accidents", qID, ignore.case = T) ~ "ACC",
    grepl("chemicals, pesticides", qID, ignore.case = T) ~ "CHE",
    grepl("climate", qID, ignore.case = T) ~ "CLC",
    grepl("noise", qID, ignore.case = T) ~ "NOI",
  )
  # Define the category OTH when no topic is found
  topic <- stringr::str_replace_na(topic, "OTH")
  
  # Step four: code agreement type 
  # Categories and key words still need some adjustments
  type <- case_when(
    # E stands for amendment
    grepl("amend|modify|extend|proces-verbal", qID, ignore.case = T) ~ "E",
    # P stands for protocols
    grepl("protocol|additional|subsidiary|supplementary|complÃ©mentaire|complementar|complementario", qID, ignore.case = T) ~ "P",
    # A stands for agreements
    grepl("agreement|arrangement|accord|acuerdo|bilateral co|technical co|treat|trait|tratado|convention|convencion|convenio|constitution|charte|instrument|statute|estatuto|provisional understanding|provisions relating|übereinkunft", qID, ignore.case = T) ~ "A",
    grepl("Act|Declaration|Covenant|Scheme|Government Of|Law", qID, ignore.case = T) ~ "A",
    # X stands for exchanges of notes
    grepl("Exchange|Letters|Notas", qID, ignore.case = T) ~ "X",
    # Y stands for memorandum of understanding
    grepl("Memorandum|MemorÃ¡ndum|Principles of Conduct|Code of Conduct", qID, ignore.case = T) ~ "Y",
    # W stands for resolutions
    grepl("Agreed Measures|Agreed Record|Consensus|Conclusions|Decision|Directive|Regulation|Reglamento|Resolution|Rules|Recommendation", qID, ignore.case = T) ~ "W",
    # Q stands for minutes
    grepl("Minute|Adjustment|First Session Of|First Meeting Of|Commission|Committee|Center", qID, ignore.case = T) ~ "Q",
    # V stands for declarations
    grepl("Statement|Communiq|Comminiq|Joint Declaration|Proclamation|Administrative Order", qID, ignore.case = T) ~ "V",
    # S stands for strategies
    grepl("Strategy|Plan|Program|Improvement|Project|Study|Working Party|Working Group", qID, ignore.case = T) ~ "S",
  )
  
  #step five: give the observation a unique ID
    # should detect similarities based and give same unique IDs to similar obs
  uID <- stringr::str_remove_all(beg, "-")
  
# step six: add items together correctly. the XXX is treated in the next step. 
  # The following coding assumes that any other types than A (= Agreement) are linked to another treaty; this coding
  # would need to be adapted for declarations, MoU, minutes, etc
  out <- ifelse((is.na(parties) & (type == "A")), paste0(topic, "_", uID),
                (ifelse((is.na(parties) & (type != "A")), paste0(topic, "_", "XXX", "-", type, uID),
                        (ifelse((!is.na(parties) & (type == "A")), paste0(parties, "_", topic, uID),
                                (ifelse((!is.na(parties) & (type != "A")), paste0(parties, "_", topic, "XXX", "-", type, uID), NA)))))))
  
  out <- stringr::str_replace_all(out, "NA_", NA_character_)
  cat(sum(is.na(out)), "entries were not matched at all.\n")
  # cat(sum(stringr::str_detect(out, "^[0-9]")), " entries were only coded by date.\n")
  cat("There were", sum(duplicated(out, incomparables = NA)), "duplicated IDs.\n")
  qID <- out
  
  # step seven: detect treaties from the same 'family' (the XXX should be replaced by the uID of the main treaty)
  # This step is commented out as the coding are still on development.
  # Adapted from: https://stackoverflow.com/questions/12999772/create-a-unique-id-by-fuzzy-matching-of-names-via-agrep-using-r
  
   line <- function(title) {
     
     same_agreements <- function(x) {
       x <- as.factor(x)
       matches <- lapply(levels(x), agrep, x=levels(x),fixed=TRUE, value=FALSE)
       levels(x) <- levels(x)[unlist(lapply(matches, function(x) x[0:20]))]
       as.character(x)
     }
     
     familyline <- same_agreements(title)
     
     familycode <- function(x) {
       
       if(familyline == TRUE & (type == "A")) {
         xx <- stringr::str_split(uID, "", 8)
         xxa <- paste0(xx [[1]][6:8])
         qID <- gsub("XXX", xxa)
         qID
       } else if(familyline == TRUE & (!type == "A")) {
         # need to give same line as parent agreement
         qID <- gsub("XXX", xxa)
         qID
         } 
       x
     }
     
    line <- familycode(title)
    
    line
    
  }
  
  # Step eight: add new qID column to data
  cbind(dataset, qID)
  
  qID 
}
