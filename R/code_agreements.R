#' @param title title column variable
#' @param date date column variable
#' @param dataset name of the dataset
#' @export
code_agreements <- function(title, date, dataset = NULL) {

  if(missing(title)){
    stop("Please declare a title column.")
  }
  if(missing(date)){
    stop("Please declare a date column.")
  }
  
  # Step one: create a new qID column
  qID <- purrr::map(title, as.character)
  # Eventually this will connect to a centralized GitHub repo or SQL file for 
  # smarter, interactive and more consistent coding of agreement titles. 
  
  # Step two: code parties if present
  qID <- code_parties(qID)
  
  # Step three: code agreement topic
  # Categories and key words still need some adjustements
  qID <- code_topic(qID)
  
  # Step four: code agreement type 
  # Categories and key words still need some adjustments
  qID <- code_type(qID)
  
  #step five: give the observation a unique ID
  uID <- stringr::str_remove_all(date, "-")
  
  # step six: add items together correctly. the XXX is treated in the next step.
  # The following coding assumes that any other types than A (= Agreement) are linked to another treaty; this coding
  # would need to be adapted for declarations, MoU, minutes, etc
  out <- ifelse((is.na(parties) & (type == "A")), paste0(topic, "_", type, uID),
                (ifelse((is.na(parties) & (type != "A")), paste0(topic, "_", "XXX", "-", type, uID),
                        (ifelse((!is.na(parties) & (type == "A")), paste0(parties, "-", topic, uID),
                                (ifelse((!is.na(parties) & (type != "A")), paste0(parties, "-", topic, "XXX", "-", type, uID), NA)))))))
  
  
  # step seven: detect treaties from the same 'family' (the XXX should be replaced by the uID of the main treaty)
  # This step is commented out as the coding are still on development.
  qID <- code_lineage(qID)
  
  out <- stringr::str_replace_all(out, "NA_", NA_character_)
  cat(sum(is.na(out)), "entries were not matched at all.\n")
  # cat(sum(stringr::str_detect(out, "^[0-9]")), " entries were only coded by date.\n")
  cat("There were", sum(duplicated(out, incomparables = NA)), "duplicated IDs.\n")
  
  qID <- out
  
  # Step eight: add new qID column to data
  
  if(is.null(dataset) == FALSE) {
    cbind(dataset, qID) 
  }
  
  qID 

}

#' Code Agreement Parties
#'
#' @param x variable
#'
#' @return
#' @export
#'
#' @examples
code_parties <- function(x) {
  
  parties <- qStates::code_states(x)
  parties <- stringr::str_replace_all(parties, "_", "-")
  parties[!grepl("-", parties)] <- NA
  parties
  
}

#' Code Agreement Type
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
code_type <- function(x) {
  
  type <- case_when(
    # E stands for amendment
    grepl("amend|modify|extend|proces-verbal", x, ignore.case = T) ~ "E",
    # P stands for protocols
    grepl("protocol|additional|subsidiary|supplementary|complÃ©mentaire|complementar|complementario", x, ignore.case = T) ~ "P",
    # A stands for agreements
    grepl("agreement|arrangement|accord|acuerdo|bilateral co|technical co|treat|trait|tratado|convention|convencion|convenio|constitution|charte|instrument|statute|estatuto|provisional understanding|provisions relating|übereinkunft", qID, ignore.case = T) ~ "A",
    grepl("Act|Declaration|Covenant|Scheme|Government Of|Law", x, ignore.case = T) ~ "A",
    # X stands for exchanges of notes
    grepl("Exchange|Letters|Notas", x, ignore.case = T) ~ "X",
    # Y stands for memorandum of understanding
    grepl("Memorandum|MemorÃ¡ndum|Principles of Conduct|Code of Conduct", x, ignore.case = T) ~ "Y",
    # W stands for resolutions
    grepl("Agreed Measures|Agreed Record|Consensus|Conclusions|Decision|Directive|Regulation|Reglamento|Resolution|Rules|Recommendation", x, ignore.case = T) ~ "W",
    # Q stands for minutes
    grepl("Minute|Adjustment|First Session Of|First Meeting Of|Commission|Committee|Center", x, ignore.case = T) ~ "Q",
    # V stands for declarations
    grepl("Statement|Communiq|Comminiq|Joint Declaration|Proclamation|Administrative Order", x, ignore.case = T) ~ "V",
    # S stands for strategies
    grepl("Strategy|Plan|Program|Improvement|Project|Study|Working Party|Working Group", x, ignore.case = T) ~ "S",
  )
  
  # What happens when title has agreements and protocol in title?
  
  for(i in x)
    if(!str_detect(i, c("amend|modify|extend|proces-verbal", "protocol|additional|subsidiary|supplementary|complÃ©mentaire|complementar|complementario",
                        "agreement|arrangement|accord|acuerdo|bilateral co|technical co|treat|trait|tratado|convention|convencion|convenio|constitution|charte|instrument|statute|estatuto|provisional understanding|provisions relating|übereinkunft",
                        "Act|Declaration|Covenant|Scheme|Government Of|Law", "Exchange|Letters|Notas",
                        "Memorandum|MemorÃ¡ndum|Principles of Conduct|Code of Conduct", "Agreed Measures|Agreed Record|Consensus|Conclusions|Decision|Directive|Regulation|Reglamento|Resolution|Rules|Recommendation",
                        "Minute|Adjustment|First Session Of|First Meeting Of|Commission|Committee|Center", "Statement|Communiq|Comminiq|Joint Declaration|Proclamation|Administrative Order",
                        "Strategy|Plan|Program|Improvement|Project|Study|Working Party|Working Group"))) {
      type <- "OTH"
    }
  
  type
  
}

#' Code Agreemnt Topic
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
code_topic <- function(x) {
  
  topic <- case_when(
    grepl("Waste|disposal|pollut|toxic|hazard", x, ignore.case = T) ~ "WAS",
    grepl("species|habitat|ecosystems|biological diversity|genetic resources|biosphere", x, ignore.case = T) ~ "SPE",
    grepl("air|atmos|climate|outer space|ozone|emissions", x, ignore.case = T) ~ "AIR",
    grepl("water|freshwater|river|rhine|hydro|basin|drought", x, ignore.case = T) ~ "WAT",
    grepl("soil|wetland|desert|erosion", x, ignore.case = T) ~ "SOI",
    grepl("nature|environment|biodiversity|flora|plant|fruit|vegetable|seed|forest|tree", x, ignore.case = T) ~ "BIO",
    grepl("fish|salmon|herring|tuna|aquaculture|mariculture|molluscs", x, ignore.case = T) ~ "FIS",
    grepl("agricultur|food|livestock|crop|irrigation|cattle|meat|farm|cultivate", x, ignore.case = T) ~ "AGR",
    grepl("culture|scien|techno|trade|research|exploration|navigation|data|information", x, ignore.case = T) ~ "BOT",
    grepl("energy|nuclear|oil|mining|gas|hydro|power", x, ignore.case = T) ~ "NUC",
    grepl("accidents", x, ignore.case = T) ~ "ACC",
    grepl("chemicals, pesticides", x, ignore.case = T) ~ "CHE",
    grepl("climate", x, ignore.case = T) ~ "CLC",
    grepl("noise", x, ignore.case = T) ~ "NOI",
  )
  # Define the category OTH when no topic is found
  for(i in x)
    if(!str_detect(i, c("Waste|disposal|pollut|toxic|hazard", "species|habitat|ecosystems|biological diversity|genetic resources|biosphere", 
                        "air|atmos|climate|outer space|ozone|emissions", "water|freshwater|river|rhine|hydro|basin|drought", "soil|wetland|desert|erosion", 
                        "nature|environment|biodiversity|flora|plant|fruit|vegetable|seed|forest|tree", "fish|salmon|herring|tuna|aquaculture|mariculture|molluscs",
                        "agricultur|food|livestock|crop|irrigation|cattle|meat|farm|cultivate", "culture|scien|techno|trade|research|exploration|navigation|data|information",
                        "energy|nuclear|oil|mining|gas|hydro|power", "accidents", "chemicals", "pesticides","climate", "noise"))) {
    topic <- "OTH"
  }
  
  topic
}

#' Code Agreement Lineage
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
code_lineage <- function(x) {
  
  # Could be one solution to add the lineage instead of the "XXX". The main treaty is detected in the attached treaty title. The
  # uID from the main treaty is replacing "XXX" in qID
  # out <- ifelse((stringr::str_detect(qID[type != "A"], as.character(qID[type == "A"]))), stringr::str_replace(out, "XXX", uID), out)
  # Add a loop (the function str_detect() is not working with the pattern as a character vector)
  
  #Other solution: but pattern >1 so only use first element
  # ifelse(apply(dataset, 1, function(x){
  #   grepl(qID[type == "A"], qID[type != "A"])
  # }), stringr::str_replace(out, "XXX", uID[type == "A"]), out)
  
  # Adapted from: https://stackoverflow.com/questions/12999772/create-a-unique-id-by-fuzzy-matching-of-names-via-agrep-using-r
  #  line <- function(title) {
  # 
  #    same_agreements <- function(x) {
  #      x <- as.factor(x)
  #      matches <- lapply(levels(x), agrep, x=levels(x),fixed=TRUE, value=FALSE)
  #      levels(x) <- levels(x)[unlist(lapply(matches, function(x) x[0:20]))]
  #      as.character(x)
  #    }
  # # the function do not return TRUE or FALSE but NA or agreement titles
  # 
  #    familyline <- same_agreements(title)
  # 
  #    familycode <- lapply(title, function(x) {
  # 
  #      if(familyline == TRUE & (type == "A")) { # Agreement type do not have XXX in the coding
  #        xx <- stringr::str_split(uID, "", 8)
  #        xxa <- paste0(xx [[1]][6:8])
  #        qID <- stringr::str_replace(qID, "XXX", xxa)
  #        qID
  #      } else if(familyline == TRUE & (!type == "A")) {
  #        # need to give same line as parent agreement
  #        qID <- stringr::str_replace(qID, "XXX", xxa)
  #        qID
  #        }
  #      x
  #    })
  #   line <- familycode(title)
  # 
  #   line <- unlist(title)
  # 
  #   line
  # 
  #}
}
