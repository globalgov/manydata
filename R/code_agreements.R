#' Code agreements title
#' 
#' Creates an ID column that contains information on the
#' parties, the topic, the date and the relations to other
#' agreements in the dataset. 
#' @param title title column variable
#' @param date date column variable
#' @param dataset name of the dataset
#' @export
code_agreements <- function(title, date, dataset = NULL) {

  if(missing(title)){
    stop("Please declare a title column.")
  }
  if(missing(date)){
    stop("Please declare a beggining date column.")
  }
  
  # Step one: create a new qID column
  qID <- purrr::map(title, as.character)
  # Eventually this will connect to a centralized GitHub repo or SQL file for 
  # smarter, interactive and more consistent coding of agreement titles. 
  
  # Step two: code parties if present
  parties <- code_parties(qID)
  
  # Step three: code agreement topic
  # Categories and key words still need some adjustements
  topic <- code_topic(qID)
  
  # Step four: code agreement type 
  # Categories and key words still need some adjustments
  type <- code_type(qID)
  
  #step five: give the observation a unique ID
  uID <- stringr::str_remove_all(date, "-")
  uID <- stringr::str_remove_all(uID, "^[:digit:]{2}")
  uID <- stringr::str_remove_all(uID, "[:digit:]{2}$")
  
  # step six: detect treaties from the same 'family'
  line <- code_linkage(qID, date)
  
  # Step seven: add items together correctly
  # The following coding assumes that any other types than A (= Agreement) are linked to another treaty; this coding
  # would need to be adapted for declarations, MoU, minutes, etc
  out <- ifelse((is.na(parties) & (type == "A")), paste0(topic, "_", line, "-", uID),
                (ifelse((is.na(parties) & (type != "A")), paste0(topic, "_", line, "-", type, uID),
                        (ifelse((!is.na(parties) & (type == "A") & (stringr::str_detect(parties, "^[:alpha:]{3}-[:alpha:]{3}$"))), paste0(parties, "_", topic, line, "-", uID),
                                (ifelse((!is.na(parties) & (type == "A") & (!stringr::str_detect(parties, "^[:alpha:]{3}-[:alpha:]{3}$"))), paste0(topic,"_", line, "-", uID),
                                        (ifelse((!is.na(parties) & (type != "A") & (stringr::str_detect(parties, "^[:alpha:]{3}-[:alpha:]{3}$"))), paste0(parties, "_", topic, line, "-", type, uID),
                                                 (ifelse((!is.na(parties) & (type != "A") & (!stringr::str_detect(parties, "^[:alpha:]{3}-[:alpha:]{3}$"))), paste0(topic, line, "-", type, uID), NA)))))))))))
  
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
#' Identify the countries that are part of the agreement.
#' @param x A character vector of treaty titles
#' @importFrom qState code_states
#' @return A character vector of parties that are mentioned in the treaty title
#' @examples
#' IEADB$Countries <- code_parties(IEADB$Title)
#' @export
code_parties <- function(x) {
  
  parties <- qStates::code_states(x)
  parties <- stringr::str_replace_all(parties, "_", "-")
  parties[!grepl("-", parties)] <- NA
  parties
  
}

#' Code Agreement Type
#'
#' Identify the type of the international agreement.
#' @param x A character vector of treaty title
#' @return A character vector of the type of treaty
#' @importFrom stringr str_replace_na
#' @examples
#' IEADB$Type <- code_type(IEADB$Title)
#' @export
code_type <- function(x) {
  
  type <- case_when(
    # E stands for amendment
    grepl("amend|modify|extend|proces-verbal", x, ignore.case = T) ~ "E",
    # P stands for protocols
    grepl("protocol|additional|subsidiary|supplementary|complÃ©mentaire|complementar|complementario", x, ignore.case = T) ~ "P",
    # A stands for agreements
    grepl("agreement|arrangement|accord|acuerdo|bilateral co|technical co|treat|trait|tratado|convention|convencion|convenio|constitution|charte|instrument|statute|estatuto|provisional understanding|provisions relating|übereinkunft", x, ignore.case = T) ~ "A",
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
  # When no type is found
  type <- stringr::str_replace_na(type, "O")
  
  type
  
  # What happens when multiple types are detected in title?
}

#' Code Agrement Topic
#'
#'Identify the main environmental issue the treaty is tackling.
#' @param x A character vector of treaty title
#' @importFrom stringr str_replace_na
#' @import dplyr
#' @return A character vector of the treaty topic abbreviation.
#' @example 
#' @export
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
  # If not topic is found, category will be "OTH"
  topic <- stringr::str_replace_na(topic, "OTH")
  
  topic
}

#' Code Agreement Lineage
#'
#' @param x A character vector of treaty title
#' @import textclean
#' @import english
#' @import stringr
#' @import dplyr
#' @return
#' @example 
#' @export
code_linkage <- function(s, date) {
  
  # After much fiddling I am going back to standardise_titles() so that we
  # rely on base to do the substitution and matching need here.
  
  cap <- function(s) paste(toupper(substring(s, 1, 1)), {
    s <- substring(s, 2)
  }
  , sep = "", collapse = " ")
  out <- sapply(strsplit(as.character(s), split = " "), cap, USE.NAMES = !is.null(names(s)))
  out <- trimws(out)
  # Step one: remove known words and articles
  out <- gsub("amendment |modify |extend |verbal |protocol |additional |subsidiary |supplementary |complementary |complementario |
  agreement |arrangement |accord |acuerdo |bilateral |technical |treaty |trait |tratado |convention |convencion |convenio |constitution |
  charte |instrument |statute |estatuto |provisional |understanding |provisions |relating |übereinkunft |
  Act|Declaration|Covenant|Scheme|Government Of|Law|Exchange|Letters|Notas|Memorandum|MemorÃ¡ndum|Principles of Conduct|
  Code of Conduct |Agreed Measures |Agreed Record |Consensus |Conclusions |Decision |Directive |Regulation |Reglamento |Resolution |
  Rules |Recommendation |Minute |Adjustment |First|Session Of |First Meeting Of |Commission |Committee |Center |
  Statement |Communiq |Comminiq |Joint Declaration |Proclamation |Administrative Order |Strategy |Plan |Program |Improvement |Project |Study |
              Working Party |Working Group", "", out, ignore.case = TRUE)
  out <- gsub("\\<and\\>|\\<the\\>|\\<of\\>|\\<for\\>|\\<to\\>|\\<in\\>|\\<a\\>|\\<an\\>|\\<on\\>\\<and\\>|\\<the\\>", "", out, ignore.case = TRUE)
  out <- trimws(out)
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
  
  out <- as.data.frame(out)
  
  # Step two: find duplicates
  dup <- duplicated(out)
  id <- date
  id <- stringr::str_remove_all(id, "-")
  id <- stringr::str_remove_all(id, "^[:digit:]{2}")
  id <- stringr::str_remove_all(id, "[:digit:]{2}$")
  id <- as.numeric(id)
  out <- cbind(out, dup, id)
  
  # Step three: make sure duplicates have the same ID number
  out <- out %>% 
    group_by_at(vars(out)) %>% 
    mutate(
      dup = row_number() > 1,
      ref = ifelse(dup, paste0(first(id)), as.character(id)))
  
  out <- out$ref
  
  out
}  
