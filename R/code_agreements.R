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
    stop("Please declare a beginning date column.")
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
  # uID <- stringr::str_remove_all(uID, "^[:digit:]{2}")
  # uID <- stringr::str_remove_all(uID, "[:digit:]{2}$")
  
  # step six: detect treaties from the same 'family'
  line <- code_linkage(qID, date)
  
  # Step seven: add items together correctly
  # The following coding assumes that any other types than A (= Agreement) are linked to another treaty; this coding
  # would need to be adapted for declarations, MoU, minutes, etc
  out <- ifelse((is.na(parties) & (type == "A")), paste0(topic, "-", uID),
                (ifelse((is.na(parties) & (type != "A")), paste0(topic, line, "-", type, uID),
                        (ifelse((!is.na(parties) & (type == "A") & (stringr::str_detect(parties, "^[:alpha:]{3}-[:alpha:]{3}$"))), paste0(parties, "_", topic, "-", uID),
                                (ifelse((!is.na(parties) & (type == "A") & (!stringr::str_detect(parties, "^[:alpha:]{3}-[:alpha:]{3}$"))), paste0(topic, "-", uID),
                                        (ifelse((!is.na(parties) & (type != "A") & (stringr::str_detect(parties, "^[:alpha:]{3}-[:alpha:]{3}$"))), paste0(parties, "_", topic, line, "-", type, uID),
                                                 (ifelse((!is.na(parties) & (type != "A") & (!stringr::str_detect(parties, "^[:alpha:]{3}-[:alpha:]{3}$"))), paste0(topic, line, "-", type, uID), NA)))))))))))
  
  out <- stringr::str_replace_all(out, "NA_", NA_character_)
  
  # Assign the specific abbreviation to the "known" treaties 
  out <- ifelse(stringr::str_detect(qID, "^United Nations Convention On The Law Of The Sea$"), paste0("UNCLOS","_", uID),
                ifelse(stringr::str_detect(qID, "United Nations Convention On The Law Of The Sea"), paste0("UNCLOS_19821210","-", type, uID), out))
  
  out <- ifelse(stringr::str_detect(qID, "^Convention On Biological Diversity$"), paste0("CBD","_", uID),
                ifelse(stringr::str_detect(qID, "Convention On Biological Diversity"), paste0("CBD_19920605","-", type, uID), out))
  
  out <- ifelse(stringr::str_detect(qID, "^Convention On The Conservation Of Antarctic Marine Living Resources$"), paste0("CCAMLR","_", uID),
                ifelse(stringr::str_detect(qID, "Convention On The Conservation Of Antarctic Marine Living Resources"), paste0("CCAMLR_19800520","-", type, uID), out))
  
  out <- ifelse(stringr::str_detect(qID, "^Convention On International Trade In Endangered Species Of Wild Fauna And Flora$"), paste0("CITES","_", uID),
                ifelse(stringr::str_detect(qID, "Convention On International Trade In Endangered Species Of Wild Fauna And Flora"), paste0("CITES_19730303","-", type, uID), out))
  
  out <- ifelse(stringr::str_detect(qID, "^International Convention On Civil Liability For Oil Pollution Damage$"), paste0("CLC","_", uID),
                ifelse(stringr::str_detect(qID, "International Convention On Civil Liability For Oil Pollution Damage"), paste0("CLC_19691129","-", type, uID), out))
  
  out <- ifelse(stringr::str_detect(qID, "^Antarctic Mineral Resources Convention$"), paste0("CRAMRA","_", uID),
                ifelse(stringr::str_detect(qID, "Antarctic Mineral Resources Convention"), paste0("CRAMRA_19880602","-", type, uID), out))
  
  out <- ifelse(stringr::str_detect(qID, "^Convention On The Protection And Use Of Transboundary Watercourses And International Lakes$"), paste0("ECE","_", uID),
                ifelse(stringr::str_detect(qID, "Convention On The Protection And Use Of Transboundary Watercourses And International Lakes"), paste0("ECE_19920317","-", type, uID), out))
  
  out <- ifelse(stringr::str_detect(qID, "^Convention On Long-Range Transboundary Air Pollution$"), paste0("LRTAP","_", uID),
                ifelse(stringr::str_detect(qID, "Convention On Long-Range Transboundary Air Pollution"), paste0("LRTAP_19791113","-", type, uID), out))
  
  out <- ifelse(stringr::str_detect(qID, "^International Convention For The Prevention Of Pollution From Ships$"), paste0("MARPOL","_", uID),
                ifelse(stringr::str_detect(qID, "International Convention For The Prevention Of Pollution From Ships"), paste0("MARPOL_19731102","-", type, uID), out))
  
  out <- ifelse(stringr::str_detect(qID, "^North American Agreement On Environmental Cooperation$"), paste0("NAAEC","_", uID),
                ifelse(stringr::str_detect(qID, "North American Agreement On Environmental Cooperation"), paste0("NAAEC_19930914","-", type, uID), out))
  
  out <- ifelse(stringr::str_detect(qID, "^Constitutional Agreement Of The Latin American Organization For Fisheries Development$"), paste0("OLDEPESCA","_", uID),
                ifelse(stringr::str_detect(qID, "Constitutional Agreement Of The Latin American Organization For Fisheries Development"), paste0("OLDEPESCA_19821029","-", type, uID), out))
  
  out <- ifelse(stringr::str_detect(qID, "^International Convention On Oil Pollution Preparedness, Response And Cooperation$"), paste0("OPRC","_", uID),
                ifelse(stringr::str_detect(qID, "International Convention On Oil Pollution Preparedness, Response And Cooperation"), paste0("OPRC_19901130","-", type, uID), out))
  
  out <- ifelse(stringr::str_detect(qID, "^Convention For The Protection Of The Marine Environment Of The North East Atlantic$"), paste0("OSPAR","_", uID),
                ifelse(stringr::str_detect(qID, "Convention For The Protection Of The Marine Environment Of The North East Atlantic"), paste0("OSPAR_19920922","-", type, uID), out))
  
  out <- ifelse(stringr::str_detect(qID, "^Paris Agreement Under The United Nations Framework Convention On Climate Change$"), paste0("PARIS","_", uID),
                ifelse(stringr::str_detect(qID, "Paris Agreement Under The United Nations Framework Convention On Climate Change"), paste0("PARIS_20151212","-", type, uID), out))
  
  out <- ifelse(stringr::str_detect(qID, "^Convention On The Prior Informed Consent Procedure For Certain Hazardous Chemicals And Pesticides In International Trade$"), paste0("PIC","_", uID),
                ifelse(stringr::str_detect(qID, "Convention On The Prior Informed Consent Procedure For Certain Hazardous Chemicals And Pesticides In International Trade"), paste0("PIC_19980910","-", type, uID), out))
  
  out <- ifelse(stringr::str_detect(qID, "^Convention On Wetlands Of International Importance Especially As Waterfowl Habitat$"), paste0("RAMSA","_", uID),
                ifelse(stringr::str_detect(qID, "Convention On Wetlands Of International Importance Especially As Waterfowl Habitat"), paste0("RAMSA_19710202","-", type, uID), out))
  
  out <- ifelse(stringr::str_detect(qID, "^Rio Declaration On Environment And Development$"), paste0("RIO","_", uID), out)
  
  out <- ifelse(stringr::str_detect(qID, "^Convention To Combat Desertification In Those Countries Experiencing Serious Drought And/Or Desertification, Particularly In Africa$"), paste0("UNCCD","_", uID),
                ifelse(stringr::str_detect(qID, "Convention To Combat Desertification In Those Countries Experiencing Serious Drought And/Or Desertification, Particularly In Africa"), paste0("UNCCD_19940617","-", type, uID), out))
  
  out <- ifelse(stringr::str_detect(qID, "^United Nations Framework Convention On Climate Change$"), paste0("UNFCCC","_", uID),
                ifelse(stringr::str_detect(qID, "United Nations Framework Convention On Climate Change"), paste0("UNFCCC_19920509","-", type, uID), out))
  
  out <- ifelse(stringr::str_detect(qID, "^Convention For The Protection Of The Ozone Layer$"), paste0("VIENNA","_", uID),
                ifelse(stringr::str_detect(qID, "Convention For The Protection Of The Ozone Layer"), paste0("VIENNA_19850322","-", type, uID), out))
  
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
#' @importFrom qStates code_states
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
    # When the title contains "Protocol amending..."
    grepl("^Protocol", x, ignore.case = T) ~ "P",
    # E stands for amendment
    grepl("amend|modify|extend|proces-verbal", x, ignore.case = T) ~ "E",
    # P stands for protocols
    grepl("protocol|additional|subsidiary|supplementary|complÃ©mentaire|complementar|complementario|annex |annexes ", x, ignore.case = T) ~ "P",
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

#' Code Agreement Topic
#'
#' Identify the main environmental issue the treaty is tackling.
#' @param x A character vector of treaty title
#' @importFrom stringr str_replace_na
#' @import dplyr
#' @return A character vector of the treaty topic abbreviation.
#' @export
code_topic <- function(x) {
  
  topic <- case_when(
    grepl("Waste|disposal|pollut|toxic|hazard", x, ignore.case = T) ~ "WAS",
    grepl("species|habitat|ecosystems|biological diversity|genetic resources|biosphere|birds", x, ignore.case = T) ~ "SPE",
    grepl("air|atmos|climate|outer space|ozone|emissions", x, ignore.case = T) ~ "AIR",
    grepl("water|freshwater|river|rhine|hydro|basin|drought|ocean|shelf", x, ignore.case = T) ~ "WAT",
    grepl("Soil|Wetland|Desert|Erosion|Land", x, ignore.case = F) ~ "SOI",
    grepl("nature|environment|biodiversity|flora|plant|fruit|vegetable|seed|forest|tree|conservation", x, ignore.case = T) ~ "BIO",
    grepl("fish|salmon|herring|tuna|aquaculture|mariculture|molluscs|whaling", x, ignore.case = T) ~ "FIS",
    grepl("agricultur|food|livestock|crop|irrigation|cattle|meat|farm|cultivate", x, ignore.case = T) ~ "AGR",
    grepl("culture|scien|techno|trade|research|exploration|navigation|data|information", x, ignore.case = T) ~ "BOT",
    grepl("energy|nuclear|oil|mining|gas|hydro|power", x, ignore.case = T) ~ "NUC",
    grepl("accidents", x, ignore.case = T) ~ "ACC",
    grepl("chemicals, pesticides|toxin", x, ignore.case = T) ~ "CHE",
    grepl("climate", x, ignore.case = T) ~ "CLC",
    grepl("noise", x, ignore.case = T) ~ "NOI",
    grepl("resource|resources|timber|antartic", x, ignore.case = T) ~ "RES",
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
  out <- gsub("\\<amendment\\>|\\<amendments\\>|\\<amending\\>|\\<modifying\\>|\\<modify\\>|\\<extension\\>|\\<extend\\>|\\<extending\\>|\\<verbal\\>|\\<protocol\\>|\\<protocol\\>|
              \\<additional\\>|\\<subsidiary\\>|\\<supplementary\\>|\\<complementary\\>|\\<complementario\\>|\\<agreement\\>|\\<agreements\\>|\\<arrangement\\>|\\<arrangements\\>|
              \\<accord\\>|\\<acuerdo\\>|\\<bilateral\\>|\\<technical\\>|\\<treaty\\>|\\<trait\\>|\\<tratado\\>|\\<convention\\>|\\<convencion\\>|\\<convenio\\>|\\<constitution\\>|
              \\<charte\\>|\\<instrument\\>|\\<statute\\>|\\<estatuto\\>|\\<provisional\\>|\\<understanding\\>|\\<provisions\\>|\\<relating\\>|\\<übereinkunft\\>|\\<Act\\>|\\<Acts\\>|
              \\<Declaration\\>|\\<Covenant\\>|\\<Scheme\\>|\\<Government Of |Law\\>|\\<Exchange\\>|\\<Letters\\>|\\<Letter\\>|\\<Notas\\>|\\<Notes\\>|\\<Memorandum\\>|\\<MemorÃ¡ndum\\>|
              \\<Principles of Conduct\\>|\\<Code of Conduct\\>|\\<Agreed Measures\\>|\\<Agreed Record\\>|\\<Consensus\\>|\\<Conclusions\\>|\\<Conclusion\\>|\\<Decision\\>|
              \\<Directive\\>|\\<Regulation\\>|\\<Reglamento\\>|\\<Resolution\\>|\\<Resolutions\\>|\\<Rule\\>|\\<Rules\\>|\\<Recommendation\\>|\\<Minute\\>|\\<Adjustment\\>|
              \\<First|Session Of\\>|\\<First Meeting Of\\>|\\<Commission\\>|\\<Committee\\>|\\<Center\\>|\\<Meeting\\>|\\<Meetings\\>|\\<Statement\\>|\\<Communiq\\>|\\<Comminiq\\>|
              \\<Joint Declaration\\>|\\<Proclamation\\>|\\<Administrative Order\\>|\\<Strategy\\>|\\<Plan\\>|\\<Program\\>|\\<Improvement\\>|\\<Project\\>|\\<Study\\>|\\<Article\\>|
              \\<Articles\\>|\\<Working Party\\>|\\<Working Group\\>|\\<Supplementary\\>|\\<supplementing\\>|\\<Annex\\>|\\<Annexes\\>|\\<extended\\>|\\<Constitutional\\>|
              \\<Constituent\\>", "", out, ignore.case = TRUE)
  out <- gsub("\\s*\\([^\\)]+\\)", "", out, ignore.case = FALSE)
  out <- gsub("-", "", out, ignore.case = FALSE)
  out <- stringr::str_replace_all(out, ",|-", "")
  out <- stringr::str_replace_all(out, " [:digit:]{1} | [:digit:]{2} ", "")
  out <- gsub(" A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y | Z", "", out, ignore.case = FALSE)
  out <- gsub("\\<and\\>|\\<the\\>|\\<of\\>|\\<for\\>|\\<to\\>|\\<in\\>|\\<a\\>|\\<an\\>|\\<on\\>|\\<the\\>|\\<as\\>", "", out, ignore.case = TRUE)
  out <- trimws(out)
  out <- stringr::str_squish(out)
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
  
  type <- case_when(
    # When the title contains "Protocol amending..."
    grepl("^Protocol", out, ignore.case = T) ~ "P",
    # E stands for amendment
    grepl("amend|modify|extend|proces-verbal", out, ignore.case = T) ~ "E",
    # P stands for protocols
    grepl("protocol|subsidiary|supplementary|complÃ©mentaire|complementar|complementario|annex |annexes ", out, ignore.case = T) ~ "P",
    # A stands for agreements
    grepl("agreement|arrangement|accord|acuerdo|bilateral co|technical co|treat|trait|tratado|convention|convencion|convenio|constitution|charte|instrument|statute|estatuto|provisional understanding|provisions relating|übereinkunft", out, ignore.case = T) ~ "A",
    grepl("Act|Declaration|Covenant|Scheme|Government Of|Law", out, ignore.case = T) ~ "A",
    # X stands for exchanges of notes
    grepl("Exchange|Letters|Notas", out, ignore.case = T) ~ "X",
    # Y stands for memorandum of understanding
    grepl("Memorandum|MemorÃ¡ndum|Principles of Conduct|Code of Conduct", out, ignore.case = T) ~ "Y",
    # W stands for resolutions
    grepl("Agreed Measures|Agreed Record|Consensus|Conclusions|Decision|Directive|Regulation|Reglamento|Resolution|Rules|Recommendation", out, ignore.case = T) ~ "W",
    # Q stands for minutes
    grepl("Minute|Adjustment|First Session Of|First Meeting Of|Commission|Committee|Center", out, ignore.case = T) ~ "Q",
    # V stands for declarations
    grepl("Statement|Communiq|Comminiq|Joint Declaration|Proclamation|Administrative Order", out, ignore.case = T) ~ "V",
    # S stands for strategies
    grepl("Strategy|Plan|Program|Improvement|Project|Study|Working Party|Working Group", out, ignore.case = T) ~ "S",
  )
  
  
  # Step two: find duplicates
  dup <- duplicated(out)
  id <- date
  id <- stringr::str_remove_all(id, "-")
  # id <- paste0(type, id)
  # id <- stringr::str_remove_all(id, "^[:digit:]{2}")
  # id <- stringr::str_remove_all(id, "[:digit:]{2}$")
  id <- as.numeric(id)
  out <- cbind(out, dup, id)
  
  # Step two: alternative way to find fuzzy duplicates
  # match <- out %>% 
  #   tidy_comb_all(out) %>% 
  #   tidy_stringdist() %>% 
  #   
  #   match <- match %>% 
  #   dplyr::filter(lv <= 20)
  
  # a <- out %>% 
  #   dplyr::filter(stringr::str_detect(out, "verbal|protocol|additional|subsidiary|supplementary|
  #                                      complÃ©mentaire|complementar|complementario|annex |annexes|Verbal|
  #                                      Protocol|Additional|Subsidiary|Supplementary|ComplÃ©mentaire|
  #                                      Complementar|Complementario|Annex |Annexes"))
  # 
  # b <- out %>% 
  #   dplyr::filter(!stringr::str_detect(out, "verbal|protocol|additional|subsidiary|supplementary|
  #                                      complÃ©mentaire|complementar|complementario|annex |annexes|Verbal|
  #                                      Protocol|Additional|Subsidiary|Supplementary|ComplÃ©mentaire|
  #                                      Complementar|Complementario|Annex |Annexes"))
  # 
  # out <- fuzzyjoin::stringdist_inner_join(a, b, by = "out", method = "lv", max_dist = 12, distance_col = "dist")
  

  # Step three: make sure duplicates have the same ID number
  out <- out %>% 
    group_by_at(vars(out)) %>% 
    mutate(
      dup = row_number() > 1,
      ref = ifelse(dup, paste0(first(id)), as.character(id)))
  
  out <- out %>% group_by(ref) %>% mutate(n = n()) %>% mutate(line = case_when(
    n != 1 ~ paste(ref), 
    n == 1 ~ "1"
  ))
  
  line <- out$line
  
  line <- stringr::str_remove_all(line, "^1$")
  
  line
  
}  
