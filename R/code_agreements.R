#' Code agreements title
#' 
#' Creates an ID column that contains information on the
#' parties, the type of agreement, the date and the relations to other
#' agreements in the dataset. 
#' @param title title column variable
#' @param date date column variable
#' @param dataset name of the dataset, optional
#' @import usethis
#' @import stringr 
#' @examples
#' \dontrun{
#' IEADB$qID <- code_agreements(IEADB$Title, IEADB$Signature)
#' }
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
  
  # Step three: code agreement topic and area
  # Categories and key words still need some adjustements
  # This does not appear on qID but information is extracted for
  # future usage.
  topic <- code_topic(qID)
  area <- code_areas(qID)
  
  # Step four: code agreement type 
  # For known agreements abbreviations are also assigned
  type <- code_type(qID)
  abbrev <- code_known_agreements(qID)
  
  #step five: give the observation a unique ID by dates
  uID <- code_dates(date)
  
  # step six: detect treaties from the same 'family'
  line <- code_linkage(qID, date)
  
  # Step seven: add items together correctly
  # The following coding assumes that any other types than A (= Agreement) are linked to another treaty.
  out <- ifelse((!is.na(abbrev) & (type == "A")), paste0(abbrev, "_", uID, type),
                (ifelse((!is.na(abbrev) & (type != "A")), paste0(abbrev, "_", uID, type, "_", line),
                        (ifelse((is.na(parties) & (type == "A")), paste0(uID, type),
                                (ifelse((is.na(parties) & (type != "A")), paste0(uID, type,"_", line),
                                        (ifelse((!is.na(parties) & (type == "A") & (stringr::str_detect(parties, "^[:alpha:]{3}-[:alpha:]{3}$"))), paste0(uID, type, "_", parties),
                                                (ifelse((!is.na(parties) & (type == "A") & (stringr::str_detect(parties, "^[:alpha:]{2}-[:alpha:]{3}$"))), paste0(uID, type, "_", parties),
                                                        (ifelse((!is.na(parties) & (type == "A") & (!stringr::str_detect(parties, "^[:alpha:]{3}-[:alpha:]{3}$"))), paste0(uID, type),
                                                                (ifelse((!is.na(parties) & (type != "A") & (stringr::str_detect(parties, "^[:alpha:]{3}-[:alpha:]{3}$"))), paste0(uID, type, "_", parties, "_", line),
                                                                        (ifelse((!is.na(parties) & (type != "A") & (stringr::str_detect(parties, "^[:alpha:]{2}-[:alpha:]{3}$"))), paste0(uID, type, "_", parties, "_", line),
                                                                                (ifelse((!is.na(parties) & (type != "A") & (!stringr::str_detect(parties, "^[:alpha:]{3}-[:alpha:]{3}$"))), paste0(uID, type, "_", line), NA)))))))))))))))))))
  
  out <- stringr::str_replace_all(out, "NA_", NA_character_)

  cat(sum(is.na(out)), "entries were not matched at all.\n")
  # cat(sum(stringr::str_detect(out, "^[0-9]")), " entries were only coded by date.\n")
  cat("There were", sum(duplicated(out, incomparables = NA)), "duplicated IDs.\n")
  
  qID <- out
  
  usethis::ui_done("Please run `vignette('agreements')` for more information.")
  
  # Step eight: add new qID column to data if dataset argument is provided
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
#' @importFrom stringr str_replace_all
#' @return A character vector of parties that are mentioned in the treaty title
#' @examples
#' \dontrun{
#' IEADB$parties <- code_parties(IEADB$Title)
#' }
#' @export
code_parties <- function(x) {
  parties <- qStates::code_states(x)
  parties <- stringr::str_replace_all(parties, "_", "-")
  # Some agreements are made between unions of countries and others,
  # but are still considered bilateral. In these cases, abbreviations
  # will have 2 letters instead of 3.
  unions <- case_when(grepl("European Community", x, ignore.case = T) ~ "EC",
                     grepl("European Union", x, ignore.case = T) ~ "EU",
                     grepl("African Union", x, ignore.case = T) ~ "AU")
  # unions <- stringr::str_replace_na(unions, "O")
  parties <- ifelse(!is.na(unions), paste0(unions, "-", parties), parties)
  parties[!grepl("-", parties)] <- NA
  parties
}

#' Code Agreement Type
#'
#' Identify the type of international agreement.
#' @param x A character vector of treaty title
#' @return A character vector of the type of treaty
#' @import stringr
#' @importFrom dplyr case_when
#' @examples
#' \dontrun{
#' IEADB$type <- code_type(IEADB$Title)
#' }
#' @export
code_type <- function(x) {
  
  type <- case_when(
    # When the title contains "Protocol amending..."
    grepl("^Protocol", x, ignore.case = T) ~ "P",
    # E stands for amendment
    grepl("amend|modify|extend|proces-verbal", x, ignore.case = T) ~ "E",
    # P stands for protocols
    grepl("protocol|additional|subsidiary|supplementary|complÃ©mentaire|
          complementar|complementario|annex |annexes ", x, ignore.case = T) ~ "P",
    # Added annex in this category
    # A stands for agreements
    grepl("agreement|arrangement|accord|acuerdo|bilateral co|technical co|treat|trait|tratado|convention|convencion|
          convenio|constitution|charte|instrument|statute|estatuto|provisional understanding|
          provisions relating|übereinkunft", x, ignore.case = T) ~ "A",
    grepl("Act|Declaration|Covenant|Scheme|Government Of|Law", x, ignore.case = T) ~ "A",
    # X stands for exchanges of notes
    grepl("Exchange|Letters|Notas", x, ignore.case = T) ~ "X",
    # Y stands for memorandum of understanding
    grepl("Memorandum|MemorÃ¡ndum|Principles of Conduct|Code of Conduct", x, ignore.case = T) ~ "Y",
    # W stands for resolutions
    grepl("Agreed Measures|Agreed Record|Consensus|Conclusions|Decision|Directive|Regulation|
          Reglamento|Resolution|Rules|Recommendation", x, ignore.case = T) ~ "W",
    # Q stands for minutes
    grepl("Minute|Adjustment|First Session Of|First Meeting Of|Commission|Committee|Center", x, ignore.case = T) ~ "Q",
    # V stands for declarations
    grepl("Statement|Communiq|Comminiq|Joint Declaration|Proclamation|Administrative Order", x, ignore.case = T) ~ "V",
    # S stands for strategies
    grepl("Strategy|Plan|Program|Improvement|Project|Study|Working Party|Working Group", x, ignore.case = T) ~ "S",
  )
  
  # Extracts meaningful ordering numbers for protocols and amendments
  
  number <- x
  number <- gsub("\\<one\\>|\\<first\\>", "1", number)
  number <- gsub("\\<two\\>|\\<second\\>", "2", number)
  number <- gsub("\\<three\\>|\\<third\\>", "3", number)
  number <- gsub("\\<four\\>|\\<fourth\\>", "4", number)
  number <- gsub("\\<five\\>|\\<fifth\\>", "5", number)
  number <- gsub("\\<six\\>|\\<sixth\\>", "6", number)
  number <- gsub("\\<seven\\>|\\<seventh\\>", "7", number)
  number <- gsub("\\<eight\\>|\\<eighth\\>", "8", number)
  number <- gsub("\\<nine\\>|\\<ninth\\>", "9", number)
  number <- stringr::str_remove(number, "[:digit:]{2}\\s[:alpha:]{3}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{4}\\s[:digit:]{4}")
  number <- stringr::str_remove(number, "[:digit:]{2}\\s[:alpha:]{5}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{6}\\s[:digit:]{4}")
  number <- stringr::str_remove(number,  "[:digit:]{2}\\s[:alpha:]{7}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{8}\\s[:digit:]{4}")
  number <- stringr::str_remove(number, "[:digit:]{2}\\s[:alpha:]{9}\\s[:digit:]{4}| [:digit:]{1}\\s[:alpha:]{3}\\s[:digit:]{4}")
  number <- stringr::str_remove(number, "[:digit:]{1}\\s[:alpha:]{4}\\s[:digit:]{4}| [:digit:]{1}\\s[:alpha:]{5}\\s[:digit:]{4}")
  number <- stringr::str_remove(number, "[:digit:]{1}\\s[:alpha:]{6}\\s[:digit:]{4}| [:digit:]{1}\\s[:alpha:]{7}\\s[:digit:]{4}")
  number <- stringr::str_remove(number, "[:digit:]{1}\\s[:alpha:]{8}\\s[:digit:]{4}| [:digit:]{1}\\s[:alpha:]{9}\\s[:digit:]{4}")
  number <- stringr::str_remove(number, "[:digit:]{4}")
  
  number <- ifelse(stringr::str_detect(number, "[:digit:]{1}|[:digit:]{2}"), stringr::str_extract(number, "[:digit:]{1}|[:digit:]{2}"), "")
  
  # When no type is found
  type <- stringr::str_replace_na(type, "O")
  
  type <- paste0(type, number)
  
  type
}

#' Cretes Unique ID numbers from dates
#'
#' Agreements should have a unique identification number that is meaningful,
#' we condense their signature date to produce this number.
#' @param x A date variable
#' @return A character vector with condensed dates
#' @importFrom stringr str_remove_all
#' \dontrun{
#' IEADB$uID <- code_dates(IEADB$dates)
#' }
#' @export
code_dates <- function(x) {

  uID <- stringr::str_remove_all(x, "-")
  # For treaties without signature date
  uID[is.na(uID)] <- paste0("9999", sample(1000:9999, sum(is.na(uID)), replace = TRUE))
  uID

}

#' Known agreements abbreviation
#' 
#' Some agreements have known abbreviations that facilitate their identification.
#' @param x A character vector of treaty title
#' @return A character vector with abbreviation of known treaties
#' @importFrom dplyr case_when
#' \dontrun{
#' IEADB$abrevv <- code_known_agreements(IEADB$titles)
#' }
#' @export
code_known_agreements <- function(x){
  
  # Assign the specific abbreviation to the "known" treaties 
  abbrev <- case_when(
    grepl("United Nations Convention On The Law Of The Sea", x, ignore.case = T) ~ "UNCLOS",
    grepl("Convention On Biological Diversity", x, ignore.case = T) ~ "CBD",
    grepl("Convention On The Conservation Of Antarctic Marine Living Resources", x, ignore.case = T) ~ "CCAMLR",
    grepl("Convention On International Trade In Endangered Species Of Wild Fauna And Flora", x, ignore.case = T) ~ "CITES",
    grepl("International Convention On Civil Liability For Oil Pollution Damage", x, ignore.case = T) ~ "CLC",
    grepl("Antarctic Mineral Resources Convention", x, ignore.case = T) ~ "CRAMRA",
    grepl("Convention On The Protection And Use Of Transboundary Watercourses And International Lakes", x, ignore.case = T) ~ "CECE",
    grepl("Convention On Long-Range Transboundary Air Pollution", x, ignore.case = T) ~ "LRTAP",
    grepl("International Convention For The Prevention Of Pollution From Ships", x, ignore.case = T) ~ "MARPOL",
    grepl("North American Agreement On Environmental Cooperation", x, ignore.case = T) ~ "NAAEC",
    grepl("Constitutional Agreement Of The Latin American Organization For Fisheries Development", x, ignore.case = T) ~ "OLDEPESCA",
    grepl("International Convention On Oil Pollution Preparedness, Response And Cooperation", x, ignore.case = T) ~ "OPRC",
    grepl("Convention For The Protection Of The Marine Environment Of The North East Atlantic", x, ignore.case = T) ~ "OSPAR",
    grepl("Paris Agreement Under The United Nations Framework Convention On Climate Change", x, ignore.case = T) ~ "PARIS",
    grepl("Convention On The Prior Informed Consent Procedure For Certain Hazardous Chemicals And Pesticides In International Trade", x, ignore.case = T) ~ "PIC",
    grepl("Convention On Wetlands Of International Importance Especially As Waterfowl Habitat", x, ignore.case = T) ~ "RAMSA",
    grepl("Convention To Combat Desertification In Those Countries Experiencing Serious Drought And/Or Desertification, Particularly In Africa", x, ignore.case = T) ~ "UNCCD",
    grepl("United Nations Framework Convention On Climate Change", x, ignore.case = T) ~ "UNFCCC",
    grepl("Convention For The Protection Of The Ozone Layer", x, ignore.case = T) ~ "VIENNA",
    )
  
  abbrev
  
}

#' Code Agreement Topic
#'
#' Identify the main environmental issue the treaty is tackling.
#' @param x A character vector of treaty title
#' @importFrom stringr str_replace_na
#' @importFrom dplyr case_when
#' @return A character vector of the treaty topic abbreviation.
#' @examples
#' \dontrun{
#' IEADB$topic <- code_topic(IEADB$Title)
#' }
#' @export
code_topic <- function(x) {
  
  topic <- case_when(
    grepl("Waste|disposal|pollut|toxic|hazard", x, ignore.case = T) ~ "WAS",
    grepl("species|habitat|ecosystems|biological diversity|genetic resources|biosphere|birds|locusts", x, ignore.case = T) ~ "SPE",
    grepl("air|atmos|climate|outer space|ozone|emissions", x, ignore.case = T) ~ "AIR",
    grepl("water|freshwater|river|rhine|hydro|basin|drought|ocean|shelf|Atlantic|Lake", x, ignore.case = T) ~ "WAT",
    grepl("Soil|Wetland|Desert|Erosion|Land|Archipelago", x, ignore.case = F) ~ "SOI",
    grepl("nature|environment|biodiversity|flora|plant|fruit|vegetable|seed|forest|tree|conservation|preservation", x, ignore.case = T) ~ "BIO",
    grepl("fish|salmon|herring|tuna|aquaculture|mariculture|molluscs|whaling", x, ignore.case = T) ~ "FIS",
    grepl("agricultur|food|livestock|crop|irrigation|cattle|meat|farm|cultivate", x, ignore.case = T) ~ "AGR",
    grepl("culture|scien|techno|trade|research|exploration|navigation|data|information", x, ignore.case = T) ~ "BOT",
    grepl("energy|nuclear|oil|mining|gas|hydro|power", x, ignore.case = T) ~ "NUC",
    grepl("accidents", x, ignore.case = T) ~ "ACC",
    grepl("chemicals, pesticides|toxin|Lead", x, ignore.case = T) ~ "CHE",
    grepl("climate", x, ignore.case = T) ~ "CLC",
    grepl("noise", x, ignore.case = T) ~ "NOI",
    grepl("disease|diseases", x, ignore.case = T) ~ "DIS",
    grepl("resource|resources|timber|antartic|fur|Ivory|Horn", x, ignore.case = T) ~ "RES",
  ) 
  
  # If not topic is found, category will be "OTH"
  topic <- stringr::str_replace_na(topic, "OTH")
  
  topic
}

#' Code the Treaty Areas
#' 
#' Identify the areas the treaty title refers to.
#' @param x A character vector of treaty title
#' @return A character vector of the treaty area
#' @importFrom dplyr case_when
#' @importFrom stringr str_replace_na
#' @examples
#' \dontrun{
#' IEADB$area <- Code_areas(IEADB$Title)
#' }
#' @export
code_areas <- function(x){
  areas <- case_when(
    # Coding for region abbreviations
    grepl("Central America|Caribbean", x, ignore.case = T) ~ "CAM_",
    grepl("Latin America| South America", x, ignore.case = T) ~ "LA_",
    # grepl("North America", x, ignore.case = F) ~ "NA_",
    grepl("Near East|Middle East| Middle East and North Africa", x, ignore.case = T) ~ "MEA_",
    grepl("Oceania", x, ignore.case = T) ~ "OCE_",
    grepl("Eastern and Central Europe|European|Western Europe", x, ignore.case = T) ~ "WEU_",
    grepl("East Africa|Eastern Africa|Sub-Saharan Africa|Central Africa|Southern Africa", x, ignore.case = T) ~ "SSA_",
    grepl("Southern Hemisphere|South Hemisphere", x, ignore.case = T) ~ "SH_",
    grepl("Southeastern Asia|South Asia", x, ignore.case = T) ~ "SEA_",
    grepl("Central Asia", x, ignore.case = T) ~ "CAS_",
    grepl("asia pacific|asian pacific", x, ignore.case = T) ~ "AP_",
    grepl("Pacific Island", x, ignore.case = T) ~ "PI_",
    grepl("Antarctic", x, ignore.case = T) ~ "ANT_",
    grepl("Arctic", x, ignore.case = T) ~ "ARC_",
    # Coding for ocean abbreviations
    # grepl("Northwest Atlantic|Northeast Atlantic|North Atlantic", x, ignore.case = T) ~ "ONA_",
    grepl("Southeast Atlantic|South East Atlantic|South Atlantic|African Atlantic", x, ignore.case = T) ~ "OSA_",
    grepl("Eastern Pacific|Northeast Pacific|Western Central Pacific", x, ignore.case = T) ~ "OPAC_",
    grepl("South Pacific|Southern Pacific", x, ignore.case = T) ~ "OSP_",
  )
  
  # areas <- ifelse(stringr::str_detect(x, "Central America|Caribbean"), "CAM_",
  #                 ifelse(stringr::str_detect(x, "Northwest Atlantic|Northeast Atlantic|North Atlantic"), "ONA_",
  #                        ifelse(stringr::str_detect(x, "Southeast Atlantic|South East Atlantic|South Atlantic|African Atlantic"), "OSA_",
  #                               ifelse(stringr::str_detect(x, "Atlantic"), "OA_",NA))))
  #
  
  areas <- stringr::str_replace_na(areas, "")
  areas
}

#' Code Agreement Lineage
#' 
#' Identify the linkage between amendments and protocols to a main agreement.
#' @param x A character vector of treaty title
#' @param date A date variable
#' @import textclean
#' @import english
#' @import stringr
#' @import dplyr
#' @examples
#' \dontrun{
#' IEADB$line <- code_linkage(IEADB$Title)
#' }
#' @export
code_linkage <- function(x, date) {
  
  s <- x
  
  type <- code_type(s)
  
  cap <- function(x) paste(toupper(substring(x, 1, 1)), {
    x <- substring(x, 2)
  }
  , sep = "", collapse = " ")
  out <- sapply(strsplit(as.character(x), split = " "), cap, USE.NAMES = !is.null(names(x)))
  out <- trimws(out)
  # Step one: remove known words and articles
  out <- gsub("\\<amendment\\>|\\<amendments\\>|\\<amend\\>|\\<amending\\>|\\<modifying\\>|\\<modify\\>|\\<extension\\>|\\<extend\\>|\\<extending\\>|\\<verbal\\>|\\<protocol\\>|
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
  
  # Step two: find duplicates
  dup <- duplicated(out)
  id <- date
  id <- stringr::str_remove_all(id, "-")
  id <- paste0(type, id)
  out <- cbind(out, dup, id)
  
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
