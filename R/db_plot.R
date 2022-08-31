#' Plot database profile
#'
#' @param database A many database.
#' @param key A variable key to join datasets by, "manyID" by default.
#' @return A plot with the dataset profile.
#' @details The plot returns the percentage of confirmed, unique, missing,
#' conflicting, or majority values in all (non-ID) variables in the datasets
#' in a 'many' package database.
#' Confirmed values are the same in all datasets in database.
#' Unique values appear once in datasets in database.
#' Missing values are missing in all datasets in database.
#' Conflicting values are different in the same number of datasets in database.
#' majority values have the same value in multiple, but not all,
#' datasets in database.
#' @importFrom purrr map
#' @importFrom dplyr summarise group_by mutate rename select %>%
#' @importFrom tidyr pivot_longer pivot_wider replace_na fill
#' @importFrom stats reorder
#' @import ggplot2
#' @examples
#' dbplot(database = emperors, key = "ID")
#' #dbplot(database = manyenviron::agreements)
#' @export
dbplot <- function(database, key = "manyID") {
  # todo: make function more concise and efficient by re-working
  # how string matching and database gathering work.
  # Step 1: run dbcomp() to check key, get variable names, and code observations
  db <- dbcomp(database = database, key = key)
  # remove extra variable level information
  db <- db[!grepl(paste(names(database), collapse = "|"), names(db))]
  # Step 2: gather and reshape data
  Category <- Variable <- Percentage <- Missing <- name <- value <- NULL
  dbgather <- db %>%
    dplyr::select(-key) %>% 
    tidyr::pivot_longer(cols = everything(), names_to = "Variable",
                        values_to = "Category") %>% 
    dplyr::group_by(Variable, Category) %>%
    dplyr::summarise(count = n(), .groups = ) %>%
    dplyr::mutate(Percentage = count / sum(count)) %>% 
    tidyr::pivot_wider(id_cols = Variable, names_from = Category,
                       values_from = Percentage) %>% 
    dplyr::mutate(across(everything(), ~tidyr::replace_na(.x, 0))) %>%
    tidyr::pivot_longer(-Variable) %>% 
    dplyr::rename(Category = name, Percentage = value) %>%
    dplyr::mutate(Category = factor(Category, levels = c("missing", "conflict",
                                                         "unique", "majority",
                                                         "confirmed"))) %>%
    dplyr::mutate(Missing = ifelse(Category == "missing",
                                   Percentage, NA_character_)) %>%
    dplyr::group_by(Variable) %>%
    tidyr::fill(Missing, .direction = "downup") %>% 
    ungroup() %>%
    dplyr::filter(Percentage != 0)
  # Step 3: plot
  cols <- c(confirmed = 'deepskyblue3', majority = 'aquamarine3',
            unique = 'khaki', conflict = 'firebrick', missing = 'grey90')
  ggplot(dbgather, aes(fill = Category, y = Percentage,
                       x = stats::reorder(Variable, as.numeric(Missing)))) + 
    geom_bar(position = "fill", stat = "identity") +
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    scale_y_reverse(labels = function(x) {
      ifelse(x == 1|x == 0.5, paste0(x*100, "%", "\n(", x*nrow(db), " obs)"),
             paste0(x*100, "%"))
    }) +
    scale_fill_manual(values = cols) +
    theme_minimal() +
    labs(title = deparse(substitute(database)),
         subtitle = paste0("Based on ", nrow(db), " consolidated observations."),
         caption = "In between the parenthesis are the number of datasets in which variable is present.",
         x = "Variable")
}

#' Compare variable(s) in a 'many' database
#'
#' @param database A many database.
#' @param key A variable key to join datasets by, "manyID" by default.
#' @param variable Would you like to focus on one, or more, specific variables?
#' By default "all".
#' For one variable, please declare the variable name (i.e. "variable").
#' For multiple variables, please declare variable names as a list
#' (i.e. c("variable1", "variable2")).
#' @param category Would you like to focus on one specific code category?
#' By default "all" are returned.
#' Other options include "confirmed", "unique", "missing", "conflicting",
#' or "majority".
#' @details The tibble returns the values for all observations
#' in the variable(s) selected from a 'many' package databasethat belong to all,
#' or a specific, code category.
#' Code categories include:
#' Confirmed values are the same in all datasets in database.
#' Unique values appear once in datasets in database.
#' Missing values are missing in all datasets in database.
#' Conflicting values are different in the same number of datasets in database.
#' majority values have the same value in multiple, but not all,
#' datasets in database.
#' @importFrom dplyr full_join filter_all
#' @importFrom purrr reduce map
#' @importFrom tibble tibble
#' @importFrom stringr str_count str_remove_all str_split str_extract_all
#' str_replace_all
#' @return A tibble with the varaible(s) profile.
#' @examples
#' dbcomp(database = emperors, key = "ID")
#' dbcomp(database = emperors, key = "ID", variable = "Beg")
#' dbcomp(database = emperors, key = "ID", variable = c("Beg", "End"),
#' category = "conflict")
#' #dbcomp(database = manyenviron::agreements, variable = c("Title", "Beg"),
#' #category = "conflict")
#' @export
dbcomp <- function(database, key = "manyID", variable = "all", category = "all") {
  # Step 1: reduce data
  if(length(grepl(key, purrr::map(database, names))) != length(database)) {
    stop("Please declare a key variable present in all datasets in the database.")
  }
  out <- purrr::reduce(database, dplyr::full_join, by = key)
  cat("There were", sum(duplicated(unname(unlist(purrr::map(database, key))))),
      "matched observations by", key, "variable across datasets in database.")
  # get variable(s) of interest if declared
  all_variables <- unname(unlist(purrr::map(database, names)))
  if (variable[1] == "all") {
    all_variables <- all_variables[!all_variables %in% key]
    # remove other ID variables
    ID_var <- grep("^ID$|ID$", all_variables, value = TRUE)
    all_variables <- all_variables[!all_variables %in% ID_var]
  } else {
    all_variables <- all_variables[all_variables %in% variable]
  }
  # create an empty data frame in case there is multiple variables
  db <- data.frame(out[,1], stringsAsFactors = TRUE)
  # Step 2: code variables
  for (var in all_variables) {
    vvars <- paste0("^", var, "$|^", var, "\\.")
    vars_to_combine <- grepl(vvars, names(out))
    vlb <- out[vars_to_combine]
    # fix bug here
    col <- purrr::map(database, var)
    col <- names(col[!sapply(col, is.null)])
    colnames(vlb) <- paste0(col, "$", var)
    if (length(vlb) > 1) {
      # check if variable is "mdate" and lower precision for matching if needed
      if (unname(unlist(purrr::map(vlb[1], class))) == "mdate" &
          min(unlist(lapply(vlb, function(x) min(nchar(x), na.rm = TRUE)))) < 6) {
        vl <- lapply(vlb, function(x)
          stringr::str_extract_all(x, "^-[:digit:]{4}|^[:digit:]{4}"))
        vl <- stringr::str_replace_all(do.call(paste, vl), " ", "!")
      } else {
        # paste variables to work at the string value
        vl <- apply(vlb, 1, paste, collapse = "!") 
      }
      # Bug: weird code added to some variables? Might need fixing in messydates
      vl <- stringr::str_remove_all(vl, "\032")
      # remove string duplicates and collapse unique values (except NAs)
      # todo: is unique() slow for lists?
      value <- unlist(lapply(stringr::str_split(vl, "!"), function(x) {
        paste(unique(trimws(x), incomparables = "NA"), collapse = '!')
      }))
      # missing
      value <- ifelse(stringr::str_count(value, "NA") ==
                        length(out[vars_to_combine]), "missing", value)
      # unique
      value <- ifelse(stringr::str_count(value, "NA") ==
                        (length(out[vars_to_combine]) - 1), "unique", value)
      # confirmed (by all datasets in database)
      value <- ifelse(stringr::str_count(value, "\\!") == 0 &
                        !grepl("^missing$|^unique$", value),
                      "confirmed", value)
      # confirmed (by multiple datasets in database, other values are NAs)
      valuec <- lapply(stringr::str_split(value, "!"), function(x) {
        x[!grepl("^NA$", x) & x != ""]
      })
      value <- ifelse(lengths(valuec) == 1 &
                        !grepl("^missing$|^unique$|^confirmed$", valuec),
                      "confirmed", value)
      # conflict (when there are no duplicates, apart from NAs possibly)
      value <- ifelse(!grepl("^missing$|^unique$|^confirmed$", value) &
                        stringr::str_count(value, "\\!") ==
                        (length(out[vars_to_combine]) - 1),
                      "conflict", value)
      # open (and close) the values to find if majority or conflict
      vl <- lapply(stringr::str_split(vl, "!"), function(x) {
        x <- x[!grepl("^NA$", x) & x != ""]
        x <- unique(rle(x)$lengths)
        x
      })
      # conflict (an even number of non-NA conflicting obs)
      value <- ifelse(!grepl("^missing$|^unique$|^confirmed$|^conflict$", value) &
                        lengths(vl) == 1, "conflict", value)
      # majority (an unbalanced number of non-NA matching obs)
      value <- ifelse(!grepl("^missing$|^unique$|^confirmed$|^conflict$", value),
                      "majority", value)
    } else {
      value <- unname(unlist(out[vars_to_combine]))
      value <- ifelse(is.na(value), "missing", "unique")
    }
    # fill data frame with variable and presence in datasets
    db <- cbind(db, vlb)
    db[, paste0(var, " (", length(vlb), ")")] <- value
  }
  db <- tibble::tibble(db[unique(colnames(db))])
  # Step 3: filter categories if necessary
  . <- NULL
  if (category != "all") {
    db <- dplyr::filter_all(db, any_vars(grepl(category, .)))
  }
  db
}
