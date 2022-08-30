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
#' @importFrom dplyr full_join summarise group_by mutate rename select %>%
#' @importFrom stringr str_count str_remove_all str_split
#' @importFrom tidyr pivot_longer pivot_wider replace_na fill
#' @importFrom stats reorder aggregate
#' @importFrom purrr reduce map
#' @import ggplot2
#' @examples
#' dbplot(database = emperors, key = "ID")
#' #dbplot(database = manyenviron::agreements)
#' #dbplot(database = manytrade::memberships)
#' @export
dbplot <- function(database, key = "manyID") {
  # todo: make function more concise and efficient by re-working
  # how string matching and database gathering work.
  if(length(grepl(key, purrr::map(database, names))) != length(database)) {
    stop("Please declare a key variable present in all datasets in the database.")
  }
  # Step 1: reduce database by key and get names
  out <- purrr::reduce(database, dplyr::full_join, by = key)
  # get the number of pairwise overlap between datasets by key variable
  cat("There were", sum(duplicated(unname(unlist(purrr::map(database, key))))),
      "matched observations by", key, "variable across datasets in database.
      For a more granular variable comparison across datasets,
      please see the `dbcomp()` function.")
  # get variable names, but key
  all_variables <- unname(unlist(purrr::map(database, names)))
  all_variables <- all_variables[!all_variables %in% key]
  # remove other ID variables
  ID_var <- grep("^ID|ID$", all_variables, value = TRUE)
  all_variables <- all_variables[!all_variables %in% ID_var]
  # create an empty data frame
  db <- data.frame(out[,1], stringsAsFactors = TRUE)
  # Step 2: check if missing, confirmed, conflict, majority, or unique
  for (var in all_variables) {
    vvars <- paste0("^", var, "$|^", var, "\\.")
    vars_to_combine <- grepl(vvars, names(out))
    vl <- out[vars_to_combine]
    if (length(vl) > 1) {
      # check if variable is "mdate" and lower precision for matching if needed
      if (unname(unlist(purrr::map(vl[1], class))) == "mdate" &
          min(unlist(lapply(vl, function(x) min(nchar(x), na.rm = TRUE)))) < 6) {
        vl <- lapply(vl, function(x)
          stringr::str_extract_all(x, "^-[:digit:]{4}|^[:digit:]{4}"))
        vl <- stringr::str_replace_all(do.call(paste, vl), " ", "!")
        } else {
          # paste variables to work at the string value
          vl <- apply(vl, 1, paste, collapse = "!") 
        }
      # weird code added to some variables? need to double check messydates...
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
        value <- out[vars_to_combine]
        value <- ifelse(is.na(value), "missing", "unique")
        }
    # fill data frame with variable and presence in datasets
    db[, paste0(var, " (", length(out[vars_to_combine]), ")")] <- value
  }
  # Step 3: gather and reshape data
  Category <- Variable <- Percentage <- Missing <- name <- NULL
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
  cols <- c(confirmed = 'deepskyblue3', majority = 'aquamarine3',
            unique = 'khaki', conflict = 'firebrick', missing = 'grey90')
  ggplot(dbgather, aes(fill = Category, y = Percentage,
                       x = stats::reorder(Variable, as.numeric(Missing)))) + 
    geom_bar(position = "fill", stat = "identity") +
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    scale_y_reverse(labels = function(x) {
      ifelse(x == 1|x == 0.5, paste0(x*100, "%", "\n(", x*nrow(out), " obs)"),
             paste0(x*100, "%"))
    }) +
    scale_fill_manual(values = cols) +
    theme_minimal() +
    labs(title = deparse(substitute(database)),
         subtitle = paste0("Based on ", nrow(out), " consolidated observations."),
         caption = "In between the parenthesis are the number of datasets in which variable is present.",
         x = "Variable")
}

#' Compare variable(s) in a 'many' database
#'
#' @param database A many database.
#' @param variable One, or more, variables present in multiple datasets
#' in a many database.
#' For multiple variables, please declare as a list
#' (i.e. c("variable1", "variable2"))
#' @param key A variable key to join datasets by, "manyID" by default.
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
#' @importFrom purrr reduce
#' @importFrom tibble tibble
#' @importFrom stringr str_count str_remove_all str_split
#' @return A tibble with the varaible(s) profile.
#' @examples
#' dbcomp(database = emperors, variable = "Beg", key = "ID")
#' dbcomp(database = emperors, variable = c("Beg", "End"), key = "ID",
#' category = "conflict")
#' #dbcomp(database = manyenviron::agreements, variable = "Title",
#' #category = "conflict")
#' @export
dbcomp <- function(database, variable, key = "manyID", category = "all") {
  # Step 1: reduce data
  out <- purrr::reduce(database, dplyr::full_join, by = key)
  # get variable(s) of interest
  all_variables <- unname(unlist(purrr::map(database, names)))
  all_variables <- all_variables[all_variables %in% variable]
  # create an empty data frame in case there is multiple variables
  db <- data.frame(out[,1], stringsAsFactors = TRUE)
  # Step 2: code variables
  db <- data.frame(out[,1], stringsAsFactors = TRUE)
  for (var in all_variables) {
    vvars <- paste0("^", var, "$|^", var, "\\.")
    vars_to_combine <- grepl(vvars, names(out))
    vlb <- out[vars_to_combine]
    colnames(vlb) <- paste0(names(database), "$", var)
    if (length(vlb) < 1) {
      stop("Please select a variable present in at least 2 datasets")
    } else {
      vl <- apply(vlb, 1, paste, collapse = "!") 
      # weird code added to some variables? need to double check messydates...
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
    }
    db <- cbind(db, vlb)
    db[, var] <- value
  }
  db <- tibble::tibble(db[unique(colnames(db))])
  # Step 3: filter categories if necessary
  . <- NULL
  if (category != "all") {
    db <- dplyr::filter_all(db, any_vars(grepl(category, .)))
  }
  db
}
