#' @name db_profile
#' @title Database profile functions
#' @description Database profiling functions that returns confirmed, unique,
#' missing, conflicting, or majority values in all (non-ID) variables
#' in the datasets in a 'many' package database.
#' @param database A many database.
#' @param key A variable key to join datasets by, "manyID" by default.
#' @param variable Would you like to focus on one, or more, specific variables?
#' By default "all".
#' For multiple variables, please declare variable names as a vector.
#' @param category Would you like to focus on one specific code category?
#' By default "all" are returned.
#' Other options include "confirmed", "unique", "missing", "conflicting",
#' or "majority".
#' For multiple variables, please declare categories as a vector.
#' @details Confirmed values are the same in all datasets in database.
#' Unique values appear once in datasets in database.
#' Missing values are missing in all datasets in database.
#' Conflicting values are different in the same number of datasets in database.
#' Majority values have the same value in multiple, but not all,
#' datasets in database.
#' @return A plot, or a tibble, with the profile of the variables across all
#' datasets in a "many" database.
#' For multiple categories across multiple variables,
#' the functions return all rows that contain at least one of the selected
#' variables coded as one of the categories.
NULL

#' @name db_profile
#' @details `db_plot()` plots the database profile.
#' @importFrom purrr map
#' @importFrom dplyr summarise group_by mutate select %>% filter all_of
#' @importFrom tidyr pivot_longer pivot_wider replace_na fill
#' @importFrom stats reorder
#' @import ggplot2
#' @examples
#' \donttest{
#' db_plot(database = emperors, key = "ID")
#' db_plot(database = emperors, key = "ID", variable = c("Beg", "End"))
#' db_plot(database = emperors, key = "ID", variable = c("Beg", "End"),
#' category = c("conflict", "unique"))
#' }
#' @export
db_plot <- function(database, key = "manyID", variable = "all",
                    category = "all") {
  # Step 1: run dbcomp() to check key, get variable names, and code observations
  db <- db_comp(database = database, key = key, variable = variable,
                category = category)
  # remove extra variable level information
  db <- db[!grepl("\\$", names(db))]
  # differentiate conflicts, for internal use and if available...
  if (any(grepl('Checked_HUGGO', names(db)))) {
    ch <- data.frame(db[grep("Checked_HUGGO", names(db))])
    as.vector(ch)
    for (v in names(db)) {
      db[v] <- ifelse(db[v] == "conflict" & ch == "unique",
                      "recognised conflict",
                  ifelse(db[v] == "conflict" & ch == "missing",
                         "open conflict",
                         ifelse(db[v] == "confirmed", "confirmed",
                                ifelse(db[v] == "unique", "unique",
                                       ifelse(db[v] == "majority", "majority",
                                              ifelse(db[v] == "missing",
                                                     "missing", ""))))))
    }
  }
  # Step 2: gather and reshape data
  Category <- Variable <- Percentage <- Missing <- NULL
  dbgather <- db %>%
    dplyr::select(-all_of(key)) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "Variable",
                        values_to = "Category") %>%
    dplyr::group_by(Variable, Category) %>%
    dplyr::summarise(count = n(), .groups = ) %>%
    dplyr::mutate(Percentage = count / sum(count)) %>%
    tidyr::pivot_wider(id_cols = Variable, names_from = Category,
                       values_from = Percentage) %>%
    dplyr::mutate(across(everything(), ~tidyr::replace_na(.x, 0))) %>%
    tidyr::pivot_longer(-Variable, names_to = "Category",
                        values_to = "Percentage")
  if (any(grepl('Checked_HUGGO', dbgather["Variable"]))) {
    dbgather <- dbgather %>%
      dplyr::mutate(Category = factor(Category, levels = c("missing", "open conflict",
                                                           "recognised conflict",
                                                           "unique", "majority",
                                                           "confirmed")),
                    Missing = ifelse(Category == "missing",
                                     Percentage, NA_character_)) %>%
      tidyr::fill(Missing, .direction = "downup") %>%
      dplyr::filter(Percentage != 0)
    # Step 3: set colors and plot
    cols <- c(confirmed = "deepskyblue3", majority = "aquamarine3",
              unique = "khaki", 'open conflict' = "orange3",
              'recognised conflict' = "rosybrown", missing = "grey90")
  } else {
    dbgather <- dbgather %>%
      dplyr::mutate(Category = factor(Category, levels = c("missing", "conflict",
                                                           "unique", "majority",
                                                           "confirmed")),
                    Missing = ifelse(Category == "missing",
                                     Percentage, NA_character_)) %>%
      tidyr::fill(Missing, .direction = "downup") %>%
      dplyr::filter(Percentage != 0)
    # Step 3: set colors and plot
    cols <- c(confirmed = "deepskyblue3", majority = "aquamarine3",
              unique = "khaki", conflict = "firebrick", missing = "grey90")
  }
  ggplot(dbgather, aes(fill = Category, y = Percentage,
                       x = stats::reorder(Variable, as.numeric(Missing)))) +
    geom_bar(position = "fill", stat = "identity") +
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    scale_y_reverse(labels = function(x) {
      ifelse(x == 1 | x == 0.5,
             paste0(x * 100, "%", "\n(", x * nrow(db), " obs)"),
             paste0(x * 100, "%"))
    }) +
    scale_fill_manual(values = cols) +
    geom_text(aes(label = paste0(round(Percentage*100, digits = 1),"%")), 
              position = position_stack(vjust = 0.5),
              size = 2, color = "white", angle = 90) +
    theme_minimal() +
    labs(title = deparse(substitute(database)),
         subtitle = paste0("Based on ", nrow(db), " consolidated observations."),
         caption = "In between the parenthesis are the number of datasets in which variable is present.",
         x = "Variable")
}

#' @name db_profile
#' @details `db_comp()` creates a tibble comparing the variables in a database.
#' @importFrom dplyr full_join filter_all %>% all_of group_by distinct any_vars
#' starts_with mutate
#' @importFrom purrr reduce map
#' @importFrom tibble tibble
#' @importFrom tidyr drop_na
#' @importFrom stringr str_count str_remove_all str_split str_extract_all
#' str_replace_all
#' @examples
#' \donttest{
#' db_comp(database = emperors, key = "ID")
#' db_comp(database = emperors, key = "ID", variable = "Beg")
#' db_comp(database = emperors, key = "ID", variable = c("Beg", "End"),
#' category = "conflict")
#' db_comp(database = emperors, key = "ID", variable = c("Beg", "End"),
#' category = c("conflict", "unique"))
#' }
#' @export
db_comp <- function(database, key = "manyID", variable = "all",
                    category = "all") {
  # Step 1: get variables of interest
  if (length(grepl(key[1], purrr::map(database, names))) != length(database)) {
    stop("Please declare a key variable present in all datasets in the database.")
  }
  # Step 2: check if multiple keys for memberships' databases
  if (grepl("membership", deparse(substitute(database)), ignore.case = TRUE) &
      length(key) == 1) {
    stop("For memberships database please indicate two keys
         (e.g. key = c('manyID', 'CountryID'))")
  }
  # Step 3: inform users about duplicates
  if (length(key) == 1) {
    db_size <- sum(duplicated(unname(unlist(purrr::map(database, key)))))
    cat("There were", db_size,
        "matched observations by", key,
        "variable across datasets in database.")
  }
  # Step 4: get variable(s) of interest if declared
  all_variables <- unique(unname(unlist(purrr::map(database, names))))
  if (variable[1] == "all") {
    all_variables <- all_variables[!all_variables %in% key]
    # remove other ID variables and text variables
    ID_var <- grep("^ID$|ID$", all_variables, value = TRUE)
    all_variables <- all_variables[!all_variables %in% ID_var]
    # remove text variables
    all_variables <- grep("text", all_variables,
                          ignore.case = TRUE, value = TRUE, invert = TRUE)
  } else {
    all_variables <- all_variables[all_variables %in% variable]
  }
  out <- purrr::map(database, extract_if_present, c(key, all_variables))
  # Step 5: reduce and join data
  out <- purrr::map(out, tidyr::drop_na, dplyr::all_of(key)) %>%
    purrr::reduce(dplyr::full_join, by = key)
  # create an empty data frame in case there is multiple variables
  db <- data.frame(out[, c(key)], stringsAsFactors = TRUE)
  # Step 6: code variables
  for (var in all_variables) {
    vvars <- paste0("^", var, "$|^", var, "\\.")
    vars_to_combine <- grep(vvars, names(out), value = TRUE)
    vlb <- out[vars_to_combine]
    col <- purrr::map(database, var)
    col <- names(col[lengths(col) != 0])
    colnames(vlb) <- paste0(col, "$", var)
    value <- NULL
    if (length(vlb) > 1) {
      # remove string duplicates and collapse unique values (except NAs)
      value <- apply(vlb, 1, function(x) {
        paste(unique(trimws(x), incomparables = c("^NA$", NA_character_)),
              collapse = "!")
      })
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
      vl <- apply(vlb, 1, function(x) {
        x <- x[!grepl("^NA$", x) & x != ""]
        x <- unique(rle(x)$lengths)
        x
      })
      # conflict (an even number of non-NA conflicting obs)
      value <- ifelse(!grepl("^missing$|^unique$|^confirmed$|^conflict$",
                             value) & lengths(vl) == 1, "conflict", value)
      # majority (an unbalanced number of non-NA matching obs)
      value <- ifelse(!grepl("^missing$|^unique$|^confirmed$|^conflict$",
                             value), "majority", value)
    } else {
      value <- unname(unlist(out[vars_to_combine]))
      value <- ifelse(is.na(value), "missing", "unique")
    }
    # fill data frame with variable and presence in datasets
    db <- cbind(db, vlb)
    if (length(value) == nrow(db)) {
      db[, paste0(var, " (", length(vlb), ")")] <- value
    }
  }
  db <- tibble::tibble(db[unique(colnames(db))]) %>%
    select(-dplyr::starts_with("dplyr"))
  # Step 7: filter categories if necessary
  . <- NULL
  if (any(category != "all")) {
    db <- dplyr::filter_all(db,
                            dplyr::any_vars(grepl(paste(category,
                                                        collapse = "|"), .)))
  }
  db
}
