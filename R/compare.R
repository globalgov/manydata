#' Compare 'many' databases and datasets
#'
#' @description
#' The `compare_` functions in `{manydata}` allows users to quickly compare
#' different information on databases and/or datasets across 'many packages'.
#' These include comparison for:
#' - data summaries
#' - missing observations
#' - categories
#' Each of these functions is accompanied by an appropriate printing method that
#' allows users to visualize the comparisons.
#' @name compare
#' @param database A database from one of the many packages.
#' @param dataset A dataset in a database from one of the many packages.
#' NULL by default.
#' That is, all datasets in the database are used.
#' A list of 2 or more datasets present in the database.
#' @param key A variable key to join datasets.
#' 'manyID' by default.
#' @param variable Would you like to focus on one, or more, specific variables
#' present in one or more datasets in the 'many' database?
#' By default "all".
#' For multiple variables, please declare variable names as a vector.
#' @param category Would you like to focus on one specific code category?
#' By default "all" are returned.
#' Other options include "confirmed", "unique", "missing", "conflict",
#' or "majority".
#' For multiple variables, please declare categories as a vector.
#' @importFrom dplyr as_tibble %>%
#' @examples
#' compare_data(emperors)
#' compare_data(database = emperors, dataset = c("wikipedia", "UNRV"))
#' compare_overlap(emperors, key = "ID")
#' plot_overlap(emperors, key = "ID")
#' compare_missing(emperors)
#' plot_missing(emperors)
#' compare_categories(emperors, key = "ID")
#' compare_categories(database = emperors, dataset = c("wikipedia", "UNRV"),
#' key = "ID", variable = c("Beg", "End"), category = c("conflict", "unique"))
#' plot_categories(emperors, key = "ID")
#' plot_categories(database = emperors, key = "ID", variable = c("Beg", "End"),
#' category = c("conflict", "unique"))
#' @return
#' The compare functions return tibbles with the respective comparative
#' information, or the same information plotted appropriately.
NULL

#' @describeIn compare Compare data summaries for 'many' data
#' @details `compare_data()` compares the number of observations, variables,
#' the earliest and latest date in each dataset in a 'many' database.
#' @importFrom purrr map
#' @export
compare_data <- function(database, dataset = NULL) {
  if (!is.null(dataset)) {
    if (length(dataset) < 2) stop("Please declare 2 or more datasets for comparison.")
    database <- database[grepl(paste(dataset, collapse = "|"), names(database))]
  }
  names <- data.frame(Dataset = names(database))
  out <- do.call(rbind, lapply(database, function(x) {
    Observations <- nrow(x)
    Variables <- ncol(x)
    Earliest_Date <- suppressWarnings(min(unlist(purrr::map(x, function(y) {
      ifelse(grepl("date", class(y), ignore.case = TRUE),
             min(y, na.rm = TRUE), NA)
    })), na.rm = TRUE))
    Latest_Date <- suppressWarnings(max(unlist(purrr::map(x, function(y) {
      ifelse(grepl("date", class(y), ignore.case = TRUE),
             max(y, na.rm = TRUE), NA)
    })), na.rm = TRUE))
    cbind(Observations, Variables, Earliest_Date, Latest_Date)
  }))
  out <- cbind(names, out)
  dplyr::as_tibble(out)
}

#' @describeIn compare Compare the overlap between datasets in 'many' databases
#' @details `compare_overlap()` compares the overlap between "key" observations
#' in each dataset in a 'many' database.
#' "manyID" by default.
#' @importFrom dplyr select rename
#' @importFrom ggVennDiagram Venn process_data venn_region
#' @export
compare_overlap <- function(database, dataset = NULL, key = "manyID") {
  name <- NULL
  if (!is.null(dataset)) {
    if (length(dataset) < 2) stop("Please declare 2 or more datasets for comparison.")
    database <- database[grepl(paste(dataset, collapse = "|"), names(database))]
  }
  out <- purrr::map(database, key)
  out <- ggVennDiagram::Venn(out)
  out <- ggVennDiagram::process_data(out)
  out <- ggVennDiagram::venn_region(out)
  dplyr::as_tibble(out) %>%
    dplyr::select(name, count) %>% 
    dplyr::rename(Dataset = name, 'Overlapping Observations' = count)
}

#' @describeIn compare Plots the overlap between datasets in 'many' databases
#' @details `plot_overlap()` plots the overlap between "key" observations
#' in each dataset in a 'many' database.
#' @importFrom ggVennDiagram ggVennDiagram
#' @export
plot_overlap <- function(database, dataset = NULL, key = "manyID") {
  if (!is.null(dataset)) {
    if (length(dataset) < 2) stop("Please declare 2 or more datasets for comparison.")
    database <- database[grepl(paste(dataset, collapse = "|"), names(database))]
  }
  out <- purrr::map(database, key)
  ggVennDiagram(out)
}

#' @describeIn compare Compare missing observations for 'many' data
#' @details `compare_missing()` compares the missing observations for variables
#' in each dataset in a 'many' database.
#' @importFrom purrr map
#' @importFrom dplyr arrange
#' @export
compare_missing <- function(database, dataset = NULL, variable = "all") {
  # Avoid notes
  Dataset <- Count <- Variable <- Missing <- 'Percent Missing' <- NULL
  # Select datasets if declared
  if (!is.null(dataset)) {
    if (length(dataset) < 2) stop("Please declare 2 or more datasets for comparison.")
    database <- database[grepl(paste(dataset, collapse = "|"), names(database))]
  }
  all_variables <- unique(unname(unlist(purrr::map(database, names))))
  # Select variables if declared
  if (variable[1] == "all") {
    # remove other ID variables and text variables
    ID_var <- grep("^ID$|ID$", all_variables, value = TRUE)
    all_variables <- all_variables[!all_variables %in% ID_var]
    # remove text variables
    all_variables <- grep("text", all_variables,
                          ignore.case = TRUE, value = TRUE, invert = TRUE)
  } else {
    all_variables <- all_variables[all_variables %in% variable]
  }
  # Report on datasets in database
  out <- purrr::map(database, extract_if_present, all_variables)
  for (n in names(out)) out[[n]]['Dataset'] = n
  out <- do.call(rbind, lapply(out, function(x) {
    varnames <- names(x)
    dataset <- x[['Dataset']][1]
    datatype <- unlist(lapply(x, class))
    counts   <- unlist(lapply(x, length))
    mvalues    <- unlist(lapply(x, function(z) sum(is.na(z))))
    mvaluesper <- round((mvalues / counts) * 100, 2)
    cbind(Variable = varnames, Dataset = dataset, Class = datatype,
          Count = counts, Missing = mvalues, 'Percent Missing' = mvaluesper)
  }))
  dplyr::as_tibble(out) %>%
    dplyr::mutate(Count = as.numeric(Count),
                  Missing = as.numeric(Missing),
                  'Percent Missing' = as.numeric(`Percent Missing`)) %>%
    dplyr::arrange(Variable)
}

#' @describeIn compare Plots missing observations for 'many' data
#' @details `plot_missing()` plots the missing observations for variables
#' in each dataset in a 'many' database.
#' @import ggplot2
#' @export
#' @export
plot_missing <- function(database, dataset = NULL, variable = "all") {
  'Percent Missing' <- Variable <- Dataset <- NULL
  out <- compare_missing(database = database, dataset = dataset,
                         variable = variable)
  # Plot missing proportions for variables
  ggplot(out, aes(reorder(Dataset, `Percent Missing`, decreasing = TRUE),
                  reorder(Variable, `Percent Missing`))) +
    geom_tile(aes(fill = `Percent Missing`)) +
    scale_fill_gradient(low = "darkgreen", high = "red3", na.value = NA,
                        name = "Proportion\nof missing\nobservations") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "Database", y = "Variable")
}

#' @describeIn compare Compare categories in 'many' databases
#' @details Confirmed values are the same in all datasets in database.
#' Unique values appear once in datasets in database.
#' Missing values are missing in all datasets in database.
#' Conflict values are different in the same number of datasets in database.
#' Majority values have the same value in multiple, but not all,
#' datasets in database.
#' @importFrom dplyr full_join filter_all %>% all_of group_by distinct any_vars
#' starts_with mutate
#' @importFrom purrr reduce map
#' @importFrom tibble tibble
#' @importFrom tidyr drop_na
#' @importFrom stringr str_count str_remove_all str_split str_extract_all
#' str_replace_all
#' @export
compare_categories <- function(database,
                               dataset = NULL,
                               key = "manyID",
                               variable = "all",
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
  # Step 3: get datasets if declared
  if (length(dataset) > 1) {
    database <- database[grepl(paste(dataset, collapse = "|"), names(database))]
  }
  # Step 4: inform users about duplicates
  if (length(key) == 1) {
    db_size <- sum(duplicated(unname(unlist(purrr::map(database, key)))))
    cat("There were", db_size,
        "matched observations by", key,
        "variable across datasets in database.")
  }
  # Step 5: get variable(s) of interest if declared
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
  # Step 6: reduce and join data
  out <- purrr::map(out, tidyr::drop_na, dplyr::all_of(key)) %>%
    purrr::reduce(dplyr::full_join, by = key)
  # create an empty data frame in case there is multiple variables
  db <- data.frame(out[, c(key)], stringsAsFactors = TRUE)
  # Step 7: code variables
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
  # Step 8: filter categories if necessary
  . <- NULL
  if (any(category != "all")) {
    db <- dplyr::filter_all(db,
                            dplyr::any_vars(grepl(paste(category,
                                                        collapse = "|"), .)))
  }
  db
}

#' @describeIn compare Plots categories in 'many' databases
#' @details Confirmed values are the same in all datasets in database.
#' Unique values appear once in datasets in database.
#' Missing values are missing in all datasets in database.
#' Conflict values are different in the same number of datasets in database.
#' Majority values have the same value in multiple, but not all,
#' datasets in database.
#' @importFrom purrr map
#' @importFrom dplyr summarise group_by mutate select %>% filter all_of
#' @importFrom tidyr pivot_longer pivot_wider replace_na fill
#' @importFrom stats reorder
#' @import ggplot2
#' @export
plot_categories <- function(database,
                               dataset = NULL,
                               key = "manyID",
                               variable = "all",
                               category = "all") {
  Category <- Variable <- Percentage <- Missing <- NULL # to avoid notes
  # Step 1: compare_categories(), get variable names, and code observations
  db <- compare_categories(database = database, dataset = dataset,
                           key = key, variable = variable, category = category)
  # Step 2: remove extra variable level information
  db <- db[!grepl("\\$", names(db))]
  # Step 3: gather and reshape the data
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
                        values_to = "Percentage") %>%
    dplyr::mutate(Category = factor(Category, levels = c("missing",
                                                         "conflict",
                                                         "unique",
                                                         "majority",
                                                         "confirmed")),
                  Missing = ifelse(Category == "missing",
                                   Percentage, NA_character_)) %>%
    tidyr::fill(Missing, .direction = "downup") %>%
    dplyr::filter(Percentage != 0)
  # Step 4: set colors and plot
  cols <- c(confirmed = "deepskyblue3", majority = "aquamarine3",
            unique = "khaki", conflict = "firebrick", missing = "grey90")
  # Step 5: plot
  ggplot2::ggplot(dbgather, aes(fill = Category, y = Percentage,
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
         subtitle = paste0("Based on ", nrow(db),
                           " consolidated observations."),
         caption = "In between the parenthesis are the number of datasets in which variable is present.",
         x = "Variable")
}
