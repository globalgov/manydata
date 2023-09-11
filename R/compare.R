#' Compare dimensions for 'many' data
#'
#' @details `compare_dimensions()` compares the number of observations,
#' variables, the earliest date, and the latest date in all observations
#' for datasets in a 'many' datacube.
#' @family compare_
#' @param datacube A datacube from one of the many packages.
#' @param dataset A dataset in a datacube from one of the many packages.
#' By default, "all".
#' That is, all datasets in the datacube are used.
#' To select two or more datasets, please declare them as a vector.
#' @import messydates
#' @importFrom purrr map
#' @importFrom dplyr as_tibble mutate
#' @examples
#' \donttest{
#' compare_dimensions(emperors)
#' }
#' @return
#' `compare_dimensions()` returns a tibble with information about each dataset
#' including the number of observations, the number of variables,
#' the earliest date, and the latest date in all observations.
#' @export
compare_dimensions <- function(datacube, dataset = "all") {
  Earliest_Date <- Latest_Date <- NULL
  if (any(dataset != "all")) {
    if (length(dataset) < 2) stop("Please declare 2 or more datasets for comparison.")
    datacube <- datacube[grepl(paste(dataset, collapse = "|"), names(datacube))]
  }
  names <- data.frame(Dataset = names(datacube))
  out <- do.call(rbind, lapply(datacube, function(x) {
    Observations <- nrow(x)
    Variables <- paste(names(x), collapse = ", ")
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
  dplyr::as_tibble(cbind(names, out)) %>%
    dplyr::mutate(Earliest_Date = messydates::as_messydate(Earliest_Date),
                  Latest_Date = messydates::as_messydate(Latest_Date))
}

#' Compare ranges of variables in 'many' data
#'
#' @details `compare_ranges()` compares the number of observations, variables,
#' the earliest and latest date in each dataset in a 'many' datacube.
#' @family compare_
#' @param datacube A datacube from one of the many packages.
#' @param dataset A dataset in a datacube from one of the many packages.
#' By default, "all".
#' That is, all datasets in the datacube are used.
#' To select two or more datasets, please declare them as a vector.
#' @param variable Please declare a variable present in one or more
#' datasets in the 'many' datacube.
#' For multiple variables, please declare variable names as a vector.
#' @import messydates
#' @importFrom purrr map
#' @importFrom dplyr as_tibble
#' @importFrom stats median
#' @examples
#' \donttest{
#' compare_ranges(emperors, variable = c("Begin", "End"))
#' }
#' @return
#' `compare_ranges()` returns a tibble with information about the minimal,
#' maximal, average, and median values for selected variables in datacubes.
#' @export
compare_ranges <- function(datacube, dataset = "all", variable) {
  if (any(dataset != "all")) {
    if (length(dataset) < 2) stop("Please declare 2 or more datasets for comparison.")
    datacube <- datacube[grepl(paste(dataset, collapse = "|"), names(datacube))]
  }
  if (missing(variable)) stop("Please declare one or more variables.")
  datacube <- lapply(datacube, function(x) {
    x[grepl(paste(variable, collapse = "|"), names(x))]
  })
  names <- data.frame(
    Dataset = unlist(lapply(names(datacube), function(x) {
    rep(x, length(variable))
  })))
  out <- suppressWarnings(do.call(rbind, lapply(datacube, function(x) {
    Variable <- names(x)
    Min <- unlist(lapply(x, function(y) {
      ifelse(grepl("date", class(y), ignore.case = TRUE),
             as.character(as.Date(messydates::as_messydate(y), min)),
             as.character(min(y, na.rm = TRUE)))
    }))
    Max <- unlist(lapply(x, function(y) {
      ifelse(grepl("date", class(y), ignore.case = TRUE),
             as.character(as.Date(messydates::as_messydate(y), max)),
             as.character(max(y, na.rm = TRUE)))
    }))
    Mean <- unlist(lapply(x, function(y) {
      ifelse(grepl("date", class(y), ignore.case = TRUE),
             as.character(as.Date(messydates::as_messydate(y), mean)),
             as.character(mean(y, na.rm = TRUE)))
    }))
    Median <- unlist(lapply(x, function(y) {
      ifelse(grepl("date", class(y), ignore.case = TRUE),
             as.character(as.Date(messydates::as_messydate(y), median)),
             as.character(stats::median(y, na.rm = TRUE)))
    }))
    data.frame(cbind(Variable, Min, Max, Mean, Median))
  })))
  dplyr::as_tibble(cbind(names, out))
}

#' Compare the overlap between datasets in 'many' datacubes
#' 
#' @details `compare_overlap()` compares the overlap between "key" observations
#' in each dataset in a 'many' datacube.
#' @family compare_
#' @param datacube A datacube from one of the many packages.
#' @param dataset A dataset in a datacube from one of the many packages.
#' By default "all".
#' That is, all datasets in the datacube are used.
#' To select two or more datasets, please declare them as a vector.
#' @param key A variable key to join datasets.
#' 'manyID' by default.
#' @importFrom dplyr select rename_with as_tibble
#' @importFrom purrr map
#' @examples
#' \donttest{
#' compare_overlap(emperors, key = "ID")
#' plot(compare_overlap(emperors, key = "ID"))
#' }
#' @return
#' `compare_overlap()` returns a tibble with information about each dataset
#' and the number of overlapping observations.
#' @export
compare_overlap <- function(datacube, dataset = "all", key = "manyID") {
  name <- db_name <- NULL
  thisRequires("ggVennDiagram")
  if (any(dataset != "all")) {
    if (length(dataset) < 2) stop("Please declare 2 or more datasets for comparison.")
    datacube <- datacube[grepl(paste(dataset, collapse = "|"), names(datacube))]
  }
  db_name <- deparse(substitute(datacube))
  out <- purrr::map(datacube, key)
  out <- ggVennDiagram::Venn(out)
  out <- ggVennDiagram::process_data(out)
  out <- ggVennDiagram::venn_region(out)
  out <- dplyr::as_tibble(out) %>%
    dplyr::select(name, count) %>%
    dplyr::rename_with(.fn = ~paste0("Datasets from ", db_name),
                       .cols = name) %>%
    dplyr::rename_with(.fn = ~paste0("Overlapping Observations by ", key),
                       .cols = count)
  class(out) <- c("compare_overlap", "tbl_df", "tbl", "data.frame")
  out
}

#' @importFrom stringr word
#' @importFrom purrr map
#' @export
plot.compare_overlap <- function(x, ...) {
  thisRequires("ggVennDiagram")
  datacube <- get(stringr::word(names(x[1]),-1))
  dataset <- grep("\\.\\.", unique(unname(unlist(x[,1]))),
                  value = TRUE, invert = TRUE)
  key <- stringr::word(names(x[2]),-1)
  if (length(names(datacube)) != length(dataset)) {
    datacube <- datacube[grepl(paste(dataset, collapse = "|"), names(datacube))]
  }
  out <- purrr::map(datacube, key)
  ggVennDiagram::ggVennDiagram(out)
}

#' Compare missing observations for 'many' data
#' 
#' @details `compare_missing()` compares the missing observations for variables
#' in each dataset in a 'many' datacube.
#' @family compare_
#' @importFrom purrr map
#' @importFrom dplyr arrange mutate as_tibble
#' @param datacube A datacube from one of the many packages.
#' @param dataset A dataset in a datacube from one of the many packages.
#' NULL by default.
#' That is, all datasets in the datacube are used.
#' To select two or more datasets, please declare them as a vector.
#' @param variable Would you like to focus on one, or more, specific variables
#' present in one or more datasets in the 'many' datacube?
#' By default "all".
#' For multiple variables, please declare variable names as a vector.
#' @examples
#' \donttest{
#' compare_missing(emperors)
#' plot(compare_missing(emperors))
#' }
#' @return
#' `compare_missing()` returns a tibble with information about each dataset
#' including the number of observations, the number of variables,
#' the earliest date, and the latest date in all observations.
#' @export
compare_missing <- function(datacube, dataset = "all", variable = "all") {
  # Avoid notes
  Dataset <- Count <- Variable <- Missing <- 'Percent Missing' <- NULL
  # Select datasets if declared
  if (any(dataset != "all")) {
    if (length(dataset) < 2) stop("Please declare 2 or more datasets for comparison.")
    datacube <- datacube[grepl(paste(dataset, collapse = "|"), names(datacube))]
  }
  all_variables <- unique(unname(unlist(purrr::map(datacube, names))))
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
  # Report on datasets in datacube
  out <- purrr::map(datacube, extract_if_present, all_variables)
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
  out <- dplyr::as_tibble(out) %>%
    dplyr::mutate(Count = as.numeric(Count),
                  Missing = as.numeric(Missing),
                  'Percent Missing' = as.numeric(`Percent Missing`)) %>%
    dplyr::arrange(Variable)
  class(out) <- c("compare_missing", "tbl_df", "tbl", "data.frame")
  out
}

#' @import ggplot2
#' @export
plot.compare_missing <- function(x, ...) {
  'Percent Missing' <- Variable <- Dataset <- NULL
  # Plot missing proportions for variables
  ggplot(x, aes(reorder(Dataset, `Percent Missing`, decreasing = TRUE),
                  reorder(Variable, `Percent Missing`))) +
    geom_tile(aes(fill = `Percent Missing`)) +
    scale_fill_gradient(low = "darkgreen", high = "red3", na.value = NA,
                        name = "Proportion\nof missing\nobservations") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "datacube", y = "Variable")
}

#' Compare categories in 'many' datacubes
#' 
#' @details Confirmed values are the same in all datasets in datacube.
#' Unique values appear once in datasets in datacube.
#' Missing values are missing in all datasets in datacube.
#' Conflict values are different in the same number of datasets in datacube.
#' Majority values have the same value in multiple, but not all,
#' datasets in datacube.
#' @family compare_
#' @param datacube A datacube from one of the many packages.
#' @param dataset A dataset in a datacube from one of the many packages.
#' By default "all".
#' That is, all datasets in the datacube are used.
#' To select two or more datasets, please declare them as a vector.
#' @param key A variable key to join datasets.
#' 'manyID' by default.
#' @param variable Would you like to focus on one, or more, specific variables
#' present in one or more datasets in the 'many' datacube?
#' By default "all".
#' For multiple variables, please declare variable names as a vector.
#' @param category Would you like to focus on one specific code category?
#' By default "all" are returned.
#' Other options include "confirmed", "unique", "missing", "conflict",
#' or "majority".
#' For multiple variables, please declare categories as a vector.
#' @importFrom dplyr full_join filter_all %>% all_of group_by distinct any_vars
#' starts_with mutate tibble
#' @importFrom purrr reduce map
#' @importFrom tidyr drop_na
#' @importFrom stringr str_count str_remove_all str_split str_extract_all
#' str_replace_all
#' @examples
#' \donttest{
#' compare_categories(emperors, key = "ID")
#' compare_categories(datacube = emperors, dataset = c("wikipedia", "UNRV"),
#' key = "ID", variable = c("Beg", "End"), category = c("conflict", "unique"))
#' plot(compare_categories(emperors, key = "ID"))
#' plot(compare_categories(datacube = emperors, dataset = c("wikipedia", "UNRV"),
#' key = "ID", variable = c("Beg", "End"), category = c("conflict", "unique")))
#' }
#' @export
compare_categories <- function(datacube,
                               dataset = "all",
                               key = "manyID",
                               variable = "all",
                               category = "all") {
  # Step 1: get variables of interest
  if (length(grepl(key[1], purrr::map(datacube, names))) != length(datacube)) {
    stop("Please declare a key variable present in all datasets in the datacube.")
  }
  # Step 2: check if multiple keys for memberships' datacubes
  if (grepl("membership", deparse(substitute(datacube)), ignore.case = TRUE) &
      length(key) == 1) {
    stop("For memberships datacube please indicate two keys
         (e.g. key = c('manyID', 'CountryID'))")
  }
  # Step 3: get datasets if declared
  if (any(dataset != "all")) {
    datacube <- datacube[grepl(paste(dataset, collapse = "|"), names(datacube))]
  }
  # Step 4: inform users about duplicates
  if (length(key) == 1) {
    db_size <- sum(duplicated(unname(unlist(purrr::map(datacube, key)))))
    cat("There were", db_size,
        "matched observations by", key,
        "variable across datasets in datacube.")
  }
  # Step 5: get variable(s) of interest if declared
  all_variables <- unique(unname(unlist(purrr::map(datacube, names))))
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
  out <- purrr::map(datacube, extract_if_present, c(key, all_variables))
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
    col <- purrr::map(datacube, var)
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
      # confirmed (by all datasets in datacube)
      value <- ifelse(stringr::str_count(value, "\\!") == 0 &
                        !grepl("^missing$|^unique$", value),
                      "confirmed", value)
      # confirmed (by multiple datasets in datacube, other values are NAs)
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
  db <- dplyr::tibble(db[unique(colnames(db))]) %>%
    select(-dplyr::starts_with("dplyr"))
  # Step 8: filter categories if necessary
  . <- NULL
  if (any(category != "all")) {
    db <- dplyr::filter_all(db,
                            dplyr::any_vars(grepl(paste(category,
                                                        collapse = "|"), .)))
  }
  class(db) <- c("compare_categories", "tbl_df", "tbl", "data.frame")
  db
}

#' @importFrom purrr map
#' @importFrom dplyr summarise group_by mutate select %>% filter
#' @importFrom tidyr pivot_longer pivot_wider replace_na fill
#' @importFrom stats reorder
#' @import ggplot2
#' @export
plot.compare_categories <- function(x, ...) {
  Category <- Variable <- Percentage <- Missing <- NULL # to avoid notes
  # Step 1: remove extra variable level information
  db <- x[!grepl("\\$", names(x))]
  
  # Step 3: gather and reshape the data
  dbgather <- db[, -1] %>%
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
    labs(title = deparse(substitute(datacube)),
         subtitle = paste0("Based on ", nrow(db),
                           " consolidated observations."),
         caption = "In between the parenthesis are the number of datasets in which variable is present.",
         x = "Variable")
}

#' #' Compare similarities and differences in treaties from 'many' datasets
#' #' 
#' #' @details Certain datasets, or consolidated datacubes, in 'many' packages
#' #' contains information on treaties which can be compared with 
#' #' `compare_treaties()`.
#' #' @family compare_
#' #' @param dataset A dataset in a datacube from one of the many packages.
#' #' @param text_variable Text variable.
#' #' @param comparison Would you like to compare similarities or differences
#' #' between treaties?
#' #' If not specified, defaults to "similarities".
#' #' Alternatively, users can also get the "differences".
#' #' @param method A method for checking similarities or differences.
#' #' If chosen comparison are similarities, compares "correlation" between
#' #' treaty texts if not specified.
#' #' Other similarity methods from `quanteda.textstats::textstat_simil()`
#' #' include "cosine", "jaccard", "ejaccard", "dice", "edice",
#' #' "simple matching", and "hamann".
#' #' If chosen comparison are differences, compares "euclidean" difference
#' #' between treaty texts if not specified.
#' #' Other difference methods from `quanteda.textstats::textstat_dist()` include
#' #' "manhattan", "maximum", "canberra", and "minkowski".
#' #' @examples
#' #' @return
#' #' `compare_treaties()` returns a matrix with the similarity or difference
#' #' scores between all the agreements.
#' #' @export
#' compare_treaties <- function(dataset, text_variable,
#'                              comparison, method) {
#'   thisRequires("quanteda")
#'   thisRequires("quanteda.textstats")
#'   out <- quanteda::dfm(dataset[,variable])
#'   if (missing(comparison) | comparison == "similarities") {
#'     if(missing(method)) {
#'       method = "correlation"
#'     }
#'     quanteda.textstats::textstat_simil(dfm, method = method)
#'   } else {
#'       if(missing(method)) {
#'         method = "euclidean"
#'       }
#'       quanteda.textstats::textstat_dist(dfm, method = method)
#'   }
#'   #todo: add ploting function that plots treaties as a dendogram
#' }
