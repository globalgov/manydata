#' #' Compare 'many' databases and datasets
#' #'
#' #' @description
#' #' The `compare_` functions in `{manydata}` allows users to quickly compare
#' #' different information on databases and/or datasets across 'many packages'.
#' #' These include comparison for: 
#' #' - data summaries
#' #' - missing observations
#' #' - categories
#' #' Each of these functions is accompanied by an appropriate printing method that
#' #' allows users to visualize the comparisons.
#' #' @name compare
#' #' @param database A database from one of the many packages.
#' #' @param dataset A dataset in a database from one of the many packages.
#' #' NULL by default.
#' #' That is, all datasets in the database are used.
#' #' A list of 2 or more datasets present in the database.
#' #' @importFrom dplyr as_tibble
#' #' @examples
#' #' compare_data(emperors)
#' #' compare_overlap(emperors, key = "ID")
#' #' plot(compare_overlap(emperors, key = "ID"))
#' #' compare_missing(emperors)
#' #' plot(compare_missing(emperors))
#' #' compare_categories(emperors)
#' #' plot(compare_categories(emperors))
#' #' @return
#' #' The compare functions return tibbles with the respective comparative
#' #' information, or the same information plotted appropriately.
#' NULL
#' 
#' #' @describeIn compare Compare data summaries for 'many' data
#' #' @details `compare_data()` compares the number of observations, variables,
#' #' the earliest and latest date in each dataset in a 'many' database.
#' #' @importFrom purrr map
#' #' @export
#' compare_data <- function(database,
#'                          dataset = NULL) UseMethod("compare_data")
#' 
#' #' @export
#' compare_data.default <- function(database, dataset = NULL) {
#'   if (!is.null(dataset)) {
#'     if (length(dataset) < 2) stop("Please declare 2 or more datasets for comparison.")
#'     database <- database[grepl(paste(dataset, collapse = "|"), names(database))]
#'   }
#'   names <- data.frame(Dataset = names(database))
#'   out <- do.call(rbind, lapply(database, function(x) {
#'     Observations <- nrow(x)
#'     Variables <- ncol(x)
#'     Earliest_Date <- suppressWarnings(min(unlist(purrr::map(x, function(y) {
#'       ifelse(grepl("date", class(y), ignore.case = TRUE),
#'              min(y, na.rm = TRUE), NA)
#'     })), na.rm = TRUE))
#'     Latest_Date <- suppressWarnings(max(unlist(purrr::map(x, function(y) {
#'       ifelse(grepl("date", class(y), ignore.case = TRUE),
#'              max(y, na.rm = TRUE), NA)
#'     })), na.rm = TRUE))
#'     cbind(Observations, Variables, Earliest_Date, Latest_Date)
#'   }))
#'   out <- cbind(names, out)
#'   dplyr::as_tibble(out)
#' }
#' 
#' #' @describeIn compare Compare the overlap between datasets in 'many' databases
#' #' @details `compare_overlap()` compares the overlap between "key" observations
#' #' in each dataset in a 'many' database.
#' #' @param key A variable key to join datasets.
#' #' "manyID" by default.
#' #' @importFrom ggVennDiagram ggVennDiagram
#' #' @export
#' compare_overlap <- function(database,
#'                             dataset = NULL,
#'                             key = "manyID") UseMethod("compare_overlap")
#' 
#' #' @export
#' compare_overlap.default <- function(database, dataset = NULL, key = "manyID") {
#'   if (!is.null(dataset)) {
#'     if (length(dataset) < 2) stop("Please declare 2 or more datasets for comparison.")
#'     database <- database[grepl(paste(dataset, collapse = "|"), names(database))]
#'   }
#'   for (n in names(database)) database[[n]]['Dataset'] = n
#'   out <- as.matrix(table(unlist(purrr::map(database, key)),
#'                          unlist(purrr::map(database, "Dataset"))))
#'   out[out > 1] <- 1
#'   out <- crossprod(out[,-1])
#'   dplyr::as_tibble(out, rownames = NA) %>% 
#'     tibble::rownames_to_column("Overlap")
#' }
#' 
#' #' @export
#' plot.compare_overlap <- function(database, dataset = NULL, key = "manyID") {
#'   if (!is.null(dataset)) {
#'     if (length(dataset) < 2) stop("Please declare 2 or more datasets for comparison.")
#'     database <- database[grepl(paste(dataset, collapse = "|"), names(database))]
#'   }
#'   out <- purrr::map(database, key)
#'   ggVennDiagram(out)
#' }
#' 
#' #' @describeIn compare Compare missing observations for 'many' data
#' #' @details `compare_missing()` compares the missing observations for variables
#' #' in each dataset in a 'many' database.
#' #' @param variable Would you like to focus on one, or more, specific variables
#' #' present in one or more datasets in the 'many' database?
#' #' By default "all".
#' #' For multiple variables, please declare variable names as a vector.
#' #' @importFrom purrr map
#' #' @importFrom ggplot2 ggplot geom_tile scale_fill_gradient theme_classic
#' #' theme labs
#' #' @export
#' compare_missing <- function(database,
#'                             dataset = NULL,
#'                             variable = "all") UseMethod("compare_missing")
#' 
#' #' @export
#' compare_missing.default <- function(database, dataset = NULL, variable = "all") {
#'   # Avoid notes
#'   Dataset <- MissingPer <- Variables <- NULL
#'   # Select datasets if declared
#'   if (!is.null(dataset)) {
#'     if (length(dataset) < 2) stop("Please declare 2 or more datasets for comparison.")
#'     database <- database[grepl(paste(dataset, collapse = "|"), names(database))]
#'   }
#'   all_variables <- unique(unname(unlist(purrr::map(database, names))))
#'   # Select variables if declared
#'   if (variable[1] == "all") {
#'     # remove other ID variables and text variables
#'     ID_var <- grep("^ID$|ID$", all_variables, value = TRUE)
#'     all_variables <- all_variables[!all_variables %in% ID_var]
#'     # remove text variables
#'     all_variables <- grep("text", all_variables,
#'                           ignore.case = TRUE, value = TRUE, invert = TRUE)
#'   } else {
#'     all_variables <- all_variables[all_variables %in% variable]
#'   }
#'   # Report on datasets in database
#'   out <- purrr::map(database, extract_if_present, all_variables)
#'   for (n in names(out)) out[[n]]['Dataset'] = n
#'   out <- do.call(rbind, lapply(out, function(x) {
#'     varnames <- names(x)
#'     dataset <- x[['Dataset']][1]
#'     datatype <- unlist(lapply(x, class))
#'     counts   <- unlist(lapply(x, length))
#'     mvalues    <- unlist(lapply(x, function(z) sum(is.na(z))))
#'     mvaluesper <- round((mvalues / counts) * 100, 2)
#'     cbind(Variable = varnames, Dataset = dataset, Class = datatype,
#'           Count = counts, Missing = mvalues, 'Missing Percentage' = mvaluesper)
#'   }))
#'   dplyr::as_tibble(out)
#' }
#' 
#' #' @export
#' plot.compare_missing <- function(database, dataset = NULL, variable = "all") {
#'   out <- compare_missing(database = database, dataset = dataset,
#'                          variable = variable)
#'   # Plot missing proportions for variables
#'   ggplot(out, aes(reorder(Dataset, 'Missing Percentage', decreasing = TRUE),
#'                   reorder(Variables, 'Missing Percentage'))) +
#'     geom_tile(aes(fill = 'Missing Percentage')) +
#'     scale_fill_gradient(low = "darkgreen", high = "red3", na.value = NA,
#'                         name = "Proportion\nof missing\nobservations") +
#'     theme_classic() +
#'     theme(axis.text.x = element_text(angle = 90)) +
#'     labs(x = "Database", y = "Variable")
#' }
#' 
#' #' @describeIn compare Compare categories in 'many' databases
#' #' @details Confirmed values are the same in all datasets in database.
#' #' Unique values appear once in datasets in database.
#' #' Missing values are missing in all datasets in database.
#' #' Conflict values are different in the same number of datasets in database.
#' #' Majority values have the same value in multiple, but not all,
#' #' datasets in database.
#' #' @param key A variable key to join datasets.
#' #' "manyID" by default.
#' #' @param variable Would you like to focus on one, or more, specific variables
#' #' present in one or more datasets in the 'many' database?
#' #' By default "all".
#' #' For multiple variables, please declare variable names as a vector.
#' #' @param category Would you like to focus on one specific code category?
#' #' By default "all" are returned.
#' #' Other options include "confirmed", "unique", "missing", "conflict",
#' #' or "majority".
#' #' For multiple variables, please declare categories as a vector.
#' #' @export
#' compare_categories <- function(database,
#'                                dataset = NULL,
#'                                key = "manyID",
#'                                variable = "all",
#'                                category = "all") UseMethod("compare_data")
#' 
#' #' @export
#' compare_categories.default <- function(database,
#'                                dataset = NULL,
#'                                key = "manyID",
#'                                variable = "all",
#'                                category = "all") {
#'   
#' }
#' 
#' #' @export
#' plot.compare_categories <- function(database,
#'                                dataset = NULL,
#'                                key = "manyID",
#'                                variable = "all",
#'                                category = "all") {
#'   
#' }
