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
#' @importFrom stringi stri_list2matrix
#' @importFrom plotly ggplotly layout
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
      "matched observations by", key, "variable across datasets in database.")
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
    if (length(out[vars_to_combine]) > 1) {
      # paste variables to work at the string value
      vl <- apply(out[vars_to_combine], 1, paste, collapse = "!")
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
      # todo: what to do with similar dates with different levels of precision?
      # For now, these are treated as different.
      } else {
        value <- out[vars_to_combine]
        value <- ifelse(is.na(value), "missing", "unique")
    }
    # fill dataframe with variable and presence in datasets
    db[, var] <- value
  }
  # Step 3: gather and reshape data
  Category <- Variable <- Percentage <- Missing <- Dataset <- name <- NULL
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
  # Step 4: add variables' source dataset
  varsc <- lapply(database, names)
  varso <- as.data.frame(stringi::stri_list2matrix(varsc))
  colnames(varso) <- names(varsc)
  varso <- na.omit(tidyr::pivot_longer(varso, cols = everything(),
                                       names_to = "Dataset",
                                       values_to = "Variable"))
  varso <- stats::aggregate(Dataset ~ Variable, unique(varso), paste,
                            collapse = ", ")
  dbgather <- dplyr::left_join(dbgather, varso, by = "Variable")
  # Step 4: Plot
  cols <- c(confirmed = 'deepskyblue3', majority = 'aquamarine3',
            unique = 'khaki', conflict = 'firebrick', missing = 'grey90')
  dbplot <- ggplot(dbgather, aes(fill = Category, y = Percentage,
                                 x = stats::reorder(Variable,
                                                    as.numeric(Missing)),
                                 text = paste("<br>Datasets: ", Dataset,
                                              "<br>Category: ", Category,
                                              "<br>Percentage: ", Percentage))) + 
    geom_bar(position = "fill", stat = "identity") +
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    scale_y_reverse(labels = function(x) {
      ifelse(x == 1|x == 0.5, paste0(x*100, "%", "\n(", x*nrow(out), " obs)"),
             paste0(x*100, "%"))
      }) +
    scale_fill_manual(values = cols) +
    theme_minimal() +
    labs(title = deparse(substitute(database)),
         x = "Variable")
  plotly::ggplotly(dbplot, tooltip = "text") %>%
    reverse_legend() %>% 
    plotly::layout(xaxis = list("Variable", tickangle = 60))
}

# Helper function for ordering legend correctly
reverse_legend <- function(plotly_plot) {
  n_labels <- length(plotly_plot$x$data)
  plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
  plotly_plot
}
