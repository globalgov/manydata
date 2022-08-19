#' Plot database profile
#'
#' @param database A many database
#' @param key A joining key
#' @return A plot with a profile of the database.
#' @examples
#' db_plot(emperors, "ID")
#' @export
db_plot <- function(database, key) {
  if(length(grepl(key, purrr::map(database, names))) != length(database)) {
    stop("Please declare a key variable present in all datasets in the database.")
  }
  # reduce database
  out <- purrr::reduce(database, dplyr::full_join, by = key)
  # get the number of pairwise overlap between datasets by key variable
  cat("There were", sum(duplicated(unname(unlist(purrr::map(database, key))))),
      "matched observations by key variable across datasets in database.")
  # get variable names, but key
  all_variables <- unname(unlist(purrr::map(database, names)))
  all_variables <- all_variables[!all_variables %in% key]
  # remove other ID variables
  ID_var <- grep("^ID|ID$", all_variables, value = TRUE)
  all_variables <- all_variables[!all_variables %in% ID_var]
  # create an empty data frame
  db <- data.frame(out[,1], stringsAsFactors = TRUE)
  # check if values are missing, confirmed, conflicting, asymmetrical, or unique
  for (var in all_variables) {
    vvars <- paste0("^", var, "$|^", var, "\\.")
    vars_to_combine <- grepl(vvars, names(out))
    if (length(out[vars_to_combine]) > 1) {
      # paste variables to work at the string value
      value <- apply(out[vars_to_combine], 1, paste, collapse = "!")
      # weird code added to some variables? need to double check messydates...
      value <- stringr::str_remove_all(value, "\032")
      # remove string duplicates and collapse unique values (except NAs)
      value <- sapply(stringr::str_split(value, "!"), function(x) {
                        paste(unique(trimws(x), incomparables = "NA"),
                              collapse = '!')
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
      # remove duplicated NAs
      valuec <- sapply(stringr::str_split(value, "!"), function(x) {
        paste(unique(trimws(x)), collapse = '!')
        })
      # confirmed (by multiple datasets in database, other values are NAs)
      value <- ifelse(stringr::str_count(valuec, "\\!") == 1 &
                        stringr::str_count(valuec, "\\!NA|NA\\!") == 1,
                      "confirmed", value)
      # conflict (when there are no duplicates apart from NAs)
      value <- ifelse(stringr::str_count(value, "\\!") ==
                        (length(out[vars_to_combine]) - 1),
                      "conflict", value)
      # asymmetric (assumes the rest is asymmetric for now)
      # todo: fix this by "opening" where duplicated the variables are across
      value <- ifelse(grepl("^missing$|^conflict$|^confirmed$|^unique$", value),
                      value, "asymmetric")
      # todo: what to do with similar dates with different levels of precision?
      } else {
        value <- out[vars_to_combine]
        value <- ifelse(is.na(value), "missing", "unique")
    }
    # fill df
    db[, var] <- value
  }
  # gather data
  dbgather <- db %>%
    dplyr::select(-key) %>% 
    tidyr::pivot_longer(cols = everything(), names_to = "variable",
                        values_to = "category") %>% 
    dplyr::group_by(variable, category) %>%
    dplyr::summarise(count = n(), .groups = ) %>%
    dplyr::mutate(perc = count / sum(count)) %>% 
    tidyr::pivot_wider(id_cols = variable, names_from = category,
                       values_from = perc) %>% 
    dplyr::mutate(across(everything(), ~tidyr::replace_na(.x, 0))) %>%
    tidyr::pivot_longer(-variable) %>% 
    dplyr::rename(Variables = variable, Category = name, Percentage = value)
  # set colors
  cols <- c(confirmed = 'Green', unique = 'Blue', missing = 'Beige',
            conflict = 'Red', asymmetric = 'Orange')
  # plot
    ggplot(dbgather, aes(fill = Category, y = Percentage, x = Variables )) + 
      geom_bar(position = "fill", stat = "identity") +
      scale_fill_manual(values = cols) +
      coord_flip() +
      theme_minimal()
}
