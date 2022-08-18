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
  # get variable names, but ID
  all_variables <- unname(unlist(purrr::map(database, names)))
  all_variables <- all_variables[!all_variables %in% key]
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
      # remove string duplicates and collapse unique values
      value <- sapply(stringr::str_split(value, "!"),
                      function(x) {paste(unique(trimws(x)), collapse = '!')})
      # work with fractions to determine category
      value <- ifelse(stringr::str_detect(value, "^NA$|^na$|^NA_character$"),
                      "missing", value)
      value <- ifelse(stringr::str_count(value, "\\!") ==
                        (length(out[vars_to_combine]) - 1), "conflict", value)
      value <- ifelse(stringr::str_count(value, "\\!") == 0 &
                        !grepl("^missing$|^conflict$", value),
                      "confirmed", value)
      value <- ifelse(!grepl("^missing$|^conflict$|^confirmed$", value),
                      "asymmetric", value)
      # Questions:
      # What to do with NAs when there are conflicts? 
      # What to do with confirmed obs in multiple datasets but not all?
      # What to do with with similar dates with different levels of precision?
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
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(perc = count / sum(count)) %>% 
    tidyr::pivot_wider(id_cols = variable, names_from = category,
                       values_from = perc) %>% 
    dplyr::mutate(across(everything(), ~tidyr::replace_na(.x, 0))) %>% 
    tidyr::pivot_longer(asymmetric:confirmed) %>% 
    dplyr::rename(Variables = variable, Category = name, Percentage = value)
  # plot
    ggplot(dbgather, aes(fill = Category, y = Percentage, x = Variables )) + 
      geom_bar(position="fill", stat="identity") +
      coord_flip()
}
