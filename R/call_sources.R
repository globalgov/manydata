# Call sources ####

# #' Call sources for datacubes and datasets in 'many' packages
# #' @details `call_sources()` displays sources of the datacubes and datasets
# #' in 'many' packages.
# #' Please declare datacube, and dataset.
# #' @family call_
# #' @param datacube A datacube from one of the many packages.
# #' @param dataset A dataset in a datacube from one of the many packages.
# #' NULL by default.
# #' That is, all datasets in the datacube are used.
# #' For multiple datasets, please declare datasets as a vector
# #' (e.g. c("dataset1", "dataset2")).
# #' @param open_script Would you like to open the preparation script
# #' for the dataset?
# #' By default false.
# #' @param open_codebook Would you like to open the codebook for the dataset?
# #' By default false.
# #' @importFrom utils help browseURL
# #' @importFrom dplyr relocate %>% as_tibble
# #' @importFrom stringr str_extract_all str_remove_all str_trim
# #' @examples
# #' \donttest{
# #' call_sources("manydata", "emperors")
# #' }
# #' @return
# #' `call_sources` returns a tibble with information on the dataset,
# #' their sources, URL, and mapping to facilitate understanding
# #' variable name changes from original data.
# #' @export
# call_sources <- function(datacube, dataset = NULL,
#                          open_script = FALSE, open_codebook = FALSE) {
# 
#   if(is.list(datacube)) datacube <- deparse(substitute(datacube))
# 
#   cli::cli_progress_message("Inferring package...")
#   pkgname <- find(datacube)
#   pkgname <- package[grepl("package:",pkgname)][1]
#   pkgname <- gsub("package:","",pkgname)
#   cli::cli_alert_success("Found {.var {datacube}} in the {.pkg {pkgname}} package.")
# 
#   # return package link for help
#   # cli::cli_alert_info(paste0("Please see ",
#   #                            cli::style_hyperlink(package,
#   #                                                 paste0("https://globalgov.github.io/",
#   #                                                        package)),
#   #                            " for  more information."))
# 
#   # Get help file as clean(ish) text ####
#   helpfile <- paste0(path.package(pkgname),"/help/",datacube)
#   # helpfile <- utils::help(topic = datacube, try.all.packages = TRUE)
#   # cli::cli_progress_message("Calling for help from {helpfile}...")
#   helptext <- .get_help_file(helpfile, pkgname)
# 
#   # Get names if one or more datasets are declared
#   cli::cli_progress_message("Calling for help...")
#   if (!is.null(dataset)) {
#     names <- unlist(dataset)
#   } else {
#     names <- trimws(unlist(strsplit(gsub(
#       "following \\d datasets\\:", "", stringr::str_extract(
#         helptext, "((following \\d datasets\\:)[^\\.]*)")), ", ")))
#   }
# 
#   # keep only portions we are interested in
#   helptext <- paste0(sub('.*</div>', '', helptext), " \\item")
#   # get sections
#   sections <- .get_sections(helptext)
# 
#   # organize information into lists of list
#   out <- list()
#   for (i in names) {
#     out[i] <- stringr::str_extract_all(helptext, paste0(i, "\\s*(.*?)\\s*\\\\item"))
#   }
#   # bind list
#   out <- .check_and_bind_df(out, names)
#   # clean observations
#   out <- data.frame(t(apply(out, 1, function(x) {
#     stringr::str_squish(gsub(
#       paste0(paste(names, collapse = "|"),
#              "|\\\\item|\\\\tabular|\\\\url|\\\\emph|\\\\section|\\\\source|Variable Mapping"), "", x))
#   })))
#   # add names to data frame
#   tryCatch({
#     colnames(out) <- sections
#   }, error = function(e) {
#     stop(paste0("Unable to get sources from documentation file,
#                 please try the help file `?", package, "::", datacube, "`"))
#   })
#   rownames(out) <- gsub(":", "", names)
#   out[] <- lapply(out, function(x) gsub("^: ", "", x))
#   # clean variable mapping
#   out$Mapping <- unlist(lapply(out$Mapping, function(x) {
#     gsub("\\|", " | ", gsub("\\_", " ", gsub("\\(|\\)", "", gsub(
#       " ", " - ", gsub("(\\S* \\S*) ","\\1|", gsub(
#         "\\s+(?=[^()]*\\))", "_", gsub("('.*?')", "(\\1)", x), perl=TRUE))))))
#   }))
#   # open preparation script if declared
#   if (open_script == TRUE & !is.null(dataset)) {
#     url <- paste0("https://github.com/globalgov/", package, "/blob/main/data-raw/",
#                   datacube, "/", dataset, "/", "prepare-", dataset, ".R")
#     tryCatch({
#       utils::browseURL(url, browser = getOption("browser"), encodeIfNeeded = FALSE)
#     }, error = function(e) {
#       message(paste0("Unable to open preparation script, please visit: ", url))
#     })
#   } else if (open_script == TRUE & is.null(dataset)) {
#     message("Please declare a dataset to open a preparation script.")
#   }
#   # open codebook if declared
#   if (open_codebook == TRUE & !is.null(dataset)) {
#     url <- paste0("https://github.com/globalgov/", package, "/raw/develop/data-raw/",
#                   datacube, "/", dataset)
#     tryCatch({
#       utils::browseURL(paste0(url, "/", "OriginalCodebook.pdf"),
#                        browser = getOption("browser"), encodeIfNeeded = FALSE)
#     }, error = function(e) {
#       message(paste0("Unable to open codebook, please visit: ", url))
#     })
#   } else if (open_codebook == TRUE & is.null(dataset)) {
#     message("Please declare a dataset to open codebook.")
#   }
#   # out a with a tibble
#   dplyr::as_tibble(out, rownames = "Dataset") %>%
#     dplyr::relocate(Dataset, Source, URL, Mapping)
# }
# 
# # Helper function to get help file into text
# .get_help_file <- function(helpfile, pkgname) {
#   path <- dirname(helpfile)
#   dirpath <- dirname(path)
#   # if (!dir.exists(dirpath)){
#   #   stop(gettextf("invalid %s argument", sQuote("file")),
#   #        domain = NA)
#   # }
#   # pkgname <- basename(dirpath)
#   RdDB <- file.path(path, pkgname)
#   fetchRdDB <- function(db) {
#     vals <- db$vals
#     vars <- db$vars
#     datafile <- db$datafile
#     compressed <- db$compressed
#     envhook <- db$envhook
#     key <- basename(file)
#     fetch <- function(key) lazyLoadDBfetch(vals[key][[1L]],
#                                            datafile, compressed, envhook)
#     fetch(key)
#   }
#   out <- as.character(lazyLoadDBexec(RdDB, fetchRdDB))
#   out <- stringr::str_remove_all(out, "\\\n|\\{|\\}|\\\\tab$|\\\\cr$|^cc$")
#   out <- paste(stringr::str_trim(out[nzchar(out)]), collapse = " ")
#   out
# }
# 
# # Helper function to get sections
# .get_sections <- function(x) {
#   sections <- c(unlist(stringr::str_extract_all(x, "section \\w*")), "Source")
#   sections <- stringr::str_trim(gsub("section", "", sections))
#   sections
# }
# 
# # Helper file for checking information
# .check_and_bind_df <- function(x, names) {
#   if (length(names) == 1) {
#     x <- data.frame(x[[1]])
#   } else {
#     if (length(unique(lengths(x))) > 1) {
#       for (i in names(x)) {
#         if (length(x[[i]]) < 3) {
#           if (all(!grepl("\\url", x[[i]]))) {
#             x[[i]] <- c(paste0(i, ": \\url NA \\item"), x[[i]])
#           } else if (all(!grepl("Variable Mapping", x[[i]]))) {
#             x[[i]] <- c(x[[i]][1], paste0(i, ": Variable Mapping \\tabular  \\emph from   \\emph to  NA NA \\item"), x[[i]][2])
#           } else x[[i]] <- c(x[[i]], paste0(i, ": NA \\item"))
#         }
#       }
#     }
#     x <- data.frame(do.call(rbind, x))
#   }
#   x
# }

#' @export
call_sources <- function(x){
  if(is.list(x)) datacube <- deparse(substitute(x)) else 
    datacube <- as.character(x)
  if(grepl("\\$", datacube)){
    dataset <- strsplit(datacube, "\\$")[[1]]
    datacube <- dataset[1]
    dataset <- dataset[2]
  } else dataset <- NULL
  infos <- paste0("info_",datacube)
  if(exists(infos)) cinfo <- get(infos) else return(invisible())
  if(!is.null(dataset)) cinfo <- dplyr::filter(cinfo, Dataset == dataset)
  if(length(cinfo)==0) cli::cli_abort("Sorry, no information found for {x}.")
  cinfo
}

#' @export
call_citations <- function(x, output = c("console","help")){
  if(is.list(x)) datacube <- deparse(substitute(x)) else 
    datacube <- as.character(x)
  if(grepl("\\$", datacube)){
    dataset <- strsplit(datacube, "\\$")[[1]]
    datacube <- dataset[1]
    dataset <- dataset[2]
  } else dataset <- NULL
  infos <- paste0("info_",datacube)
  if(exists(infos)) cinfo <- get(infos) else return(invisible())
  if(!is.null(dataset)) cinfo <- dplyr::filter(cinfo, Dataset == dataset)
  if(length(cinfo)==0) cli::cli_abort("Sorry, no citation data found for {x}.")

  output <- match.arg(output)
  if(output == "console"){
    if(nrow(cinfo)>1)
      cat("Please cite the included datasets: \n") else 
        cat("Please cite the dataset: \n")
    cli::cat_bullet(cinfo$Source) 
  } else if(output == "help"){
    paste0("* ", cinfo$Source, collapse = "\n\n")
  }
}

