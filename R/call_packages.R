# Call packages ####

#' Call, download, and update many* packages
#' @description 
#'   `call_packages()` finds and download other packages
#'   that belong to the many universe of packages.
#'   It allows users to rapidly access the names and other
#'   descriptive information of these packages.
#'   If users intend to download and install a package listed,
#'   they can type the package name within the function.
#' @family call_
#' @param package A character vector of package name.
#' For multiple packages,
#' please declare package names as a vector (e.g. c("package1", "package2")).
#' @param develop Would you like to download the develop
#' version of the package?
#' FALSE by default.
#' @importFrom dplyr bind_rows rename relocate %>% as_tibble
#' @importFrom stringr str_detect str_remove
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content
#' @importFrom remotes install_github
#' @importFrom utils packageVersion askYesNo
#' @examples
#' \donttest{
#' #call_packages()
#' #call_packages("manyenviron")
#' }
#' @return
#' `call_packages()` returns a tibble with the 'many packages'
#' currently available.
#' If one or more package names are provided,
#' these will be installed from Github.
#' @export
call_packages <- function(package, develop = FALSE) {
  # introduce variables to avoid check notes
  Description <- Installed <- Latest <- Name <- updated <-
    description <- full_name <- name <- NULL
  # get info from GitHub if package is missing
  if (missing(package)) {
    # get package releases, versions, and bind information
    repo <- httr::GET("https://api.github.com/users/globalgov/repos",
                      query = list(state = "all", per_page = 100, page = 1))
    repo <- suppressMessages(httr::content(repo, type = "text"))
    repo <- jsonlite::fromJSON(repo, flatten = TRUE)
    repo <- repo[c("name", "full_name", "description")]
    if (length(repo) < 3 | nrow(repo) < 5) {
      stop(
        "The download limit from GitHub has been reached.
      To see all the available 'many packages' packages,
      please go to the following link: https://github.com/globalgov")
    }
    # search for locally installed packages
    repo$Installed <- sapply(repo$name, function(x) {
      tryCatch({
        as.character(utils::packageVersion(x))
        }, error = function(e) {
          NA_character_
          })
      })
    # get latest release
    repo$Latest <- get_latest_release(repo$full_name)
    repo <- subset(repo, !grepl("Unreleased", repo$Latest))
    # format tibble
    repo <- repo %>%
      dplyr::bind_rows() %>%
      dplyr::select(-full_name) %>%
      dplyr::rename(Name = name, Description = description) %>%
      dplyr::relocate(Name, Description, Installed, Latest) %>%
      dplyr::as_tibble()
    # prints tibble before asking about updates
    print(repo, justify = "right")
    # ask users if they want to install/update packages
    if (any(repo$Installed != repo$Latest)) {
      not_up_to_date <- repo[repo$Installed != repo$Latest,]
      install <- not_up_to_date[is.na(not_up_to_date$Installed), "Name"]
      update <- not_up_to_date[!is.na(not_up_to_date$Installed), "Name"]
      if (nrow(install) > 0) {
        if(utils::askYesNo(msg = paste0("Would you like to install ",
                                        paste(install$Name, collapse = ", "),
                                        " ?"))) {
          tryCatch({
            for (i in install$Name) {
              remotes::install_github(paste0("globalgov/", i))
            }
          }, error = function(e) {
            stop(paste0("The download limit from GitHub has been reached.
       Please download the package using:
              remotes::install_github(globalgov/", package, ")"))
          })
        }
      } 
      if (nrow(update) > 0) {
        if(utils::askYesNo(msg = paste0("Would you like to update ",
                                        paste(update$Name, collapse = ", "),
                                        " ?"))) {
          tryCatch({
            for (i in update$Name) {
              remotes::install_github(paste0("globalgov/", i))
            }
          }, error = function(e) {
            stop(paste0("The download limit from GitHub has been reached.
       Please download the package using:
              remotes::install_github(globalgov/", package, ")"))
          })
        }
      }
    }
    # add message with hyperlinks
    usethis::ui_info(c("For more information on each of the packages please see:",
                       lapply(repo$Name, function(x) {
                         cli::style_hyperlink(x, paste0("https://globalgov.github.io/", x))
                       })))
  } else {
    # download package if declared
    tryCatch({
      if (stringr::str_detect(package, "/")) {
        remotes::install_github(package)
        package <- strsplit(package, "/")[[1]][2]
      }
      if (develop == FALSE) {
        remotes::install_github(paste0("globalgov/", package))
      } else {
        remotes::install_github(paste0("globalgov/", package), ref = "develop")
      }
      library(package, character.only = TRUE)
    }, error = function(e) {
      stop(paste0("The download limit from GitHub has been reached.
       Please download the package using:
              remotes::install_github(globalgov/", package, ")"))
    })
    usethis::ui_info(paste0("Please see ",
                            cli::style_hyperlink(package, paste0("https://globalgov.github.io/", package)),
                            " for  more information."))
  }
}

# Helper function to get package releases
get_latest_release <- function(full_name) {
  latest <- paste0("https://api.github.com/repos/",
                   full_name, "/releases/latest")
  if (length(latest) == 1) {
    latest <- httr::GET(latest)
    latest <- suppressMessages(httr::content(latest, type = "text"))
    latest <- jsonlite::fromJSON(latest, flatten = TRUE)$tag_name
  } else {
    latest <- sapply(latest, function(x) {
      x <- httr::GET(x)
      x <- suppressMessages(httr::content(x, type = "text"))
      x <- jsonlite::fromJSON(x, flatten = TRUE)$tag_name
      if (is.null(x)) {
        x <- "Unreleased"
        x
      } else {
        x <- stringr::str_remove(x, "v")
        x
      }
    })
  }
  unlist(latest)
}

#' Call releases historical milestones/releases
#'
#' The function will take a data frame that details this information,
#' or more usefully, a Github repository listing.
#' @param repo the github repository to track, e.g. "globalgov/manydata"
#' @param begin When to begin tracking repository milestones.
#'   By default NULL, two months before the first release.
#' @param end When to end tracking repository milestones.
#'   By default NULL, two months after the latest release.
#' @family call_
#' @importFrom httr GET content warn_for_status stop_for_status http_error
#' @importFrom jsonlite fromJSON
#' @importFrom stats ave
#' @importFrom stringr str_split str_remove
#' @importFrom messydates as_messydate
#' @import ggplot2
#' @details The function creates a project timeline graphic using ggplot2
#' with historical milestones and milestone statuses gathered from a
#' specified GitHub repository.
#' @source
#' https://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/
#' @return A ggplot graph object
#' @examples
#' \donttest{
#' #call_releases("globalgov/manydata")
#' #call_releases("manypkgs")
#' }
#' @export
call_releases <- function(repo, begin = NULL, end = NULL) {
  # Step one: get releases from repo
  if (!is.data.frame(repo)) {
    if (!grepl("/", repo)) {
      usethis::ui_info("Looking for package in 'globalgov' repo.")
      repo <- paste0("globalgov/", repo)
    }
    # return link for more information
    usethis::ui_info(paste0("Please see ",
                            cli::style_hyperlink(strsplit(repo, "/")[[1]][2],
                                                 paste0("https://globalgov.github.io/",
                                                        strsplit(repo, "/")[[1]][2])),
                            " for  more information."))
    df <- get_releases(repo = repo, begin = begin, end = end)
  } else df <- repo
  # Step two: assign colors to releases
  milestone <- NULL
  milestone_levels <- c("Patch", "Minor", "Major")
  milestone_colors <- c("darkgreen", "blue", "red")
  df$milestone <- factor(df$milestone, levels = milestone_levels,
                         ordered = TRUE)
  positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
  directions <- c(1, -1)
  # Step three: add lines
  line_pos <- data.frame("date" = unique(df$date),
                         "position" = rep(positions,
                                          length.out = length(unique(df$date))),
                         "direction" = rep(
                           directions, length.out = length(unique(df$date))))
  df <- merge(df, line_pos, by = "date", all = TRUE)
  df <- df[with(df, order(date, milestone)), ]
  # Step four: get text in the right position
  df$month_count <- stats::ave(df$date == df$date, df$date, FUN = cumsum)
  df$text_position <- (df$month_count * 0.05 * df$direction) + df$position
  if (!messydates::is_messydate(df$date)) {
    df$date <- as.Date(messydates::as_messydate(df$date), mean)
  } else df$date <- as.Date(df$date, mean)
  # Step five: get months date range
  month_date_range <- seq(min(as.Date(df$date, min)) - months(2),
                          max(as.Date(df$date, max)) + months(2), by = "month")
  month_format <- format(month_date_range, "%b")
  month_df <- data.frame(month_date_range, month_format)
  # Step six: get years date range
  year_date_range <- c(min(month_date_range), max(month_date_range))
  year_format <- format(year_date_range, "%Y")
  year_df <- data.frame(year_date_range, year_format)
  # Step seven: plot
  timeline_plot <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = 0,
                                                    col = .data$milestone,
                                                    label = .data$milestone)) + 
    ggplot2::labs(col = "Milestones") +
    ggplot2::scale_color_manual(values = milestone_colors,
                                labels = milestone_levels, drop = FALSE) + 
    ggplot2::theme_classic() +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.3) +
    ggplot2::geom_segment(data = df[df$month_count == 1, ],
                          ggplot2::aes(y = .data$position, yend = 0,
                                       xend = date),
                          color = "black", linewidth = 0.2) +
    ggplot2::geom_point(ggplot2::aes(y = 0), size = 3) +
    ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.line.x = ggplot2::element_blank(),
                   legend.position = "bottom") +
    ggplot2::geom_text(data = month_df,
                       ggplot2::aes(x = month_date_range,
                                    y = -0.1, label = month_format),
                       size = 2.5, vjust = 0.5, color = "black", angle = 90)
  # Show year text if applicable
  if (nrow(month_df) > 12) timeline_plot <- timeline_plot +
    ggplot2::geom_text(data = year_df, ggplot2::aes(x = year_date_range,
                                                    y = -0.2, fontface = "bold",
                                                    label = year_format),
                       size = 2.5, color = "black")
  # Show text for each milestone
  timeline_plot + ggplot2::geom_text(ggplot2::aes(y = .data$text_position,
                                                  label = .data$tag_name),
                                     size = 2.5)
}

# Helper function for getting onformation from GitHub repos
get_releases <- function(repo, begin, end) {
  repo <- paste0("https://api.github.com/repos/", repo, "/releases")
  df <- httr::GET(repo, query = list(state = "all",
                                     per_page = 100, page = 1))
  httr::stop_for_status(df)
  httr::warn_for_status(df)
  df <- httr::content(df, type = "text", encoding = "UTF-8")
  df <- jsonlite::fromJSON(df, flatten = TRUE)
  df <- df[, c("tag_name", "url", "published_at")]
  df$date <- stringr::str_remove(df$published_at, "T.*$")
  df$date <- messydates::as_messydate(stringr::str_replace(df$date,
                                                           "-[:digit:]*$",
                                                           "-01"))
  if(!is.null(begin)) df <- dplyr::filter(df, date >= begin)
  if(!is.null(end)) df <- dplyr::filter(df, date <= end)
  # Get milestones
  code_milestone <- function(tag_name) {
    tags <- c(tag_name, "v0.0.0")
    test <- lapply(stringr::str_split(stringr::str_remove(tags, "v"),
                                      "\\."), function(x) as.numeric(x))
    elemt <- function(lst, n) {
      sapply(lst, `[`, n)
    }
    ifelse(elemt(test, 3) > dplyr::lead(elemt(test, 3)), "Patch",
           ifelse(elemt(test, 2) > dplyr::lead(elemt(test, 2)),
                  "Minor", "Major"))[-length(tags)]
  }
  df$milestone <- code_milestone(df$tag_name)
  df
}

