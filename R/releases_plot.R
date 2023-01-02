#' A plotting function that visualises historical milestones/releases
#'
#' The function will take a data frame that details this information,
#' or more usefully, a Github repository listing.
#' @param repo the github repository to track, e.g. "globalgov/manydata"
#' @param begin When to begin tracking repository milestones.
#'   By default NULL, two months before the first release.
#' @param end When to end tracking repository milestones.
#'   By default NULL, two months after the latest release.
#' @importFrom httr GET content warn_for_status stop_for_status http_error
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
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
#' #plot_releases("globalgov/manydata")
#' }
#' @export
plot_releases <- function(repo, begin = NULL, end = NULL) {
  # Step one: get releases from repo
  if (!is.data.frame(repo)) {
    get_releases <- function(repo) {
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
    df <- get_releases(repo)
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
  text_offset <- 0.05
  df$month_count <- stats::ave(df$date == df$date, df$date, FUN = cumsum)
  df$text_position <- (df$month_count * text_offset *
                         df$direction) + df$position
  month_buffer <- 2

  # Step five: get months date range
  month_date_range <- seq(as.Date(min(df$date)) - months(month_buffer),
                          as.Date(max(df$date)) + months(month_buffer),
                          by = "month")
  month_format <- format(month_date_range, "%b")
  month_df <- data.frame(month_date_range, month_format)

  # Step five: get years date range
  year_date_range <- c(min(month_date_range), max(month_date_range))
  year_format <- format(year_date_range, "%Y")
  year_df <- data.frame(year_date_range, year_format)

  # Step seven: plot
  timeline_plot <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = 0,
                                                    col = .data$milestone,
                                                    label = .data$milestone))
  timeline_plot <- timeline_plot + ggplot2::labs(col = "Milestones")
  timeline_plot <- timeline_plot +
    ggplot2::scale_color_manual(values = milestone_colors,
                                labels = milestone_levels, drop = FALSE)
  timeline_plot <- timeline_plot + ggplot2::theme_classic()

  # Plot horizontal black line for timeline
  timeline_plot <- timeline_plot + ggplot2::geom_hline(yintercept = 0,
                                                       color = "black",
                                                       linewidth = 0.3)
  # Plot vertical segment lines for milestones
  timeline_plot <- timeline_plot +
    ggplot2::geom_segment(data = df[df$month_count == 1, ],
                          ggplot2::aes(y = .data$position,
                                       yend = 0, xend = date),
                          color = "black", linewidth = 0.2)

  # Plot scatter points at zero and date
  timeline_plot <- timeline_plot +
    ggplot2::geom_point(ggplot2::aes(y = 0), size = 3)

  # Don't show axes, appropriately position legend
  timeline_plot <- timeline_plot +
    ggplot2::theme(axis.line.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.line.x = ggplot2::element_blank(),
                   legend.position = "bottom")
  # Show text for each month
  timeline_plot <- timeline_plot +
    ggplot2::geom_text(data = month_df,
                       ggplot2::aes(x = as.character(month_date_range),
                                    y = -0.1, label = month_format),
                       size = 2.5, vjust = 0.5, color = "black", angle = 90)
  # Show year text if applicable
  if (nrow(month_df) > 12) timeline_plot <- timeline_plot +
    ggplot2::geom_text(data = year_df, ggplot2::aes(x = as.character(year_date_range),
                                                    y = -0.2,
                                                    label = year_format,
                                                    fontface = "bold"),
                       size = 2.5, color = "black")
  # Show text for each milestone
  timeline_plot <- timeline_plot +
    ggplot2::geom_text(ggplot2::aes(y = .data$text_position,
                                    label = .data$tag_name), size = 2.5)
  print(timeline_plot)
}
