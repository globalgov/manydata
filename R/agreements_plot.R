#' Plot agreements network
#' @description Facilitates plotting of 'many' data.
#' @param dataset A dataset from one of the many packages
#' or a "consolidated" database.
#' Other options are "bilateral" or "multilateral".
#' @param actor An actor variable.
#' "StateID", by default.
#' @param treaty_type The type of treaties to be returned.
#' NULL, by default.
#' Other options are "bilateral" or "multilateral".
#' @name agreements_plot
NULL

#' @rdname agreements_plot
#' @param layout How do you want the plot to look like?
#' An igraph layout algorithm, currently defaults to 'concentric'.
#' Some other options are 'stress', 'bipartite', and 'alluvial'.
#' For more information please check ´?migraph::autographr´.
#' @return A network of agreements' relations.
#' @examples
#' @export
plot_agreements <- function(dataset, treaty_type = NULL, layout = "concentric") {
  manyID <- NULL 
  dplyr::select(dataset, manyID) %>% 
      dplyr::mutate(link = ifelse(grepl(":", manyID),
                                  sapply(strsplit(manyID, ":"), "[", 2 ), NA)) %>% 
      dplyr::distinct() %>%
      migraph::as_igraph() %>%
      migraph::autographr(layout = layout)
}

#' @rdname agreements_plot
#' @param layout How do you want the plot to look like?
#' An igraph layout algorithm, currently defaults to 'concentric'.
#' Some other options are 'stress', 'bipartite', and 'alluvial'.
#' For more information please check ´?migraph::autographr´.
#' @return A network of agreements' relations.
#' @examples
#' @export
plot_memberships <- function(dataset, actor = "StateID", treaty_type = NULL,
                             layout = "concentric") {
  manyID <- NULL
  dplyr::select(dataset, manyID, actor) %>%
    dplyr::distinct() %>%
    migraph::as_igraph() %>%
    migraph::autographr(layout = layout)
}

#' @rdname agreements_plot
#' @param date String date from the network snapshot.
#' Used by \code{{cshapes}} to plot the correct map.
#' By default, 2019-12-31.
#' Date can be between 1886-01-01 and 2019-12-31.
#' @param theme Theme you would like to use to plot the graph.
#' bey defalt, "light".
#' Available themes are "light", "dark", and "earth".
#' @details Creates a plot of the a unimodal geographical network at a
#' single point in time.
#' @importFrom migraph is_graph is_multiplex as_edgelist as_tidygraph node_names
#' @importFrom ggraph create_layout ggraph geom_edge_arc
#' scale_edge_width_continuous geom_node_point geom_node_text
#' @importFrom dplyr mutate inner_join rename filter
#' @importFrom cshapes cshp
#' @return A map of a country level geographical network.
#' @examples
#' \donttest{
#' #memberships <- dplyr::filter(manyenviron::memberships$ECOLEX_MEM,
#' #Beg > "2000-01-01" & Beg < "2000-12-12")
#' #network_map(memberships, actor = "CountryID", treaty_type = "bilateral") +
#' #ggplot2::labs(title = "Bilateral International Environmental Treaties Signed in the year 2000",
#' #subtitle = "Ecolex data")
#'}
#' @export
network_map <- function(dataset, actor = "StateID", treaty_type = NULL,
                        date = "2019-12-31", theme = "light") {
  # Checks for correct input
  weight <- NULL
  # Step 1: get membership list
  dataset <- retrieve_membership_list(dataset = dataset, actor = actor,
                                      treaty_type = treaty_type)
  # Step 2: set up empty matrix
  actor <- unique(unlist(strsplit(dataset$Memberships, ", ")))
  out <- matrix(0, nrow = length(actor), ncol = length(actor))
  rownames(out) <- actor
  colnames(out) <- actor
  # Step 3: fill matrix with values
  for (k in colnames(out)) {
    m <- data.frame(table(unlist(strsplit(grep(k, dataset$Memberships,
                                               value = TRUE), ", "))))
    m <- m[!(m$Var1 %in% k),]
    out[k, ] <- ifelse(names(out[k,]) %in% m$Var1 == TRUE, m$Freq/100, out[k,])
  }
  out  <- igraph::get.data.frame(igraph::graph.adjacency(out, weighted = TRUE))
  # Step 4 = get theme
  if (theme == "dark") {
    maptheme <- maptheme(palette = c("#FFFAFA", "#596673"))
    countrycolor <- "#FFFAFA"
  }
  if (theme == "earth") {
    maptheme <- maptheme(palette = c("#79B52F", "#4259FD"))
    countrycolor <- "#79B52F"
  }
  if (theme == "light") {
    maptheme <- maptheme(palette = c("#596673", "#FFFAFA"))
    countrycolor <- "#596673"
  }
  # Step 5: import the historical shapefile data
  cshapes <- cshapes::cshp(as.Date(date), useGW = FALSE)
  coment <- vapply(countryregex[, 3], # add stateID abbreviations
                   function(x) grepl(x, cshapes$country_name,
                                     ignore.case = TRUE, perl = TRUE) * 1,
                   FUN.VALUE = double(length(cshapes$country_name)))
  colnames(coment) <- countryregex[, 1]
  rownames(coment) <- cshapes$country_name
  ab <- apply(coment, 1, function(x) paste(names(x[x == 1]),
                                           collapse = "_"))
  ab[ab == ""] <- NA
  cshapes <- dplyr::mutate(cshapes, stateID = unname(ab))
  # Step 6: create edges with from/to lat/long
  edges <- out %>%
    dplyr::inner_join(cshapes, by = c("from" = "stateID")) %>%
    dplyr::rename(x = .data$caplong, y = .data$caplat) %>%
    dplyr::inner_join(cshapes, by = c("to" = "stateID")) %>%
    dplyr::rename(xend = .data$caplong, yend = .data$caplat)
  # Step 7: Create plotted network from computed edges
  g <- migraph::as_tidygraph(edges)
  # Step 8: Get the country shapes from the edges dataframe
  country_shapes <- ggplot2::geom_sf(data = cshapes$geometry,
                                     fill = countrycolor)
  # Step 9: generate the point coordinates for capitals
  cshapes_pos <- cshapes %>%
    dplyr::filter(.data$stateID %in% migraph::node_names(g)) %>%
    dplyr::rename(x = .data$caplong, y = .data$caplat)
  # Reorder things according to nodes in plotted network g
  cshapes_pos <- cshapes_pos[match(migraph::node_names(g),
                                   cshapes_pos[["stateID"]]), ]
  # Step 10: generate the layout
  lay <- ggraph::create_layout(g, layout = cshapes_pos)
  edges$circular <- rep(FALSE, nrow(edges))
  edges$edge.id <- rep(1, nrow(edges))
  # Step 11: plot
  ggraph::ggraph(lay) +
    country_shapes +
    ggraph::geom_edge_arc(data = edges, ggplot2::aes(edge_width = weight),
                          strength = 0.33, alpha = 0.25) +
    ggraph::scale_edge_width_continuous(range = c(0.5, 2), # scales edge widths
                                        guide = "none") +
    ggraph::geom_node_point(shape = 21, # draw nodes
                            fill = "white", color = "black", stroke = 0.5) +
    ggraph::geom_node_text(ggplot2::aes(label = migraph::node_names(g)),
                           repel = TRUE, size = 3, color = "white",
                           fontface = "bold") +
    maptheme
}

# Helper function providing the network map function with a few map themes.
maptheme <- function(palette = c("#FFFAFA", "#596673")) {
  oceancolor <- palette[2]
  titlecolor <- ifelse(is_dark(palette[2]), "white", "black")
  # Create map theme
  maptheme <- ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::theme(axis.text = ggplot2::element_blank()) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    ggplot2::theme(axis.title = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::theme(panel.background = ggplot2::element_blank()) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = oceancolor)) +
    ggplot2::theme(plot.title = ggplot2::element_text(color = titlecolor,
                                                      hjust = 0.1, vjust = 0.1),
                   plot.subtitle = ggplot2::element_text(color = titlecolor,
                                                         hjust = 0.065,
                                                         vjust = 0.1),
                   plot.caption = ggplot2::element_text(color = titlecolor, hjust = 0.96)) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(0, 0, 0.5, 0), "cm"))
  # This function returns a map theme for ggplot
  maptheme
}

# Helper function to check whether a color is light or dark:
is_dark <- function(hex) {
  # Google luma formula for details.
  luma <- 0.33 * grDevices::col2rgb(hex)[[1]] +
    0.5 * grDevices::col2rgb(hex)[[2]] +
    0.16 * grDevices::col2rgb(hex)[[3]]
  isdark <- ifelse(luma < 186, TRUE, FALSE)
  isdark
}
