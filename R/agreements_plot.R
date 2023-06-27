#' Plot agreements network
#' 
#' @description Facilitates plotting of 'many' data.
#' @param dataset A dataset from one of the many packages
#' or a "consolidated" database.
#' @param actor An actor variable.
#' "StateID", by default.
#' @param treaty_type The type of treaties to be returned.
#' NULL, by default.
#' Other options are "bilateral" or "multilateral".
#' @param key An ID column to collapse by.
#' By default "manyID".
#' @param layout How do you want the plot to look like?
#' An `{ggraph}` layout algorithm.
#' If not declared, reasonable defaults are used.
#' @name plot_agreements
NULL

#' @rdname plot_agreements
#' @importFrom dplyr %>% select mutate distinct rename
#' @importFrom manynet as_igraph autographr
#' @return A network of agreements' relations.
#' @examples
#' \donttest{
#' #agreements <- dplyr::filter(manyenviron::agreements$ECOLEX,
#' #Beg > "2000-01-01" & Beg < "2000-12-12")
#' #agreements_plot(agreements)
#'}
#' @export
agreements_plot <- function(dataset, treaty_type = NULL, key = "manyID",
                            layout = "circle") {
  manyID <- treatyID <- name <- NULL
  if (key == "manyID") {
    out <- dplyr::select(dataset, manyID) %>%
      dplyr::rename(key = manyID) %>%
      dplyr::distinct()
  } else if (key == "treatyID") {
    out <- dplyr::select(dataset, treatyID) %>%
      dplyr::rename(key == treatyID) %>%
      dplyr::distinct()
  } else stop("Please declare either 'manyID' or 'treatyID'.")
  if (!is.null(treaty_type)) {
    if (treaty_type == "bilateral") {
      out <- out[grep("-", out$key),]
    }
    if (treaty_type == "multilateral") {
      out <- out[grep("-", out$key, invert = TRUE),]
    }
  }
  dplyr::mutate(out,
                link = ifelse(grepl(":", key), sapply(strsplit(key, ":"),
                                                      "[", 2), "NA"),
                key = gsub("\\:.*", "", key)) %>%
    manynet::as_tidygraph() %>%
    dplyr::filter(name != "NA") %>%
    manynet::autographr(layout = layout)
}

#' @rdname plot_agreements
#' @importFrom dplyr %>% select distinct all_of rename
#' @importFrom manynet as_igraph autographr
#' @return A network of agreements' memberships.
#' @examples
#' \donttest{
#' #memberships <- dplyr::filter(manyenviron::memberships$ECOLEX_MEM,
#' #Beg > "2000-01-01" & Beg < "2000-01-31")
#' #membership_plot(memberships)
#'}
#' @export
membership_plot <- function(dataset, actor = "stateID", treaty_type = NULL,
                            key = "manyID", layout = "bipartite") {
  manyID <- treatyID <- name <- NULL
  if (key == "manyID") {
    out <- dplyr::select(dataset, manyID, dplyr::all_of(actor)) %>%
      dplyr::rename(key = manyID) %>%
      dplyr::distinct()
  } else if (key == "treatyID") {
    out <- dplyr::select(dataset, treatyID, dplyr::all_of(actor)) %>%
      dplyr::rename(key == treatyID) %>%
      dplyr::distinct()
  } else stop("Please declare either 'manyID' or 'treatyID'.")
  if (!is.null(treaty_type)) {
    if (treaty_type == "bilateral") {
      out <- out[grep("-", out$key),]
    }
    if (treaty_type == "multilateral") {
      out <- out[grep("-", out$key, invert = TRUE),]
    }
  }
  na.omit(out) %>%
    manynet::as_tidygraph() %>%
    manynet::mutate(type = ifelse(grepl("[0-9][0-9][0-9][0-9][A-Za-z]",
                                        name), TRUE, FALSE)) %>%
    manynet::autographr(layout = layout)
}

#' @rdname plot_agreements
#' @importFrom manynet autographr
#' @importFrom dplyr %>% select mutate distinct filter rename
#' @return A plot of agreements' lineages.
#' @examples
#' \donttest{
#' #lineage <- dplyr::filter(manyenviron::agreements$HUGGO, Beg > "2000-01-01",
#' #Beg < "2001-12-31")
#' #lineage_plot(lineage)
#' }
#' @export
lineage_plot <- function(dataset, treaty_type = NULL, key = "manyID",
                         layout = "nicely") {
  manyID <- treatyID <- name <- NULL
  if (key == "manyID") {
    out <- dplyr::select(dataset, manyID) %>%
      dplyr::rename(key = manyID) %>%
      dplyr::distinct()
  } else if (key == "treatyID") {
    out <- dplyr::select(dataset, treatyID) %>%
      dplyr::rename(key == treatyID) %>%
      dplyr::distinct()
  } else stop("Please declare either 'manyID' or 'treatyID'.")
  if (!is.null(treaty_type)) {
    if (treaty_type == "bilateral") {
      out <- out[grep("-", out$key),]
    }
    if (treaty_type == "multilateral") {
      out <- out[grep("-", out$key, invert = TRUE),]
    }
  }
  out %>%
    dplyr::filter(grepl(":", key)) %>%
    dplyr::mutate(key1 = gsub(".*\\:", "", key),
                  key = gsub("\\:.*", "", key)) %>%
    dplyr::distinct() %>%
    manynet::as_tidygraph() %>%
    manynet::autographr(layout = "nicely")
} 

#' @rdname plot_agreements
#' @param date String date from the network snapshot.
#' Used by \code{{cshapes}} to plot the correct map.
#' By default, 2019-12-31.
#' Date can be between 1886-01-01 and 2019-12-31.
#' @param theme Theme you would like to use to plot the graph.
#' bey defalt, "light".
#' Available themes are "light", "dark", and "earth".
#' @details `map_plot()` creates a plot of the a unimodal geographical network 
#' at a single point in time.
#' @importFrom manynet is_graph is_multiplex as_edgelist as_tidygraph node_names
#' @importFrom dplyr mutate inner_join rename filter
#' @return A map of a country level geographical network.
#' @examples
#' \donttest{
#' #memberships <- dplyr::filter(manyenviron::memberships$ECOLEX_MEM,
#' #Beg > "2000-01-01" & Beg < "2000-12-12")
#' #map_plot(memberships, actor = "stateID") +
#' #ggplot2::labs(title = "Bilateral International Environmental Treaties Signed in the year 2000",
#' #subtitle = "Ecolex data")
#'}
#' @export
map_plot <- function(dataset, actor = "StateID", treaty_type = NULL,
                     date = "2019-12-31", theme = "light") {
  # check packages
  if (!requireNamespace("cshapes", quietly = TRUE)) {
    if(utils::askYesNo(msg = "The `cshapes` package is required.
                       Would you like to install `cshapes` from CRAN?")) {
      utils::install.packages('cshapes')
    } else {
      stop("Please install `cshapes` from CRAN to plot maps.")
    }
  }
  if (!requireNamespace("ggraph", quietly = TRUE)) {
    if(utils::askYesNo(msg = "The `ggraph` package is required.
                       Would you like to install `ggraph` from CRAN?")) {
      utils::install.packages('ggraph')
    } else {
      stop("Please install `ggraph` from CRAN for plot layouts.")
    }
  }
  if (!requireNamespace("igraph", quietly = TRUE)) {
    if(utils::askYesNo(msg = "The `igraph` package is required.
                       Would you like to install `igraph` from CRAN?")) {
      utils::install.packages('igraph')
    } else {
      stop("Please install `igraph` from CRAN to work with network data.")
    }
  }
  # Checks for correct input
  weight <- NULL
  # Step 1: get membership list
  dataset <- call_treaties(dataset = dataset, actor = actor,
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
  g <- manynet::as_tidygraph(edges)
  # Step 8: Get the country shapes from the edges dataframe
  country_shapes <- ggplot2::geom_sf(data = cshapes$geometry,
                                     fill = countrycolor)
  # Step 9: generate the point coordinates for capitals
  cshapes_pos <- cshapes %>%
    dplyr::filter(.data$stateID %in% manynet::node_names(g)) %>%
    dplyr::rename(x = .data$caplong, y = .data$caplat)
  # Reorder things according to nodes in plotted network g
  cshapes_pos <- cshapes_pos[match(manynet::node_names(g),
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
    ggraph::geom_node_text(ggplot2::aes(label = manynet::node_names(g)),
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
