#' Creates a data frame of the parameter table from lavaan model
#'
#' @param model A fitted model of class lavaan
#' @param include which parameters to include in the plot. Default is all regression and latent relationships. "covs" will also include covariances, while "all will also include error variances.
#' @return a data frame with lavaan model parameters
#' @importFrom dplyr filter bind_cols
extract_coefs <- function(model, include = NULL) {
  if (is.null(include)) {
    bind_cols(model@ParTable) |>
      filter(.$op == "=~" | .$op == "~")
  } else if (include == "covs") {
    bind_cols(model@ParTable) |>
      filter(.$op == "=~" | .$op == "~" | (.$op == "~~" & .$lhs != .$rhs))
  } else if (include == "all") {
    bind_cols(model@ParTable) |>
      filter(all())
  }
}

#' Creates node data frame and adds formatting
#'
#' @param coefs a coefficient table from lavaan model created by \code{extract_coefs}
#' @param labels An optional list of labels
#' @param node_options a named list of graphviz node attributes
#' @return an edge data frame
#' @importFrom purrr map_lgl map_dfc
#' @importFrom dplyr bind_cols
create_nodes <- function(coefs, labels = NULL, node_options) {
  regress <- coefs$op == "~"
  latent <- coefs$op == "=~"
  observed_nodes <- c()
  latent_nodes <- c()
  if (any(regress)) {
    observed_nodes <- c(observed_nodes, unique(coefs$rhs[regress]))
    observed_nodes <- c(observed_nodes, unique(coefs$lhs[regress]))
  }
  if (any(latent)) {
    observed_nodes <- c(observed_nodes, unique(coefs$rhs[latent]))
    latent_nodes <- c(latent_nodes, unique(coefs$lhs[latent]))
  }
  # make sure latent variables don't show up in both
  observed_nodes <- setdiff(observed_nodes, latent_nodes)

  nodes <- unique(c(coefs$lhs, coefs$rhs))

  latent <- ifelse(nodes %in% latent_nodes, TRUE, FALSE)

  if (is.null(labels)) {
    label <- unique(c(coefs$lhs, coefs$rhs))
  } else {
    label <- labels
  }

  # extract the info here about which ones are latents and which aren't and tag them in the node df
  node_df <- DiagrammeR::create_node_df(
    n = length(unique(c(coefs$lhs, coefs$rhs))),
    label = label,
    latent = latent, # keep this here for conditional formatting later
    shape = ifelse(latent, "oval", "box"),
    orig_label = unique(c(coefs$lhs, coefs$rhs))
  )

  if (any(map_lgl(node_options, is.list))) {
    NULL # not sure yet what goes here
  } else if (!is.null(node_options)) {
    # this might not work for nodes
    node_opts <- map_dfc(node_options, function(x) rep(x, nrow(node_df)))
  }

  if (exists("node_opts")) {
    bind_cols(node_df, node_opts)
  } else {
    node_df
  }
}

#' Creates edge data frame and adds formatting
#'
#' @param coefs a coefficient table from lavaan model created by \code{extract_coefs}
#' @param ndf A node data frame created by \code{create_nodes}
#' @param edge_options a named list of graphviz edge attributes
#' @param coef_labels whether to label edges with coefficient values
#' @return an edge data frame
#' @importFrom purrr map_int map_lgl map_dfc
#' @importFrom dplyr bind_cols
create_edges <- function(coefs, ndf, edge_options, coef_labels = FALSE) {

  # put some ifelse statements here to toggle options, because I don't think null hurts anything

  if(coef_labels){
    label = round(coefs$est, digits = 2)
  } else {
    label = NULL
  }

  edge_df <- DiagrammeR::create_edge_df(
    from = map_int(coefs$rhs, function(x) {
      which(ndf$orig_label == x)
    }),
    to = map_int(coefs$lhs, function(x) {
      which(ndf$orig_label == x)
    }),
    rel = "a",
    oper = coefs$op,
    #label = label,
    dir = ifelse(coefs$op == "=~", "back", ifelse(coefs$op == "~~", "both", "forward"))
  )

  if (!is.null(coef_labels)){
    edge_df <- bind_cols(edge_df, label = label)
  }

  # if any(is.list(edge_options))
  if (any(map_lgl(edge_options, is.list))) {
    NULL # not sure yet what goes here
  } else if (!is.null(edge_options)) {
    edge_opts <- map_dfc(edge_options, function(x) rep(x, nrow(coefs)))
  }

  if (exists("edge_opts")) {
    bind_cols(edge_df, edge_opts)
  } else {
    edge_df
  }
  # create a frame with all the edge options and then bind it to the edge_df
}

#' Uses the diagrammeR functions to turn the ndf and edf into dot
#'
#' @param ndf A node data frame created by \code{create_nodes}
#' @param edf An edge data frame created by \code{create_edges}
#' @param graph_options a named list of graphviz graph attributes
#' @return DOT specification of model
#' @importFrom dplyr bind_rows
#' @importFrom purrr map2
convert_graph <- function(ndf, edf, graph_options) {
  gr <- DiagrammeR::create_graph(ndf, edf, attr_theme = NULL)

  if(!is.null(graph_options)){
    gr_opts <- map2(graph_options, names(graph_options), function(x, names){c(attr = names, value = x, attr_type = "graph")}) |>
        bind_rows()

    gr$global_attrs <- bind_rows(gr$global_attrs, gr_opts)
  }
  gr |>
    DiagrammeR::generate_dot()
}

#' Creates the grViz dot language code for plotting
#'
#' @param model A model fit object of class lavaan.
#' @param labels An optional named list of variable labels.
#' @param include which parameters to include in the plot. Default is all regression and latent relationships. "covs" will also include covariances, while "all will also include error variances.
#' @param graph_options a named list of graphviz graph attributes
#' @param node_options a named list of graphviz node attributes
#' @param edge_options a named list of graphviz edge attributes
#' @param ... Additional arguments to be passed to \code{create_edges}
#' @return A string specifying the path diagram for \code{model}
create_grviz <- function(model, labels = labels, include = include, graph_options = graph_options, node_options = node_options, edge_options = edge_options, ...) {
  coefs <- extract_coefs(model, include)

  ndf <- create_nodes(coefs, labels, node_options)
  edf <- create_edges(coefs, ndf, edge_options, ...)

  # maybe have a way to keep the ndf and edf here in the output for later use
  # graph <- DiagrammeR::create_graph(ndf, edf)
  dot <- convert_graph(ndf, edf, graph_options)
  dot
}

#' Plots lavaan path model with DiagrammeR
#'
#' @param model A model fit object of class lavaan.
#' @param labels An optional named list of variable labels.
#' @param include which parameters to include in the plot. Default is all regression and latent relationships. "covs" will also include covariances, while "all will also include error variances.
#' @param gr_viz pass a gr_viz model generated from \code{create_grviz} to create plot from that directly
#' @param graph_options a named list of graphviz graph attributes
#' @param node_options a named list of graphviz node attributes
#' @param edge_options a named list of graphviz edge attributes
#' @param ... Additional arguments to be passed to create_grviz for creating edges
#' @return A Diagrammer plot of the path diagram for \code{model}
#' @importFrom DiagrammeR grViz
#' @export
lavaanPlot2 <- function(model, labels = NULL, include = NULL, gr_viz = NULL, graph_options = NULL, node_options = NULL, edge_options = NULL, ...) {
  if (!is.null(gr_viz)) {
    grViz(gr_viz)
  } else {
    create_grviz(model, labels, include, graph_options, node_options, edge_options, ...) |>
      grViz()
  }
}
