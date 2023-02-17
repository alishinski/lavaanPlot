#' Creates a data frame of the parameter table from lavaan model
#'
#' @param model A fitted model of class lavaan
#' @param include which parameters to include in the plot. Default is all regression and latent relationships. "covs" will also include covariances, while "all" will also include error variances.
#' @param stand Should the coefficients being used be standardized coefficients
#' @return a data frame with lavaan model parameters
#' @importFrom dplyr filter bind_cols .data
#' @export
extract_coefs <- function(model, include = NULL, stand = FALSE) {
  if(stand){
    par_table <- lavaan::standardizedSolution(model) %>%
      dplyr::mutate(p_val = (1 - stats::pnorm(abs(.data$z))) * 2)
  } else {
    par_table <- bind_cols(model@ParTable) %>%
      dplyr::mutate(z = .data$est / .data$se) %>%
      dplyr::mutate(p_val = (1 - stats::pnorm(abs(.data$z))) * 2)
  }

  par_table$p_val[is.na(par_table$p_val)] <- 1

  par_table <- par_table %>%
    dplyr::mutate(stars = map(.data$p_val, sig_stars))

  if (is.null(include)) {
    bind_cols(par_table) %>%
      filter(.data$op == "=~" | .data$op == "~")
  } else if (include == "covs") {
    bind_cols(par_table) %>%
      dplyr::filter(.data$op == "=~" | .data$op == "~" | (.data$op == "~~" & .data$lhs != .data$rhs))
  } else if (include == "all") {
    bind_cols(par_table) %>%
      dplyr::filter(all())
  }
}

#' Enables conditional formatting for different parts of the model
#'
#' @param ... lists of node or edge options for each of the groups
#' @param type type of conditional formatting being used, node, edge, or custom; custom only works with edges
#' @param groups character vector of the names of custom groups, with nodes and edges default values are set and you need to match the order: for nodes: c("latent", "obs"), for edges: c("regress", "latent", "covs"). For custom groups of edges, you must match names that you pre-multiply with coefficients in your model specification.
#' @return a formatting data frame that can work with the create_nodes and create_edges functions
#' @export
formatting <- function(..., type, groups){

  # set the group options
  if(type == "node"){
    groups <- c("latent", "obs")
  } else if (type == "edge"){
    groups <- c("regress", "latent", "covs")
  } else if (type == "custom") {
    groups <- groups
  }

  # check number of groups against the number of inputs
  if(type == "custom"){
    if(nargs() - 2 != length(groups)){stop("Number of formatting options must equal number of groups")}

  } else {
    if(nargs() - 1 != length(groups)){stop("Number of formatting options must equal number of groups")}
  }

  # combine these into a df that will join with the node (or edge) data frames
  format <- purrr::map2_dfr(list(...), .f = ~data.frame(.x) %>% dplyr::mutate(group = .y), .y = groups)
  #format

  # # set the group options
  if(type == "node"){
      attr(format, "type") <- "node"
    } else if (type == "edge"){
      attr(format, "type") <- "edge"
    } else if (type == "custom") {
      attr(format, "type") <- "custom"
    }
    format
}

#' Creates node data frame and adds formatting
#'
#' @param coefs a coefficient table from lavaan model created by \code{extract_coefs}
#' @param labels An optional list of labels
#' @param node_options a named list of graphviz node attributes, or a data frame of node options created by \code{formatting},
#' @return an edge data frame
#' @importFrom purrr map_lgl map_dfc
#' @importFrom dplyr bind_cols recode
#' @importFrom rlang exec
#' @export
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
    label <- exec(recode, unique(c(coefs$lhs, coefs$rhs)), !!!labels)
  }

  # extract the info here about which ones are latents and which aren't and tag them in the node df
  node_df <- DiagrammeR::create_node_df(
    n = length(unique(c(coefs$lhs, coefs$rhs))),
    label = label,
    latent = latent, # keep this here for conditional formatting later
    shape = ifelse(latent, "oval", "box"),
    orig_label = unique(c(coefs$lhs, coefs$rhs))
  )

  # make the anchor for the formatting data frame
  node_df$group <- ifelse(node_df$latent, "latent", "obs")

  # check for a dataframe here instead
  if (is.data.frame(node_options)) {
  # then link em up
    node_df <- dplyr::left_join(node_df, node_options, by = "group", suffix = c("_orig", "")) %>% dplyr::mutate(shape = ifelse(is.na(.data$shape), .data$shape_orig, .data$shape))
  } else if (!is.null(node_options)) {
    node_opts <- map_dfc(node_options, function(x) rep(x, nrow(node_df)))
    node_df <- bind_cols(node_df, node_opts)
  }

  node_df

}

#' Creates edge data frame and adds formatting
#'
#' @param coefs a coefficient table from lavaan model created by \code{extract_coefs}
#' @param ndf A node data frame created by \code{create_nodes}
#' @param edge_options a named list of graphviz edge attributes, or a data frame of edge options created by \code{formatting}, or a list of such data frames containing 1 set of edge options and one set of custom options
#' @param coef_labels whether to label edges with coefficient values
#' @param stand Should the coefficients being used be standardized coefficients
#' @param stars a character vector indicating which parameters should include significance stars be included for regression paths, latent paths, or covariances. Include which of the 3 you want ("regress", "latent", "covs"), default is none.
#' @param sig significance level for determining what significant paths are
#' @return an edge data frame
#' @importFrom purrr map map_int map_lgl map_dfc pmap_lgl
#' @importFrom dplyr bind_cols
#' @export
create_edges <- function(coefs, ndf, edge_options, coef_labels = FALSE, stand = FALSE, stars = NULL, sig = 1.00) {

  regress <- coefs$op == "~"
  latent <- coefs$op == "=~"
  covs <- coefs$op == "~~" & (coefs$rhs != coefs$lhs)

  stars_yn <- if(!is.null(stars)) pmap_lgl(map(stars, dynGet), any) else NULL
  coef_vals <- round(coefs$est, digits = 2)

  if(coef_labels & !is.null(stars)){
    label = ifelse(stars_yn, paste(coef_vals, coefs$stars, sep = ""), coef_vals)
  } else if(coef_labels){
    label = coef_vals
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
    tag = coefs$label,
    regress = regress,
    latent = latent,
    covs = covs,
    dir = ifelse(coefs$op == "=~", "back", ifelse(coefs$op == "~~", "both", "forward"))
  )

  if (!is.null(coef_labels)){
    edge_df <- bind_cols(edge_df, label = label)
  }


    edge_df <- edge_df %>% dplyr::mutate(group = dplyr::case_when(edge_df$regress ~ "regress",
                                                    edge_df$latent ~ "latent",
                                                    edge_df$covs ~ "covs"))

    # checking if it's a list of data frames
    if(any(map_lgl(edge_options, is.data.frame))){
    names(edge_options) <- map(edge_options, ~attr(., "type"))

    # join 1
    join1 <- dplyr::left_join(edge_df, edge_options$edge, by = "group", suffix = c("_orig", ""))
    #print(join1)
    # join 2
    join2 <- dplyr::left_join(join1, edge_options$custom, by = c("tag" = "group"), suffix = c("_older", ""))
    # find var names diff
    var_names <- colnames(edge_options$custom)[colnames(edge_options$custom) %in% colnames(edge_options$edge) & colnames(edge_options$custom) != "group"]
    # fall back
    join2[,var_names] <- map(var_names, function(x){ifelse(is.na(join2[,x]), join2[,paste(x, "older", sep = "_")], join2[,x])})
    edge_df <- join2
  } else if (is.data.frame(edge_options)) {
    edge_df <- dplyr::left_join(edge_df, edge_options, by = "group", suffix = c("_orig", ""))
  } else if (!is.null(edge_options)) {
    edge_opts <- map_dfc(edge_options, function(x) rep(x, nrow(coefs)))
  }

  if (exists("edge_opts")) {
    bind_cols(edge_df, edge_opts)
  } else {
    edge_df
  }
}

#' Generates standard significance stars
#'
#' @param pvals a vector of p values
sig_stars <- function(pvals){

  if(pvals <= 0.001){
    star = "***"
  } else if (pvals <= 0.01){
    star = "**"
  } else if (pvals <= 0.05){
    star = "*"
  } else {
    star = ""
  }
  star
}

#' Uses the diagrammeR functions to turn the ndf and edf into dot
#'
#' @param ndf A node data frame created by \code{create_nodes}
#' @param edf An edge data frame created by \code{create_edges}
#' @param graph_options a named list of graphviz graph attributes
#' @return DOT specification of model
#' @importFrom dplyr bind_rows
#' @importFrom purrr map2
#' @export
convert_graph <- function(ndf, edf, graph_options) {
  gr <- DiagrammeR::create_graph(ndf, edf, attr_theme = NULL)

  if(!is.null(graph_options)){
    gr_opts <- map2(graph_options, names(graph_options), function(x, names){c(attr = names, value = x, attr_type = "graph")}) %>%
        bind_rows()

    gr$global_attrs <- bind_rows(gr$global_attrs, gr_opts)
  }
  gr %>%
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
#' @param stand Should the coefficients being used be standardized coefficients
#' @param ... Additional arguments to be passed to \code{create_edges}
#' @return A string specifying the path diagram for \code{model}
#' @export
create_grviz <- function(model, labels = labels, include = include, graph_options = graph_options, node_options = node_options, edge_options = edge_options, stand = stand, ...) {
  coefs <- extract_coefs(model, include, stand)

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
#' @param stand Should the coefficients being used be standardized coefficients
#' @param ... Additional arguments to be passed to create_grviz for creating edges
#' @return A Diagrammer plot of the path diagram for \code{model}
#' @importFrom DiagrammeR grViz
#' @export
lavaanPlot2 <- function(model, labels = NULL, include = NULL, gr_viz = NULL, graph_options = NULL, node_options = NULL, edge_options = NULL, stand = FALSE, ...) {
  if (!is.null(gr_viz)) {
    grViz(gr_viz)
  } else {
    create_grviz(model, labels, include, graph_options, node_options, edge_options, stand, ...) %>%
      grViz()
  }
}
