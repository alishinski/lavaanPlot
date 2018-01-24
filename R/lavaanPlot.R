#' Extracts the paths from the lavaan model.
#'
#' @param fit A model fit object of class lavaan.
#' @param coefs whether or not to include significant path coefficient values in diagram
#' @param sig significance level for determining what significant paths are
#' @param stand Should the coefficients being used be standardized coefficients
buildPaths <- function(fit, coefs = coefs, sig = sig, stand = stand){
  if(stand){
    ParTable <- lavaan::standardizedsolution(fit)
  } else {
    ParTable <- fit@ParTable
  }
  regress <- ParTable$op == "~"
  latent <- ParTable$op == "=~"
  zval_reg <- ParTable$est[regress] / ParTable$se[regress]
  pval_reg <- (1 - stats::pnorm(abs(zval_reg))) * 2
  signif_reg <- pval_reg < sig
  coef <- ifelse(signif_reg, round(ParTable$est[regress], digits = 2), "")
  #penwidths <- ifelse(coefs == "", 1, 2)
  if(any(regress)){
    if(coefs){
      regress_paths <- paste(paste(ParTable$rhs[regress], ParTable$lhs[regress], sep = "->"), paste("[label = '", coef, "']", sep = ""), collapse = " ")
    } else {
      regress_paths <- paste(paste(ParTable$rhs[regress], ParTable$lhs[regress], sep = "->"), collapse = " ")
    }
  } else {
  regress_paths <- ""
  }
  if(any(latent)) {
  latent_paths <- paste(paste(ParTable$rhs[latent], ParTable$lhs[latent], sep = "->"), collapse = " ")
  } else {
  latent_paths <- ""
  }
  paste(regress_paths, latent_paths, sep = " ")
}

#' Extracts the paths from the lavaan model.
#'
#' @param fit A model fit object of class lavaan.
getNodes <- function(fit){
  regress <- fit@ParTable$op == "~"
  latent <- fit@ParTable$op == "=~"
  observed_nodes <- c()
  latent_nodes <- c()
  if(any(regress)){
    observed_nodes <- c(observed_nodes, unique(fit@ParTable$rhs[regress]))
    observed_nodes <- c(observed_nodes, unique(fit@ParTable$lhs[regress]))
  }
  if(any(latent)) {
    observed_nodes <- c(observed_nodes, unique(fit@ParTable$rhs[latent]))
    latent_nodes <- c(latent_nodes, unique(fit@ParTable$lhs[latent]))
  }
  # make sure latent variables don't show up in both
  observed_nodes <- setdiff(observed_nodes, latent_nodes)
  list(observeds = observed_nodes, latents = latent_nodes)
}

#' Adds variable labels to the Diagrammer plot function call.
#'
#' @param label_list A named list of variable labels.
buildLabels <- function(label_list){
  labs <- paste(names(label_list), " [label = ", "'", label_list, "'", "]", sep = "")
  paste(labs, collapse = "\n")
}

#' Builds the Diagrammer function call.
#'
#' @param name A string of the name of the plot.
#' @param model A model fit object of class lavaan.
#' @param labels  An optionalnamed list of variable labels fit object of class lavaan.
#' @param graph_options  A named list of graph options for Diagrammer syntax.
#' @param node_options  A named list of node options for Diagrammer syntax.
#' @param edge_options  A named list of edge options for Diagrammer syntax.
#' @param coefs whether or not to include significant path coefficient values in diagram
#' @param sig significance level for determining what significant paths are
#' @param stand Should the coefficients being used be standardized coefficients
#' @return A string specifying the path diagram for \code{model}
buildCall <- function(name = "plot", model, labels = NULL, graph_options, node_options, edge_options, coefs = coefs, sig = sig, stand = stand){
  string <- ""
  string <- paste(string, "digraph", name, "{")
  string <- paste(string, "\n")
  string <- paste(string, "graph", "[",  paste(paste(names(graph_options), graph_options, sep = " = "), collapse = ", "), "]")
  string <- paste(string, "\n")
  string <- paste(string, "node", "[", paste(paste(names(node_options), node_options, sep = " = "), collapse = ", "), "]")
  string <- paste(string, "\n")
  nodes <- getNodes(model)
  string <- paste(string, "node [shape = box] \n")
  string <- paste(string, paste(nodes$observeds, collapse = "; "))
  string <- paste(string, "\n")
  string <- paste(string, "node [shape = oval] \n")
  string <- paste(string, paste(nodes$latents, collapse = "; "))
  string <- paste(string, "\n")
  if(!is.null(labels)){
    labels_string = buildLabels(labels)
    string <- paste(string, labels_string)
  }
  string <- paste(string, "\n")
  string <- paste(string, "edge", "[", paste(paste(names(edge_options), edge_options, sep = " = "), collapse = ", "), "]")
  string <- paste(string, "\n")
  string <- paste(string, buildPaths(model, coefs = coefs, sig = sig, stand = stand))
  string <- paste(string, "}", sep = "\n")
  string
}

#' Plots lavaan path model with DiagrammeR
#'
#' @param name A string of the name of the plot.
#' @param model A model fit object of class lavaan.
#' @param labels  An optional named list of variable labels fit object of class lavaan.
#' @param graph_options  A named list of graph options for Diagrammer syntax, default provided.
#' @param node_options  A named list of node options for Diagrammer syntax, default provided.
#' @param edge_options  A named list of edge options for Diagrammer syntax., default provided.
#' @param coefs whether or not to include significant path coefficient values in diagram
#' @param sig significance level for determining what significant paths are
#' @param stand Should the coefficients being used be standardized coefficients
#' @return A Diagrammer plot of the path diagram for \code{model}
#' @import DiagrammeR
#' @export
lavaanPlot <- function(name = "plot", model, labels = NULL, graph_options = list(overlap = "true", fontsize = "10"), node_options = list(shape = "box"), edge_options = list(color = "black"), coefs = FALSE, sig = 0.05, stand = TRUE){
  plotCall <- buildCall(name = name, model = model, labels = labels, graph_options = graph_options, node_options = node_options, edge_options = edge_options, coefs = coefs, sig = sig, stand = stand)
  grViz(plotCall)
}

