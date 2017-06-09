
#' Extracts the paths from the lavaan model.
#'
#' @param fit A model fit object of class lavaan.
buildPaths <- function(fit){
  ops <- fit@ParTable$op == "~"
  paste(paste(fit@ParTable$rhs[ops], fit@ParTable$lhs[ops], sep = "->"), collapse = " ")
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
#' @param edge_options  A named list of edge options for Diagrammer syntax..
#' @return A string specifying the path diagram for \code{model}
buildCall <- function(name = "plot", model, labels = NULL, graph_options, node_options, edge_options){
  string <- ""
  string <- paste(string, "digraph", name, "{")
  string <- paste(string, "\n")
  string <- paste(string, "graph", "[",  paste(paste(names(graph_options), graph_options, sep = " = "), collapse = ", "), "]")
  string <- paste(string, "\n")
  string <- paste(string, "node", "[", paste(paste(names(node_options), node_options, sep = " = "), collapse = ", "), "]")
  string <- paste(string, "\n")
  if(!is.null(labels)){
    labels_string = buildLabels(labels)
    string <- paste(string, labels_string)
  }
  string <- paste(string, "\n")
  string <- paste(string, "edge", "[", paste(paste(names(edge_options), edge_options, sep = " = "), collapse = ", "), "]")
  string <- paste(string, "\n")
  string <- paste(string, buildPaths(model))
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
#' @return A Diagrammer plot of the path diagram for \code{model}
#' @import DiagrammeR
#' @export
lavaanPlot <- function(name = "plot", model, labels = NULL, graph_options = list(overlap = "true", fontsize = "10"), node_options = list(shape = "box"), edge_options = list(color = "black")){
  plotCall <- buildCall(name = name, model = model, labels = labels, graph_options = graph_options, node_options = node_options, edge_options = edge_options)
  grViz(plotCall)
}

