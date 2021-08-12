#' Extracts the paths from the lavaan model.
#'
#' @param fit A model fit object of class lavaan.
#' @param coefs whether or not to include significant path coefficient values in diagram
#' @param sig significance level for determining what significant paths are
#' @param stand Should the coefficients being used be standardized coefficients
#' @param covs Should model covariances be included in the diagram
#' @param stars a character vector indicating which parameters should include significance stars be included for regression paths, latent paths, or covariances. Include which of the 3 you want ("regress", "latent", "covs"), default is none.
#' @param digits A number indicating the desired number of digits for the coefficient values in the plot
#' @importFrom stringr str_replace_all
buildPaths <- function(fit, coefs = FALSE, sig = 1.00, stand = FALSE, covs = FALSE, stars = NULL, digits = 2){
  if(stand){
    ParTable <- lavaan::standardizedsolution(fit)
    ParTableAlt <- fit@ParTable
  } else {
    ParTable <- fit@ParTable
    ParTableAlt <- fit@ParTable
  }

  # get rid of . from variable names
  ParTable$lhs <- stringr::str_replace_all(fit@ParTable$lhs, pattern = "\\.", replacement = "")
  ParTable$rhs <- stringr::str_replace_all(fit@ParTable$rhs, pattern = "\\.", replacement = "")

  regress <- ParTable$op == "~"
  latent <- ParTable$op == "=~"
  #cov <- ParTable$op == "~~" & (ParTable$rhs %in% ParTable$lhs[latent | regress]) & (ParTable$rhs != ParTable$lhs)
  cov <- ParTable$op == "~~" & (ParTable$rhs != ParTable$lhs)


  zval_reg <- ParTableAlt$est[regress] / ParTableAlt$se[regress]
  pval_reg <- (1 - stats::pnorm(abs(zval_reg))) * 2
  signif_reg <- pval_reg < sig
  coef <- ifelse(signif_reg, round(ParTable$est[regress], digits = digits), "")

  zval_lat <- ParTableAlt$est[latent] / ParTableAlt$se[latent]
  pval_lat <- (1 - stats::pnorm(abs(zval_lat))) * 2
  signif_lat <- pval_lat < sig
  latent_coef <- ifelse(signif_lat, round(ParTable$est[latent], digits = digits), "")

  zval_cov <- ParTableAlt$est[cov] / ParTableAlt$se[cov]
  pval_cov <- (1 - stats::pnorm(abs(zval_cov))) * 2
  signif_cov <- pval_cov < sig
  cov_vals <- ifelse(signif_cov, round(ParTable$est[cov], digits = digits), "")

  if("regress" %in% stars){
    #pval_reg <- ParTable$pvalue[regress]
    stars_reg <- unlist(lapply(X = pval_reg, FUN = sig_stars))
  } else {
    stars_reg <- ""
  }

  if("latent" %in% stars){
    #pval_lat <- ParTable$pvalue[latent]
    stars_lat <- unlist(lapply(X = pval_lat, FUN = sig_stars))
  } else {
    stars_lat <- ""
  }

  if("covs" %in% stars){
    #pval_cov <- ParTable$pvalue[cov]
    stars_cov <- unlist(lapply(X = pval_cov, FUN = sig_stars))
  } else {
    stars_cov <- ""
  }

  #penwidths <- ifelse(coefs == "", 1, 2)
  if(any(regress)){
    if(coefs){
      regress_paths <- paste(paste(ParTable$rhs[regress], ParTable$lhs[regress], sep = "->"), paste("[label = '", coef, stars_reg, "']", sep = ""), collapse = " ")
    } else {
      regress_paths <- paste(paste(ParTable$rhs[regress], ParTable$lhs[regress], sep = "->"), collapse = " ")
    }
  } else {
  regress_paths <- ""
  }
  if(any(latent)) {
    if(coefs){
      latent_paths <- paste(paste(ParTable$lhs[latent], ParTable$rhs[latent], sep = "->"), paste("[label = '", latent_coef, stars_lat, "']", sep = ""), collapse = " ")
    } else {
      latent_paths <- paste(paste(ParTable$lhs[latent], ParTable$rhs[latent], sep = "->"), collapse = " ")
    }
  } else {
  latent_paths <- ""
  }
  if(any(cov)){
    if(covs){

      covVals <- round(ParTable$est[cov], digits = 2)
      if(coefs) {
      cov_paths <- paste(
        paste(
          ParTable$rhs[cov],
          ParTable$lhs[cov], sep = " -> "),
        paste("[label = '", cov_vals, stars_cov, "', dir = 'both']", sep = ""),
        collapse = " "
      )
    } else {
      cov_paths <- paste(
        paste(
          ParTable$rhs[cov],
          ParTable$lhs[cov], sep = " -> "),
        paste("[dir = 'both']", sep = ""),
        collapse = " "
      )
    }


    } else {
      cov_paths <- ""
    }
  } else {
    cov_paths <- ""
  }
  paste(regress_paths, latent_paths, cov_paths, sep = " ")
}

#' Extracts the paths from the lavaan model.
#'
#' @param fit A model fit object of class lavaan.
getNodes <- function(fit){
  # remove . from variable names
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

  # remove . from variable names
  observed_nodes <- stringr::str_replace_all(observed_nodes, pattern = "\\.", replacement = "")
  latent_nodes <- stringr::str_replace_all(latent_nodes, pattern = "\\.", replacement = "")

  list(observeds = observed_nodes, latents = latent_nodes)
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

#' Adds variable labels to the Diagrammer plot function call.
#'
#' @param label_list A named list of variable labels.
buildLabels <- function(label_list){
  names(label_list) <- stringr::str_replace_all(names(label_list), pattern = "\\.", replacement = "")
  labs <- paste(names(label_list), " [label = ", "'", label_list, "'", "]", sep = "")
  paste(labs, collapse = "\n")
}

#' Builds the Diagrammer function call.
#'
#' @param name A string of the name of the plot.
#' @param model A model fit object of class lavaan.
#' @param labels  An optional named list of variable labels fit object of class lavaan.
#' @param graph_options  A named list of graph options for Diagrammer syntax.
#' @param node_options  A named list of node options for Diagrammer syntax.
#' @param edge_options  A named list of edge options for Diagrammer syntax.
#' @param ... additional arguments to be passed to \code{buildPaths}
#' @return A string specifying the path diagram for \code{model}
buildCall <- function(name = name, model = model, labels = labels, graph_options = list(overlap = "true", fontsize = "10"), node_options = list(shape = "box"), edge_options = list(color = "black"), ...){
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
  string <- paste(string, buildPaths(model, ...))
  string <- paste(string, "}", sep = "\n")
  string
}

#' Plots lavaan path model with DiagrammeR
#'
#' @param name A string of the name of the plot.
#' @param model A model fit object of class lavaan.
#' @param labels  An optional named list of variable labels.
#' @param ... Additional arguments to be called to \code{buildCall} and \code{buildPaths}
#' @return A Diagrammer plot of the path diagram for \code{model}
#' @importFrom DiagrammeR grViz
#' @export
#' @examples
#' library(lavaan)
#' model <- 'mpg ~ cyl + disp + hp
#'           qsec ~ disp + hp + wt'
#' fit <- sem(model, data = mtcars)
#' lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"),
#'  edge_options = list(color = "grey"), coefs = FALSE)
lavaanPlot <- function(name = "plot", model, labels = NULL, ...){
  plotCall <- buildCall(name = name, model = model, labels = labels, ...)
  grViz(plotCall)
}

