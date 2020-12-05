#' Saves a plot as a png
#'
#' @param plot plot object created by \code{lavaanPlot}
#' @param path Filename to save the image
#' @export
save_png <- function(plot, path){
  DiagrammeRsvg::export_svg(plot) %>%
    charToRaw() %>%
    rsvg::rsvg() %>%
    png::writePNG(path)
}


#' Embeds a plot into an rmarkdown pdf
#'
#' @param plot plot object created by \code{lavaanPlot}
#' @param path Filename to save the image
#' @export
embed_plot_pdf <- function(plot, path){
  DiagrammeRsvg::export_svg(plot) %>%
    charToRaw() %>%
    rsvg::rsvg_pdf(path)

  knitr::include_graphics(path)
}
