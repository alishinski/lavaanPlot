#' Saves a plot as a png
#'
#' @param plot plot object created by \code{lavaanPlot}
#' @param path filename to save the image
#' @param width width of image in pixels, NULL for default
#' @param height height of image, NULL for default
#' @return no return value saves plot as png
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#' library(lavaan)
#' model <- 'mpg ~ cyl + disp + hp
#'           qsec ~ disp + hp + wt'
#' fit <- sem(model, data = mtcars)
#' pl <- lavaanPlot(model = fit)
#' \dontrun{
#' save_png(pl, "plot.png")
#' }
save_png <- function(plot, path, width = NULL, height = NULL){
  if (!requireNamespace("DiagrammeRsvg", quietly = TRUE)) {
    stop("Package \"DiagrammeRsvg\" needed for this function to work. Please install it.",
         call. = FALSE)
  } else if (!requireNamespace("rsvg", quietly = TRUE)) {
    stop("Package \"rsvg\" needed for this function to work. Please install it.",
         call. = FALSE)
  } else if (!requireNamespace("png", quietly = TRUE)) {
    stop("Package \"png\" needed for this function to work. Please install it.",
         call. = FALSE)
  } else {
    DiagrammeRsvg::export_svg(plot) %>%
      charToRaw() %>%
      rsvg::rsvg(width = width, height = height) %>%
      png::writePNG(path)
  }
}

#' Embeds a plot into an rmarkdown pdf
#'
#' @param plot plot object created by \code{lavaanPlot}
#' @param path Filename to save the image
#' @param width width of image in pixels, NULL for default
#' @param height height of image, NULL for default
#' @return no return value calls \code{include_graphics} to embed plot in pdf
#' @export
#' @importFrom magrittr "%>%"
#' @examples
#' library(lavaan)
#' model <- 'mpg ~ cyl + disp + hp
#'           qsec ~ disp + hp + wt'
#' fit <- sem(model, data = mtcars)
#' pl <- lavaanPlot(model = fit)
#' \dontrun{
#' embed_plot_pdf(pl, "plot2.pdf")
#' }
embed_plot_pdf <- function(plot, path, width = NULL, height = NULL){
  if (!requireNamespace("DiagrammeRsvg", quietly = TRUE)) {
    stop("Package \"DiagrammeRsvg\" needed for this function to work. Please install it.",
         call. = FALSE)
  } else if (!requireNamespace("rsvg", quietly = TRUE)) {
    stop("Package \"rsvg\" needed for this function to work. Please install it.",
         call. = FALSE)
  } else if (!requireNamespace("png", quietly = TRUE)) {
    stop("Package \"png\" needed for this function to work. Please install it.",
         call. = FALSE)
  } else {
    DiagrammeRsvg::export_svg(plot) %>%
      charToRaw() %>%
      rsvg::rsvg_pdf(path, width = width, height = height)

    knitr::include_graphics(path)
  }
}







