## ----setup, include = FALSE---------------------------------------------------
library(lavaan)
library(lavaanPlot)
library(rsvg)
library(magrittr)
library(DiagrammeRsvg)

## -----------------------------------------------------------------------------
model <- 'mpg ~ cyl + disp + hp
          qsec ~ disp + hp + wt'

fit <- sem(model, data = mtcars)

pl <- lavaanPlot(model = fit)


# Not run for documentation only works on pdf
#embed_plot_pdf(pl, "plot2.pdf")

save_png(pl, "plot.png")

