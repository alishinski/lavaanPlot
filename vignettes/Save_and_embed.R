## ----setup, include = FALSE, eval = FALSE-------------------------------------
#  library(lavaan)
#  library(lavaanPlot)
#  library(rsvg)
#  library(magrittr)
#  library(DiagrammeRsvg)

## ---- eval = FALSE------------------------------------------------------------
#  model <- 'mpg ~ cyl + disp + hp
#            qsec ~ disp + hp + wt'
#  
#  fit <- sem(model, data = mtcars)
#  
#  pl <- lavaanPlot(model = fit)
#  
#  # Example for pdf embed
#  embed_plot_pdf(pl, "plot2.pdf")
#  
#  # Example for saving to .png
#  save_png(pl, "plot.png")

