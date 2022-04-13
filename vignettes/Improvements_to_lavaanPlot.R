## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(lavaan)
library(lavaanPlot)

model <- 'mpg ~ cyl + disp + hp
          qsec ~ disp + hp + wt'

fit <- sem(model, data = mtcars)
summary(fit)

## -----------------------------------------------------------------------------
lavaanPlot2(fit)

## -----------------------------------------------------------------------------
labels <- c(mpg = "Miles Per Gallon", cyl = "Cylinders", disp = "Displacement", hp = "Horsepower", qsec = "Speed", wt = "Weight")

lavaanPlot2(fit, labels = labels)

## -----------------------------------------------------------------------------
lavaanPlot2(fit, labels = labels, graph_options = list(title = "my first graph", rankdir = "LR"), node_options = list( fontname = "Helvetica"), edge_options = list(color = "grey"))

## -----------------------------------------------------------------------------
lavaanPlot2(fit, include = "covs", labels = labels, graph_options = list(title = "my first graph"), node_options = list( fontname = "Helvetica"), edge_options = list(color = "grey"))

lavaanPlot2(fit, include = "all", labels = labels, graph_options = list(title = "my first graph"), node_options = list( fontname = "Helvetica"), edge_options = list(color = "grey"))

## -----------------------------------------------------------------------------
lavaanPlot2(fit, include = "covs", coef_labels = TRUE, labels = labels, graph_options = list(title = "my first graph"), node_options = list( fontname = "Helvetica"), edge_options = list(color = "grey"))

## -----------------------------------------------------------------------------
# this one is still buggy
lavaanPlot2(fit, include = "covs", labels = labels, graph_options = list(title = "my first graph"), node_options = list( fontname = "Helvetica"), edge_options = list(color = "grey"), stars = c("regress"))

