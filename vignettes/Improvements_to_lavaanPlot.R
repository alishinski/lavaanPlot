## ----setup, include = FALSE---------------------------------------------------
# knitr::opts_chunk$set(
#   collapse = TRUE,
#   comment = "#>"
# )

## -----------------------------------------------------------------------------
library(lavaan)
library(lavaanPlot)

model <- 'mpg ~ cyl + disp + hp
          qsec ~ disp + hp + wt'

fit <- sem(model, data = mtcars)
summary(fit)

HS.model <- ' visual  =~ x1 + x2 + x3      
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9 
'

fit2 <- cfa(HS.model, data=HolzingerSwineford1939)
summary(fit2)

labels2 = c(visual = "Visual Ability", textual = "Textual Ability", speed = "Speed Ability")

## -----------------------------------------------------------------------------
lavaanPlot2(fit)

## -----------------------------------------------------------------------------
labels <- c(mpg = "Miles Per Gallon", cyl = "Cylinders", disp = "Displacement", hp = "Horsepower", qsec = "Speed", wt = "Weight")

lavaanPlot2(fit, labels = labels)

## -----------------------------------------------------------------------------
lavaanPlot2(fit, labels = labels, graph_options = list(label = "my first graph", rankdir = "LR"), node_options = list( fontname = "Helvetica"), edge_options = list(color = "grey"))

## -----------------------------------------------------------------------------
lavaanPlot2(fit, include = "covs", labels = labels, graph_options = list(label = "Including covariates"), node_options = list( fontname = "Helvetica"), edge_options = list(color = "grey"))

## -----------------------------------------------------------------------------
lavaanPlot2(fit, include = "all", labels = labels, graph_options = list(label = "including error variances"), node_options = list( fontname = "Helvetica"), edge_options = list(color = "grey"))

## -----------------------------------------------------------------------------
lavaanPlot2(fit, include = "covs", coef_labels = TRUE, labels = labels, graph_options = list(label = "including coefficient labels"), node_options = list(fontname = "Helvetica"), edge_options = list(color = "grey"))

## -----------------------------------------------------------------------------
lavaanPlot2(fit, include = "covs", labels = labels, graph_options = list(label = "my first graph with significance stars"), node_options = list( fontname = "Helvetica"), edge_options = list(color = "grey"), stars = c("regress"), coef_labels = TRUE)

## -----------------------------------------------------------------------------
lavaanPlot2(fit2, include = "covs", labels = labels2, graph_options = list(label = "my first graph with signficance stars"), node_options = list( fontname = "Helvetica"), edge_options = list(color = "grey"), stars = c("latent"), coef_labels = TRUE)

## -----------------------------------------------------------------------------
lavaanPlot2(fit2, include = "covs", labels = labels2, graph_options = list(label = "my first graph, which is being used to illustrate how to use the new code in the lavaanPlot package"), node_options = list( fontname = "Helvetica"), edge_options = list(color = "grey"), stars = c("covs"), coef_labels = TRUE)

