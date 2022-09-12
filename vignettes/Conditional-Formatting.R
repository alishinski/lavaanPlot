## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(lavaanPlot)
library(lavaan)
library(tidyverse)

## -----------------------------------------------------------------------------
HS.model <- ' visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
'

fit2 <- cfa(HS.model, data=HolzingerSwineford1939)
summary(fit2)
labels2 = c(visual = "Visual Ability", textual = "Textual Ability", speed = "Speed Ability")

## -----------------------------------------------------------------------------
lavaanPlot2(fit2, include = "covs", labels = labels2,
            graph_options = list(label = "my first graph with signficance stars"),
            node_options = list( fontname = "Helvetica"),
            edge_options = list(color = "grey"),
            stars = c("latent"),
            coef_labels = TRUE)


## -----------------------------------------------------------------------------
n_opts <- formatting(list(shape = "polygon", sides = "6", color = "orange"), list(shape = "polygon", sides = "8",color = "blue"), type = "node")

lavaanPlot2(fit2, include = "covs", labels = labels2,
            graph_options = list(label = "my first graph with signficance stars"),
            node_options = n_opts,
            edge_options = list(color = "grey"),
            stars = c("latent"),
            coef_labels = TRUE)

## -----------------------------------------------------------------------------
e_opts <- formatting(list(color = "orange"),list(color = "red", penwidth = 6), list(color = "blue"), type = "edge")

lavaanPlot2(fit2, include = "covs", labels = labels2,
            graph_options = list(label = "my first graph with signficance stars"),
            node_options = n_opts,
            edge_options = e_opts,
            stars = c("latent"),
            coef_labels = TRUE)

## -----------------------------------------------------------------------------
HS.model <- ' visual  =~ A*x1 + x2 + x3
textual =~ x4 + x5 + B*x6
speed   =~ x7 + x8 + x9
'

fit2 <- cfa(HS.model, data=HolzingerSwineford1939)

## -----------------------------------------------------------------------------
c_opts <- formatting(list(color = "yellow", penwidth = 8), list(color = "blue", penwidth = 10), type = "custom", groups = c("A", "B"))

lavaanPlot2(fit2, include = "covs", labels = labels2,
            graph_options = list(label = "my first graph with signficance stars"),
            node_options = n_opts,
            edge_options = list(e_opts, c_opts),
            stars = c("latent"),
            coef_labels = TRUE)

