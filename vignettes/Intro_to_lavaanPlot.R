## ------------------------------------------------------------------------
library(lavaan)

model <- 'mpg ~ cyl + disp + hp
          qsec ~ disp + hp + wt'

fit <- sem(model, data = mtcars)
summary(fit)

## ------------------------------------------------------------------------
library(lavaanPlot)

lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = F)

## ------------------------------------------------------------------------
labels <- list(mpg = "Miles Per Gallon", cyl = "Cylinders", disp = "Displacement", hp = "Horsepower", qsec = "Speed", wt = "Weight")

lavaanPlot(model = fit, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = F)

## ------------------------------------------------------------------------
HS.model <- ' visual  =~ x1 + x2 + x3      
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9 
'

fit <- cfa(HS.model, data=HolzingerSwineford1939)

lavaanPlot(model = fit, edge_options = list(color = "grey"))

## ------------------------------------------------------------------------
model <- 'mpg ~ cyl + disp + hp
          qsec ~ disp + hp + wt'

fit <- sem(model, data = mtcars)
summary(fit)

# significant standardized paths only
lavaanPlot(model = fit, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE)

# significant unstandardized paths
lavaanPlot(model = fit, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, stand = FALSE)

# All paths unstandardized
lavaanPlot(model = fit, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, stand = FALSE, sig = 1.00)

