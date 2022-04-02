library(testthat)

library(lavaan)
library(lavaanPlot)

model <- 'mpg ~ cyl + disp + hp
qsec ~ disp + hp + wt'

fit <- sem(model, data = mtcars)
#summary(fit)

plot1 <- lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = FALSE)

plot1$x$diagram

plot1_ref <- " digraph plot { \n graph [ overlap = true, fontsize = 10 ] \n node [ shape = box, fontname = Helvetica ] \n node [shape = box] \n cyl; disp; hp; wt; mpg; qsec \n node [shape = oval] \n  \n \n edge [ color = grey ] \n cyl->mpg disp->mpg hp->mpg disp->qsec hp->qsec wt->qsec  \n}"

test_that("plot1", {
  expect_identical(plot1$x$diagram, plot1_ref)
})

## Plot customization options

labels <- list(mpg = "Miles Per Gallon", cyl = "Cylinders", disp = "Displacement", hp = "Horsepower", qsec = "Speed", wt = "Weight")
plot2 <- lavaanPlot(model = fit, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = FALSE)


plot2_ref <- " digraph plot { \n graph [ overlap = true, fontsize = 10 ] \n node [ shape = box, fontname = Helvetica ] \n node [shape = box] \n cyl; disp; hp; wt; mpg; qsec \n node [shape = oval] \n  \n mpg [label = \"Miles Per Gallon\"]\ncyl [label = \"Cylinders\"]\ndisp [label = \"Displacement\"]\nhp [label = \"Horsepower\"]\nqsec [label = \"Speed\"]\nwt [label = \"Weight\"] \n edge [ color = grey ] \n cyl->mpg disp->mpg hp->mpg disp->qsec hp->qsec wt->qsec  \n}"


test_that("plot2", {
  expect_identical(plot2$x$diagram, plot2_ref)
})

### latents

HS.model <- ' visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data=HolzingerSwineford1939)
plot3 <- lavaanPlot(model = fit, edge_options = list(color = "grey"))

plot3_ref <- " digraph plot { \n graph [ overlap = true, fontsize = 10 ] \n node [ shape = box ] \n node [shape = box] \n x1; x2; x3; x4; x5; x6; x7; x8; x9 \n node [shape = oval] \n visual; textual; speed \n \n edge [ color = grey ] \n  visual->x1 visual->x2 visual->x3 textual->x4 textual->x5 textual->x6 speed->x7 speed->x8 speed->x9 \n}"

test_that("plot3", {
  expect_identical(plot3$x$diagram, plot3_ref)
})


### coef labels

model <- 'mpg ~ cyl + disp + hp
qsec ~ disp + hp + wt'

fit <- sem(model, data = mtcars)
#summary(fit)

plot4 <- lavaanPlot(model = fit, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE)

plot4_ref <- " digraph plot { \n graph [ overlap = true, fontsize = 10 ] \n node [ shape = box, fontname = Helvetica ] \n node [shape = box] \n cyl; disp; hp; wt; mpg; qsec \n node [shape = oval] \n  \n mpg [label = \"Miles Per Gallon\"]\ncyl [label = \"Cylinders\"]\ndisp [label = \"Displacement\"]\nhp [label = \"Horsepower\"]\nqsec [label = \"Speed\"]\nwt [label = \"Weight\"] \n edge [ color = grey ] \n cyl->mpg [label = \"-0.99\"] disp->mpg [label = \"-0.02\"] hp->mpg [label = \"-0.02\"] disp->qsec [label = \"-0.01\"] hp->qsec [label = \"-0.02\"] wt->qsec [label = \"1.69\"]  \n}"

test_that("plot4", {
  expect_identical(plot4$x$diagram, plot4_ref)
})

# significant standardized paths only
plot5 <- lavaanPlot(model = fit, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, sig = .05)

plot5_ref <- " digraph plot { \n graph [ overlap = true, fontsize = 10 ] \n node [ shape = box, fontname = Helvetica ] \n node [shape = box] \n cyl; disp; hp; wt; mpg; qsec \n node [shape = oval] \n  \n mpg [label = \"Miles Per Gallon\"]\ncyl [label = \"Cylinders\"]\ndisp [label = \"Displacement\"]\nhp [label = \"Horsepower\"]\nqsec [label = \"Speed\"]\nwt [label = \"Weight\"] \n edge [ color = grey ] \n cyl->mpg [label = \"\"] disp->mpg [label = \"-0.02\"] hp->mpg [label = \"\"] disp->qsec [label = \"-0.01\"] hp->qsec [label = \"-0.02\"] wt->qsec [label = \"1.69\"]  \n}"

test_that("plot5", {
  expect_identical(plot5$x$diagram, plot5_ref)
})

# All paths unstandardized
plot6 <- lavaanPlot(model = fit, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, stand = TRUE)

plot6_ref <- " digraph plot { \n graph [ overlap = true, fontsize = 10 ] \n node [ shape = box, fontname = Helvetica ] \n node [shape = box] \n cyl; disp; hp; wt; mpg; qsec \n node [shape = oval] \n  \n mpg [label = \"Miles Per Gallon\"]\ncyl [label = \"Cylinders\"]\ndisp [label = \"Displacement\"]\nhp [label = \"Horsepower\"]\nqsec [label = \"Speed\"]\nwt [label = \"Weight\"] \n edge [ color = grey ] \n cyl->mpg [label = \"-0.29\"] disp->mpg [label = \"-0.44\"] hp->mpg [label = \"-0.19\"] disp->qsec [label = \"-0.56\"] hp->qsec [label = \"-0.85\"] wt->qsec [label = \"0.91\"]  \n}"

test_that("plot6", {
  expect_identical(plot6$x$diagram, plot6_ref)
})

### latent vars

HS.model <- ' visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
'

fit <- cfa(HS.model, data=HolzingerSwineford1939)
labels = list(visual = "Visual Ability", textual = "Textual Ability", speed = "Speed Ability")

# Show coefs
plot7 <- lavaanPlot(model = fit, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE)

plot7_ref <- " digraph plot { \n graph [ overlap = true, fontsize = 10 ] \n node [ shape = box, fontname = Helvetica ] \n node [shape = box] \n x1; x2; x3; x4; x5; x6; x7; x8; x9 \n node [shape = oval] \n visual; textual; speed \n visual [label = \"Visual Ability\"]\ntextual [label = \"Textual Ability\"]\nspeed [label = \"Speed Ability\"] \n edge [ color = grey ] \n  visual->x1 [label = \"1\"] visual->x2 [label = \"0.55\"] visual->x3 [label = \"0.73\"] textual->x4 [label = \"1\"] textual->x5 [label = \"1.11\"] textual->x6 [label = \"0.93\"] speed->x7 [label = \"1\"] speed->x8 [label = \"1.18\"] speed->x9 [label = \"1.08\"] \n}"

test_that("plot7", {
  expect_identical(plot7$x$diagram, plot7_ref)
})

# Significant paths
plot8 <- lavaanPlot(model = fit, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, sig = .05)

plot8_ref <- " digraph plot { \n graph [ overlap = true, fontsize = 10 ] \n node [ shape = box, fontname = Helvetica ] \n node [shape = box] \n x1; x2; x3; x4; x5; x6; x7; x8; x9 \n node [shape = oval] \n visual; textual; speed \n visual [label = \"Visual Ability\"]\ntextual [label = \"Textual Ability\"]\nspeed [label = \"Speed Ability\"] \n edge [ color = grey ] \n  visual->x1 [label = \"1\"] visual->x2 [label = \"0.55\"] visual->x3 [label = \"0.73\"] textual->x4 [label = \"1\"] textual->x5 [label = \"1.11\"] textual->x6 [label = \"0.93\"] speed->x7 [label = \"1\"] speed->x8 [label = \"1.18\"] speed->x9 [label = \"1.08\"] \n}"

test_that("plot8", {
  expect_identical(plot8$x$diagram, plot8_ref)
})

# All paths standardized
plot9 <- lavaanPlot(model = fit, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, stand = TRUE)

plot9_ref <- " digraph plot { \n graph [ overlap = true, fontsize = 10 ] \n node [ shape = box, fontname = Helvetica ] \n node [shape = box] \n x1; x2; x3; x4; x5; x6; x7; x8; x9 \n node [shape = oval] \n visual; textual; speed \n visual [label = \"Visual Ability\"]\ntextual [label = \"Textual Ability\"]\nspeed [label = \"Speed Ability\"] \n edge [ color = grey ] \n  visual->x1 [label = \"0.77\"] visual->x2 [label = \"0.42\"] visual->x3 [label = \"0.58\"] textual->x4 [label = \"0.85\"] textual->x5 [label = \"0.86\"] textual->x6 [label = \"0.84\"] speed->x7 [label = \"0.57\"] speed->x8 [label = \"0.72\"] speed->x9 [label = \"0.67\"] \n}"

test_that("plot9", {
  expect_identical(plot9$x$diagram, plot9_ref)
})

### covariances
HS.model <- ' visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data=HolzingerSwineford1939)
labels = list(visual = "Visual Ability", textual = "Textual Ability", speed = "Speed Ability")

# significant standardized paths only
plot10 <- lavaanPlot(model = fit, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE)

plot10_ref <- " digraph plot { \n graph [ overlap = true, fontsize = 10 ] \n node [ shape = box, fontname = Helvetica ] \n node [shape = box] \n x1; x2; x3; x4; x5; x6; x7; x8; x9 \n node [shape = oval] \n visual; textual; speed \n visual [label = \"Visual Ability\"]\ntextual [label = \"Textual Ability\"]\nspeed [label = \"Speed Ability\"] \n edge [ color = grey ] \n  visual->x1 [label = \"1\"] visual->x2 [label = \"0.55\"] visual->x3 [label = \"0.73\"] textual->x4 [label = \"1\"] textual->x5 [label = \"1.11\"] textual->x6 [label = \"0.93\"] speed->x7 [label = \"1\"] speed->x8 [label = \"1.18\"] speed->x9 [label = \"1.08\"] textual -> visual [label = \"0.41\", dir = \"both\"] speed -> visual [label = \"0.26\", dir = \"both\"] speed -> textual [label = \"0.17\", dir = \"both\"]\n}"

test_that("plot10", {
  expect_identical(plot10$x$diagram, plot10_ref)
})

### stars
plot11 <- lavaanPlot(model = fit, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = "covs")

plot11_ref <- " digraph plot { \n graph [ overlap = true, fontsize = 10 ] \n node [ shape = box, fontname = Helvetica ] \n node [shape = box] \n x1; x2; x3; x4; x5; x6; x7; x8; x9 \n node [shape = oval] \n visual; textual; speed \n visual [label = \"Visual Ability\"]\ntextual [label = \"Textual Ability\"]\nspeed [label = \"Speed Ability\"] \n edge [ color = grey ] \n  visual->x1 [label = \"1\"] visual->x2 [label = \"0.55\"] visual->x3 [label = \"0.73\"] textual->x4 [label = \"1\"] textual->x5 [label = \"1.11\"] textual->x6 [label = \"0.93\"] speed->x7 [label = \"1\"] speed->x8 [label = \"1.18\"] speed->x9 [label = \"1.08\"] textual -> visual [label = \"0.41***\", dir = \"both\"] speed -> visual [label = \"0.26***\", dir = \"both\"] speed -> textual [label = \"0.17***\", dir = \"both\"]\n}"

test_that("plot11", {
  expect_identical(plot11$x$diagram, plot11_ref)
})

plot12 <- lavaanPlot(model = fit, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = "latent")

plot12_ref <- " digraph plot { \n graph [ overlap = true, fontsize = 10 ] \n node [ shape = box, fontname = Helvetica ] \n node [shape = box] \n x1; x2; x3; x4; x5; x6; x7; x8; x9 \n node [shape = oval] \n visual; textual; speed \n visual [label = \"Visual Ability\"]\ntextual [label = \"Textual Ability\"]\nspeed [label = \"Speed Ability\"] \n edge [ color = grey ] \n  visual->x1 [label = \"1***\"] visual->x2 [label = \"0.55***\"] visual->x3 [label = \"0.73***\"] textual->x4 [label = \"1***\"] textual->x5 [label = \"1.11***\"] textual->x6 [label = \"0.93***\"] speed->x7 [label = \"1***\"] speed->x8 [label = \"1.18***\"] speed->x9 [label = \"1.08***\"] textual -> visual [label = \"0.41\", dir = \"both\"] speed -> visual [label = \"0.26\", dir = \"both\"] speed -> textual [label = \"0.17\", dir = \"both\"]\n}"

test_that("plot12", {
  expect_identical(plot12$x$diagram, plot12_ref)
})

plot13 <- lavaanPlot(model = fit, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = TRUE, digits = 1)

plot13_ref <- " digraph plot { \n graph [ overlap = true, fontsize = 10 ] \n node [ shape = box, fontname = Helvetica ] \n node [shape = box] \n x1; x2; x3; x4; x5; x6; x7; x8; x9 \n node [shape = oval] \n visual; textual; speed \n visual [label = \"Visual Ability\"]\ntextual [label = \"Textual Ability\"]\nspeed [label = \"Speed Ability\"] \n edge [ color = grey ] \n  visual->x1 [label = \"1\"] visual->x2 [label = \"0.6\"] visual->x3 [label = \"0.7\"] textual->x4 [label = \"1\"] textual->x5 [label = \"1.1\"] textual->x6 [label = \"0.9\"] speed->x7 [label = \"1\"] speed->x8 [label = \"1.2\"] speed->x9 [label = \"1.1\"] textual -> visual [label = \"0.4\", dir = \"both\"] speed -> visual [label = \"0.3\", dir = \"both\"] speed -> textual [label = \"0.2\", dir = \"both\"]\n}"

test_that("plot13", {
  expect_identical(plot13$x$diagram, plot13_ref)
})


### rankdir
plot14 <- lavaanPlot(model = fit, labels = labels, graph_options = list(rankdir = "LR"), node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = TRUE, digits = 1)

plot14_ref <- " digraph plot { \n graph [ rankdir = LR ] \n node [ shape = box, fontname = Helvetica ] \n node [shape = box] \n x1; x2; x3; x4; x5; x6; x7; x8; x9 \n node [shape = oval] \n visual; textual; speed \n visual [label = \"Visual Ability\"]\ntextual [label = \"Textual Ability\"]\nspeed [label = \"Speed Ability\"] \n edge [ color = grey ] \n  visual->x1 [label = \"1\"] visual->x2 [label = \"0.6\"] visual->x3 [label = \"0.7\"] textual->x4 [label = \"1\"] textual->x5 [label = \"1.1\"] textual->x6 [label = \"0.9\"] speed->x7 [label = \"1\"] speed->x8 [label = \"1.2\"] speed->x9 [label = \"1.1\"] textual -> visual [label = \"0.4\", dir = \"both\"] speed -> visual [label = \"0.3\", dir = \"both\"] speed -> textual [label = \"0.2\", dir = \"both\"]\n}"

test_that("plot14", {
  expect_identical(plot14$x$diagram, plot14_ref)
})

plot15 <- lavaanPlot(model = fit, labels = labels, graph_options = list(rankdir = "RL"), node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = TRUE, digits = 1)

plot15_ref <- " digraph plot { \n graph [ rankdir = RL ] \n node [ shape = box, fontname = Helvetica ] \n node [shape = box] \n x1; x2; x3; x4; x5; x6; x7; x8; x9 \n node [shape = oval] \n visual; textual; speed \n visual [label = \"Visual Ability\"]\ntextual [label = \"Textual Ability\"]\nspeed [label = \"Speed Ability\"] \n edge [ color = grey ] \n  visual->x1 [label = \"1\"] visual->x2 [label = \"0.6\"] visual->x3 [label = \"0.7\"] textual->x4 [label = \"1\"] textual->x5 [label = \"1.1\"] textual->x6 [label = \"0.9\"] speed->x7 [label = \"1\"] speed->x8 [label = \"1.2\"] speed->x9 [label = \"1.1\"] textual -> visual [label = \"0.4\", dir = \"both\"] speed -> visual [label = \"0.3\", dir = \"both\"] speed -> textual [label = \"0.2\", dir = \"both\"]\n}"

test_that("plot15", {
  expect_identical(plot15$x$diagram, plot15_ref)
})

plot16 <- lavaanPlot(model = fit, labels = labels, graph_options = list(rankdir = "BT"), node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = TRUE, digits = 1)

plot16_ref <- " digraph plot { \n graph [ rankdir = BT ] \n node [ shape = box, fontname = Helvetica ] \n node [shape = box] \n x1; x2; x3; x4; x5; x6; x7; x8; x9 \n node [shape = oval] \n visual; textual; speed \n visual [label = \"Visual Ability\"]\ntextual [label = \"Textual Ability\"]\nspeed [label = \"Speed Ability\"] \n edge [ color = grey ] \n  visual->x1 [label = \"1\"] visual->x2 [label = \"0.6\"] visual->x3 [label = \"0.7\"] textual->x4 [label = \"1\"] textual->x5 [label = \"1.1\"] textual->x6 [label = \"0.9\"] speed->x7 [label = \"1\"] speed->x8 [label = \"1.2\"] speed->x9 [label = \"1.1\"] textual -> visual [label = \"0.4\", dir = \"both\"] speed -> visual [label = \"0.3\", dir = \"both\"] speed -> textual [label = \"0.2\", dir = \"both\"]\n}"

test_that("plot16", {
  expect_identical(plot16$x$diagram, plot16_ref)
})

### layouts
plot17 <- lavaanPlot(model = fit, labels = labels, graph_options = list(layout = "neato"), node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = TRUE, digits = 1)

plot17_ref <- " digraph plot { \n graph [ layout = neato ] \n node [ shape = box, fontname = Helvetica ] \n node [shape = box] \n x1; x2; x3; x4; x5; x6; x7; x8; x9 \n node [shape = oval] \n visual; textual; speed \n visual [label = \"Visual Ability\"]\ntextual [label = \"Textual Ability\"]\nspeed [label = \"Speed Ability\"] \n edge [ color = grey ] \n  visual->x1 [label = \"1\"] visual->x2 [label = \"0.6\"] visual->x3 [label = \"0.7\"] textual->x4 [label = \"1\"] textual->x5 [label = \"1.1\"] textual->x6 [label = \"0.9\"] speed->x7 [label = \"1\"] speed->x8 [label = \"1.2\"] speed->x9 [label = \"1.1\"] textual -> visual [label = \"0.4\", dir = \"both\"] speed -> visual [label = \"0.3\", dir = \"both\"] speed -> textual [label = \"0.2\", dir = \"both\"]\n}"

test_that("plot17", {
  expect_identical(plot17$x$diagram, plot17_ref)
})

plot18 <- lavaanPlot(model = fit, labels = labels, graph_options = list(layout = "circo"), node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = TRUE, digits = 1)

plot18_ref <- " digraph plot { \n graph [ layout = circo ] \n node [ shape = box, fontname = Helvetica ] \n node [shape = box] \n x1; x2; x3; x4; x5; x6; x7; x8; x9 \n node [shape = oval] \n visual; textual; speed \n visual [label = \"Visual Ability\"]\ntextual [label = \"Textual Ability\"]\nspeed [label = \"Speed Ability\"] \n edge [ color = grey ] \n  visual->x1 [label = \"1\"] visual->x2 [label = \"0.6\"] visual->x3 [label = \"0.7\"] textual->x4 [label = \"1\"] textual->x5 [label = \"1.1\"] textual->x6 [label = \"0.9\"] speed->x7 [label = \"1\"] speed->x8 [label = \"1.2\"] speed->x9 [label = \"1.1\"] textual -> visual [label = \"0.4\", dir = \"both\"] speed -> visual [label = \"0.3\", dir = \"both\"] speed -> textual [label = \"0.2\", dir = \"both\"]\n}"

test_that("plot18", {
  expect_identical(plot18$x$diagram, plot18_ref)
})

plot19 <- lavaanPlot(model = fit, labels = labels, graph_options = list(layout = "twopi"), node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = TRUE, digits = 1)

plot19_ref <- " digraph plot { \n graph [ layout = twopi ] \n node [ shape = box, fontname = Helvetica ] \n node [shape = box] \n x1; x2; x3; x4; x5; x6; x7; x8; x9 \n node [shape = oval] \n visual; textual; speed \n visual [label = \"Visual Ability\"]\ntextual [label = \"Textual Ability\"]\nspeed [label = \"Speed Ability\"] \n edge [ color = grey ] \n  visual->x1 [label = \"1\"] visual->x2 [label = \"0.6\"] visual->x3 [label = \"0.7\"] textual->x4 [label = \"1\"] textual->x5 [label = \"1.1\"] textual->x6 [label = \"0.9\"] speed->x7 [label = \"1\"] speed->x8 [label = \"1.2\"] speed->x9 [label = \"1.1\"] textual -> visual [label = \"0.4\", dir = \"both\"] speed -> visual [label = \"0.3\", dir = \"both\"] speed -> textual [label = \"0.2\", dir = \"both\"]\n}"

test_that("plot19", {
  expect_identical(plot19$x$diagram, plot19_ref)
})


