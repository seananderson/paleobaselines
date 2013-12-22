#' Partial dependence plots using the caret output models
#' Building on Seth's gbm models and partial dependence plots.R code
#' Sean - 2013-05-22

# build the model:
# source("Caret run multiple models for interval 1 and predict interval 3.R")
# or load the model from the last run:
load("../Final data/gbm_dat.rda")

# model is in gbmFit1$finalModel

m <- gbmFit1$finalModel


