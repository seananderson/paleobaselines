
### clear all stored objects
rm(list = ls())
setwd("~/Dropbox/nescent_extinction_map/r")

### specify values for data standardization:
Min.Lat.Bin <- 10
Max.Lat.Bin <- 10
Lat.Range.Bin <- 10
Mean.Lat.Bin <- 10
Mean.Lat.Zone.Bin <- .5
Tropical.Only.Bin <- 1
Great.Circle.Bin <- 2000
### choose number of bins to use for standardizing richness, occurrences, occupancy
Num_Bins <- 10
### choose number of quantiles to use for standardizing extinction risk, if desired (see below)
Num_risk_quantiles <- 10
### choose "Interpolated" to use within-realm province interpolated data, otherwise choose "Raw"
Input_ranges <- "Interpolated"
Min_PBDB_Occurrences <- 2
Min_Modern_Occurrences <- 2
### Minimum number of genera for province to be plotted on Burrows and Halpern crossplots
Min.Prov.Genera <- 100
### Minimum duration (in stages) for a genera included: set "1" to include singletons, and "2" to include them
Minimum_Duration <- 1
### choose the minimum number of equal-area grid cells occupied for inclusion
Min_Occupancy <- 1
### choose the minimum number of PaleoDB localities for inclusion
Min_PBDB_Loc <- 1
### choose the minimum number of OBIS localities for inclusion
Min_OBIS_Loc <- 1
### choose the minimum number of Spalding provinces occupied for inclusion (applies only to modern genera)
Min_Prov <- 1
### set minimum great circle distance for inclusion (single occurrence PBDB genera assigned great.circle = 200)
Min_gcd <- 1

### specify model parameters:
##Metrics: "Kappa","ROC","Sens","Spec". If "Kappa", "summaryFunction = twoClassSummary" must be suppressed in "Produce gbm models for prediction2.R" object "fitControl"
Optimize_Metric <- "Kappa"
###Use weights to balance classes (extinct versus survive) in gbm models: "yes" or "no"
Use_Weights <- "yes"
### method for cross-validation.  I haven't experimented with this yet but the options are laid out in the Caret package documentation
CV_Method <- "repeatedcv"
Number_run <- 20
Number_rep <- 5
### choose prediction model.  Options are: Plio_Pleistocene,U_Miocene,M_Miocene,L_Miocene,Neogene.all,Neogene.mean
Prediction_model <- "Neogene.mean"
### set "rand.ex" = "yes" to randomize extinction/survival column, otherwise set to "no"
rand.ex <- "no"
### set "rand.group" = "yes" to randomize class and group columns, otherwise set to "no"
rand.group <- "no"

### specify plotting options
### choose extinction risk metric."Raw Risk" uses raw risk estimates, "Risk Quantile" uses quantiles (increment defined above),"Risk Rank" uses rank order
Extinction_metric <- "Raw Risk"
### set "plot.value" to "mean" for mean log.risk, "median" for median log.risk (this sets the estimator that is plotted, rather than the way risk is measured)
plot.value <- "mean"

### source commands for running all analyses
### use Standardize predictors by interval faster 2.0.R for non-quartile-based hybrid standardization. use Standardized predictors by interval.R for all predictors rescaled. Use Unstandardized predictors by interval.R for raw predictors

#Use Standardize predictors by interval faster 3.0.R for non-quartile-based standardization with Plio-Pleistocene vs. Modern calibrated corrections to range data, use Standardize predictors by interval faster no tranformation.R to use unstandardized data.

#source("~/Dropbox/nescent_extinction_map/r/Standardized predictors by interval.R")
#source("~/Dropbox/nescent_extinction_map/r/Standardize predictors by interval faster 3.0.R")
source("~/Dropbox/nescent_extinction_map/r/Standardize predictors by interval faster 2.0.R")
##source("~/Dropbox/nescent_extinction_map/r/Standardize predictors by interval faster.R")
source("~/Dropbox/nescent_extinction_map/r/Produce gbm models for prediction2.R")
# to calibrate the models by class-interval:
# source("~/Dropbox/nescent_extinction_map/r/calibrate-models.R")
source("~/Dropbox/nescent_extinction_map/r/Set model predictions for maps.R")
source("~/Dropbox/nescent_extinction_map/r/Make matrix of model predictions2.R")
source("~/Dropbox/nescent_extinction_map/r/Make figure 1 partial dependence plot 2.0.R")
source("~/Dropbox/nescent_extinction_map/r/make-figure1-partial-dependence-baseplot.r")
source("~/Dropbox/nescent_extinction_map/r/plot-class-ext-maps.R")
source("~/Dropbox/nescent_extinction_map/r/hotspotmaps3.R")
source("~/Dropbox/nescent_extinction_map/r/Make Burrows and Halpern crossplots.R")

# source("make-figure1-partial-dependence-baseplot.r") # once it's done
