# parallel processing:
require(doMC)
registerDoMC(4)

CV_Method <- "repeatedcv"
Number_run <- 20 # number of folds
Number_rep <- 5 # number of times to repeat CV testing

fitControl <- trainControl(
  method = CV_Method,
  number = Number_run,
  repeats = Number_rep,
  returnResamp = "all",
  classProbs = TRUE)

gbmFit <- train(data2, ext,
  method    = "gbm",
  trControl = fitControl,
  metric    = Optimize_Metric,
  verbose   = FALSE,
  weights   = class_weights)
