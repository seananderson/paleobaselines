


#### ATTENTION: THIS SCRIPT HAS BEEN SUPERCEDED BY "gbm-models-and-partial-dependence-plots2.r"  !!

predict_models <- function(x) {

  require(caret)
  require(gbm)
  require(gdata)




data <- load("~/Dropbox/nescent_extinction_map/Final data/standardized.predictors.Cenozoic.OBIS.rda")  
data <- standardized.cenozoic  
data1 <- drop.levels(subset(data,data$stage != "Spalding_raw" & data$stage != "Spalding_merged"))
predict_data <- drop.levels(subset(data,data$stage == "Spalding_merged"))


load("~/Dropbox/nescent_extinction_map/Final data/caret.gbm.models.rda")
train.model <- caret.gbm.models[[14]]

stage <- predict_data$stage
stage_top <- predict_data$stage_top
genus <- predict_data$genus
group <- as.factor(predict_data$group)

drops <- c("stage","stage_top","genus","group","Ex")


predict_data <- predict_data[,!(names(predict_data) %in% drops)]
predict_data <- data.frame(predict_data,group)

preds <- data.frame(group,genus,predict(train.model, newdata = predict_data, type="response",n.trees = train.model$n.trees))

write.table(preds,"~/Dropbox/nescent_extinction_map/Final data/Extinction_risk_predictions.csv",sep=",")









  # make vector of weights for train_data
obs <- length(train_data$Ex)
exes <- sum(train_data$Ex)
ExFreq <- (exes/obs)
SurFreq <- (1-(exes/obs))
MaxFreq <- max(ExFreq,SurFreq)
ExWeight <- 1/(ExFreq/MaxFreq)
SurWeight <- 1/(SurFreq/MaxFreq)
weights <- ifelse(train_data$Ex==1,ExWeight,SurWeight)

  # parallel processing:
  require(doMC)
  registerDoMC(2)
  # set up training parameters for models
  fitControl <- caret::trainControl(## 10-fold CV
    method = "repeatedcv",
    number = 10,
    repeats = 5,
    ## Save all the resampling results
    returnResamp = "all",
    ## twoClassSummary computes sensitivity,
    ## specificity and the area under the ROC
    ## curve. To use this function, the
    ## classProbs argument of trainControl
    ## should be TRUE. 
    classProbs = TRUE,
    allowParallel = TRUE,
    #summaryFunction = twoClassSummary
    )

  ext <- as.factor(ifelse(train_data$Ex == 1,"extinct","survive"))
  drops <- c("Ex")
  train_data2 <- train_data[,!(names(train_data) %in% drops)]
  predict_data2 <- predict_data[,!(names(predict_data) %in% drops)]
  train_data2$group <- factor(train_data2$group)
  predict_data2$group <- factor(predict_data2$group)

  gbmFit1 <- caret::train(x         = train_data2, 
                          y         = ext,
                          method    = "gbm",
                          trControl = fitControl,
                          metric    = "Kappa",
                          verbose   = FALSE,
                          weights   = weights
                          )

  # grab the gbm model:
  m <- gbmFit1$finalModel
  
preds <- data.frame(genus,predict(m, newdata = predict_data2, type="response",n.trees = m$n.trees))

write.table(preds,"~/Dropbox/nescent_extinction_map/Final data/Extinction_risk_predictions.csv",sep=",")
