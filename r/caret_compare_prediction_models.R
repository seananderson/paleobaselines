# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Sep 18, 2012
# Last modified: Sep 19, 2012
# Purpose:       Try using the caret package to figure out the best
# predictive model and the best tuning parameters.
# ====================================================================

library(randomForest)
library(party)
library(plyr)
library(ROCR)
library(caret)

data <- load("data_for_caret_20120918.rda")
data1factors$probabilities <- NULL

rf_weights <- randomForest(ext ~.,data = data1factors, classwt = c(ExWeight,SurWeight))
rf_noweights <- randomForest(ext ~.,data = data1factors)

weights <- ifelse(data1factors$ext=="extinct",ExWeight,SurWeight)

# parallel processing:
library(doMC)
registerDoMC(2)

fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           ## repeated three times
                           repeats = 2,
                           ## Save all the resampling results
                           returnResamp = "all",
                           ## twoClassSummary computes sensitivity,
                           ## specificity and the area under the ROC
                           ## curve. To use this function, the
                           ## classProbs argument of trainControl
                           ## should be TRUE. 
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary
                           )

                           ## "in problems where there are a low
                           ## percentage of samples in one class,
                           ## using metric = "Kappa" can improve
                           ## quality of the final model."
                           ## caretTrain.pdf


gbmFit1 <- train(data1factors[,-8], data1factors[,8],
                  method = "gbm",
                  trControl = fitControl,
                  #metric = "Kappa",
                  verbose = FALSE, 
                  weights = weights)

rfFit1 <- train(data1factors[,-8], data1factors[,8],
                  method = "rf",
                  #metric = "Kappa",
                  trControl = fitControl, weights = weights)

cfFit1 <- train(data1factors[,-8], data1factors[,8],
                  method = "cforest",
                  #metric = "Kappa",
                  trControl = fitControl, weights = weights)


extModels <- list(rf = rfFit1, gbm = gbmFit1, cf = cfFit1)

# predict from multiple models at once:
#extPred <- predict(extModels)
extPred <- extractPrediction(list(rf = rfFit1, cf = cfFit1, gbm = gbmFit1))

with(subset(extPred, object == "rf"), confusionMatrix(pred, obs))
with(subset(extPred, object == "cf"), confusionMatrix(pred, obs))
with(subset(extPred, object == "gbm"), confusionMatrix(pred, obs))

extProb <- extractProb(list(rf = rfFit1, cf = cfFit1, gbm = gbmFit1))

# ddply(testPred, .(model), defaultSummary)

pdf("ROC_learning.pdf", width = 8, height = 3)
par(mfrow = c(1, 3))
d_ply(extProb, .(model), transform, {
      x <- roc(as.numeric(obs), extinct, plot = TRUE, main = unique(object), yaxs = "i", xaxs = "i")
      mtext(paste("AUC =", round(x$auc, 2)), side = 1)
                  })
dev.off()


pdf("class_probabilities_learning.pdf", width = 5, height = 8)
plotClassProbs(extProb)
dev.off()

