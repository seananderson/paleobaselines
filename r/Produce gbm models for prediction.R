#source("/Users/Seth/Dropbox/nescent_extinction_map/r/Standardize predictors by interval faster.R")

run_ml_models <- function(x) {

  require(caret)
  require(gbm)
  require(gdata)

data <- drop.levels(subset(data,data$stage %in% x))

ext <- as.factor(ifelse(data$Ex == 1,"extinct","survive"))

 # make vector of weights
  obs <- length(data$Ex)
  exes <- sum(data$Ex)
  ExFreq <- (exes/obs)
  SurFreq <- (1-(exes/obs))
  MaxFreq <- max(ExFreq,SurFreq)
  ExWeight <- 1/(ExFreq/MaxFreq)
  SurWeight <- 1/(SurFreq/MaxFreq)
  weights <- ifelse(data$Ex==1,ExWeight,SurWeight)

drops <- c("stage","stage_top","genus","Ex")
data2 <- data[,!(names(data) %in% drops)]
data2$group <- as.factor(data2$group)
data2$class <- as.factor(data2$class)

# parallel processing:
  require(doMC)
  registerDoMC(2)
  # set up training parameters for models
 fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           ## repeated three times
                           repeats = 5,
                           ## Save all the resampling results
                           returnResamp = "all",
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary
                           )
                           
              gbmFit <-   train(data2, ext,
                          method    = "gbm",
                          trControl = fitControl,
                          metric    = "Kappa",
                          verbose   = FALSE,
                          weights   = weights)
                          
  # grab the gbm model:
  m <- gbmFit$finalModel                        
                          
  ##extract extinction probabilities from suite of models
  All.Models <- list(gbm = gbmFit)
  All.Preds <- predict(All.Models,newdata = predict_data, type = "prob")
  str(All.Preds)
  Ext.Prob <- All.Preds$gbm$extinct

 return(list(model = m, model.suite = All.Models))
}


data <- load("~/Dropbox/nescent_extinction_map/Final data/standardized.predictors.Cenozoic.OBIS.rda")  
data <- standardized.cenozoic 

### limit to Neogene if desired 

data <- drop.levels(subset(data,data$stage_top < 22 & data$stage_top != 0))

data_copy <- data
data_copy$stage <- rep("Cenozoic",length(data_copy[,1]))  
data_copy$stage_top <- rep(.011,length(data_copy[,1])) 
data <- rbind(data,data_copy)
  
stage_names <- list()
for(i in 1:length(unique(data$stage))) {
  stage_names[[i]] <- drop.levels(unique(data$stage)[i])
}


# for all stages:
out <- lapply(stage_names, function(x) run_ml_models(x))

models.out <- list()
model.suites.out <- list()

for(i in 1:length(out)) {
  models.out[[i]] <- out[[i]]$model
  model.suites.out[[i]] <- out[[i]]$model.suite
}
caret.gbm.models <- models.out
caret.gbm.model.sets <- model.suites.out
caret.gbm.model.stages <- stage_names
