#' This version uses the caret package and automates running multiple
#' stages

#' @param x A character vector containing stage names


##OPTIONAL: redo standardization and rescaling of raw paleo and modern data if desired

#source("/Users/Seth/Dropbox/nescent_extinction_map/r/Standardize predictors by interval faster.R")

run_ml_models <- function(x) {

  require(caret)
  require(gbm)
  require(gdata)

  train_data <- drop.levels(subset(training_data,training_data$stage %in% x))
  predict_data <- predict_data

   # current stage name 
  stage_name <- train_data$stage[1]
  stage_upper <- train_data$stage_top[1]
  NumGenera <- length(train_data[,1])
  
  predict_genus <- predict_data$genus
  predict_group <- predict_data$group
  predict_class <- predict_data$class
  
  # get data into shape for model prediction -transform group into
  # factor and remove extraneous variables
  
  group <- as.factor(train_data$group)
  drops <- c("stage","stage_top","genus","group","class")
  train_data <- train_data[,!(names(train_data) %in% drops)]
  train_data <- data.frame(train_data,group)
  drops <- c("stage","stage_top","genus","group","Ex","class")
  group <- as.factor(predict_data$group)
  predict_data <- predict_data[,!(names(predict_data) %in% drops)]
  predict_data <- data.frame(predict_data,group)
  train_stage <- rep(stage_name,length(predict_genus))

  # make vector of weights for train_data
  obs <- length(train_data$Ex)
  exes <- sum(train_data$Ex)
  ExFreq <- (exes/obs)
  SurFreq <- (1-(exes/obs))
  MaxFreq <- max(ExFreq,SurFreq)
  ExWeight <- 1/(ExFreq/MaxFreq)
  SurWeight <- 1/(SurFreq/MaxFreq)
  weights <- ifelse(train_data$Ex==1,ExWeight,SurWeight)

  # run the model:
  # Fossil.mod <- gbm(Ex ~ ., distribution="bernoulli", data=train_data,
  # weights = Weights, n.trees=2000, shrinkage=0.01, cv.folds=5,
  # verbose=FALSE)

  # replace with caret:
  # Fossil.mod <- gbm(Ex ~ ., distribution="bernoulli",
  # data=train_data, weights = Weights, n.trees=2000, shrinkage=0.01,
  # cv.folds=5, verbose=FALSE)

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
    #p = .67,
    #selectionFunction = "best",
    selectionFunction = "oneSE",
    classProbs = TRUE,
    allowParallel = TRUE,
    #summaryFunction = twoClassSummary
    )

  ext <- as.factor(ifelse(train_data$Ex == 1,"extinct","survive"))
  drops <- c("Ex")
  train_data2 <- train_data[,!(names(train_data) %in% drops)]
  train_data2$group <- factor(train_data2$group)

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

  #gbm.perf(m, plot.it=FALSE,method="cv")
  preds <- data.frame(train_stage,predict_class,predict_group,predict_genus,predict(m, newdata = predict_data , type="response", n.trees = m$n.trees))
  colnames(preds) <- c("training_interval","class","group","genus","risk")
  #write.table(preds,"~/Dropbox/nescent_extinction_map/Final data/Extinction_risk_predictions.csv",sep=",")

  #save(Fossil.mod, file = "~/Dropbox/nescent_extinction_map/Final data/Neogene.gbm.model.rda")

  #model <- load("~/Dropbox/nescent_extinction_map/Final data/Neogene.gbm.model.rda")
  #Fossil.mod <- data$Fossil.mod

  #relative.influence(Fossil.mod, 1000)
  #par(mfrow=c(2,4))
  #plot(...)

  one <- plot.gbm(m, i.var = 1, n.trees = m$n.trees,
    continuous.resolution = 100, return.grid = TRUE, type =
    "response")
  two <- plot.gbm(m, i.var = 2, n.trees = m$n.trees,
    continuous.resolution = 100, return.grid = TRUE, type =
    "response")
  three <- plot.gbm(m, i.var = 3, n.trees = m$n.trees,
    continuous.resolution = 100, return.grid = TRUE, type =
    "response")
  four <- plot.gbm(m, i.var = 4, n.trees = m$n.trees,
    continuous.resolution = 100, return.grid = TRUE, type =
    "response")
  five <- plot.gbm(m, i.var = 5, n.trees = m$n.trees,
    continuous.resolution = 100, return.grid = TRUE, type =
    "response")
  six <- plot.gbm(m, i.var = 6, n.trees = m$n.trees,
    continuous.resolution = 100, return.grid = TRUE, type =
    "response")
  seven <- plot.gbm(m, i.var = 7, n.trees = m$n.trees,
    continuous.resolution = 100, return.grid = TRUE, type =
    "response")

  Group <- data.frame(rep("group",length(seven[,1])),seven)
  colnames(Group) <- c("predictor","value","response")
  #stage <- rep(names(stage_names[i]),length(Group[,1]))
  stage <- rep(stage_name,length(Group[,1]))
  stage_top <- rep(stage_upper,length(Group[,1]))
  num_genera <- rep(NumGenera,length(Group[,1]))
  Group <- data.frame(stage,stage_top,num_genera,Group)

  one<- data.frame(rep("richness",length(one[,1])),one)
  colnames(one) <- c("predictor","value","response")

  two <- data.frame(rep("occupancy",length(two[,1])),two)
  colnames(two) <- c("predictor","value","response")

  three <- data.frame(rep("occurrences",length(three[,1])),three)
  colnames(three) <- c("predictor","value","response")

  four <- data.frame(rep("min.lat",length(five[,1])),four)
  colnames(four) <- c("predictor","value","response")

  five <- data.frame(rep("max.lat",length(five[,1])),five )
  colnames(five) <- c("predictor","value","response")

  six <- data.frame(rep("lat.range",length(six[,1])),six)
  colnames(six) <- c("predictor","value","response")

  Others <- rbind(one,two,three,four,five,six)
  #stage <- rep(names(stage_names[i]),length(Others[,1]))
  stage <- rep(stage_name,length(Others[,1]))
  stage_top <- rep(stage_upper,length(Others[,1]))
  num_genera <- rep(NumGenera,length(Others[,1]))
  Others <- data.frame(stage,stage_top,num_genera,Others)
  
  return(list(group = Group, others = Others, model = m, predictions = preds))
}

### open file of standardized predictors output by "Standardize predictors by interval faster.R"
library(gdata)
data <- load("~/Dropbox/nescent_extinction_map/Final data/standardized.predictors.Cenozoic.OBIS.rda")  
data <- standardized.cenozoic 

### limit to Neogene if desired 

data <- drop.levels(subset(data,data$stage_top < 22))

### select data from which training data will be subset
training_data <- drop.levels(subset(data,data$stage != "Spalding_raw" & data$stage != "Spalding_merged"))

### select data to be predicted
predict_data <- drop.levels(subset(data,data$stage == "Spalding_merged"))
  
data_copy <- training_data
data_copy$stage <- rep("Cenozoic",length(data_copy[,1]))  
data_copy$stage_top <- rep(.011,length(data_copy[,1])) 
training_data <- rbind(training_data,data_copy)
  
stage_names <- list()
for(i in 1:length(unique(training_data$stage))) {
  stage_names[[i]] <- drop.levels(unique(training_data$stage)[i])
}

# you could enter multiple stages in an element of stage_names if you
# want

# test for one stage:
#out <- run_ml_models(stage_names[[1]])

# for all stages:
out <- lapply(stage_names, function(x) run_ml_models(x))

groups.out <- list()
others.out <- list()
models.out <- list()
predictions.out <- list()

for(i in 1:length(out)) {
  groups.out[[i]] <- out[[i]]$group
  others.out[[i]] <- out[[i]]$others
  models.out[[i]] <- out[[i]]$model
  predictions.out[[i]] <- out[[i]]$predictions
}

partial.groups.cenozoic <- do.call("rbind", groups.out)
partial.others.cenozoic <- do.call("rbind", others.out)
predictions.cenozoic <- do.call("rbind", predictions.out)
caret.gbm.models <- models.out
caret.gbm.model.stages <- stage_names



save(partial.groups.cenozoic, file = "~/Dropbox/nescent_extinction_map/Final data/partial.groups.cenozoic.rda")
save(partial.others.cenozoic, file = "~/Dropbox/nescent_extinction_map/Final data/partial.others.cenozoic.rda")
save(predictions.cenozoic, file = "~/Dropbox/nescent_extinction_map/Final data/predictions.cenozoic.rda")
save(caret.gbm.models, file = "~/Dropbox/nescent_extinction_map/Final data/caret.gbm.models.rda")
save(caret.gbm.model.stages, file = "~/Dropbox/nescent_extinction_map/Final data/caret.gbm.model.stages.rda")

#######################################

 
