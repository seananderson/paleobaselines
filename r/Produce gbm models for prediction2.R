
####OPTIONAL: run to redo standardization of PBDB and OBIS data
#source("/Users/Seth/Dropbox/nescent_extinction_map/r/Standardize predictors by interval faster.R")

run_ml_models <- function(x) {

  require(caret)
  require(gbm)
  require(gdata)

data <- drop.levels(subset(data,data$stage %in% x))
## single test case: 
  #data <- drop.levels(subset(data,data$stage =="Lower Miocene"))

###set specific stage to test
##data <- drop.levels(subset(data,data$stage == "Middle Miocene"))
# set aside prediction class, group, genus, prediction interval, stage top, number of genera

  stage_name <- data$stage[1]
  stage_upper <- data$stage_top[1]
  NumGenera <- length(data[,1])

  predict_genus <- predict_data$genus
  predict_group <- predict_data$group
  predict_class <- predict_data$class
  predict_occupancy <- predict_data$occupancy
  predict_occurrences <- predict_data$occurrences
  predict_great.circle <- predict_data$great.circle
  predict_richness <- predict_data$richness
  predict_lat.range <- predict_data$lat.range
  predict_min.lat <- predict_data$min.lat
  predict_max.lat <- predict_data$max.lat
  predict_mean.lat <- predict_data$mean.lat


  train_stage <- rep(stage_name,length(predict_genus))

ext <- as.factor(ifelse(data$Ex == 1,"extinct","survive"))

 # make vector of weights
  obs <- length(data$Ex)
  exes <- sum(data$Ex)
  ExFreq <- (exes/obs)
  SurFreq <- (1-(exes/obs))
  MaxFreq <- max(ExFreq,SurFreq)
  ExWeight <- (1/(ExFreq/MaxFreq))
  SurWeight <- (1/(SurFreq/MaxFreq))
  weights <- ifelse(data$Ex==1,ExWeight,SurWeight)
  #weights <- ifelse(data$Ex==1,1,1)
  null_weights <- rep(1,length(weights))
  use_weights <- rep(Use_Weights,length(weights))
  class_weights <-ifelse(use_weights == "yes",weights,null_weights)

drops <- c("stage","stage_top","genus","Ex")
data2 <- data[,!(names(data) %in% drops)]
predict_data2 <- predict_data[,!(names(predict_data) %in% drops)]
data2$group <- as.factor(data2$group)
data2$class <- as.factor(data2$class)
predict_data2$group <- as.factor(predict_data2$group)
predict_data2$class <- as.factor(predict_data2$class)


# parallel processing:
  require(doMC)
  registerDoMC(4)
  # set up training parameters for models
 fitControl <- trainControl(
                           method = CV_Method,
                           number = Number_run,
                           repeats = Number_rep,
                           ## Save all the resampling results
                           returnResamp = "all",
                           classProbs = TRUE,
                           allowParallel = TRUE
                           #summaryFunction = twoClassSummary
                           )
  ## choose metric to optimize: "Kappa","ROC","Spec" or "Sens". If Kappa, disable "summaryFunction = twoClassSummary".

 #browser()
              gbmFit <-   train(data2, ext,
                          method    = "gbm",
                          trControl = fitControl,
                          metric    = Optimize_Metric,
                          weights   = class_weights,
                          verbose   = FALSE,
                          tuneGrid  = expand.grid(.interaction.depth = 4, .n.trees = 500,.shrinkage = .001))
                        
                          

  # grab the gbm model:
  m <- gbmFit$finalModel

  ##extract extinction probabilities from suite of models
  All.Models <- list(gbm = gbmFit)
  All.Preds <- predict(All.Models,newdata = predict_data2, type = "prob")
  #str(All.Preds)
  Ext.Prob <- All.Preds$gbm$extinct


  #### script for turning raw risk estimates into quantiles
  Quantiles <- function(x){
	Q1 <- data.frame(cut2(x, g=Num_risk_quantiles, levels.mean = FALSE))
    Q1 <- data.frame(seq(1,length(Q1[,1]),1),Q1)
    colnames(Q1) <- c("num","Q")
    Qlev <- levels(as.factor(Q1$Q))
    Qord <- seq(1,length(Qlev),by = 1)
    Qlookup <- data.frame(Qlev,Qord)
    colnames(Qlookup) <- c("Q","quant")
    Qquant <- merge(Qlookup,Q1,by = "Q",sort = FALSE)
    Qquant <- Qquant[order(Qquant$num),]
    return(Qquant$quant)
    }

  Ext.Prob.Quant <- Quantiles(Ext.Prob)
  Ext.Prob.Quant <- Ext.Prob.Quant/max(Ext.Prob.Quant)

  preds <- data.frame(train_stage,predict_class,predict_group,predict_genus,Ext.Prob,Ext.Prob.Quant,predict_occupancy,predict_occurrences,predict_great.circle,predict_richness,predict_lat.range,predict_data$min.lat,predict_data$max.lat,predict_data$mean.lat)

  colnames(preds) <- c("training_interval","class","group","genus","risk","risk_quantile","occupancy","occurrences","great.circle","richness","lat_range","min.lat","max.lat","mean.lat")

  class <- plot.gbm(m, i.var = 1, n.trees = m$n.trees,
                    continuous.resolution = 100, return.grid = TRUE, type =
                      "response")
  group <- plot.gbm(m, i.var = 2, n.trees = m$n.trees,
                    continuous.resolution = 100, return.grid = TRUE, type =
                      "response")
  richness <- plot.gbm(m, i.var = 3, n.trees = m$n.trees,
                       continuous.resolution = 100, return.grid = TRUE, type =
                         "response")
  occupancy <- plot.gbm(m, i.var = 4, n.trees = m$n.trees,
                        continuous.resolution = 100, return.grid = TRUE, type =
                          "response")
  occurrences <- plot.gbm(m, i.var = 5, n.trees = m$n.trees,
                        continuous.resolution = 100, return.grid = TRUE, type =
                         "response")
  min.lat <- plot.gbm(m, i.var = 6, n.trees = m$n.trees,
                       continuous.resolution = 100, return.grid = TRUE, type =
                         "response")
  max.lat <- plot.gbm(m, i.var = 7, n.trees = m$n.trees,
                      ontinuous.resolution = 100, return.grid = TRUE, type =
                          "response")
  lat.range <- plot.gbm(m, i.var = 8, n.trees = m$n.trees,
                        continuous.resolution = 100, return.grid = TRUE, type =
                          "response")
  mean.lat <- plot.gbm(m, i.var = 9, n.trees = m$n.trees,
                        continuous.resolution = 100, return.grid = TRUE, type =
                           "response")
  #mean.lat.zone <- plot.gbm(m, i.var = 10, n.trees = m$n.trees,
                       # continuous.resolution = 100, return.grid = TRUE, type =
                            #"response")
 great.circle <- plot.gbm(m, i.var = 11, n.trees = m$n.trees,
                           continuous.resolution = 100, return.grid = TRUE, type =
                          "response")
 # tropical.only <- plot.gbm(m, i.var = 11, n.trees = m$n.trees,
                          # continuous.resolution = 100, return.grid = TRUE, type =
                             #"response")


  Group <- data.frame(rep("group",length(group[,1])),group)
  colnames(Group) <- c("predictor","value","response")
  #stage <- rep(names(stage_names[i]),length(Group[,1]))
  stage <- rep(stage_name,length(Group[,1]))
  stage_top <- rep(stage_upper,length(Group[,1]))
  num_genera <- rep(NumGenera,length(Group[,1]))
  Group <- data.frame(stage,stage_top,num_genera,Group)

  richness <- data.frame(rep("richness",length(richness[,1])),richness)
  colnames(richness) <- c("predictor","value","response")

  occupancy <- data.frame(rep("occupancy",length(occupancy[,1])),occupancy)
  colnames(occupancy) <- c("predictor","value","response")

  occurrences <- data.frame(rep("occurrences",length(occurrences[,1])),occurrences)
  colnames(occurrences) <- c("predictor","value","response")

  min.lat <- data.frame(rep("min.lat",length(min.lat[,1])),min.lat)
  colnames(min.lat) <- c("predictor","value","response")

  max.lat <- data.frame(rep("max.lat",length(max.lat[,1])),max.lat)
  colnames(max.lat) <- c("predictor","value","response")

  lat.range <- data.frame(rep("lat.range",length(lat.range[,1])),lat.range)
  colnames(lat.range) <- c("predictor","value","response")

   mean.lat <- data.frame(rep("mean.lat",length(mean.lat[,1])),mean.lat)
  colnames(mean.lat) <- c("predictor","value","response")

  # mean.lat.zone <- data.frame(rep("mean.lat.zone",length(mean.lat.zone[,1])),mean.lat.zone)
  #colnames(mean.lat.zone) <- c("predictor","value","response")

   great.circle <- data.frame(rep("great.circle",length(great.circle[,1])),great.circle)
  colnames(great.circle) <- c("predictor","value","response")

  #tropical.only <- data.frame(rep("tropical.only",length(tropical.only[,1])),tropical.only)
  #colnames(tropical.only) <- c("predictor","value","response")

  Others <- rbind(richness,occupancy,occurrences,min.lat,max.lat,lat.range,mean.lat,great.circle)
  #Others <- rbind(richness,occupancy,occurrences,lat.range,great.circle,tropical.only)
  #stage <- rep(names(stage_names[i]),length(Others[,1]))
  stage <- rep(stage_name,length(Others[,1]))
  stage_top <- rep(stage_upper,length(Others[,1]))
  num_genera <- rep(NumGenera,length(Others[,1]))
  Others <- data.frame(stage,stage_top,num_genera,Others)

  return(list(group = Group, others = Others, model = m, model.suite = All.Models, predictions = preds))
}


data <- load("~/Dropbox/nescent_extinction_map/Final data/standardized.predictors.Cenozoic.OBIS.rda")
data <- standardized.cenozoic

predict_data <- drop.levels(subset(data,data$stage == "Modern"))

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


# test for one stage:
#out <- run_ml_models(stage_names[[1]])

# for all stages:
out <- lapply(stage_names, function(x) run_ml_models(x))

groups.out <- list()
others.out <- list()
models.out <- list()
model.suites.out <- list()
predictions.out <- list()

for(i in 1:length(out)) {
  groups.out[[i]] <- out[[i]]$group
  others.out[[i]] <- out[[i]]$others
  models.out[[i]] <- out[[i]]$model
  model.suites.out[[i]] <- out[[i]]$model.suite
  predictions.out[[i]] <- out[[i]]$predictions
}

partial.groups.cenozoic <- do.call("rbind", groups.out)
partial.others.cenozoic <- do.call("rbind", others.out)
predictions.cenozoic <- do.call("rbind", predictions.out)
caret.gbm.models <- models.out
caret.gbm.model.sets <- model.suites.out
caret.gbm.model.stages <- stage_names

### extract matrix of variable importance for all intervals

names(caret.gbm.models) <- c("Lower_Miocene","Middle_Miocene","Plio_Pleistocene","Upper_Miocene","Cenozoic")
names(caret.gbm.model.sets) <- c("Lower_Miocene","Middle_Miocene","Plio_Pleistocene","Upper_Miocene","Cenozoic")

varimps<- data.frame(cbind(varImp(caret.gbm.models$Lower_Miocene),varImp(caret.gbm.models$Middle_Miocene),varImp(caret.gbm.models$Upper_Miocene),varImp(caret.gbm.models$Plio_Pleistocene),varImp(caret.gbm.models$Cenozoic)))

varimps$variable <- c("class","group","richness","occupancy","occurrences","min.lat","max.lat","lat.range","mean.lat","mean.lat.zone","great.circle","tropical_only")
colnames(varimps) <- c("Lower_Miocene","Middle_Miocene","Upper_Miocene","Plio_Pleistocene","Cenozoic","Predictor")
varimps <- data.frame(melt(varimps))
colnames(varimps) <- c("Predictor","Interval","Importance")

 ###extract predictions, calibrate within classes
index <- c("Lower_Miocene","Middle_Miocene","Plio_Pleistocene","Upper_Miocene","Cenozoic")
Predictions <- list()
for(i in index) {
  Preds <- predict(caret.gbm.model.sets[[i]]$gbm, type="prob")
  interval <- rep(i,length(Preds[,1]))
  outcome <- caret.gbm.model.sets[[i]]$gbm$trainingData$.outcome
  class <- as.factor(caret.gbm.model.sets[[i]]$gbm$trainingData$class)
  Predictions[[i]] <- data.frame(interval,class,Preds,outcome)
}
preds <- do.call("rbind",Predictions)
preds$pred.outcome.raw <- ifelse(preds$extinct >= .5, "extinct","survive")
preds <- data.frame(lapply(preds,as.character), stringsAsFactors=FALSE)
preds2 <- preds

source("~/Dropbox/nescent_extinction_map/r/calibrate-models.R")
preds2$pred.outcome.calibrated <- ifelse(preds2$calibrated_ext_prob >= .3,"extinct","survive")

## extract number of extinctions and survivals
#prop.extinct <- function(df) {
  #df <- preds
 # ext <- drop.levels(subset(df$outcome,df$outcome=="extinct"))
 # ext <- length(ext)
 # all <- length(df$outcome)
 # prop.ext <-ext/all
 # prop.ext <- ifelse(all==0 | ext == 0,NA,ext/all)
  #num.ext <- ext
  #return(data.frame(prop.ext,num.ext))
#}
#prop.exts <- ddply(preds2,.(class,interval),prop.extinct)


#preds3 <- merge(preds2,prop.exts,by=c("class","interval"))



###drop interval-class combinations that are 100% survive or extinct
#factor.levels1 <- function(df) length(levels(as.factor(df$outcome)))
#factor.levels2 <- function(df) length(levels(as.factor(df$pred.outcome.calibrated)))
#fac.levs <- ddply(preds3,.(class,interval),each(factor.levels1,factor.levels2))
#preds3 <- merge(preds3,fac.levs)
#preds3 <- drop.levels(subset(preds3,preds3$factor.levels1==2 & preds3$factor.levels2==2))


#library(epiR)
#Matrix.Stats <- function(df){
  #df <- preds2
 # confmat <- confusionMatrix(df$outcome,df$pred.outcome.calibrated)
 # byClass <-  t(data.frame(confmat$byClass))
 # overall <- t(data.frame(confmat$overall))
#n.genera <- length(df$outcome)
#gen.Ex <- length(drop.levels(subset(df$outcome,df$outcome=="extinct")))
#pred.gen.Ex <- length(drop.levels(subset(df$pred.outcome.calibrated,df$pred.outcome.calibrated=="extinct")))
# return(data.frame(byClass,overall,n.genera,gen.Ex,pred.gen.Ex))}

#df <- drop.levels(subset(preds,preds$interval=="Upper_Miocene"))
#k.data <- table(df$outcome,df$pred.outcome.adjusted)

#Matrix.Statistics <- ddply(preds3,.(class,interval),each(Matrix.Stats))


#Matrix.Statistics$Predicted.minus.observed.Prop.Ex <- (Matrix.Statistics$pred.gen.Ex/Matrix.Statistics$n.genera) - (Matrix.Statistics$gen.Ex/Matrix.Statistics$n.genera)
#Matrix.Statistics2 <- drop.levels(subset(Matrix.Statistics,Matrix.Statistics$interval == "Cenozoic"))

#Matrix.Statistics3 <- data.frame(melt(Matrix.Statistics2))

#Matrix.Statistics3 <- drop.levels(subset(Matrix.Statistics3,Matrix.Statistics3$variable == "Sensitivity" | Matrix.Statistics3$variable == "Specificity" | Matrix.Statistics3$variable == "Accuracy" |Matrix.Statistics3$variable == "Kappa" | Matrix.Statistics3$variable =="Predicted.minus.observed.Prop.Ex" ))

#quartz()
#p <- ggplot(Matrix.Statistics3,aes(class,value))
#p + geom_point(pch=1) + facet_wrap(~variable,ncol=2,scales="free")

#test.table<- table(df$outcome,df$pred.outcome.raw)
#test2 <- epi.kappa(test.table)

save(partial.groups.cenozoic, file = "~/Dropbox/nescent_extinction_map/Final data/partial.groups.cenozoic.rda")
save(partial.others.cenozoic, file = "~/Dropbox/nescent_extinction_map/Final data/partial.others.cenozoic.rda")
save(predictions.cenozoic, file = "~/Dropbox/nescent_extinction_map/Final data/predictions.cenozoic.rda")
save(caret.gbm.models, file = "~/Dropbox/nescent_extinction_map/Final data/caret.gbm.models.rda")
save(caret.gbm.model.sets, file = "~/Dropbox/nescent_extinction_map/Final data/caret.gbm.model.sets.rda")
save(caret.gbm.model.stages, file = "~/Dropbox/nescent_extinction_map/Final data/caret.gbm.model.stages.rda")
save(varimps, file = "~/Dropbox/nescent_extinction_map/Final data/variable.importance.matrix.rda")
#save(Matrix.Statistics3 , file = "~/Dropbox/nescent_extinction_map/Final data/matrix.statistics.by.class.rda")
save(preds2 , file = "~/Dropbox/nescent_extinction_map/Final data/Preds2.rda")
### make plot of variable importance
#dev.off()
png()
p <- ggplot(varimps,aes(Interval,Predictor,fill=Importance)) + geom_tile(colour="black") + scale_fill_gradient(low="lemonchiffon",high="red3") + opts(axis.text.x = theme_text(size=10)) + opts(axis.text.y = theme_text(size=10))+ opts(panel.background = theme_rect(colour=NA, size =1, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank())
print(p)
ggsave(p,file="~/Dropbox/nescent_extinction_map/r/variable.importance.matrix.png",width=9,height=6)
dev.off()


