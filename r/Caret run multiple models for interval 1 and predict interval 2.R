library(randomForest)
library(matrixStats)
require(verification)
library(gdata)
library(party)
library(plyr)
library(Hmisc)

data <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Modern and PaleoDB ranges FINAL.csv",header = TRUE,stringsAsFactors = FALSE)
#data <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Modern and PaleoDB ranges no interpolation FINAL.csv",header = TRUE,stringsAsFactors = FALSE)
data <- drop.levels(subset(data, data$use==1))
head(data)

# > unique(data$Interval_Name)
#  [1] "Danian"              "Selandian-Thanetian" "Ypresian"            "Lutetian"           
#  [5] "Bartonian"           "Priabonian"          "Rupelian"            "Chattian"           
#  [9] "Lower Miocene"       "Middle Miocene"      "Upper Miocene"       "Pliocene"           
# [13] "Pleistocene"         "Spalding_raw"        "Spalding_merged"    

### select first interval to analyze; take subset if desired
data1 <- drop.levels(subset(data,data$stage_top >0 & data$stage_top < 20 & data$occurrences > 1))
#c("Lower Miocene", "Middle Miocene", "Upper Miocene", "Pliocene", "Pleistocene")))

### select interval to analyze with forest model from first interval
data2 <- drop.levels(subset(data,data$Interval_Name == "Spalding_raw" & data$OBIS_occurrences >= 30))
## limit analyses to groups shared between both intervals
MatchGenera <- data.frame(data2$genus)
colnames(MatchGenera) <- c("genus")

#### drop groups that do not occur in stage
Taxa1 <- data.frame(levels(as.factor(data1$MatchTaxon)))
colnames(Taxa1) <- c("Taxa")
Taxa2 <- data.frame(levels(as.factor(data2$MatchTaxon)))
colnames(Taxa2) <- c("Taxa")
Taxa <- merge(Taxa1,Taxa2,by = "Taxa",all.y = FALSE)
colnames(Taxa) <- c("group")


Quartiles <- function(x){
	Q1 <- data.frame(cut2(x, g=4, levels.mean = FALSE))
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

### create factors to analyze from first interval, round if desired
genus <- data1$genus
group <- as.factor(data1$MatchTaxon)
richness <-Quartiles(round(log(data1$richness+1),1))
occupancy <-Quartiles(round(log(data1$eac+1),1))
#isNA <- is.na(data1$gcd)
#gcd <- ifelse(isNA == "TRUE",0,data1$gcd)
#great.circle <- round(log(gcd+1),1)
min.lat <- data1$MinLat
max.lat <- data1$MaxLat
lat.range <- max.lat-min.lat
life.habit <- as.factor(data1$life.habit)
locomotion <- as.factor(data1$loco)
diet <- as.factor(data1$diet)
eyes <- as.factor(data1$eyes)
class <- as.factor(data1$class)
Ex <- data1$Extinct.in.stage

### designate categorical response variable
ext <- as.factor(ifelse(Ex == 1,"extinct","survive"))

### create dataframe of interval 1 predictors
data1factors.all <- drop.levels(na.omit(data.frame(class,group,richness,occupancy,min.lat,max.lat,lat.range,Ex,ext)))

### remove groups that do not occur in both intervals
data1factors <- drop.levels(merge(data1factors.all,Taxa,by = "group", all.x = FALSE))
ext <- data1factors$ext
drops <- c("Ex","ext")
data1factors <- data1factors[,!(names(data1factors) %in% drops)]

### make vector of weights for interval 1
obs <- length(Ex)
exes <- sum(Ex)
ExFreq <- (exes/obs)
SurFreq <- (1-(exes/obs))
MaxFreq <- max(ExFreq,SurFreq)
ExWeight <- 1/(ExFreq/MaxFreq)
SurWeight <- 1/(SurFreq/MaxFreq)
weights <- ifelse(ext=="extinct",ExWeight,SurWeight)

### create factors to use for prediction of second interval (must be same as first!)

genus <- data2$genus
group <- as.factor(data2$MatchTaxon)
richness <-Quartiles(round(log(data2$richness+1),1))
occupancy <-Quartiles(round(log(data2$eac+1),1))
#isNA <- is.na(data2$gcd)
#gcd <- ifelse(isNA == "TRUE",0,data2$gcd)
#great.circle <- round(log(gcd+1),1)
min.lat <- data2$MinLat
max.lat <- data2$MaxLat
lat.range <- max.lat-min.lat
life.habit <- as.factor(data2$life.habit)
locomotion <- as.factor(data2$loco)
diet <- as.factor(data2$diet)
eyes <- as.factor(data2$eyes)
class <- as.factor(data2$class)
Ex <- data2$Extinct.in.stage

### create dataframe of interval 2 predictors
data2factors.gen <- drop.levels(na.omit(data.frame(class,group,genus,richness,occupancy,min.lat,max.lat,lat.range)))  
### remove groups that do not occur in both intervals
data2factors.cull <- drop.levels(merge(data2factors.gen,Taxa,by = "group", all.x = FALSE))
### drop genus from predictors 
data2factors <- drop.levels(subset(data2factors.cull, select=-c(genus)))


# caret
library(caret)
# parallel processing:
library(doMC)
registerDoMC(2)

### set up training parameters for models

fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           ## repeated three times
                           repeats = 5,
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

#### run generalized boosted model, random forest model, and conditional inference forest model for interval 1

gbmFit1 <- train(data1factors, ext,
                  method = "gbm",
                  trControl = fitControl,
                  metric = "Kappa",
                  verbose = FALSE, 
                  weights = weights)

#rfFit1 <- train(data1factors, ext,

                  #method = "rf",
                  #metric = "Kappa",
                  #trControl = fitControl, weights = weights)

#cfFit1 <- train(data1factors, ext,
                  #method = "cforest",
                  #metric = "Kappa",
                  #trControl = fitControl, weights = weights)

save(data1factors, data2factors, gbmFit1, file = "~/Dropbox/nescent_extinction_map/Final data/gbm_dat.rda")

#extModels <- list(rf = rfFit1, gbm = gbmFit1, cf = cfFit1)
extModels <- list(gbm = gbmFit1)
### Compare cross-validation stats, print out pdfs of ROC learnin and class probabilities learning
#extPred <- extractPrediction(list(rf = rfFit1, cf = cfFit1, gbm = gbmFit1))
extPred <- extractPrediction(list(gbm = gbmFit1))

#with(subset(extPred, object == "rf"), confusionMatrix(pred, obs))
#with(subset(extPred, object == "cf"), confusionMatrix(pred, obs))
with(subset(extPred, object == "gbm"), confusionMatrix(pred, obs))

#extProb <- extractProb(list(rf = rfFit1, cf = cfFit1, gbm = gbmFit1))
extProb <- extractProb(list(gbm = gbmFit1))


p <- extractProb(list(gbm = gbmFit1))
with(p, plot.default(jitter(as.numeric(obs)), extinct, type = "p", col = "#00000040"))

# ddply(testPred, .(model), defaultSummary)

#pdf("ROC_learning.pdf", width = 8, height = 3)
#par(mfrow = c(1, 3))
#ddply(extProb, .(model), transform, {
  #    x <- roc(as.numeric(obs), extinct, plot = TRUE, main = unique(object), yaxs = "i", xaxs = "i")
      # mtext(paste("AUC =", round(gbmEx$auc, 2)), side = 1)
                 # })
#dev.off()


#pdf("class_probabilities_learning.pdf", width = 5, height = 8)
plotClassProbs(extProb)
dev.off()


#### predict interval 2 using optimal models from each method
#All.Models <- list(gbm = gbmFit1,rf = rfFit1, cf = cfFit1)
All.Models <- list(gbm = gbmFit1)
All.Preds <- predict(All.Models,newdata = data2factors, type = "prob")
str(All.Preds)
gbmEx <- All.Preds$gbm$extinct
#rfEx <- All.Preds$rf$extinct
#cfEx <- All.Preds$cf$extinct
genus <- data2factors.cull$genus
#Predictions <- data.frame(genus,gbmEx,rfEx,cfEx)
Predictions <- data.frame(genus,gbmEx)
Mod.exrisk.preds <- merge(data2factors.gen,Predictions,by="genus",all.x = TRUE)
write.table(Mod.exrisk.preds,"~/Dropbox/nescent_extinction_map/Final data/Mod.exrisk.preds2.csv",sep = ",")