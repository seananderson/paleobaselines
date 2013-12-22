library(randomForest)
library(matrixStats)
require(verification)
library(gdata)
library(party)
library(plyr)
library(Hmisc)
library(caret)

data <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Modern and PaleoDB ranges FINAL.csv",header = TRUE,stringsAsFactors = FALSE)
data <- drop.levels(subset(data,data$OBIS_occurrences>0 & data$use==1))
head(data)


### select first interval to analyze; take subset if desired
data1 <- drop.levels(subset(data,stage_top < 22 & stage_top > 0 & data$Interval_Name != "ePliocene"))


Quartiles <- function(x){
	Q1 <- data.frame(cut2(x, g=5, levels.mean = FALSE))
    Q1 <- data.frame(seq(1,length(Q1[,1]),1),Q1)
    colnames(Q1) <- c("num","Q")
    Qlev <- levels(as.factor(Q1$Q))
    Qord <- seq(1,length(Qlev),by = 1)
    Qlookup <- data.frame(Qlev,Qord)
    colnames(Qlookup) <- c("Q","quant")
    Qquant <- merge(Qlookup,Q1,by = "Q",sort = F)
    Qquant <- Qquant[order(Qquant$num),]
    return(Qquant$quant)
    }

### select factors to analyze from first interval, round if desired
genus <- data1$genus
group <- as.factor(data1$MatchTaxon)
richness <-Quartiles(round(log(data1$richness+1),1))
occupancy <-Quartiles(round(log(data1$eac+1),1))
isNA <- is.na(data1$gcd)
gcd <- ifelse(isNA == "TRUE",0,data1$gcd)
great.circle <- round(log(gcd+1),1)
min.lat <- data1$MinLat
max.lat <- data1$MaxLat
lat.range <- max.lat-min.lat
life.habit <- as.factor(data1$life.habit)
locomotion <- as.factor(data1$loco)
diet <- as.factor(data1$diet)
eyes <- as.factor(data1$eyes)
class <- as.factor(data1$class)
Ex <- data1$Extinct.in.stage
ext <- as.factor(ifelse(Ex ==1,"extinct","survive"))

obs <- length(Ex)
exes <- sum(Ex)
ExFreq <- (exes/obs)
SurFreq <- (1-(exes/obs))
MaxFreq <- max(ExFreq,SurFreq)
ExWeight <- 1/(ExFreq/MaxFreq)
SurWeight <- 1/(SurFreq/MaxFreq)


data1factors <- data.frame(class,group,richness,great.circle,occupancy,min.lat,max.lat,lat.range)





weights <- ifelse(ext=="extinct",ExWeight,SurWeight)

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


#extModels <- list(rf = rfFit1, gbm = gbmFit1, cf = cfFit1)
extModels <- list(gbm = gbmFit1)

# predict from multiple models at once:
#extPred <- predict(extModels)
#extPred <- extractPrediction(list(rf = rfFit1, cf = cfFit1, gbm = gbmFit1))
extPred <- extractPrediction(list(gbm = gbmFit1))

#with(subset(extPred, object == "rf"), confusionMatrix(pred, obs))
#with(subset(extPred, object == "cf"), confusionMatrix(pred, obs))
with(subset(extPred, object == "gbm"), confusionMatrix(pred, obs))

#extProb <- extractProb(list(rf = rfFit1, cf = cfFit1, gbm = gbmFit1))
extProb <- extractProb(list(gbm = gbmFit1))

write.table(extProb,"~/Dropbox/nescent_extinction_map/Final data/extPredictions.csv",sep=",")





# ddply(testPred, .(model), defaultSummary)

pdf("ROC_learning.pdf", width = 8, height = 3)
par(mfrow = c(1, 3))
ddply(extProb, .(model), transform, {
      x <- roc(as.numeric(obs), extinct, plot = TRUE, main = unique(object), yaxs = "i", xaxs = "i")
      mtext(paste("AUC =", round(x$auc, 2)), side = 1)
                  })
dev.off()


pdf("class_probabilities_learning.pdf", width = 5, height = 8)
plotClassProbs(extProb)
dev.off()
