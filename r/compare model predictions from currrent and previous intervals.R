library(randomForest)
library(matrixStats)
require(verification)
library(gdata)
library(ggplot2)
library(party)
library(plyr)
library(Hmisc)
library(proto)
library(boot)

data <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Modern and PaleoDB ranges FINAL.csv",header = TRUE,stringsAsFactors = FALSE)
data <- drop.levels(subset(data,data$use==1 & data$occurrences > 1 & stage_top > 0))
head(data)

# > unique(data$Interval_Name)
#  [1] "Danian"              "Selandian-Thanetian" "Ypresian"            "Lutetian"           
#  [5] "Bartonian"           "Priabonian"          "Rupelian"            "Chattian"           
#  [9] "Lower Miocene"       "Middle Miocene"      "Upper Miocene"       "Pliocene"           
# [13] "Pleistocene"         "Spalding_raw"        "Spalding_merged"    

### select prediction interval; take subset if desired
Prediction_Interval <-  "????" 

data1 <- drop.levels(subset(data,data$Interval_Name != Prediction_Interval))
#data1 <- drop.levels(subset(data,data$stage_top > 0 & data$stage_top < 23))


### select predicted interval; take subset if desired
Modeled_Interval <-  "Pliocene" 

data2 <- drop.levels(subset(data,data$Interval_Name == Modeled_Interval))
#data2 <- drop.levels(subset(data,data$stage_top > 0 & data$stage_top < 23))

## limit analyses to groups shared between both intervals
MatchGenera <- data.frame(data2$genus)
colnames(MatchGenera) <- c("genus")

#### drop groups that do not occur in predicted stage

Taxa1 <- data.frame(levels(as.factor(data1$MatchTaxon)))
colnames(Taxa1) <- c("Taxa")
Taxa2 <- data.frame(levels(as.factor(data2$MatchTaxon)))
colnames(Taxa2) <- c("Taxa")
Taxa <- merge(Taxa1,Taxa2,by = "Taxa",all.y = FALSE)
colnames(Taxa) <- c("group")

#### define function for converting to quantiles
Quantiles <- function(x){
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


### create factors to analyze from prediction interval, round if desired
genus <- data1$genus
group <- as.factor(data1$MatchTaxon)
richness <-Quantiles(round(log(data1$richness+1),1))
occupancy <-Quantiles(round(log(data1$eac+1),1))
occurrences <- Quantiles(round(log(data1$occurrences+1),1))
isNA <- is.na(data1$gcd)
#gcd <- ifelse(isNA == "TRUE",0,data1$gcd_corrected)
#great.circle <- round(log(gcd),0)
min.lat <- data1$MinLat
max.lat <- data1$MaxLat
lat.range <- round(max.lat-min.lat,0)
class <- as.factor(data1$class)
Ex <- data1$Extinct.in.stage

data1factors <- drop.levels(na.omit(data.frame(class,group,richness,occupancy,occurrences,min.lat,max.lat,lat.range,Ex)))

### create factors to analyze from predicted interval, round if desired
genus <- data2$genus
group <- as.factor(data2$MatchTaxon)
richness <-Quantiles(round(log(data2$richness+1),1))
occupancy <-Quantiles(round(log(data2$eac+1),1))
occurrences <- Quantiles(round(log(data2$occurrences+1),1))
isNA <- is.na(data2$gcd)
#gcd <- ifelse(isNA == "TRUE",0,data2$gcd_corrected)
#great.circle <- round(log(gcd),0)
min.lat <- data2$MinLat
max.lat <- data2$MaxLat
lat.range <- round(max.lat-min.lat,0)
class <- as.factor(data2$class)
Ex <- data2$Extinct.in.stage

data2factors <- drop.levels(na.omit(data.frame(class,group,richness,occupancy,occurrences,min.lat,max.lat,lat.range,Ex))) 


### remove groups that do not occur in both intervals
data1factors <- drop.levels(merge(data1factors,Taxa,by = "group", all.x = FALSE))
data2factors <- drop.levels(merge(data2factors,Taxa,by = "group", all.x = FALSE))

### define extinction variables
Ex1 <- data1factors$Ex
Ex2 <- data2factors$Ex

### columns to drop

drops <- c("Ex")
data1factors <- data1factors[,!(names(data1factors) %in% drops)]
data2factors <- data2factors[,!(names(data2factors) %in% drops)]

### designate categorical response variable
ext1 <- as.factor(ifelse(Ex1 == 1,"extinct","survive"))
ext2 <- as.factor(ifelse(Ex2 == 1,"extinct","survive"))

### randomize ext2 extinctions for comparison
rand <- runif(length(ext1))
ext1_rand <- data.frame(ext1,rand)
ext1_rand <- ext1_rand[order(rand) , ]
ext1_rand <- ext1_rand[,1]
Ex1_rand <- ifelse(ext1_rand=="survive",0,1)

### make vector of weights for interval 1
obs <- length(Ex1)
exes <- sum(Ex1)
ExFreq <- (exes/obs)
SurFreq <- (1-(exes/obs))
MaxFreq <- max(ExFreq,SurFreq)
ExWeight <- 1/(ExFreq/MaxFreq)
SurWeight <- 1/(SurFreq/MaxFreq)
weights1 <- ifelse(ext1=="extinct",ExWeight,SurWeight)
#weights1 <- ifelse(ext1=="extinct",25,1)

### make vector of weights for interval 1 rand
obs <- length(Ex1_rand)
exes <- sum(Ex1_rand)
ExFreq <- (exes/obs)
SurFreq <- (1-(exes/obs))
MaxFreq <- max(ExFreq,SurFreq)
ExWeight <- 1/(ExFreq/MaxFreq)
SurWeight <- 1/(SurFreq/MaxFreq)
weights1_rand <- ifelse(ext1_rand =="extinct",ExWeight,SurWeight)
#weights1_rand <- ifelse(ext1_rand =="extinct",25,1)

### make vector of weights for interval 2
obs <- length(Ex2)
exes <- sum(Ex2)
ExFreq <- (exes/obs)
SurFreq <- (1-(exes/obs))
MaxFreq <- max(ExFreq,SurFreq)
ExWeight <- 1/(ExFreq/MaxFreq)
SurWeight <- 1/(SurFreq/MaxFreq)
weights2 <- ifelse(ext2=="extinct",ExWeight,SurWeight)
#weights2 <- ifelse(ext2=="extinct",25,1)

# caret
library(caret)
# parallel processing:
library(doMC)
registerDoMC(2)

### set up training parameters for models

fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 25,
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
                           #summaryFunction = twoClassSummary
                           )

                           ## "in problems where there are a low
                           ## percentage of samples in one class,
                           ## using metric = "Kappa" can improve
                           ## quality of the final model."
                           ## caretTrain.pdf

#### run generalized boosted model, random forest model, and conditional inference forest model for interval 1

gbmFit1 <- train(data1factors, ext1,
                  method = "gbm",
                  #distribution="adaboost",
                  trControl = fitControl,
                  metric = "Kappa",
                  verbose = FALSE,            
                  weights = weights1)

plot(gbmFit1,metric="Kappa")
varImp(gbmFit1)

gbmFit2 <- train(data2factors, ext2,
                  method = "gbm",
                  #distribution="adaboost",
                  trControl = fitControl,
                  metric = "Accuracy",
                  verbose = FALSE, 
                  weights = weights2)
                  
plot(gbmFit2,metric="Kappa")
varImp(gbmFit2)                  
                  
                  
gbmFit1_rand <- train(data1factors, ext1_rand,
                  method = "gbm",
                  #distribution="adaboost",
                  trControl = fitControl,
                  metric = "Accuracy",
                  verbose = FALSE, 
                  weights = weights1_rand)
                  
                  
                  

save(data1factors, data2factors, gbmFit1, gbmFit2, gbmFit1_rand,file = "~/Dropbox/nescent_extinction_map/Final data/gbm_comparison.rda")

ext1Models <- list(gbm = gbmFit1)
ext1Pred <- extractPrediction(list(gbm = gbmFit1))

ext2Models <- list(gbm = gbmFit2)
ext2Pred <- extractPrediction(list(gbm = gbmFit2))

ext1Models_rand <- list(gbm = gbmFit1_rand)
ext1Pred_rand <- extractPrediction(list(gbm = gbmFit1_rand))


with(subset(ext1Pred, object == "gbm"), confusionMatrix(pred, obs))
with(subset(ext2Pred, object == "gbm"), confusionMatrix(pred, obs))
with(subset(ext1Pred_rand, object == "gbm"), confusionMatrix(pred, obs))

ext1Prob <- extractProb(list(gbm = gbmFit1))
ext2Prob <- extractProb(list(gbm = gbmFit2))
ext1Prob_rand <- extractProb(list(gbm = gbmFit1_rand))


Int_1_pred <- ext1Prob$extinct
Int_2_pred <- ext2Prob$extinct
Int_1_pred_rand <- ext1Prob_rand$extinct


#### predict interval 2 using interval 1

Preds_prev <- predict(ext1Models,newdata = data2factors, type = "prob")
Preds_class_prev <- data.frame(predict(ext1Models,newdata = data2factors, type = "raw"))
str(Preds_prev)
Int_1_pred <- Preds_prev$gbm$extinct

Preds_prev_rand <- predict(ext1Models_rand,newdata = data2factors, type = "prob")
Preds_class_prev_rand <- data.frame(predict(ext1Models_rand,newdata = data2factors, type = "raw"))
str(Preds_prev_rand)
Int_1_pred_rand <- Preds_prev_rand$gbm$extinct

Preds <- predict(ext2Models,newdata = data2factors, type = "prob")
Preds_class <- data.frame(predict(ext2Models,newdata = data2factors, type = "raw"))
str(Preds)
Int_2_pred <- Preds$gbm$extinct

predicted_prev1 <- ifelse(Preds_class_prev =="survive",0,1)
predicted_prev2 <- ifelse(Preds_class =="survive",0,1)
predicted_prev1_rand <- ifelse(Preds_class_prev_rand =="survive",0,1)
actual_prev <- ifelse(ext2=="survive",0,1)

###Extract summary stats for Interval Model predictions

con_mat2 <-confusionMatrix(actual_prev,predicted_prev2)
auc2 <- auc(actual_prev,predicted_prev2)[1]
by_class <- data.frame(con_mat2$byClass)
colnames(by_class) <- c("value")
overall <- data.frame(con_mat2$overall)
colnames(overall) <- c("value")
AUC <- auc2
FN <- con_mat2$table[1,2]
FP <- con_mat2$table[2,1]
TP <- con_mat2$table[2,2]
TN <- con_mat2$table[1,1]
Int_Model_stats <- rbind(overall,by_class,AUC,TP,FP,TN,FN)
rownames(Int_Model_stats)[15:19] <- c("AUC","True_Positive","False Positive","True Negative","False Negative")
rho <- "NA"
rho_UCI <- "NA"
rho_LCI <- "NA"
#colnames(rho_LCI) <- c("value")
#rownames(rho_LCI) <- c("rho_LCI")
#colnames(rho_UCI) <- c("value")
#rownames(rho_UCI) <- c("rho_UCI")
#colnames(rho) <- c("value")
#rownames(rho) <- c("rho")

stats <- Int_Model_stats

old_names <- data.frame(rownames(stats))
Model <- data.frame(rep("Int",length(old_names[,1])))
new_names <- paste(old_names[,1],Model[,1], sep="_")
rownames(stats) <- new_names
Interval <- stats


###Extract summary stats for Previous Interval Model predictions

con_mat2 <-confusionMatrix(actual_prev,predicted_prev1)
auc2 <- auc(actual_prev,predicted_prev1)[1]
by_class <- data.frame(con_mat2$byClass)
colnames(by_class) <- c("value")
overall <- data.frame(con_mat2$overall)
colnames(overall) <- c("value")
AUC <- auc2
FN <- con_mat2$table[1,2]
FP <- con_mat2$table[2,1]
TP <- con_mat2$table[2,2]
TN <- con_mat2$table[1,1]
Int_Model_stats <- rbind(overall,by_class,AUC,TP,FP,TN,FN)
rownames(Int_Model_stats)[15:19] <- c("AUC","True_Positive","False Positive","True Negative","False Negative")
cor.data <- data.frame(Int_2_pred,Int_1_pred)
spear <- function(cor.data,d) {
  rsq <- spearman(Int_2_pred[d],Int_1_pred[d])^2
  return(rsq)
}
Spearman_boot <- boot(data=cor.data,statistic =spear,R=1000)
ci = boot.ci(Spearman_boot, type="basic")
rho <- data.frame(ci[2])
rho_conf <- data.frame(ci[4])
colnames(rho) <- c("value")
rho_UCI <- rho_conf[5]
colnames(rho_UCI) <- c("value")
rownames(rho_UCI) <- c("rho_UCI")
rho_LCI <- rho_conf[4]
colnames(rho_LCI) <- c("value")
rownames(rho_LCI) <- c("rho_LCI")
stats <- rbind(Int_Model_stats,rho,rho_LCI,rho_UCI)

old_names <- data.frame(rownames(stats))
Model <- data.frame(rep("Prec_Int",length(old_names[,1])))
new_names <- paste(old_names[,1],Model[,1], sep="_")
rownames(stats) <- new_names
Prec_Interval <- stats

###Extract summary stats for Previous Interval randomized Model predictions

con_mat2 <-confusionMatrix(actual_prev,predicted_prev1_rand)
auc2 <- auc(actual_prev,predicted_prev1_rand)[1]
by_class <- data.frame(con_mat2$byClass)
colnames(by_class) <- c("value")
overall <- data.frame(con_mat2$overall)
colnames(overall) <- c("value")
AUC <- auc2
FN <- con_mat2$table[1,2]
FP <- con_mat2$table[2,1]
TP <- con_mat2$table[2,2]
TN <- con_mat2$table[1,1]
Int_Model_stats <- rbind(overall,by_class,AUC,TP,FP,TN,FN)
rownames(Int_Model_stats)[15:19] <- c("AUC","True_Positive","False Positive","True Negative","False Negative")
cor.data <- data.frame(Int_2_pred,Int_1_pred_rand)
spear <- function(cor.data,d) {
  rsq <- spearman(Int_2_pred[d],Int_1_pred_rand[d])^2
  return(rsq)
}
Spearman_boot <- boot(data=cor.data,statistic =spear,R=1000)
ci = boot.ci(Spearman_boot, type="basic")
rho <- data.frame(ci[2])
rho_conf <- data.frame(ci[4])
colnames(rho) <- c("value")
rho_UCI <- rho_conf[5]
colnames(rho_UCI) <- c("value")
rownames(rho_UCI) <- c("rho_UCI")
rho_LCI <- rho_conf[4]
colnames(rho_LCI) <- c("value")
rownames(rho_LCI) <- c("rho_LCI")
stats <- rbind(Int_Model_stats,rho,rho_LCI,rho_UCI)

old_names <- data.frame(rownames(stats))
Model <- data.frame(rep("Prec_Int_rand",length(old_names[,1])))
new_names <- paste(old_names[,1],Model[,1], sep="_")
rownames(stats) <- new_names
Prec_Interval_rand <- stats


Stage_outs <- t(rbind(Interval,Prec_Interval,Prec_Interval_rand))
Stage_outs <- Stage_outs[,order(colnames(Stage_outs))]


write.table(Stage_outs,"~/Dropbox/nescent_extinction_map/Final data/Stage_model_comparison.csv",sep=",",row.names=TRUE)





