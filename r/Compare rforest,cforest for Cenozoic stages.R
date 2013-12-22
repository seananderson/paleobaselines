library(randomForest)
library(matrixStats)
require(verification)
library(gdata)
library(party)
library(plyr)
library(Hmisc)
library(randomForest)
library(ROCR)

data<-read.csv("~/Dropbox/nescent_extinction_map/data/Combined_paleo_and_modern_data.csv", header = TRUE)
head(data)
data <- drop.levels(subset(data,data$occupancy>1))

### select interval to analyze
data1 <- drop.levels(subset(data,data$stage == "Danian"))


Quartiles <- function(x){
	Q1 <- data.frame(cut2(x, g=10, levels.mean = FALSE))
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

### select factors to analyze from first interval
genus <- data1$genus
group <- as.factor(data1$New_Group)
richness <-Quartiles(round(log(data1$richness+1),1))
occupancy <-Quartiles(round(log(data1$occupancy+1),1))
convex.hull <- Quartiles(round(log(data1$convex_hull+1),1))
great.circle <- Quartiles(round(log(data1$great_circle+1),1))
lat.aff <- round((data1$Trop_Aff*100)/20,0)*20
prop.trop  <- round((data1$Prop_Trop*100)/20,0)*20
life.habit <- as.factor(data1$life.habit)
locomotion <- as.factor(data1$loco)
clone <- as.factor(data1$clone)
diet <- as.factor(data1$diet)
eyes <- as.factor(data1$eyes)

Ex <- data1$Extinct_in_stage
Ex1 <- data1$Extinct_in_stage
ext <- as.factor(ifelse(Ex ==1,"extinct","survive"))
Class <- as.factor(data1$class)



data1factors <- data.frame(group,richness,great.circle,occupancy,convex.hull,lat.aff,prop.trop,ext,Class,life.habit,locomotion,diet,eyes)
data1factors <- drop.levels(na.omit(data1factors))

NumEx <- ifelse(data1factors$ext == "extinct",1,0)
obs <- length(NumEx)
exes <- sum(NumEx)
ExFreq <- (exes/obs)
SurFreq <- (1-(exes/obs))
MaxFreq <- max(ExFreq,SurFreq)
ExWeight <- 1/(ExFreq/MaxFreq)
SurWeight <- 1/(SurFreq/MaxFreq)

###generate random forest model

weights <- ifelse(data1factors$ext=="extinct",ExWeight,SurWeight)
#outs <- randomForest(ext ~.,data = train, classwt = c(ExWeight, SurWeight))
outs2 <- cforest(ext ~.,data = data1factors, weights = weights,controls = cforest_unbiased(mtry =3, trace = TRUE))
outs1 <- randomForest(ext ~.,data = data1factors, classwt = c(ExWeight,SurWeight))

#### predict class probabilities
pr1 <- prediction(predict(outs1, type = "prob")[,2], labels = data1factors$ext)
performance(pr1,measure="auc")
data1factors$probabilities <- 1-unlist(treeresponse(outs2),use.names=FALSE)[seq(1,nrow(data1factors)*2,2)]
pr2 <- prediction(data1factors$probabilities,data1factors$ext)
performance(pr2,measure="auc")
perf1 <- performance(pr1,"tpr","fpr")
perf2 <- performance(pr2,"tpr","fpr")
quartz("rforest",5,5)
plot(perf1,main="rforest ROC",colorize = TRUE)
quartz("cforest",5,5)
plot(perf2,main="cforest ROC",colorize = TRUE)


###########################################################################################################






plot(perf1)

pr2 <- prediction(predict(outs2, type = "prob"), labels = data1factors$ext)




Act <- data1factors$ext
pr1 <- predict(outs1,type="prob")
pr2 <- predict(outs2,type="prob")


table(pr1,Act)
table(pr2,Act)

preds1 <- ifelse(pr1 == "extinct",1,0)
preds2 <- ifelse(pr2 == "extinct",1,0)
Ext <-ifelse(Act == "extinct",1,0)
AUC1 <- roc.area(preds1,Ext)$A
AUC2 <- roc.area(preds2,Ext)$A
AUC1
AUC2










perf1 <- performance(pr1,"tpr","fpr")
performance(pr1,measure="auc")
plot(perf1)
varImpPlot(outs1)


perf <- performance(pr,"tpr","fpr")
performance(pr,measure="auc")
varImpPlot(outs)
plot(perf)
par(mfrow = c(2, 4), cex = 0.8, mar = c(4, 1, 1, 1))

