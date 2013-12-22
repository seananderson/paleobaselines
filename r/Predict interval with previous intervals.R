library(randomForest)
library(matrixStats)
require(verification)
library(gdata)
library(party)
library(plyr)
library(Hmisc)
library(randomForest)
library(ROCR)
gc()
data<-read.csv("~/Dropbox/nescent_extinction_map/data/Combined_paleo_and_modern_data.csv", header = TRUE)
head(data)
data <- drop.levels(subset(data,data$occupancy>1))

### select interval to analyze
data1 <- drop.levels(subset(data,data$stage == "Middle Miocene"))

### select interval to analyze with forest models from first interval
data2 <- drop.levels(subset(data,data$stage == "Upper Miocene"))
MatchGenera <- data.frame(data2$genus)
colnames(MatchGenera) <- c("genus")

#### drop groups that do not occur in stage
Taxa1 <- data.frame(levels(data1$New_Group))
colnames(Taxa1) <- c("Taxa")
Taxa2 <- data.frame(levels(data2$New_Group))
colnames(Taxa2) <- c("Taxa")
Taxa <- merge(Taxa1,Taxa2,by = "Taxa",all.y = FALSE)
colnames(Taxa) <- c("group")

data2 <- drop.levels(merge(data2,Taxa,by = "group", all.x = FALSE))
data1 <- drop.levels(merge(data1,Taxa,by = "group", all.x = FALSE))

### create function to divide observations into quartiles (or,deciles,quintiles,etc.)
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

### select factors to analyze from first interval, round if desired
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



data1factors <- data.frame(group,richness,great.circle,occupancy,convex.hull,lat.aff,prop.trop,ext,Class)
data1factors <- drop.levels(na.omit(data1factors))


### select factors to analyze from first interval
genus <- data2$genus
group <- as.factor(data2$New_Group)
richness <-Quartiles(round(log(data2$richness+1),1))
occupancy <-Quartiles(round(log(data2$occupancy+1),1))
convex.hull <- Quartiles(round(log(data2$convex_hull+1),1))
great.circle <- Quartiles(round(log(data2$great_circle+1),1))
lat.aff <- round((data2$Trop_Aff*100)/20,0)*20
prop.trop  <- round((data2$Prop_Trop*100)/20,0)*20
life.habit <- as.factor(data2$life.habit)
locomotion <- as.factor(data2$loco)
clone <- as.factor(data2$clone)
diet <- as.factor(data2$diet)
eyes <- as.factor(data2$eyes)

Ex <- data2$Extinct_in_stage
Ex1 <- data2$Extinct_in_stage
ext <- as.factor(ifelse(Ex ==1,"extinct","survive"))
Class <- as.factor(data2$class)



data2factors <- data.frame(group,richness,great.circle,occupancy,convex.hull,lat.aff,prop.trop,ext,Class)
data2factors <- drop.levels(na.omit(data2factors))

###Create interval 1 weights

NumEx <- ifelse(data1factors$ext == "extinct",1,0)
obs <- length(NumEx)
exes <- sum(NumEx)
ExFreq <- (exes/obs)
SurFreq <- (1-(exes/obs))
MaxFreq <- max(ExFreq,SurFreq)
ExWeight <- 1/(ExFreq/MaxFreq)
SurWeight <- 1/(SurFreq/MaxFreq)



###generate rforest and cforest models for first interval

weights <- ifelse(data1factors$ext=="extinct",ExWeight,SurWeight)
outs2 <- cforest(ext ~.,data = data1factors, weights = weights,controls = cforest_unbiased(mtry =3, trace = TRUE))
outs1 <- randomForest(ext ~.,data = data1factors, classwt = c(ExWeight,SurWeight))

#### predict class probabilities within interval using both cforest and rforest
pr1 <- prediction(predict(outs1, type = "prob")[,2], labels = data1factors$ext)
performance(pr1,measure="auc")
outs2new <- predict(outs2,type="prob")
data1factors$probabilities <- 1-unlist(treeresponse(outs2),use.names=FALSE)[seq(1,nrow(data1factors)*2,2)]
pr2 <- prediction(data1factors$probabilities,data1factors$ext)
performance(pr2,measure="auc")
perf1 <- performance(pr1,"tpr","fpr")
perf2 <- performance(pr2,"tpr","fpr")
rforest.x <- unlist(perf1@x.values)
rforest.y <- unlist(perf1@y.values)
rforest.alpha <- unlist(perf1@alpha.values)
rforest.type <- rep("rforest",length(rforest.x))
rforest.Interval <- rep("In_Stage",length(rforest.x))
rforest.ROC <- data.frame(rforest.type,rforest.x,rforest.y,rforest.alpha,rforest.Interval)
colnames(rforest.ROC) <- c("Model_type","False_positive_rate","True_positive_rate","alpha","Interval")
cforest.x <- unlist(perf2@x.values)
cforest.y <- unlist(perf2@y.values)
cforest.alpha <- unlist(perf2@alpha.values)
cforest.type <- rep("cforest",length(cforest.x))
cforest.Interval <- rep("In_Stage",length(cforest.x))
cforest.ROC <- data.frame(cforest.type,cforest.x,cforest.y,cforest.alpha,cforest.Interval)
colnames(cforest.ROC) <- c("Model_type","False_positive_rate","True_positive_rate","alpha","Interval")
ROCdata.InStage <- rbind(rforest.ROC,cforest.ROC)

#### predict class probabilities in next interval using both cforest and rforest
pr1 <- prediction(predict(outs1,newdata = data2factors, type = "prob")[,2], labels = data2factors$ext)
performance(pr1,measure="auc")
outs2new <- predict(outs2,newdata=data2factors,type="prob")
data2factors$probabilities <- 1-unlist(outs2new,use.names=FALSE)[seq(1,nrow(data2factors)*2,2)]
pr2 <- prediction(data2factors$probabilities,data2factors$ext)
performance(pr2,measure="auc")
perf1 <- performance(pr1,"tpr","fpr")
perf2 <- performance(pr2,"tpr","fpr")
rforest.x <- unlist(perf1@x.values)
rforest.y <- unlist(perf1@y.values)
rforest.alpha <- unlist(perf1@alpha.values)
rforest.type <- rep("rforest",length(rforest.x))
rforest.Interval <- rep("Next_Stage",length(rforest.x))
rforest.ROC <- data.frame(rforest.type,rforest.x,rforest.y,rforest.alpha,rforest.Interval)
colnames(rforest.ROC) <- c("Model_type","False_positive_rate","True_positive_rate","alpha","Interval")
cforest.x <- unlist(perf2@x.values)
cforest.y <- unlist(perf2@y.values)
cforest.alpha <- unlist(perf2@alpha.values)
cforest.type <- rep("cforest",length(cforest.x))
cforest.Interval <- rep("Next_Stage",length(cforest.x))
cforest.ROC <- data.frame(cforest.type,cforest.x,cforest.y,cforest.alpha,cforest.Interval)
colnames(cforest.ROC) <- c("Model_type","False_positive_rate","True_positive_rate","alpha","Interval")
ROCdata.NextStage <- rbind(rforest.ROC,cforest.ROC)

### combine 
ROCdata <- rbind(ROCdata.InStage,ROCdata.NextStage)

#### plot cforest and rforest ROC curves for within-interval prediction and next interval prediction
library(ggplot2)
p <- ggplot(ROCdata,aes(False_positive_rate,True_positive_rate,colour=Model_type))
p + geom_line() + facet_wrap(~ Interval)

