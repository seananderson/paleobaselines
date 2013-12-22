library(randomForest)
library(matrixStats)
require(verification)
library(gdata)
library(party)
library(plyr)
library(Hmisc)
library(randomForest)
library(ROCR)

data<-read.csv("~/Dropbox/nescent_extinction_map/data/CombinedData2.csv", header = TRUE, na.strings = "")
head(data)
data <- drop.levels(subset(data,data$occupancy>1))

### select first interval to analyze
data1 <- drop.levels(subset(data,data$stagetop > 0 & data$stagetop < 27))

### select interval to analyze with forest model from first interval
data2 <- drop.levels(subset(data,data$stage == "modern"))
MatchGenera <- data.frame(data2$genus)
colnames(MatchGenera) <- c("genus")

#### drop groups that do not occur in stage
Taxa1 <- data.frame(levels(data1$group))
colnames(Taxa1) <- c("Taxa")
Taxa2 <- data.frame(levels(data2$group))
colnames(Taxa2) <- c("Taxa")
Taxa <- merge(Taxa1,Taxa2,by = "Taxa",all.y = FALSE)
colnames(Taxa) <- c("group")

data2 <- drop.levels(merge(data2,Taxa,by = "group", all.x = FALSE))


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

obs <- length(Ex)
exes <- sum(Ex)
ExFreq <- (exes/obs)
SurFreq <- (1-(exes/obs))
MaxFreq <- max(ExFreq,SurFreq)
ExWeight <- 1/(ExFreq/MaxFreq)
SurWeight <- 1/(SurFreq/MaxFreq)


data1factors <- data.frame(group,richness,great.circle,occupancy,convex.hull,lat.aff,prop.trop,ext,Class,life.habit,locomotion,clone,diet,eyes)
data1factors <- na.omit(data1factors)
###generate random forest model

data1factors <- subset(data1factors, !group %in% c("Testudines_Testudines")) 
data1factors <- drop.levels(data1factors)
#data1factors$classwt <- ifelse(data1factors$ext == "extinct", ExWeight, SurWeight)

#training_rows <- sample(1:nrow(data1factors), 700)
#train <- data1factors[training_rows, ]
#oob <- data1factors[-training_rows, ]

#oob$ext <- sample(1:nrow(oob), )

#outs <- randomForest(ext ~.,data = train, classwt = c(ExWeight, SurWeight))
outs <- randomForest(ext ~.,data = data1factors, classwt = c(ExWeight, SurWeight))

pr <- prediction(predict(outs, type = "prob")[,2], labels = data1factors$ext)
perf <- performance(pr,"tpr","fpr")
performance(pr,measure="auc")
varImpPlot(outs)
plot(perf)
par(mfrow = c(2, 4), cex = 0.8, mar = c(4, 1, 1, 1))

source("partialPlot.R") # A custom version so that xvar will work in a function
partialPlotCustom <- function(x.var) {
  #browser()
  p <- partialPlot(outs, pred.data = data1factors, main = "", ylim = c(-6.5, 0), x.var =x.var, plot = FALSE, xlab = "")
  #plot(p$x, exp(p$y), type = "l", xlab = x.var, ylab = "", ylim = c(0, 0.4))
  plot(p$x, p$y, type = "l", xlab = x.var, ylab = "", ylim = c(-8.0, 0), lwd = 1.8, col = "grey10", axes = FALSE)
  axis(1)
  axis(2, at = log(c(0.0001, 0.001, 0.01, 0.1,  0.5, 1)), labels = c(0.0001, 0.001, 0.01, 0.1, 0.5, 1))
  box()

# add taxa lines:
  col.i <<- 0
  sapply(unique(data1factors$Class), function(i) {
         col.i <<- col.i + 1

  p <- partialPlot(outs, pred.data = subset(data1factors, Class == i), main = "", ylim = c(-6.5, 0), x.var =x.var, plot = FALSE, xlab = "")
  lines(p, col = paste(pal[col.i], "80", sep = ""))

})
  


}

library(RColorBrewer)
pal <- brewer.pal(8, "Dark2")
par(mfrow = c(3, 4), cex = 0.75, mar = c(4.5, 3, 1, 1), las = 1)
partialPlotCustom("richness")
partialPlotCustom("great.circle")
partialPlotCustom("occupancy")
partialPlotCustom("convex.hull")
partialPlotCustom("lat.aff")
partialPlotCustom("life.habit")
partialPlotCustom("locomotion")
partialPlotCustom("clone")
partialPlotCustom("diet")
partialPlotCustom("eyes")


legend("topright", legend = unique(data1factors$Class), col = paste(pal[1:length(data1factors$Class)], "95", sep = ""), lty = 1, bty = "n")




#outs <- randomForest(ext ~.,data = data1factors, ntree = 100)


### select factors to analyze from second interval (must be same as first!)

### select factors to analyze from first interval
genus <- data2$genus
group <- as.factor(data2$group)
richness <-Quartiles(round(log(data2$richness+1),1))
occupancy <-Quartiles(round(log(data2$occupancy+1),1))
convex.hull <- Quartiles(round(log(data2$convex_hull+1),1))
great.circle <- Quartiles(round(log(data2$great_circle+1),1))
lat.aff <- round((data2$Trop_Aff*100)/20,0)*20
prop.trop  <- round((data2$Prop_Trop*100)/20,0)*20
Ex <- data2$Extinct_in_stage
Ex2 <- data2$Extinct_in_stage
ext <- as.factor(ifelse(Ex ==1,"extinct","survive"))

data2factors <- data.frame(group,richness,great.circle,occupancy,convex.hull,lat.aff,prop.trop,ext)
data2factors <- na.omit(data2factors)

preds1 <- predict(outs,newdata = data2factors, type = "prob")
predExRisk <- ldply(preds1)
predExRisk <- predExRisk[,2]

### output file with risk estimates for modern taxa
RiskEstimate <- data.frame(data2$genus,predExRisk)
colnames(RiskEstimate) <- c("genus","risk_estimate")
write.table(RiskEstimate,"Modern fauna risk estimates STAGE.csv", sep=",")







p <- ggplot(RiskEstimate,aes(log(RiskEstimate$occupancy+1),RiskEstimate$risk_estimate,colour=as.factor(RiskEstimate$class)))
p + geom_point(pch =1)
