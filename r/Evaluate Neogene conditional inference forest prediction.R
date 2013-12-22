library(randomForest)
library(matrixStats)
require(verification)
library(gdata)
library(party)
library(plyr)
library(Hmisc)

data <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Modern and PaleoDB ranges FINAL.csv",header = TRUE,stringsAsFactors = FALSE)
data <- drop.levels(subset(data,data$occurrences>1 & data$use==1))
head(data)

### select first interval to analyze; take subset if desired
data1 <- drop.levels(subset(data,data$stage_top > 0 & data$stage_top < 22))

### select interval to analyze with forest model from first interval
data2 <- drop.levels(subset(data,data$Interval_Name == "Modern Spalding Merged"))

MatchGenera <- data.frame(data2$genus)
colnames(MatchGenera) <- c("genus")

#### drop groups that do not occur in stage
Taxa1 <- data.frame(levels(as.factor(data1$MatchTaxon)))
colnames(Taxa1) <- c("Taxa")
Taxa2 <- data.frame(levels(as.factor(data2$MatchTaxon)))
colnames(Taxa2) <- c("Taxa")
Taxa <- merge(Taxa1,Taxa2,by = "Taxa",all.y = FALSE)
colnames(Taxa) <- c("MatchTaxon")

data2 <- drop.levels(merge(data2,Taxa,by = "MatchTaxon", all.x = FALSE))
data1 <- drop.levels(merge(data1,Taxa,by = "MatchTaxon", all.x = FALSE))

Quartiles <- function(x){
	Q1 <- data.frame(cut2(x, g=4, levels.mean = FALSE))
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


data1factors <- data.frame(class,group,richness,great.circle,occupancy,min.lat,max.lat,lat.range,ext)
data1factors <- drop.levels(na.omit(data1factors))

###generate random forest model
outs <- cforest(ext ~.,data = data1factors, weights = ifelse(ext=="extinct",ExWeight,SurWeight),controls = cforest_unbiased(mtry =3, trace = TRUE))
preds <- predict(outs)
table(preds,ext)
preds <- ifelse(preds == "extinct",1,0)
ext <-ifelse(ext == "extinct",1,0)
AUC <- roc.area(preds,ext)$A
AUC

#Imps <- varimp(outs)

#barplot(sort(Imps), horiz=TRUE, xlab="Variable Importance\n(predictors to right of dashed vertical line are significant)")
#abline(v=abs(min(Imps)), col="red", lty="longdash", lwd=2)
#abline(v=0, col="black")




### select factors to analyze from second interval (must be same as first!)

### select factors to analyze from first interval
### select factors to analyze from first interval, round if desired
genus <- data2$genus
group <- as.factor(data2$MatchTaxon)
richness <-Quartiles(round(log(data2$richness+1),1))
occupancy <-Quartiles(round(log(data2$eac+1),1))
isNA <- is.na(data2$gcd)
gcd <- ifelse(isNA == "TRUE",0,data2$gcd)
great.circle <- round(log(gcd+1),1)
min.lat <- data2$MinLat
max.lat <- data2$MaxLat
lat.range <- max.lat-min.lat
life.habit <- as.factor(data2$life.habit)
locomotion <- as.factor(data2$loco)
diet <- as.factor(data2$diet)
eyes <- as.factor(data2$eyes)
class <- as.factor(data2$class)
Ex <- data2$Extinct.in.stage
ext <- as.factor(ifelse(Ex ==1,"extinct","survive"))

data2factors <- data.frame(class,group,richness,great.circle,occupancy,min.lat,max.lat,lat.range,ext)
data2factors <- na.omit(data2factors)

preds1 <- predict(outs,newdata = data2factors, type = "prob")
predExRisk <- ldply(preds1)
predExRisk <- predExRisk[,2]

### output file with risk estimates for modern taxa
RiskEstimate <- data.frame(data2factors$class,data2factors$group,data2$genus,data2factors$richness,data2factors$occupancy,data2factors$great.circle,data2factors$min.lat,data2factors$max.lat,data2factors$lat.range,predExRisk)
colnames(RiskEstimate) <- c("class","group","genus","richness","occupancy","great_circle","min_lat","max_lat","lat_range","risk_estimate")
write.table(RiskEstimate,"Modern fauna risk estimates STAGE_FINAL.csv", sep=",")



p <- ggplot(RiskEstimate,aes(log(RiskEstimate$occupancy+1),RiskEstimate$risk_estimate,colour=as.factor(RiskEstimate$class)))
p + geom_point(pch =1)
