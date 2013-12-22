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
library(gridExtra)

data <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Modern and PaleoDB ranges FINAL.csv",header = TRUE,stringsAsFactors = FALSE)
OBIS_data <- drop.levels(subset(data,data$use==1 & data$occurrences > 30 & stage_top == 0 & data$class != "Foraminifera" & data$Interval_Name == "Spalding_merged"))
data <- drop.levels(subset(data,data$use==1 & data$occurrences > 1 & stage_top > 0 & data$class != "Foraminifera"))
head(OBIS_data)

#unique(data$Interval_Name)
#  [1] "Danian"              "Selandian-Thanetian" "Ypresian"            "Lutetian"           
#  [5] "Bartonian"           "Priabonian"          "Rupelian"            "Chattian"           
#  [9] "Lower Miocene"       "Middle Miocene"      "Upper Miocene"       "Pliocene"           
# [13] "Pleistocene"         "Spalding_raw"        "Spalding_merged"    

### break out datasets for each interval

Pleistocene <- drop.levels(subset(data,data$Interval_Name == "Pleistocene"))
Pliocene <- drop.levels(subset(data,data$Interval_Name == "Pliocene"))
U_Miocene <- drop.levels(subset(data,data$Interval_Name == "Upper Miocene"))
M_Miocene <- drop.levels(subset(data,data$Interval_Name == "Middle Miocene"))
L_Miocene <- drop.levels(subset(data,data$Interval_Name == "Lower Miocene"))
Chattian <- drop.levels(subset(data,data$Interval_Name == "Chattian"))
Rupelian <- drop.levels(subset(data,data$Interval_Name == "Rupelian"))
Priabonian <- drop.levels(subset(data,data$Interval_Name == "Priabonian"))
Bartonian <- drop.levels(subset(data,data$Interval_Name == "Bartonian"))
Lutetian <- drop.levels(subset(data,data$Interval_Name == "Lutetian"))
Ypresian <- drop.levels(subset(data,data$Interval_Name == "Ypresian"))
Selandian_Thanetian <- drop.levels(subset(data,data$Interval_Name == "Selandian-Thanetian"))
Danian <- drop.levels(subset(data,data$Interval_Name == "Danian"))
Modern <- OBIS_data



### make list of numbers for taxa

Taxon <- levels(as.factor(data$MatchTaxon))
Taxon_Num <- seq(1:length(Taxon))
Taxon_Number <- data.frame(Taxon,Taxon_Num)


### set working stage

data1 <- Modern
stage <- "Modern" 

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
min.lat <- ceiling(abs(data1$MinLat)/10)*10
max.lat <- ceiling(abs(data1$MaxLat)/10)*10
lat.range <- ceiling((data1$MaxLat-data1$MinLat)/10)*10
class <- as.factor(data1$class)
Ex <- data1$Extinct.in.stage
stage <- rep(stage,length(Ex))

data1factors <- drop.levels(na.omit(data.frame(stage,group,richness,occupancy,occurrences,min.lat,max.lat,lat.range,Ex)))




Modern_trans <- data1factors


Pleistocene_trans
Pliocene_trans
U_Miocene_trans
M_Miocene_trans
L_Miocene_trans
Chattian_trans
Rupelian_trans
Priabonian_trans
Bartonian_trans
Lutetian_trans
Ypresian_trans
Selandian_Thanetian_trans
Danian_trans
Modern_trans


All_Neogene_Transformed <- rbind(Pleistocene_trans,Pliocene_trans,U_Miocene_trans,M_Miocene_trans,L_Miocene_trans,Modern_trans)
All_Cenozoic_Transformed <-rbind(Pleistocene_trans,Pliocene_trans,U_Miocene_trans,M_Miocene_trans,L_Miocene_trans,Chattian_trans,Rupelian_trans,Priabonian_trans,Bartonian_trans,Lutetian_trans,Ypresian_trans,Selandian_Thanetian_trans,Danian_trans,Modern_trans)

write.table(All_Neogene_Transformed,"~/Dropbox/nescent_extinction_map/Final data/All_Neogene_Transformed.csv",sep=",")
write.table(All_Cenozoic_Transformed,"~/Dropbox/nescent_extinction_map/Final data/All_Cenozoic_Transformed.csv",sep=",")

#logistic_model <- glm(Ex ~ ., data=data1factors, family=binomial(link="logit"))

#anova(logistic_model , test="Chisq")

#plot(logistic_model$fitted)




library(gbm)

iris.mod <- gbm(Ex ~ ., distribution="bernoulli", data=data1factors, n.trees=2000, shrinkage=0.01, cv.folds=5, verbose=FALSE)
iris.mod
relative.influence( iris.mod, 1000)
par(mfrow=c(2,4))
plot(...)
one <- plot.gbm(iris.mod, i.var = 1, n.trees = iris.mod$n.trees, continuous.resolution = 100, return.grid = TRUE, type = "response")
two <- plot.gbm(iris.mod, i.var = 2, n.trees = iris.mod$n.trees, continuous.resolution = 100, return.grid = TRUE, type = "response")
three <- plot.gbm(iris.mod, i.var = 3, n.trees = iris.mod$n.trees, continuous.resolution = 100, return.grid = TRUE, type = "response")
four <- plot.gbm(iris.mod, i.var = 4, n.trees = iris.mod$n.trees, continuous.resolution = 100, return.grid = TRUE, type = "response")
five <- plot.gbm(iris.mod, i.var = 5, n.trees = iris.mod$n.trees, continuous.resolution = 100, return.grid = TRUE, type = "response")
six <- plot.gbm(iris.mod, i.var = 6, n.trees = iris.mod$n.trees, continuous.resolution = 100, return.grid = TRUE, type = "response")
seven <- plot.gbm(iris.mod, i.var = 7, n.trees = iris.mod$n.trees, continuous.resolution = 100, return.grid = TRUE, type = "response")

one <- data.frame(rep("group",length(one[,1])),one)
colnames(one) <- c("predictor","value","response")

two<- data.frame(rep("richness",length(two[,1])),two)
colnames(two) <- c("predictor","value","response")

three <- data.frame(rep("occupancy",length(three[,1])),three)
colnames(three) <- c("predictor","value","response")

four <- data.frame(rep("occurrences",length(four[,1])),four)
colnames(four) <- c("predictor","value","response")

five <- data.frame(rep("min.lat",length(five[,1])),five)
colnames(five) <- c("predictor","value","response")

six <- data.frame(rep("max.lat",length(six[,1])),six )
colnames(six ) <- c("predictor","value","response")

seven <- data.frame(rep("lat.range",length(seven[,1])),seven)
colnames(seven) <- c("predictor","value","response")



p1 <-  ggplot(one, aes(x=value, y=response)) + geom_point() + ggtitle("Group") + scale_y_continuous(limits = c(0,0.5)) + ylab(expression("")) + xlab(expression("")) + opts(axis.text.x = theme_text(size=8,angle=-25))

p2 <-  ggplot(two, aes(x=value, y=response)) + geom_line() + ggtitle("Richness") + scale_y_continuous(limits = c(0,0.12))+ ylab(expression("")) + xlab(expression(""))

p3 <- ggplot(three, aes(x=value, y=response)) + geom_line() + ggtitle("Occupancy")+ scale_y_continuous(limits = c(0,0.12))+ ylab(expression("")) + xlab(expression(""))

p4 <- ggplot(four, aes(x=value, y=response)) + geom_line() + ggtitle("Occurrences")+ scale_y_continuous(limits = c(0,0.12))+ ylab(expression("")) + xlab(expression(""))

p5 <- ggplot(five, aes(x=value, y=response)) + geom_line() + ggtitle("Minimum Latitude")+ scale_y_continuous(limits = c(0,0.12))+ ylab(expression("")) + xlab(expression(""))

p6 <- ggplot(six, aes(x=value, y=response)) + geom_line() + ggtitle("Maximum Latitude")+ scale_y_continuous(limits = c(0,0.12))+ ylab(expression("")) + xlab(expression(""))

p7 <- ggplot(seven, aes(x=value, y=response)) + geom_line() + ggtitle("Latitudinal Range")+ scale_y_continuous(limits = c(0,0.12))+ 
ylab(expression("")) + xlab(expression(""))


multiplot(p1, p2, p3, p4, p5, p6, p7, layout = matrix(c(1,1,1,2,3,4,5,6,7), 3, 3, byrow = TRUE))



Pleis_p1 <- p1
Pleis_p2 <- p2
Pleis_p3 <- p3
Pleis_p4 <- p4
Pleis_p5 <- p5
Pleis_p6 <- p6
Pleis_p7 <- p7











#grid.arrange(arrangeGrob(p1,p2,p3,p4, p5, p6, p7))

#p <- arrangeGrob(p1, p2, p3, p4, p5, p6, p7,layout = matrix(c(1,1,1,2,3,4,5,6,7), 3, 3, byrow = TRUE))








#grid1 <- do.call(grid.Arrange, c(p2,p3,p4,p5,p6,p7))





nf <- layout(matrix(c(1,1,1,2,3,4,5,6,7), 3, 3, byrow = TRUE),heights=c(4,2,2))
layout.show(nf)





nf <- layout(matrix(c(2,0,1,3),2,2,byrow=TRUE),widths=c(6,1),heights=c(1,6),TRUE)
layout.show(nf)
