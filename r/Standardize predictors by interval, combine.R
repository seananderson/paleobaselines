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
OBIS_data <- drop.levels(subset(data,data$use==1 & data$occurrences >= 30 & stage_top == 0 & data$class != "Foraminifera" & data$Interval_Name == "Spalding_merged"))
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

data1 <- L_Miocene
stage <- "L_Miocene" 

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
#isNA <- is.na(data1$gcd)
#gcd <- ifelse(isNA == "TRUE",0,data1$gcd_corrected)
#great.circle <- round(log(gcd),0)
#min.lat <- ceiling(abs(data1$MinLat)/10)*10
#max.lat <- ceiling(abs(data1$MaxLat)/10)*10
#lat.range <- ceiling((data1$MaxLat-data1$MinLat)/10)*10
lats <- data.frame(abs(data1$MinLat),abs(data1$MaxLat))
min.lat <- floor(apply(lats,1,min)/10)*10
max.lat <- ceiling(apply(lats,1,max)/10)*10
lat.range <- round((data1$MaxLat-data1$MinLat)/10,0)*10

class <- as.factor(data1$class)
Ex <- data1$Extinct.in.stage
stage <- rep(stage,length(Ex))

data1factors <- drop.levels(na.omit(data.frame(stage,group,genus,richness,occupancy,occurrences,min.lat,max.lat,lat.range,Ex)))
#data1factors <- drop.levels(na.omit(data.frame(stage,group,richness,occupancy,occurrences,min.lat,max.lat,lat.range,Ex)))



Chattian <- data1factors


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


All_Neogene_Transformed <- rbind(Pleistocene_trans,Pliocene_trans,U_Miocene_trans,M_Miocene_trans,L_Miocene_trans)
All_Cenozoic_Transformed <-rbind(Pleistocene_trans,Pliocene_trans,U_Miocene_trans,M_Miocene_trans,L_Miocene_trans,Chattian_trans,Rupelian_trans,Priabonian_trans,Bartonian_trans,Lutetian_trans,Ypresian_trans,Selandian_Thanetian_trans,Danian_trans)


write.table(All_Neogene_Transformed,"~/Dropbox/nescent_extinction_map/Final data/All_Neogene_Transformed.csv",sep=",")
write.table(All_Cenozoic_Transformed,"~/Dropbox/nescent_extinction_map/Final data/All_Cenozoic_Transformed.csv",sep=",")
write.table(Modern_trans,"~/Dropbox/nescent_extinction_map/Final data/Modern_Transformed.csv",sep=",")


