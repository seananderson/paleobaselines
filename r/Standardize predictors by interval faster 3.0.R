library(Hmisc)
library(gdata)
library(matrixStats)

standardize_data <- function(x) {
	
mod_data <- drop.levels(subset(data,data$Interval_Name %in% x))
#mod_data <- drop.levels(subset(data,data$Interval_Name == "Upper Miocene"))
	
data1 <- mod_data
stage_name <- data1$Interval_Name[1]
stage_top <- data1$stage_top[1]	
	

### create factors to analyze from prediction interval, round if desired
genus <- data1$genus
class <- data1$class
group <- data1$MatchTaxon
class.rand <- sample(class,length(class),replace=FALSE)
group.rand <- sample(group,length(group),replace=FALSE)

## take randomized class and group if desired
GroupRand <- rep(rand.group,length(group))

group <- as.factor(ifelse(GroupRand=="yes",group.rand,group))
class <- as.factor(ifelse(GroupRand=="yes",class.rand,class))

max.rich <- max(round(log(data1$richness),0))
richness <- round(log(data1$richness),0)/max.rich

max.occupancy <- max(round(log(data1$eac),0))
occupancy <- round(log(data1$eac),0)/max.occupancy

max.occurrences <- max(round(log(data1$occurrences),0))
occurrences <- round(log(data1$occurrences),0)/max.occurrences 

#isNA <- is.na(data1$gcd)
gcd <- data1$gcd_corrected
great.circle <- ceiling(gcd/Great.Circle.Bin)*Great.Circle.Bin
lats <- data.frame(abs(data1$MinLat),abs(data1$MaxLat))

#paleolats & longs rounded
min.lat <- ceiling(apply(lats,1,min)/Min.Lat.Bin)*Min.Lat.Bin
max.lat <- ceiling(apply(lats,1,max)/Max.Lat.Bin)*Max.Lat.Bin
lat.range <- max.lat-min.lat
#lat.range <- ceiling((data1$MaxLat-data1$MinLat +.1)/Lat.Range.Bin)*Lat.Range.Bin
mean.lat <- round(abs(data1$mean_lat)/Mean.Lat.Bin)*Mean.Lat.Bin
mean.lat.zone <- round(data1$mean_lat_zone/Mean.Lat.Zone.Bin)*Mean.Lat.Zone.Bin
tropical_only <- ifelse(max.lat > 30,0,1)

#paleolats & longs unrounded
#min.lat <- apply(lats,1,min)
#max.lat <- apply(lats,1,max)
#lat.range <- (data1$MaxLat-data1$MinLat)

## use randomized extinction/survival column if desired
Ex.Actual <- data1$Extinct.in.stage
Ex.Rand <- sample(Ex.Actual,length(Ex.Actual),replace=FALSE)
UseRand <- rep(rand.ex,length(Ex.Actual))

Ex <- ifelse(UseRand=="yes",Ex.Rand,Ex.Actual)


stage <- rep(stage_name,length(Ex))
stage_top <- rep(stage_top,length(Ex))


data1factors <- drop.levels(na.omit(data.frame(stage,stage_top,class,group,genus,richness,occupancy,occurrences,min.lat,max.lat,lat.range,mean.lat,mean.lat.zone,great.circle,tropical_only,Ex)))
#data1factors <- drop.levels(na.omit(data.frame(stage,stage_top,class,group,genus,richness,occupancy,occurrences,lat.range,great.circle,tropical_only,Ex)))
return(list(stand_data = data1factors))
}

### select whether to use within-realm interpolated or non-interpolated OBIS ranges
data <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Modern and PaleoDB ranges FINAL 2.0.csv",header = TRUE,stringsAsFactors = FALSE)

data <- drop.levels(subset(data,data$use==1 & data$occurrences >= Min_PBDB_Occurrences &  data$OBIS_occurrences >= Min_Modern_Occurrences & data$Num_Stage >= Minimum_Duration & data$class != "Foraminifera" & (data$OBIS_Ranges == Input_ranges | data$OBIS_Ranges == 0)))


### merge in mean lats -a posteriori fix, should eventually be included in Simpson script
mean.lats <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Genus mean occurrence lats.csv",header = TRUE,stringsAsFactors = FALSE)

data <- merge(data,mean.lats)
data$AbsLatRange <- data$MaxAbsLat - data$MinAbsLat 

###HERE IS THE PLACE TO EXTRACT MODERN VS. PLIO-PLEIS RANGES ETC., COMPARE, COME UP WITH CORRECTIONS

PlioPleisData <- drop.levels(subset(data,data$Interval_Name == "Plio-Pleistocene"))
ModData <- drop.levels(subset(data,data$Interval_Name == "Modern"))
newdata <- merge(ModData,PlioPleisData,by="genus",all.x=FALSE,all.y=FALSE)
#quartz()
#qplot(log(newdata$gcd.x),log(newdata$gcd.y)) + geom_abline() + geom_smooth(method="lm")
#gcd_mod <- coefficients(lm(log(newdata$gcd.y) ~log(newdata$gcd.x)))
#gcdslope <- gcd_mod[2]
#gcdint <- gcd_mod[1]
#gcd_corrected <- log(newdata$gcd.y)/gcdslope - gcdint
#quartz()
#qplot(log(newdata$gcd.x),gcd_corrected )+ geom_abline() + geom_smooth(method="lm")

corrections <- function(df) {
  #df <- newdata
   gcd_mod <- coefficients(lm(log(df$gcd_corrected.y) ~ log(df$gcd_corrected.x)))
   gcd_slope <- gcd_mod[2]
   gcd_int <- gcd_mod[1]
   rich_mod <- coefficients(lm(log(df$richness.y) ~ log(df$richness.x)))
   rich_slope <-  rich_mod[2]
   rich_int <-  rich_mod[1]
   occupancy_mod <- coefficients(lm(log(df$eac.y) ~ log(df$eac.x)))
   occupancy_slope <-  occupancy_mod[2]
   occupancy_int <-  occupancy_mod[1]
   occurrences_mod <- coefficients(lm(log(df$occurrences.y) ~ log(df$occurrences.x)))
   occurrences_slope <-  occurrences_mod[2]
   occurrences_int <-  occurrences_mod[1]
   MinLat_mod <- coefficients(lm(log(df$MinAbsLat.y) ~ log(df$MinAbsLat.x)))
   MinLat_slope <-  MinLat_mod[2]
   MinLat_int <-  MinLat_mod[1]
   MaxLat_mod <- coefficients(lm(log(df$MaxAbsLat.y) ~ log(df$MaxAbsLat.x)))
   MaxLat_slope <-  MaxLat_mod[2]
   MaxLat_int <-  MaxLat_mod[1]
   MeanLat_mod <- coefficients(lm(log(df$mean_lat.y ~ log(df$mean_lat.x)))
   MeanLat_slope <-  MeanLat_mod[2]
   MeanLat_int <-  MeanLat_mod[1]
   LatRange_mod <- coefficients(lm(log(df$AbsLatRange.y) ~ log(df$AbsLatRange.x)))
   LatRange_slope <- LatRange_mod[2]
   LatRange_int <- LatRange_mod[2]
   
   return(data.frame(gcd_slope,gcd_int,rich_slope,rich_int,occupancy_slope,occupancy_int,occurrences_slope,occurrences_int,MinLat_slope,MinLat_int,MaxLat_slope,MaxLat_int,MeanLat_slope,MeanLat_int,LatRange_slope,LatRange_int))
}
coefs <- ddply(newdata,.(Interval_Name.x ),corrections)

###Apply calibrated corrections to all non-modern intervals
data$gcd_corrected <- ifelse(data$Interval_Name=="Modern",data$gcd_corrected,exp(log(data$gcd_corrected)/coefs$gcd_slope - coefs$gcd_int))

data$richness <- ifelse(data$Interval_Name=="Modern",data$richness,exp(log(data$richness)/coefs$rich_slope - coefs$rich_int))

data$eac <- ifelse(data$Interval_Name=="Modern",data$eac,exp(log(data$eac)/coefs$occupancy_slope - coefs$occupancy_int))
#eac2 <- ifelse(data$Interval_Name=="Modern",data$eac,(data$eac/coefs$occupancy_slope - coefs$occupancy_int))

data$occurrences <- ifelse(data$Interval_Name=="Modern",data$occurrences,exp(log(data$occurrences)/coefs$occurrences_slope - coefs$occurrences_int))

data$MaxAbsLat <- ifelse(data$Interval_Name=="Modern",data$MaxAbsLat,exp(log(data$MaxAbsLat)/coefs$MaxLat_slope - coefs$MaxLat_int))
#MaxAbsLat2 <- ifelse(data$Interval_Name=="Modern",data$MaxAbsLat,(data$MaxAbsLat/coefs$MaxLat_slope - coefs$MaxLat_int))

data$MinAbsLat <- ifelse(data$Interval_Name=="Modern",data$MinAbsLat,exp(log(data$MinAbsLat)/coefs$MinLat_slope - coefs$MinLat_int))

data$mean_lat <- ifelse(data$Interval_Name=="Modern",data$mean_lat,exp(log(data$mean_lat)/coefs$MeanLat_slope - coefs$MeanLat_int))

data$AbsLatRange <- ifelse(data$Interval_Name=="Modern",data$AbsLatRange,exp(log(data$AbsLatRange)/coefs$LatRange_slope - coefs$LatRange_int))



stage_names <- list()
for(i in 1:length(unique(data$Interval_Name))) {
  stage_names[[i]] <- unique(data$Interval_Name)[i]
}

out <- lapply(stage_names, function(x) standardize_data(x))

data.out <- list()

for(i in 1:length(out)) {
  data.out[[i]] <- out[[i]]$stand_data
}

standardized.cenozoic <- do.call("rbind", data.out)
save(standardized.cenozoic,file="~/Dropbox/nescent_extinction_map/Final data/standardized.predictors.Cenozoic.OBIS.rda")	
	
	
