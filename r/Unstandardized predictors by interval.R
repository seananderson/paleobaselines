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

#max.rich <- max(round(log(data1$richness),0))
richness <- round(log(data1$richness),0)

#max.occupancy <- max(round(log(data1$eac),0))
occupancy <- round(log(data1$eac),0)

#max.occurrences <- max(round(log(data1$occurrences),0))
occurrences <- round(log(data1$occurrences),0)

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
	
	
