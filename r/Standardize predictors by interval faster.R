library(Hmisc)
library(gdata)
library(plotrix)

standardize_data <- function(x) {
	
mod_data <- drop.levels(subset(data,data$Interval_Name %in% x))
#mod_data <- drop.levels(subset(data,data$Interval_Name == "Upper Miocene"))
	
data1 <- mod_data
stage_name <- data1$Interval_Name[1]
stage_top <- data1$stage_top[1]	
	
#### define function for converting to quantiles
#Quantiles1 <- function(x){
#	Q1 <- data.frame(cut2(x, g=Num_pred_quantiles1, levels.mean = FALSE))
#    Q1 <- data.frame(seq(1,length(Q1[,1]),1),Q1)
#    colnames(Q1) <- c("num","Q")
#    Qlev <- levels(as.factor(Q1$Q))
#    Qord <- seq(1,length(Qlev),by = 1)
 #   Qlookup <- data.frame(Qlev,Qord)
 #   colnames(Qlookup) <- c("Q","quant")
 #   Qquant <- merge(Qlookup,Q1,by = "Q",sort = FALSE)
  #  Qquant <- Qquant[order(Qquant$num),]
  #  return(Qquant$quant)
  #  }

#### define function for converting to quantiles
#Quantiles2 <- function(x){
  #Q1 <- data.frame(cut2(x, g=Num_pred_quantiles2, levels.mean = FALSE))
  #Q1 <- data.frame(seq(1,length(Q1[,1]),1),Q1)
  #colnames(Q1) <- c("num","Q")
  #Qlev <- levels(as.factor(Q1$Q))
  #Qord <- seq(1,length(Qlev),by = 1)
  #Qlookup <- data.frame(Qlev,Qord)
  #colnames(Qlookup) <- c("Q","quant")
  #Qquant <- merge(Qlookup,Q1,by = "Q",sort = FALSE)
 # Qquant <- Qquant[order(Qquant$num),]
 #return(Qquant$quant)
#}


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

#richness <-Quantiles1(round(log(data1$richness+1),1))
#occupancy <-Quantiles2(round(log(data1$eac+1),1))
#occurrences <- Quantiles1(round(log(data1$occurrences+1),1))
richness <- ceiling(rescale(log(data1$richness+1),c(0,1))*Num_Bins)/Num_Bins
occupancy<- ceiling(rescale(log(data1$eac+1),c(0,1))*Num_Bins)/Num_Bins
occurrences <- ceiling(rescale(log(data1$occurrences+1),c(0,1))*Num_Bins)/Num_Bins
#isNA <- is.na(data1$gcd)
gcd <- data1$gcd_corrected
#unrescaled great circle
great.circle <- ceiling(gcd/Great.Circle.Bin)*Great.Circle.Bin
# rescaled great circle
#great.circle <- ceiling(rescale(gcd,c(0,1))*Num_Bins)/Num_Bins

lats <- data.frame(abs(data1$MinLat),abs(data1$MaxLat))

#paleolats & longs rounded
min.lat <- floor(apply(lats,1,min)/Min.Lat.Bin)*Min.Lat.Bin
max.lat <- ceiling(apply(lats,1,max)/Max.Lat.Bin)*Max.Lat.Bin
lat.range <- ceiling((data1$MaxLat-data1$MinLat +.1)/Lat.Range.Bin)*Lat.Range.Bin
mean.lat <- ceiling(data1$mean_lat/Mean.Lat.Bin)*Mean.Lat.Bin
#mean.lat.zone <- round(data1$mean_lat_zone/Mean.Lat.Zone.Bin,0)*Mean.Lat.Zone.Bin
tropical_only <- ifelse(max.lat > 30,0,1)
#temp.min <- apply(lats,1,min)
#temp.max <- apply(lats,1,max)
#min.lat <- ifelse(temp.min <= 30 & temp.max <= 30,30,60)
#rescale lat variables if desired:
#min.lat <- rescale(min.lat,c(0,1))
#mean.lat <- rescale(mean.lat,c(0,1))
#max.lat <- rescale(max.lat,c(0,1))
#lat.range <- rescale(lat.range,c(0,1))



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


data1factors <- drop.levels(na.omit(data.frame(stage,stage_top,class,group,genus,richness,occupancy,occurrences,min.lat,max.lat,lat.range,mean.lat,great.circle,Ex)))
#data1factors <- drop.levels(na.omit(data.frame(stage,stage_top,class,group,genus,richness,occupancy,occurrences,lat.range,great.circle,tropical_only,Ex)))
return(list(stand_data = data1factors))
}

### select whether to use within-realm interpolated or non-interpolated OBIS ranges
data <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Modern and PaleoDB ranges FINAL 4.0.csv",header = TRUE,stringsAsFactors = FALSE)



data <- drop.levels(subset(data,data$use==1 & data$occurrences >= Min_PBDB_Occurrences &  data$OBIS_occurrences >= Min_Modern_Occurrences & data$OBIS_provinces >= Min_Prov & data$gcd_corrected  >= Min_gcd & data$Localities >= Min_PBDB_Loc & data$OBIS_localities >= Min_OBIS_Loc & data$Num_Stage >= Minimum_Duration & data$eac >= Min_Occupancy & data$class != "Foraminifera" & (data$OBIS_Ranges == Input_ranges | data$OBIS_Ranges == 0)))


# & data$Single_Cenozoic_Occ==0 
#Merge in mean lats if using older spreadsheet
###merge in mean lats -a posteriori fix, should eventually be included in Simpson script
#mean.lats <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Genus mean occurrence lats.csv",header = TRUE,stringsAsFactors = FALSE)
#data <- merge(data,mean.lats)
#p <- ggplot(data,aes(gcd))
#p + geom_histogram() + facet_wrap(~Interval_Name) + scale_y_log10()




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
	
	
