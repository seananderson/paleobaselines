load("~/Dropbox/nescent_extinction_map/Final data/PaleoDB_6.11.2013_grid.cells.rda")
data<-pd
head(data)
library(fields)
library(plyr)
library(PBSmapping)
library(maptools)

slcs <- data.frame(c(69,70,71,72),c("Lower Miocene","Middle Miocene","Upper Miocene","Plio-Pleistocene"))
colnames(slcs) <- c("slc","Interval")

data <- merge(data,slcs,by="Interval")

data$mid_lat <- round(data$mid_lat,0)
data$mid_long <- round(data$mid_long,0)

# process function
load("~/Dropbox/nescent_extinction_map/data/equal_area_grid/global_45x14.rda")
LatSeq <- global_45x14$latitude
LongSeq <- global_45x14$longitude
polys<- makeGrid (x=round(LongSeq,0), y=round(LatSeq,0),projection="UTM")

genbybin <- function(df){
	#df <- data
#   write.table(unique(as.character(df$clgen1)), "genus.fail.txt")
  if(length(unique(na.omit(as.factor(paste(df$mid_lat,df$mid_long)))))>1){
    events <- data.frame(EID=1:length(df$mid_lat), X =
      as.numeric(as.vector(df$mid_long)),
                         Y=as.numeric(as.vector(df$mid_lat)))
    events <- as.EventData(na.omit(events), projection='UTM')
    fc <- findCells(events, polys)
    eac <-length(unique(subset(fc,select=c(PID,SID)))[,1])
    occs <- length(df$mid_lat)
    
    lats <- data.frame(events, Z = events$Y) # take within cell lats and long
    lons <- data.frame(events, Z = events$X)
    Y <- combineEvents(lats, findPolys(lats, polys), mean)
    X <- combineEvents(lons, findPolys(lons, polys), mean)
    gcd <-max(rdist.earth(cbind(X$Z, Y$Z), miles=FALSE))
    Locality <- paste(df$paleolatdec,df$paleolngdec)
    Localities <- length(unique(Locality))
    max.lat <- max(Y$Z)
    mean.lat <- mean(abs(Y$Z))
    min.lat <- min(Y$Z)}else{    
      eac=1
      gcd=NA
      max.lat = max(df$mid_lat)
      min.lat = min(df$mid_lat)
      mean.lat = mean(abs(df$mid_lat))
      occs <- length(df$mid_lat)
      Locality <- paste(df$paleolatdec,df$paleolngdec)
      Localities <- length(unique(Locality))
    }
  
  #class <- unique(as.character(df$class_name))
  #order <- unique(as.character(df$order_name))
  #family <- unique(as.character(df$family_name))
  #clgen <- unique(as.character(df$clgen1))
  #genus <- as.character(unique(df$occurrence.genus_name))
  #bin <- unique(df$slc)
  #rich <- NA
  #rich <- length(unique(df$occurrence.species_name))
  #FA <- unique(df$FA)
  #LA <- unique(df$LA)
  
#life.habit <-  unique(as.character(df$life_habit))[1]
  #taxon.environment <- unique(as.character(df$taxon_environment))[1]
  #loco <- unique(as.character(df$locomotion))[1]
  #diet <- unique(as.character(df$diet1))[1]
  #clone <- unique(as.character(df$clonal))[1]
  #eyes <- unique(as.character(df$vision))[1]
  
  out <- data.frame(eac, gcd, occs, max.lat, min.lat,mean.lat,Localities)
}

processed.data <- ddply(data,.(slc,class_name,order_name,family_name,occurrence.genus_name),genbybin,.progress="text")
write.table(processed.data,"PaleoDB ranges binned2.csv", sep = ",")