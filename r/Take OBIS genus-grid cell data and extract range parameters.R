####import genus grid cells for both raw spalding realms and for central and western Indo-Pacific merged, create a column of labels, and merge

### use interpolated data
data <- load("~/Dropbox/nescent_extinction_map/data/genus_cells.rda")
Spalding_raw <- genus_cells

### use uninterpolated data
#load("~/Dropbox/nescent_extinction_map/data/genus_cells_no_interpolation.rda")
#Spalding_raw <- genus_cells_no_interpolation



provinces <- rep("Spalding_raw",length(Spalding_raw[,1]))
Spalding_raw <- data.frame(Spalding_raw,provinces)
load("~/Dropbox/nescent_extinction_map/Final data/genus_cells_alt.rda")
Spalding_merged <- genus_cells_alt
provinces <- rep("Spalding_merged",length(Spalding_merged[,1]))
Spalding_merged <- data.frame(Spalding_merged,provinces)
genus_grid_merged <- rbind(Spalding_raw,Spalding_merged)
colnames(genus_grid_merged) <- c("genus","num_cells","long_mid","lat_mid","provinces")
save(genus_grid_merged,file="genus_grid_merged.rda")


#### now merge in higher taxonomy, create colums for tropical or polar occurrences
data <- load("~/Dropbox/nescent_extinction_map/Final data/genus_grid_merged.rda")
OBIS.genera <- genus_grid_merged
genus.Matches <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Modern genera matchtaxa.csv", header = TRUE)
OBIS.for.plot <- merge(OBIS.genera,genus.Matches,by.x = "genus",all.x = FALSE)
latnow <- ifelse(OBIS.for.plot$lat_mid < 30,"t","nt")
polarnow <- ifelse(OBIS.for.plot$lat_mid > 60,"p","np")
OBIS.for.plot <- data.frame(OBIS.for.plot,latnow,polarnow)
save(OBIS.for.plot,file = "OBIS.for.plot.rda")


#### now extract range parameters from the OBIS data
library(fields)
library(plyr)
library(PBSmapping)
library(gdata)
library(gmt)


newdata <- load("~/Dropbox/nescent_extinction_map/Final data/OBIS.for.plot.rda")
head(newdata)
OBIS.grid <- OBIS.for.plot
head(OBIS.grid)


latitude <- round(OBIS.grid$lat_mid)
longitude <- round(OBIS.grid$long_mid)
Locality <- as.factor(paste(OBIS.grid$lat_mid,OBIS.grid$long_mid))
LongLat <- paste(round(longitude,2),"_",round(latitude,2),sep="")
genus <- OBIS.grid$genus
Spalding.version <- OBIS.grid$provinces
richness <- OBIS.grid$Richness
class <- OBIS.grid$class
MatchTaxon <- OBIS.grid$NewMatchTaxon
trop <- OBIS.grid$latnow
polar <- OBIS.grid$polarnow

OBIS.grid.2 <- unique(data.frame(Spalding.version,class,MatchTaxon,genus,richness,latitude,longitude,trop,polar,LongLat,Locality))


load("~/Dropbox/nescent_extinction_map/data/equal_area_grid/global_45x14.rda")

LatSeq <- global_45x14$latitude
LongSeq <- global_45x14$longitude
polys<- makeGrid (x=round(LongSeq,0), y=round(LatSeq,0),projection="UTM")
library(sp)

ranges <- function(df){
        if(length(na.omit(as.factor(paste(df$latitude,df$longitude))))>1){
        events <- data.frame(EID=1:length(df$latitude), X = as.numeric(as.vector(df$longitude)),Y=as.numeric(as.vector(df$latitude)))
        events <- as.EventData(na.omit(events), projection='UTM')
        fc <- findCells(events, polys)
        lats <- data.frame(events, Z = events$Y) # take within cell lats and long
    		lons <- data.frame(events, Z = events$X)
    		Y <- combineEvents(lats, findPolys(lats, polys), mean)
    		X <- combineEvents(lons, findPolys(lons, polys), mean)
        richness <- mean(unique(drop.levels(df$richness)))
        eac <-length(unique(subset(fc,select=c(PID,SID)))[,1])
        #gcd <- NA
        gcd <-max(rdist.earth(cbind(X$Z, Y$Z), miles=FALSE))
        gcd2 <- NA
        #gcd2 <- max(geodist(cbind(X$Z, Y$Z),col.swap=TRUE))
        MaxLat <- max(df$latitude)
        MinLat <- min(df$latitude)
        Localities <- length(unique(df$Locality))
        Occurrences <- length(df$latitude)
        MaxAbsLat <- max(abs(df$latitude))
        MinAbsLat <- min(abs(df$latitude))}     
        else{
                        eac=1
                        gcd=NA
                        gcd2=NA
                        richness <- mean(unique(drop.levels(df$richness)))
                        MaxLat <- max(df$latitude)
        				MinLat <- min(df$latitude)
        				MaxAbsLat <- max(abs(df$latitude))
       				 	MinAbsLat <- min(abs(df$latitude))}
       				 	Occurrences <- length(df$latitude)
       				 	Localities <- length(unique(df$Locality))
                        return(data.frame(Occurrences,eac,gcd,gcd2,Localities,richness,MaxLat,MinLat,MaxAbsLat,MinAbsLat))
                        }

                        
byGenus <- ddply(OBIS.grid.2,.(Spalding.version,class,MatchTaxon,genus),ranges,.progress="text")
write.table(byGenus,"~/Dropbox/nescent_extinction_map/Final data/Modern taxon range parameters no interpolation FINAL2.csv", sep = ",")

