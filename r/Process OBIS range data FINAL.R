library(fields)
library(plyr)
library(PBSmapping)
library(gdata)


newdata <- load("~/Dropbox/nescent_extinction_map/Final data/OBIS.for.plot.rda")
head(newdata)
OBIS.grid <- OBIS.for.plot
head(OBIS.grid)


latitude <- round(OBIS.grid$lat_mid)
longitude <- round(OBIS.grid$long_mid)
LongLat <- paste(round(longitude,2),"_",round(latitude,2),sep="")
genus <- OBIS.grid$genus
Spalding.version <- OBIS.grid$provinces
richness <- OBIS.grid$Richness
class <- OBIS.grid$class
MatchTaxon <- OBIS.grid$NewMatchTaxon
trop <- OBIS.grid$latnow
polar <- OBIS.grid$polarnow

OBIS.grid.2 <- unique(data.frame(Spalding.version,class,MatchTaxon,genus,richness,latitude,longitude,trop,polar,LongLat))

load("~/Dropbox/nescent_extinction_map/data/equal_area_grid/global_45x14.rda")

LatSeq <- global_45x14$latitude
LongSeq <- global_45x14$longitude
polys<- makeGrid (x=round(LongSeq,0), y=round(LatSeq,0),projection="UTM")

ranges <- function(df){
        if(length(na.omit(df$latitude))>1){
        events <- data.frame(EID=1:length(df$latitude), X = as.numeric(as.vector(df$longitude)),Y=as.numeric(as.vector(df$latitude)))
        events <- as.EventData(na.omit(events), projection='UTM')
        fc <- findCells(events, polys)
        richness <- mean(unique(drop.levels(df$richness)))
        eac <-length(unique(subset(fc,select=c(PID,SID)))[,1])
        gcd <-max(rdist.earth(events[,2:3]))
        MaxLat <- max(df$latitude)
        MinLat <- min(df$latitude)
        MaxAbsLat <- max(abs(df$latitude))
        MinAbsLat <- min(abs(df$latitude))}     
        else{
                        eac=1
                        gcd=1
                        richness <- mean(unique(drop.levels(df$richness)))
                        MaxLat <- max(df$latitude)
        				MinLat <- min(df$latitude)
        				MaxAbsLat <- max(abs(df$latitude))
       				 	MinAbsLat <- min(abs(df$latitude))}
                        return(data.frame(eac,gcd,richness,MaxLat,MinLat,MaxAbsLat,MinAbsLat))
                        }

                        
byGenus <- ddply(OBIS.grid.2,.(Spalding.version,class,MatchTaxon,genus),ranges)
write.table(byGenus,"Modern taxon range parameters FINAL.csv", sep = ",")