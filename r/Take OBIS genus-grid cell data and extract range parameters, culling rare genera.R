####import genus grid cells for both raw spalding realms and for central and western Indo-Pacific merged, create a column of labels, and merge
library(gdata)
load("~/Dropbox/nescent_extinction_map/Final data/genus_cells.rda")
Spalding_raw <- genus_cells
provinces <- rep("Spalding_raw",length(Spalding_raw[,1]))
Spalding_raw <- data.frame(Spalding_raw,provinces)
load("~/Dropbox/nescent_extinction_map/Final data/genus_cells_alt.rda")
Spalding_merged <- genus_cells_alt
provinces <- rep("Spalding_merged",length(Spalding_merged[,1]))
Spalding_merged <- data.frame(Spalding_merged,provinces)
genus_grid_merged <- rbind(Spalding_raw,Spalding_merged)
colnames(genus_grid_merged) <- c("genus","num_cells","long_mid","lat_mid","provinces")
occs <- read.csv("~/Dropbox/nescent_extinction_map/data/OBIS_genus_occurrences.csv",header=TRUE)
genus <- occs$genus
occ.freq <- occs$total_occurrences
occ.merge <- data.frame(genus,occ.freq)
genus_grid_merged2 <- merge(genus_grid_merged,occ.merge,by="genus")
genus_grid_merged3 <- drop.levels(subset(genus_grid_merged2,genus_grid_merged2$occ.freq>=30))
length(genus_grid_merged3[,1])

save(genus_grid_merged3,file="~/Dropbox/nescent_extinction_map/Final data/genus_grid_merged_rare_removed.rda")


#### now merge in higher taxonomy, create colums for tropical or polar occurrences
data <- load("~/Dropbox/nescent_extinction_map/Final data/genus_grid_merged_rare_removed.rda")
OBIS.genera <- genus_grid_merged3
genus.Matches <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Modern genera matchtaxa.csv", header = TRUE)
OBIS.for.plot <- merge(OBIS.genera,genus.Matches,by.x = "genus",all.x = FALSE)
latnow <- ifelse(OBIS.for.plot$lat_mid < 30,"t","nt")
polarnow <- ifelse(OBIS.for.plot$lat_mid > 60,"p","np")
OBIS.for.plot <- data.frame(OBIS.for.plot,latnow,polarnow)
save(OBIS.for.plot,file = "~/Dropbox/nescent_extinction_map/Final data/OBIS.for.plot_rare_removed.rda")


#### now extract range parameters from the OBIS data
library(fields)
library(plyr)
library(PBSmapping)
library(gdata)


newdata <- load("~/Dropbox/nescent_extinction_map/Final data/OBIS.for.plot_rare_removed.rda")
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
        gcd <-max(rdist.earth(events[,2:3], mile = TRUE))
        #gcd <-max(rdist.earth(events[,2:3], miles = FALSE))
        #with(events, plot(X, Y, main = paste(round(gcd/1000, 0), "(1000 km)\n", unique(df$class), "-", unique(df$genus)), axes = FALSE, ylim = c(-90, 90), xlim = c(-180, 180)))
        box(col = "grey")
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

                        
#pdf("gcd_test.pdf", width = 10, height = 10)
#par(mfrow = c(6, 5),mar = c(0,0,4,0), cex = 0.6)
#byGenus <- ddply(OBIS.grid.2[1:9000,],.(Spalding.version,class,MatchTaxon,genus),ranges,.progress="text")
#dev.off()

byGenus <- ddply(OBIS.grid.2,.(Spalding.version,class,MatchTaxon,genus),ranges,.progress="text")
write.table(byGenus,"~/Dropbox/nescent_extinction_map/Final data/Modern taxon range parameters_less_30_occs_removed.csv", sep = ",")

