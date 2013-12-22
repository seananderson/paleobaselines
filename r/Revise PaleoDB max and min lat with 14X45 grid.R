library(fields)
library(plyr)
library(PBSmapping)
library(gdata)


newdata <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Occs.09.30.csv",header =TRUE)
head(newdata)
OBIS.grid <- newdata
head(OBIS.grid)


latitude <- round(OBIS.grid$paleolatdec)
longitude <- round(OBIS.grid$paleolngdec)
genus <- OBIS.grid$occurrence.genus_name
stage <- OBIS.grid$slc

OBIS.grid.2 <- unique(data.frame(stage,genus,latitude,longitude))
OBIS.grid.2 <- na.omit(OBIS.grid.2)


load("~/Dropbox/nescent_extinction_map/data/equal_area_grid/global_45x14.rda")

LatSeq <- global_45x14$latitude
LongSeq <- global_45x14$longitude
polys<- makeGrid (x=round(LongSeq,0), y=round(LatSeq,0),projection="UTM")

ranges <- function(df){
        events <- data.frame(EID=1:length(df$latitude), X = as.numeric(as.vector(df$longitude)),Y=as.numeric(as.vector(df$latitude)))
        events <- as.EventData(na.omit(events), projection='UTM')
        cells <- makeGrid(x = global_45x14$longitude, y =global_45x14$latitude)
        fc <- findCells(events, polys)
        # now count how many grid cells there are:
  		fc_ps <- fc[,c("PID", "SID")]
		# bring in the X and Y coordinates:
  		fc_xy <- merge(fc_ps, cells)
		# take the lower-left corner co-ordinates only:
  		fc_xy_lower_left <- fc_xy[fc_xy$POS == 1, c("X", "Y")]
  		fc_xy_lower_left <- fc_xy_lower_left[!duplicated(fc_xy_lower_left), ]
		# now merge in the mid points:
  		fc_xy_mid <- merge(fc_xy_lower_left, grid_mid_to_lower_left_corner_long, all.x = TRUE)
  		fc_xy_mid <- merge(fc_xy_mid, grid_mid_to_lower_left_corner_lat, all.x = TRUE)
 		num_cells <- nrow(fc_xy_lower_left)
  		cell_coords <- fc_xy_mid[,c("X_mid", "Y_mid")]
        
        d <- data.frame(num_cells = rep(num_cells, nrow(cell_coords)), X_mid = round(cell_coords$X_mid), Y_mid = round(cell_coords$Y_mid))
        d <- na.omit(d)
        MaxLat <- max(d$Y_mid)
        MinLat <- min(d$Y_mid)
        MaxAbsLat <- max(abs(d$Y_mid))
        MinAbsLat <- min(abs(d$Y_mid))}

                        
byGenus <- ddply(OBIS.grid.2,.(stage,genus),ranges)
write.table(byGenus,"PaleoDB ranges binned.csv", sep = ",")