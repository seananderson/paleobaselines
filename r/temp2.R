library(plyr)
library(PBSmapping)
library(sp)

mydata<-read.csv("~/Dropbox/nescent_extinction_map/Final data/Occs.09.30.csv", header=TRUE, sep=',')
ds <- load("~/Dropbox/nescent_extinction_map/data/equal_area_grid/global_45x14.rda")
latitude <- mydata$paleolatdec
longitude <- mydata$paleolngdec
occurrence <- paste(mydata$occurrence_genus_name,mydata$Record_Num)
PaleoDB <- data.frame(longitude,latitude)
PaleoDB <- SpatialPoints(PaleoDB)
proj4string(PaleoDB) = CRS("+proj=longlat")

grid_mid_lat <- c(69.98829, 51.74316, 40.03754, 30.1738, 21.24611, 12.84422,
4.72115, -3.3057, -11.3996, -19.73712, -28.54919, -38.20473,
-49.46882, -65.22635)
grid_mid_lat <- sort(grid_mid_lat)
grid_mid_long <- c(-176, -168, -160, -152, -144, -136, -128, -120, -112, -104,
-96, -88, -80, -72, -64, -56, -48, -40, -32, -24, -16, -8, 0,
8, 16, 24, 32, 40, 48, 56, 64, 72, 80, 88, 96, 104, 112, 120,
128, 136, 144, 152, 160, 168, 176)
grid_mid_long <- sort(grid_mid_long)
grid_mid_to_lower_left_corner_long <- data.frame(X = global_45x14$longitude[-length(global_45x14$longitude)], X_mid = grid_mid_long) 
grid_mid_to_lower_left_corner_lat <- data.frame(Y = global_45x14$latitude[-length(global_45x14$latitude)], Y_mid = grid_mid_lat) 


require(sp)
require(maptools)
require(PBSmapping)
source("eq.area.grid.2.R")
grid1 <- as.PolySet(eq.area.grid.2())
grid2 <- PolySet2SpatialPolygons(grid1)
coords <- coordinates(grid2)
Longs <- coords[,1]
Lats <- coords[,2]
grid3 <- SpatialPolygonsDataFrame(grid2,data=data.frame(x=Longs, y=Lats, row.names=getSpPPolygonsIDSlots(grid2)))


matchLong <- over(PaleoDB,grid3[,1],fn=mean)
matchLat <- over(PaleoDB,grid3[,2],fn=mean)
PaleoDB_grid_cells <- data.frame(mydata,matchLong,matchLat)
write.table(PaleoDB_grid_cells,"PaleoDB_09.30_grid_cells.csv",sep=",")





fc <- findPolys(PB, grid, maxRows = 1e+07)
fc_ps <- fc[,c("PID", "SID")]
fc_xy <- merge(fc_ps, grid)
# take the lower-left corner co-ordinates only:
fc_xy_lower_left <- fc_xy[fc_xy$POS == 1, c("X", "Y")]
fc_xy_lower_left <- fc_xy_lower_left[!duplicated(fc_xy_lower_left), ]
# now merge in the mid points:
fc_xy_mid <- merge(fc_xy_lower_left,grid_mid_to_lower_left_corner_long, all.x = TRUE)
fc_xy_mid <- merge(fc_xy_mid, grid_mid_to_lower_left_corner_lat, all.x = TRUE)
grid_coords <-data.frame(fc_xy_mid$X_mid,fc_xy_mid$Y_mid)
grid_coords}

byRecord <- ddply(PaleoDB,.(occurrence),grid.PaleoDB.data.2 )