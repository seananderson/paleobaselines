library(plyr)
library(PBSmapping)

mydata<-read.csv("~/Dropbox/nescent_extinction_map/data/PaleoDB downloads/6.11.2013/PaleoDB_6.11.2013_resolved_only.csv", header=TRUE, sep=',',stringsAsFactors=FALSE)
#mydata <- mydata[1:20,]
load("~/Dropbox/nescent_extinction_map/data/equal_area_grid/global_45x14.rda")
latitude <- mydata$paleolatdec
longitude <- mydata$paleolngdec
occurrence <- mydata$Record_Num
PaleoDB <- data.frame(longitude,latitude,occurrence)

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


require(PBSmapping)
source("eq.area.grid.2.R")
grid <- eq.area.grid.2()
GridID <- as.factor(paste(grid$PID,grid$SID))
Namedgrid <- data.frame(GridID,grid)


grid.hal.data.2 <- function(df){
hlp <- df
hlp <- hlp[,c("longitude","latitude","occurrence")]
names(hlp) <- c("X", "Y","Z")
hlp$EID <- 1:nrow(hlp)
hlp <- as.EventData(hlp, projection = "LL") 
fc <- findPolys(hlp, Namedgrid, maxRows = 1e+07)
fc_xy <- merge(fc, Namedgrid,all.y=FALSE)

#fc_ps <- as.factor(paste(fc$PID,fc$SID))
#fc_xy <- merge(GridID, Namedgrid,all.y=FALSE)



# take the lower-left corner co-ordinates only:
fc_xy_lower_left <- fc_xy[fc_xy$POS == 1, c("X", "Y")]
fc_xy_lower_left <- fc_xy_lower_left[!duplicated(fc_xy_lower_left), ]
# now merge in the mid points:
fc_xy_mid <- merge(fc_xy_lower_left,grid_mid_to_lower_left_corner_long, all.x = TRUE)
fc_xy_mid <- merge(fc_xy_mid, grid_mid_to_lower_left_corner_lat, all.x = TRUE)
num_cells <- nrow(fc_xy_lower_left)
cell_coords <- fc_xy_mid[,c("X_mid", "Y_mid")]
cell_coords}
byRecord <- ddply(PaleoDB,.(occurrence),grid.hal.data.2,.progress="text")
save(byRecord,file="~/Dropbox/nescent_extinction_map/data/PaleoDB.equal.area.cell.coords.rda")