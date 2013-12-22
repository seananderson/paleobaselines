library(PBSmapping)
library(sp)
library(plyr)
library(maptools)

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


# load equal area grid
load("~/Dropbox/nescent_extinction_map/data/equal_area_grid/global_45x14.rda")
# load paleoDB occurences
load("~/Dropbox/nescent_extinction_map/Final data/PaleoDB_Cenozoic_100212.rda")
PaleoDB <- data.frame(PaleoDB)
head(PaleoDB)

latitude <- round(PaleoDB$paleolatdec)
longitude <- round(PaleoDB$paleolngdec)
genus <- PaleoDB$occurrence.genus_name
stage <- PaleoDB$FR2_bin


LatSeq <- global_45x14$latitude
LongSeq <- global_45x14$longitude
polys<- makeGrid(x=round(LongSeq,0), y=round(LatSeq,0),projection="UTM")

