# Created by:    Sean C. Anderson
# Created:       Feb 06, 2012
# Last modified: Feb 06, 2013
# Purpose:       Overlay the equal area grid on the raw occurrence
#                data as a sensitivity test for our interpolation
#                method.


load("../data/composite.occ2.rda")
comp.dat <- composite.occ2
#comp.dat <- composite.occ2[sample(1:nrow(composite.occ2), 30000), ] # for testing
rm(composite.occ2)
load("../data/equal_area_grid/global_45x14.rda")
grid_mid_lat <- read.csv("../data/grid_mid_lat.csv")$x
grid_mid_long <- read.csv("../data/grid_mid_long.csv")$x

# match this: (genus_cells)
   #genus num_cells X_mid     Y_mid
#1 Abatus        83  -112 -65.22635
#2 Abatus        83  -144 -65.22635
#3 Abatus        83   -88 -65.22635

# first remove rows that don't occur in an ecoregion:
require(maptools)
gpclibPermit()
er <- readShapePoly("../data/MEOW2/meow_ecos.shp")
pts <- SpatialPoints(comp.dat[,c("longitude", "latitude")])
# save time if the file exists
# NEED TO DELETE THE FILE IF YOU WANT TO FEED IN NEW DATA!
if(file.exists("pts.over.all.cache.rda")) {
  load("pts.over.all.cache.rda")
}else{
  pts.over <- over(pts, er)
  save(pts.over, file = "pts.over.all.cache.rda")
}
oc.df <- cbind(comp.dat, pts.over)
rm(pts.over)
gc()
# remove rows without an ecoregion, we aren't dealing with them:
oc.df.init <- oc.df
oc.df <- oc.df[!is.na(oc.df$RLM_CODE), ]

#pts2 <- SpatialPoints(oc.df[,c("longitude", "latitude")])

oc.df$longitude[oc.df$longitude == 180] <- 179.99 # to avoid NAs on findInterval
# remove those with latitudes outside of our equal area grid:
oc.df <- subset(oc.df, latitude > min(global_45x14$latitude) & latitude < max(global_45x14$latitude))

oc.df$X_mid <- grid_mid_long[findInterval(oc.df$longitude, global_45x14$longitude)]
oc.df$Y_mid <- grid_mid_lat[findInterval(oc.df$latitude, global_45x14$latitude)]

genus_cells_no_interpolation <- data.frame(genus = oc.df$genus, X_mid = oc.df$X_mid, Y_mid = oc.df$Y_mid)
genus_cells_no_interpolation <- genus_cells_no_interpolation[!duplicated(genus_cells_no_interpolation), ]

library(plyr)
genus_cells_no_interpolation <- ddply(genus_cells_no_interpolation, c("X_mid", "Y_mid"), transform, num_cells = length(genus))

#pts3 <- SpatialPoints(genus_cells_no_interpolation[,c("X_mid", "Y_mid")])

genus_cells_no_interpolation <- genus_cells_no_interpolation[,c("genus", "num_cells", "X_mid", "Y_mid")]
save(genus_cells_no_interpolation, file = "../data/genus_cells_no_interpolation.rda")

rm(genus_cells_no_interpolation, comp.dat, grid_mid_lat, grid_mid_long, oc.df, er, pts)

