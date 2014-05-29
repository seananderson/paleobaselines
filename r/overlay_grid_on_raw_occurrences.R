# ====================================================================
# Created by:    Sean C. Anderson
# Created:       Feb 06, 2012
# Last modified: May 27, 2014
# Purpose:       Overlay the equal area grid on the raw occurrence
#                data as a sensitivity test for our interpolation
#                method.
# ====================================================================

load("../data/composite.occ2.rda")
comp.dat <- composite.occ2
# comp.dat <- composite.occ2[sample(1:nrow(composite.occ2), 30000), ] # for testing
rm(composite.occ2)
load("../data/global_45x14.rda")
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
if(file.exists("../data/pts.over.all.cache.rda")) {
  load("../data/pts.over.all.cache.rda")
}else{
  pts.over <- over(pts, er)
  save(pts.over, file = "../data/pts.over.all.cache.rda")
}
oc.df <- cbind(comp.dat, pts.over)
rm(pts.over)
gc()
# remove rows without an ecoregion, we aren't dealing with them:
oc.df.init <- oc.df
oc.df <- oc.df[!is.na(oc.df$RLM_CODE), ]

oc.df$longitude[oc.df$longitude == 180] <- 179.99 # to avoid NAs on findInterval
# remove those with latitudes outside of our equal area grid:
oc.df <- subset(oc.df, latitude > min(global_45x14$latitude) & latitude < max(global_45x14$latitude))

oc.df$X_mid <- grid_mid_long[findInterval(oc.df$longitude, global_45x14$longitude)]
oc.df$Y_mid <- grid_mid_lat[findInterval(oc.df$latitude, global_45x14$latitude)]

genus_cells_no_interpolation <- data.frame(genus = oc.df$genus, X_mid = oc.df$X_mid, Y_mid = oc.df$Y_mid)
genus_cells_no_interpolation <- genus_cells_no_interpolation[!duplicated(genus_cells_no_interpolation), ]

library(plyr)
genus_cells_no_interpolation <- ddply(genus_cells_no_interpolation,
  c("X_mid", "Y_mid"), transform, num_cells = length(genus))

genus_cells_no_interpolation <- genus_cells_no_interpolation[ ,
  c("genus", "num_cells", "X_mid", "Y_mid")]
save(genus_cells_no_interpolation, file = "../data/genus_cells_no_interpolation.rda")


# also save a data frame with the raw province-genera combinations we observe:
oc.df.temp <- oc.df[,c("genus", "PROV_CODE")]
#obis_prov_obs_no_interp <- data.frame(dplyr::summarise(dplyr::group_by(oc.df.temp, genus, PROV_CODE),
  #n_obs = length(genus), genus = genus[1]))

obis_prov_obs_no_interp <- plyr::ddply(oc.df.temp, c("genus", "PROV_CODE"), function(x) {
  data.frame(genus = x$genus[1], n_obs = length(x$genus))}, .progress = "text")
saveRDS(obis_prov_obs_no_interp, file = "../data/obis_prov_obs_no_interp.rds")

rm(genus_cells_no_interpolation, comp.dat, grid_mid_lat, grid_mid_long, oc.df, er, pts, oc.df.temp)

