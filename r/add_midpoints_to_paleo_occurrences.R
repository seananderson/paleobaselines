# ====================================================================
# Created:       Oct 04, 2012
# Last modified: Oct 04, 2012
# Purpose:       Add midpoint columns from an equal area grid to the
#                paleo data.
# ====================================================================

PaleoDB<-read.csv("~/Dropbox/nescent_extinction_map/Final data/Occs.09.30.csv", header=TRUE, sep=',')
load("~/Dropbox/nescent_extinction_map/data/equal_area_grid/global_45x14.rda")

grid_mid_lat <- read.csv("~/Dropbox/nescent_extinction_map/data/grid_mid_lat.csv")$x
grid_mid_long <- read.csv("~/Dropbox/nescent_extinction_map/data/grid_mid_long.csv")$x

rows_with_no_cells <- which(PaleoDB$paleolatdec < min(global_45x14$latitude) | PaleoDB$paleolatdec > max(global_45x14$latitude))

PaleoDB_nocells <- PaleoDB[rows_with_no_cells, ]
PaleoDB_withcells <- PaleoDB[-rows_with_no_cells, ]

if(nrow(PaleoDB_withcells) + nrow(PaleoDB_nocells) != nrow(PaleoDB)) stop("You've lost data! Stop!")

PaleoDB_withcells$X_mid <- grid_mid_long[findInterval(PaleoDB_withcells$paleolngdec, global_45x14$longitude)]
PaleoDB_withcells$Y_mid <- grid_mid_lat[findInterval(PaleoDB_withcells$paleolatdec, global_45x14$latitude)]

PaleoDB_nocells$X_mid <- NA
PaleoDB_nocells$Y_mid <- NA

PaleoDB <- rbind(PaleoDB_nocells, PaleoDB_withcells)
PaleoDB <- PaleoDB[order(PaleoDB$Record_Num), ]

save(PaleoDB, file = "~/Dropbox/nescent_extinction_map/Final data/Occs.09.30.with.midcells.rda")

