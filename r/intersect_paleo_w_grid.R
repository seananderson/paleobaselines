pd <- read.csv("~/Dropbox/nescent_extinction_map/data/PaleoDB downloads/6.11.2013/PaleoDB_6.11.2013_resolved_only.csv", stringsAsFactors = FALSE)

load("~/Dropbox/nescent_extinction_map/data/equal_area_grid/global_45x14.rda")

# mid points of grid:
grid_mid_lat <- c(69.98829, 51.74316, 40.03754, 30.1738, 21.24611, 12.84422, 4.72115, -3.3057, -11.3996, -19.73712, -28.54919, -38.20473, -49.46882, -65.22635)
grid_mid_lat <- sort(grid_mid_lat)
grid_mid_long <- c(-176, -168, -160, -152, -144, -136, -128, -120, -112, -104, -96, -88, -80, -72, -64, -56, -48, -40, -32, -24, -16, -8, 0, 8, 16, 24, 32, 40, 48, 56, 64, 72, 80, 88, 96, 104, 112, 120, 128, 136, 144, 152, 160, 168, 176)
grid_mid_long <- sort(grid_mid_long)

# remove those outside our grid: (or on outer grid borders)
pd <- pd[-which(pd$latdec >= max(global_45x14$latitude)), ]
pd <- pd[-which(pd$latdec <= min(global_45x14$latitude)), ]

pd$mid_long <- grid_mid_long[findInterval(pd$lngdec, global_45x14$longitude)]
pd$mid_lat <- grid_mid_lat[findInterval(pd$latdec, global_45x14$latitude)]

save(pd,file="~/Dropbox/nescent_extinction_map/Final data/PaleoDB_6.11.2013_grid.cells.rda")



# check:
#with(pd, plot(latdec, mid_lat))
#with(pd, plot(lngdec, mid_long))
#plot(sort(unique(pd$mid_lat)))
#plot(sort(unique(grid_mid_long)))
#hist(sort(unique(grid_mid_long)))
#hist(sort(unique(grid_mid_lat)))

