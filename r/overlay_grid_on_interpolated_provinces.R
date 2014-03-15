# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Sep 24, 2012
# Last modified: Feb 20, 2014
# Purpose:       Take the interpolated provinces (by bounding box
# within realms) and overlay a grid.
# ====================================================================

# bring in data:
load("../data/interpolated_provs_all_taxa.rda")
load("../data/interpolated_provs_alt_all_taxa.rda")
load("prov_SpatialPolygons.rda")
load("../data/equal_area_grid/global_45x14.rda")
library(PBSmapping)
library(sp)
library(plyr)
library(maptools)

# in older versions there were still some duplicated rows
# so, just in case, since it will create errors below:
interpolated_provs_all_taxa <-
  interpolated_provs_all_taxa[!duplicated(interpolated_provs_all_taxa), ]
interpolated_provs_alt_all_taxa <-
  interpolated_provs_alt_all_taxa[!duplicated(interpolated_provs_alt_all_taxa), ]

# TODO bring over these files:
grid_mid_lat <- read.csv("../data/grid_mid_lat.csv")$x
grid_mid_long <- read.csv("../data/grid_mid_long.csv")$x

grid_mid_to_lower_left_corner_long <-
  data.frame(X = global_45x14$longitude[-length(global_45x14$longitude)],
    X_mid = grid_mid_long)
grid_mid_to_lower_left_corner_lat <-
  data.frame(Y = global_45x14$latitude[-length(global_45x14$latitude)],
    Y_mid = grid_mid_lat)

combine_province_shapefiles <- function(prov_ids) {
  junk <- list()
  for(i in 1:length(prov_ids)){
    junk[[i]] <- Polygons(prov_SpatialPolygons[[prov_ids[i]]]@polygons[[1]]@Polygons,
      prov_ids[i])
  }
  out <- SpatialPolygons(junk)
  out
}

get_num_grid_cells <- function(SpatialPolygon_object, test_plot = FALSE) {

# switch to PBS mapping format:
  j <- SpatialPolygons2PolySet(SpatialPolygon_object)

# 2 x 2 degree cells - fill in the polygons:
  g <- expand.grid(X = seq(-180, 180, 2), Y = seq(-90, 90, 2))
  g$EID <- 1:nrow(g)
  fp <- findPolys(g, j)
  g2 <- subset(g, EID %in% fp$EID)
  g2$EID <- 1:nrow(g2)
  g2 <- as.EventData(g2)
# g2 contains the fine-scale grid version of the polygons

  # the equal area grid:
  cells <- makeGrid(x = global_45x14$longitude, y =global_45x14$latitude)

# find which of the equal area grid cells contain the fine grid
# pattern:
  fc <- findPolys(g2, cells)

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
  if(test_plot){
    plot(SpatialPolygon_object, asp = 1, xlim = c(-180, 180), ylim = c(-90, 90))
    box(col = "grey")
    with(cell_coords, points(X_mid, Y_mid, pch = 4,cex = 1.5, col = "red"))
  }
  return(data.frame(num_cells = rep(num_cells, nrow(cell_coords)),
      X_mid = cell_coords$X_mid, Y_mid = cell_coords$Y_mid))
}

# example:
# shp_file <- combine_province_shapefiles(c(1, 2, 3))
# get_num_grid_cells(shp_file)
gc()

genus_cells <- ddply(interpolated_provs_all_taxa, "genus", function(x) {
  shp_file <- combine_province_shapefiles(x$PROV_CODE)
  get_num_grid_cells(shp_file)
})

gc()

genus_cells_alt <- ddply(interpolated_provs_alt_all_taxa, "genus", function(x) {
  shp_file <- combine_province_shapefiles(x$PROV_CODE)
  get_num_grid_cells(shp_file)
})

save(genus_cells, file = "../data/genus_cells.rda")
save(genus_cells_alt, file = "../data/genus_cells_alt.rda")

# ---------
# get 6 genuses to sample:
genus_to_plot <- unique(interpolated_provs_all_taxa$genus)[sample(1:length(unique(interpolated_provs_all_taxa$genus)), 6)]

pdf("../figs/test_grid_overlay.pdf", width = 8, height = 9)
par(mfrow = c(3, 2), cex = 0.7, oma = c(0,0,0,0))
for(i in 1:6){
temp <- subset(interpolated_provs_all_taxa, genus == genus_to_plot[i])
  shp_file <- combine_province_shapefiles(temp$PROV_CODE)
  throw_away <- get_num_grid_cells(shp_file, test_plot = TRUE)
mtext(genus_to_plot[i])
}
dev.off()
