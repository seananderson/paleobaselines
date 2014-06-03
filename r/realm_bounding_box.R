# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Sep 21, 2012
# Last modified: Jun 02, 2014
# Purpose:       Implement the by-realm bounding box.
# ====================================================================

require(plyr)
require(sp)
require(rgeos) # for overlays with 2 sets of polygons
require(ggplot2)
require(maptools)
gpclibPermit()
gc()

# Do you want to plot out a diagnostic plot with each genus? Probably not a
# good thing to enable to if you're running the whole dataset.
check_plot <- FALSE

# add spalding regions to the occurrence data
load("../data/composite.occ2.rda")

composite.occ2 <- composite.occ2[!composite.occ2$group %in%
  c("Mammalia", "Elasmobranchii"), ]
# these came from range maps; no interpolation necessary

er <- readShapePoly("../data/MEOW2/meow_ecos.shp")
pts <- SpatialPoints(composite.occ2[,c("longitude", "latitude")])
# save time if the file exists
# NEED TO DELETE THE FILE IF YOU WANT TO FEED IN NEW DATA!
if(file.exists("../data/pts.over.cache.rda")) {
  load("../data/pts.over.cache.rda")
} else {
  pts.over <- over(pts, er)
  save(pts.over, file = "../data/pts.over.cache.rda")
}
oc.df <- cbind(composite.occ2, pts.over)
# remove rows without an ecoregion, we aren't dealing with them:
oc.df <- oc.df[!is.na(oc.df$RLM_CODE), ]
# and remove redundant rows:
oc.df <- oc.df[!duplicated(oc.df), ]

# --------
# now re-arrange the spalding data into a dataframe:
er@data$id = rownames(er@data)
er.points = ggplot2::fortify(er, region = "id")
er.df <- plyr::join(er.points, er@data, by = "id")
er@data$ID <- 1:nrow(er@data)

rm(composite.occ2, pts.over)
gc()
# So, oc.df = the occurrence dataframe.
# And, er.df = the ecoregion dataframe.

# -----------
# Now I'm going to pull out merged versions of the realm and province
# polygons.
#
# First, create a list object ordered by realm order as ordered in
# er@data: (unique(er@data$RLM_CODE))
# each element of the list contains a SpatialPolygons object
realms_SpatialPolygons <- dlply(er@data, "RLM_CODE", function(x) {
  regions_to_join <- list()
# go through each region poly and gather it
# then we'll join them by realm
  for(i in 1:nrow(x)) {
    regions_to_join[[i]] <-  slot(er, "polygons")[[x$ID[i]]]
  }
  realm_with_many_regions <- SpatialPolygons(regions_to_join)
# now join them where possible:
  realm_union_regions <- unionSpatialPolygons(SpP = realm_with_many_regions,
    IDs = rep(1, length(realm_with_many_regions)))
  realm_union_regions
})
save(realms_SpatialPolygons, file = "../data/realms_SpatialPolygons.rda")

# Then, create a list object ordered by province order as ordered in
# er@data: (unique(er@data$PROV_CODE))
# each element of the list contains a SpatialPolygons object
prov_SpatialPolygons <- dlply(er@data, "PROV_CODE", function(x) {
  regions_to_join <- list()
# go through each province poly and gather it
# then we'll join them by province
  for(i in 1:nrow(x)) {
    regions_to_join[[i]] <-  slot(er, "polygons")[[x$ID[i]]]
  }
  prov_with_many_regions <- SpatialPolygons(regions_to_join)
# now join them where possible:
  prov_union_regions <- unionSpatialPolygons(SpP = prov_with_many_regions,
    IDs = rep(1, length(prov_with_many_regions)))
  prov_union_regions
})
save(prov_SpatialPolygons, file = "../data/prov_SpatialPolygons.rda")

# Now create a handy lookup table to make this all possible.
# ddply automatically orders things, so we can do this:
er_lookup <- ddply(er@data, "PROV_CODE", function(x) {
  data.frame(PROV_CODE = unique(x$PROV_CODE), RLM_CODE = unique(x$RLM_CODE))
})
save(er_lookup, file = "../data/er_lookup.rda")

# -------------------------------------------------
# Now let's go through each realm of occurrence data and see which
# provinces to fill in.
# the main function:
interpolate_provinces <- function(y) {

# check if we need to deal with the 180 degree boundary:
y <- y[order(y$longitude), ] # first order the longitudes so we can check for gaps
if(nrow(y) > 1) # need this or the diff command will give an error
  sphere_issue <- max(diff(y$longitude)) > 90 # 90 degrees should separate little islands from realms that transverse the 180 and -180 border
else
  sphere_issue <- FALSE

if(sphere_issue) { # crosses the 180 border, so you'll need 2 boxes
  row_to_split_on <- which(diff(y$longitude) == max(diff(y$longitude)))
  dat_east <- y[seq(row_to_split_on + 1, nrow(y)), ]
  dat_west <- y[seq(1, row_to_split_on), ]
# the east box coordinates
    min_long_east <- min(dat_east$longitude)
    max_long_east <- 180 # if we're in this if statement we know there are occurrences on the other side

# the west box coordinates
    min_long_west <- -180 # if we're in this if statement we know there are occurrences on the other side
    max_long_west <- max(dat_west$longitude)
# the latitude is shared:
    min_lat <- min(y$latitude)
    max_lat <- max(y$latitude)

if(min_lat == max_lat) {
  max_lat <- max_lat + 0.001
}
if(min_long_east == max_long_east) {
  min_long_east <- min_long_east - 0.001
}
if(min_long_west == max_long_west) {
  min_long_west <- min_long_west - 0.001
}

# and create the two bounding boxes:
# Now create an sp polygon object with the bounding box coordinates:
poly_west <- matrix(nrow = 5, ncol = 2)
poly_west[1,] <- c(min_long_west, min_lat)
poly_west[2,] <- c(min_long_west, max_lat)
poly_west[3,] <- c(max_long_west, max_lat)
poly_west[4,] <- c(max_long_west, min_lat)
poly_west[5,] <- poly_west[1,] # must complete the polygon

poly_east <- matrix(nrow = 5, ncol = 2)
poly_east[1,] <- c(min_long_east, min_lat)
poly_east[2,] <- c(min_long_east, max_lat)
poly_east[3,] <- c(max_long_east, max_lat)
poly_east[4,] <- c(max_long_east, min_lat)
poly_east[5,] <- poly_east[1,] # must complete the polygon

box_west_Polygon <- Polygon(poly_west, hole = TRUE)
box_west_Polygons <- Polygons(list(box_west_Polygon), ID = 2)
box_east_Polygon <- Polygon(poly_east, hole = TRUE)
box_east_Polygons <- Polygons(list(box_east_Polygon), ID = 1)

box_SpatialPolygons <- SpatialPolygons(list(box_east_Polygons, box_west_Polygons))

} else { # doesn't cross 180 border, so one 1 box will do
# Get bounding box coordinates:
min_lat <- min(y$latitude)
max_lat <- max(y$latitude)
min_long <- min(y$longitude)
max_long <- max(y$longitude)

if(min_lat == max_lat) {
  max_lat <- max_lat + 0.001
}
if(min_long == max_long) {
  max_long <- max_long + 0.001
}

# Now create an sp polygon object with the bounding box coordinates:
poly <- matrix(nrow = 5, ncol = 2)
poly[1,] <- c(min_long, min_lat)
poly[2,] <- c(min_long, max_lat)
poly[3,] <- c(max_long, max_lat)
poly[4,] <- c(max_long, min_lat)
poly[5,] <- poly[1,] # must complete the polygon
box_Polygon <- Polygon(poly, hole = TRUE)
box_Polygons <- Polygons(list(box_Polygon), ID = 1)
box_SpatialPolygons <- SpatialPolygons(list(box_Polygons))
}

# Now, time to figure out which provinces are within these bounding
# boxes

# return the province ids that are overlapped
# by the bounding box
# We will do this by checking each province in succession
# - cycle through possible provinces (those within the realm)
# - keep track of which ones are crossed by a bounding box

# find all provinces that occur in the realm we're working with:
provs_to_check <- er_lookup[er_lookup$RLM_CODE %in% unique(y$RLM_CODE), "PROV_CODE"]
# and figure out which ones overlap with the box:

if(check_plot) {
  par(mfrow = c(1,1))
  plot(1,1, xlim = c(-180, 180), ylim= c(-90, 90), type = "n", xlab = "",
    ylab = "", axes = FALSE)
  box()
}

provs_to_interpolate <- sapply(provs_to_check, function(j) {

  if(check_plot) {
    plot(prov_SpatialPolygons[[j]], xlim = c(-180, 180),
      ylim = c(-90, 90), col = "#00000020", add = T)
  }

  ol <- gOverlaps(prov_SpatialPolygons[[j]],box_SpatialPolygons)
  co1 <- gContains(box_SpatialPolygons,prov_SpatialPolygons[[j]])
  co2 <- gContains(prov_SpatialPolygons[[j]],box_SpatialPolygons) # reversed
  if(check_plot){
    plot(prov_SpatialPolygons[[j]], xlim = c(-180, 180), ylim = c(-90, 90),
      col = "#00000050", add = T)
  }
  if(ol | co1 | co2) {
    j
  } else NA
})
if(check_plot) {
  plot(box_SpatialPolygons, xlim = c(-180, 180), ylim = c(-90, 90), add = T)
  with(y, points(longitude, latitude, pch = 4, col = "red"))
}
provs_to_interpolate <- as.numeric(na.omit(provs_to_interpolate))
data.frame(PROV_CODE = provs_to_interpolate)
}

rm(pts, er.points, er, er.df)
gc()
# now call the function:
# we're going to do it for 2 blocks of realms:
# 1. for everything except the indo-pacific (west and central)
# 2a. for just those realms
# 2b. for just those realms, but merged across the 2 realms

# 1.
interpolated_provs_no_indo <- ddply(subset(oc.df, !REALM %in%
    c("Central Indo-Pacific", "Western Indo-Pacific")),
  c("genus", "RLM_CODE"), function(y) interpolate_provinces(y))

gc()
# now just indo pacific:
# 2a.
interpolated_provs_indo_only <- ddply(subset(oc.df, REALM %in%
    c("Central Indo-Pacific", "Western Indo-Pacific")),
  c("genus", "RLM_CODE"), function(y) interpolate_provinces(y))
# 2b.
interpolated_provs_indo_only_merged <- ddply(subset(oc.df, REALM %in%
    c("Central Indo-Pacific", "Western Indo-Pacific")),
  "genus", function(y) interpolate_provinces(y))

# now remove the realm column since it doesn't mean much for the
# merged version
interpolated_provs_indo_only$RLM_CODE <- NULL
interpolated_provs_no_indo$RLM_CODE <- NULL

# troubleshooting:
# print(names(interpolated_provs_no_indo))
# print(names(interpolated_provs_indo_only))

# bring back in the rest of the data from the first time we ran the
# function:
interpolated_provs <- rbind(interpolated_provs_no_indo, interpolated_provs_indo_only)
interpolated_provs_alt <- rbind(interpolated_provs_no_indo, interpolated_provs_indo_only_merged)

interpolated_provs <- interpolated_provs[!duplicated(interpolated_provs), ]
interpolated_provs_alt <-
  interpolated_provs_alt[!duplicated(interpolated_provs_alt), ]

save(interpolated_provs, file = "../data/interpolated_provs.rda")
save(interpolated_provs_alt, file = "../data/interpolated_provs_alt.rda")

# which were affected by zero width or height boxes?
# check <- plyr::ddply(oc.df, c("genus", "REALM"), function(x) {
#   out <- "all good"
#   if((max(x$longitude) - min(x$longitude) == 0) & max(x$latitude) - min(x$latitude) == 0) out <- "same location"
#   if((max(x$longitude) - min(x$longitude) == 0) & max(x$latitude) - min(x$latitude) > 0) out <- "same longitude"
#   if((max(x$longitude) - min(x$longitude) > 0) & max(x$latitude) - min(x$latitude) == 0) out <- "same latitude"
#   return(out)
#   }
#   )



