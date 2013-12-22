# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Month 00, 2012
# Last modified: Sep 07, 2012
# Purpose:       Take the bounded box version of the modern occurrence
#                data and assign Spalding provinces.
# ====================================================================

# warning - these are pretty big files (~42 million rows!)
# probably best not to do this on a laptop
rm(list = ls())
load("../data/composite.occ2.filled.rda")
n <- nrow(composite.occ2.filled)
composite.occ2.filled <- composite.occ2.filled[1:(n/3), ]
gc()

library(maptools)
library(gdata)
library(ggplot2)
require(plyr)
gpclibPermit()

er <- readShapePoly("../data/MEOW2/meow_ecos.shp")

pts <- SpatialPoints(composite.occ2.filled[,c("longitude", "latitude")])
pts.over <- over(pts, er)

d.eco.filled <- cbind(composite.occ2.filled, pts.over)

d.eco.filled <- na.omit(d.eco.filled)
d.eco.filled1 <- d.eco.filled[!duplicated(d.eco.filled[, c("genus", "group", "match", "ECO_CODE", "ECOREGION", "PROV_CODE", "PROVINCE", "RLM_CODE", "REALM", "ALT_CODE", "ECO_CODE_X", "Lat_Zone")]), ] # remove duplicates (i.e., remove extra grid cells)

save(d.eco.filled1, file = "../data/d.eco.filled1.rda")

