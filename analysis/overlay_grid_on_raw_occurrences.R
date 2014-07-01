# ====================================================================
# Created by:    Sean C. Anderson
# Created:       Feb 06, 2012
# Last modified: Jun 03, 2014
# Purpose:       Overlay the equal area grid on the raw occurrence
#                data as a sensitivity test for our interpolation
#                method.
# ====================================================================

load("../data/composite.occ2.rda")
composite.occ2 <- composite.occ2[!composite.occ2$group %in%
  c("Mammalia", "Elasmobranchii"), ]
comp.dat <- composite.occ2
# comp.dat <- composite.occ2[sample(1:nrow(composite.occ2), 30000), ] # for testing
rm(composite.occ2)

# Fix a single point (genus = Herrita) that sits right on the border of a grid
# cell and realm. It gets placed in Province 5 in the polygons but sits in the
# grid cell below in the raw data. Here I'm moving the raw data point up
# slightly to match the interpolation version (and polygon matched version)
# which is where it will get plotted.
comp.dat[comp.dat$genus == "Herrita", "latitude"] <- comp.dat[comp.dat$genus == "Herrita", "latitude"] + 0.1

# replace sharks and mammals with the original raw data:
sharks <- read.csv("../data/SharkProvinceJoin2pt5DegreeBuffer_20121009.csv",
  stringsAsFactors = FALSE)[,c("PROV_CODE", "species")]
mar_mamm <- read.csv("../data/MarineMammalProvinceJoin2.csv",
  stringsAsFactors = FALSE)[,c("PROV_CODE", "BINOMIAL")]
names(mar_mamm) <- c("PROV_CODE", "species")
sharks$genus <- gsub("([a-zA-Z]+)_[a-zA-Z]+", "\\1", sharks$species)
mar_mamm$genus <- gsub("([a-zA-Z]+) [a-zA-Z]+", "\\1", mar_mamm$species)
sharks$species <- NULL
mar_mamm$species <- NULL
sharks <- sharks[!duplicated(sharks), ]
mar_mamm <- mar_mamm[!duplicated(mar_mamm), ]
sharks_and_mamm <- rbind(sharks, mar_mamm)
# remove those rows without a genus:
sharks_and_mamm <- subset(sharks_and_mamm, genus != "")
# and substitute later...

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
if(file.exists("../data/pts.over.cache.rda")) {
  load("../data/pts.over.cache.rda")
}else{
  pts.over <- over(pts, er)
  save(pts.over, file = "../data/pts.over.cache.rda")
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

#oc.df <- subset(oc.df, !group %in% c("Mammalia", "Elasmobranchii"))
# splice in the 'interpolated' sharks and mammals
# which aren't really interpolated
# they came from range maps
load("../data/genus_cells.rda")
sharks_and_mammals_gridded <- genus_cells[genus_cells$genus %in%
  sharks_and_mamm$genus,]
sharks_and_mammals_gridded$num_cells <- NULL
rm(genus_cells)
gc()

genus_cells_no_interpolation <- data.frame(genus = oc.df$genus, X_mid = oc.df$X_mid, Y_mid = oc.df$Y_mid)
genus_cells_no_interpolation <- rbind(genus_cells_no_interpolation, sharks_and_mammals_gridded)
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

# now add in the equivalent sharks and mammals:
sharks_and_mamm$n_obs <- NA # to match; from range maps
obis_prov_obs_no_interp <- rbind(obis_prov_obs_no_interp, sharks_and_mamm)


saveRDS(obis_prov_obs_no_interp, file = "../data/obis_prov_obs_no_interp.rds")

rm(genus_cells_no_interpolation, comp.dat, grid_mid_lat, grid_mid_long, oc.df, er, pts, oc.df.temp)

