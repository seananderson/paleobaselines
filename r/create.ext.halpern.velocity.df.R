# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       May 31, 2012
# Last modified: Jun 08, 2012
# Purpose:       create data frame with ext, halpern, taxonomic,
# richness, and velocity data
# ====================================================================

create.ext.halpern.velocity.df <- function(ext.dat, grid.object, mid.pt.lat, mid.pt.long) {
halpern.data <- read.csv("~/Dropbox/NESCent-extinction/map/data/Halpern_impact_grid.csv")

# original data:
#bt <- read.table("../data/Global_Bathymetry/SFGlobal-4531/SFGlobal-4531.txt")
# faster loading:
load("../data/Global_Bathymetry/SFGlobal-4531/SFGlobal-4531.rda")
names(bt) <- c("longitude", "latitude", "ext")
bt$taxon <- 1:nrow(bt)
bt$class <- "IGNORE"
bt.out.mean <- grid.ext.data.2(bt, grid.object = grid.object, combine.type = "mean")
print("Created bathymetric mean data.")
bt.out.median <- grid.ext.data.2(bt, grid.object = grid.object, combine.type = "median")
print("Created bathymetric median data.")
bt.out.min <- grid.ext.data.2(bt, grid.object = grid.object, combine.type = "min")
print("Created bathymetric min data.")
bt.out.max <- grid.ext.data.2(bt, grid.object = grid.object, combine.type = "max")
print("Created bathymetric max data.")

names(bt.out.mean)[3] <- "Z_depth_mean"
names(bt.out.median)[3] <- "Z_depth_median"
names(bt.out.min)[3] <- "Z_depth_min"
names(bt.out.max)[3] <- "Z_depth_max"

# bring in the halpern data:
halpern.data$taxon <- 1:nrow(halpern.data)
halpern.data$class <- "IGNORE"
names(halpern.data)[5:6] <- c("longitude", "latitude")
names(halpern.data)[8] <- c("ext")
hal.out <- grid.ext.data.2(halpern.data, grid.object = grid.object)
names(hal.out)[3] <- "Z_hlp"
print("Created Halpern mean data.")

# bring in the velocity data:
vc <- read.csv("../data/velocity-of-climate-change/Velocity.csv", stringsAsFactors = FALSE)
names(vc)[c(1, 2, 9)] <- c("longitude", "latitude", "ext")
vc$taxon <- 1:nrow(vc)
vc$class <- "IGNORE"
vc.grid <- grid.ext.data.2(vc, grid.object = grid.object)
names(vc.grid)[3] <- "Z_vel"
print("Created climate change mean data.")

# look at the data?
#library(ggplot2)
#vc.grid[vc.grid$Z_vel > 0, "Z_vel"] <- log(abs(vc.grid[vc.grid$Z_vel > 0, "Z_vel"]))
#vc.grid[vc.grid$Z_vel < 0, "Z_vel"] <- -log(abs(vc.grid[vc.grid$Z_vel < 0, "Z_vel"]))
#ggplot(vc.grid, aes(PID, SID)) + geom_tile(aes(fill = Z_vel))

# bring in richness data by matchtaxon
richness.ext.dat <- ddply(ext.dat, "MatchTaxon", function(x) {
  grid.ext.data.2(x, combine.type = "number", return.events = FALSE, grid.object = grid.object)
})
names(richness.ext.dat)[4] <- "Z_rich"

# bring in taxonomic and raw events data:
out.ext.events <- ddply(ext.dat, "MatchTaxon", function(x) {
  grid.ext.data.2(x, combine.type = "mean", return.events = TRUE, grid.object = grid.object)
})
out.ext.events$Z <- NULL # already in "ext" so "Z" is redundant
#subset(out, Z > 0)
out.ext.events$X <- NULL # a random coordinate of one species in the matchtaxon-cell. Not really useful
out.ext.events$Y <- NULL # a random coordinate of one species in the matchtaxon-cell. Not really useful

# merge all together
d <- merge(out.ext.events, richness.ext.dat, all = TRUE)
d <- merge(d, richness.ext.dat, all = TRUE)
d <- merge(d, vc.grid, all = TRUE)
d <- merge(d, hal.out, all = TRUE)
d <- merge(d, bt.out.mean, all = TRUE)
d <- merge(d, bt.out.median, all = TRUE)
d <- merge(d, bt.out.min, all = TRUE)
d <- merge(d, bt.out.max, all = TRUE)

# remove those on land by taking those with a Halpern index above 0:
d <- subset(d, Z_hlp > 0)

# reorder columns and make the names prettier:
d <- d[,c("MatchTaxon", "class", "taxon", "EID", "PID", "SID", "X.int", "Y.int", "ext", "Z_rich", "Z_vel", "Z_hlp", "Z_depth_mean", "Z_depth_median", "Z_depth_min", "Z_depth_max")]
names(d) <- c("MatchTaxon", "class", "taxon", "EID", "PID", "SID", "long", "lat", "ext", "rich", "vel", "hlp", "depth_mean", "depth_median", "depth_min", "depth_max") 

# strip factor levels
d$MatchTaxon <- as.character(d$MatchTaxon)
d$taxon <- as.character(d$taxon)
d$class <- as.character(d$class)

# EIDs are no longer necessary here:
d$EID <- NULL

# switch to lat and long from midpoint of cells:
d$long <- NULL
d$lat <- NULL

d <- merge(d, mid.pt.lat)
d <- merge(d, mid.pt.long)
d
}

d.45x14 <- create.ext.halpern.velocity.df(ext.dat = data.finite, grid.object = global_45x14, mid.pt.lat = mid.pt.lat_45x14, mid.pt.long = mid.pt.long_45x14)

d.22x7 <- create.ext.halpern.velocity.df(ext.dat = data.finite, grid.object = global_22x7, mid.pt.lat = mid.pt.lat_22x7, mid.pt.long = mid.pt.long_22x7)

save(d.45x14, file = "../data/global_45x14_ext_rich_vel_hlp_20120607.rda")
save(d.22x7, file = "../data/global_22x7_ext_rich_vel_hlp_20120607.rda")

# visualize the depth data:
#temp <- melt(d.45x14, id.vars = c("PID", "SID"), measure.vars = c("depth_mean", "depth_median", "depth_min", "depth_max"))
#p <- ggplot(temp, aes(PID, SID)) + geom_tile(aes(fill=value)) + facet_wrap(~variable)
#ggsave("../fig/depth_grid.pdf", width = 10, height = 6)

#p <- ggplot(d.45x14, aes(PID, SID)) + geom_tile(aes(fill=depth_mean)) 
#ggsave("../fig/depth_grid_mean.pdf", width = 7, height = 4)
