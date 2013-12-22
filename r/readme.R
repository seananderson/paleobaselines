# ====================================================================
# Purpose:       To document all steps needed to run the analysis.
# ====================================================================

# clear workspace, if necessary
rm(list = ls())

# necessary packages:
require(maps)
require(mapproj)
require(mapdata)
require(PBSmapping)
require(RColorBrewer)
require(plyr)
require(data.table) # faster than plyr and easier to read than base
require(gstat) # for spatial smoothing

# necessary functions:
source("map2.R") # custom function - removes ugly country borders
source("col.key.R")
source("smooth.pal.R")
source("grid.ext.data.2.R")
source("plot.map.mollweide.R")

# temporary pre-compiled data file:
load("../data/data.finite.rda")

# grid objects:
# (the object names are the same as the file names)
load("../data/equal_area_grid/global_90x28.rda")
load("../data/equal_area_grid/global_45x14.rda")
source("create.22x7.grid.R") # global_22x7

# get equal area mid points for the 45x14 grid:
m <- subset(data.finite, class == "Mammalia")
mid.pt.lat_45x14 <- data.frame(SID = seq_len(14), lat = sort(unique(m$lat)))
mid.pt.long_45x14 <- data.frame(PID = seq_len(45), long = sort(unique(m$long)))
rm(m)

# get equal area mid points for the 22x7 grid:
mid.pt.long_22x7 <- data.frame(long = global_45x14$long[seq(2, length(global_45x14$long)-2,2)], PID = 1:22) # ignore last long one because 45/2 = 22.5
mid.pt.lat_22x7 <- data.frame(lat = global_45x14$lat[seq(2, length(global_45x14$lat),2)], SID = 1:7) 


## some stuff that I haven't sourced here yet...
## and then...
temp <- grid.ext.data.2(data.finite, grid.object = global_45x14)
pdf("../fig/mar-ext-moll-45x14-8noshift.pdf", width = 3, height = 1.6)
plot.map.mollweide(temp, grid.object = global_45x14)
dev.off()

# spatially smooth the richness data:
# takes a bit of time
# creates the object srdat: [s]mooth [r]ichness [dat]a 
source("idw.richness.sphere.R") 

# visualize it:
p <- ggplot(srdat, aes(PID, SID)) + geom_tile(aes(fill = log(richness))) + facet_wrap(~MatchTaxon)
ggsave("../fig/smooth-richness-matchtaxon.pdf", width = 12, height = 8)

p <- ggplot(rrdat, aes(PID, SID)) + geom_tile(aes(fill = log(Z))) + facet_wrap(~MatchTaxon)
ggsave("../fig/raw-richness-matchtaxon.pdf", width = 12, height = 8)

# grid the smoothed data:
# creates srdat.ext
# for some reason this needs to be entered line by line, can't source
# it:
source("grid.ext.data.idw.R")

pdf("../fig/mar-ext-moll-idw-sphere-2-45x14-8shift.pdf", width = 3, height = 1.6)
plot.map.mollweide(srdat.ext, grid.object = global_45x14)
dev.off()

# now by class
srdat.ext.by.class <- ddply(srdat.with.rates, c("class", "PID", "SID"), summarize, Z = sum(richness * ext)/sum(richness))

p <- ggplot(srdat.ext.by.class, aes(PID, SID)) + geom_tile(aes(fill = log(Z))) + facet_wrap(~class)
ggsave("../fig/ext-smoothed-idw-sphere-2-by-class.pdf", width = 12, height = 7)

# and with a proper projected map with land and separate colour
# schemes:
pdf("../fig/mar-ext-moll-idw-sphere-2-45x14-noshift-by-class.pdf", width = 5, height = 7.75)
par(mfrow = c(5, 2), mar = c(0,0,0,0), oma = c(0,.5,2,.5))
d_ply(srdat.ext.by.class, "class", function(x){
plot.map.mollweide(x, grid.object = global_45x14, set.par.mar = FALSE)
mtext(unique(x$class), side = 3, line = -1.15, col = "grey35")
}
)
dev.off()

#### put this (and above) into separate file
## and do the same thing with the raw data:
rrdat <- ddply(data.finite, "MatchTaxon", function(x) {
  print(paste("Calculating", as.character(unique(x$MatchTaxon)), "richness"))
  grid.ext.data.2(x, grid.object = global_45x14, combine.type = "number", return.events = FALSE)
})
names(rrdat)[4] <- "richness"
uerates <- data.finite[!duplicated(data.finite[,c("MatchTaxon")]), c("MatchTaxon", "class", "ext")]
rrdat.with.rates <- merge(rrdat, uerates, all = TRUE)
rrdat.ext.by.class <- ddply(rrdat.with.rates, c("class", "PID", "SID"), summarize, Z = sum(richness * ext)/sum(richness))
pdf("../fig/mar-ext-moll-raw-sphere-2-45x14-8shift-by-class.pdf", width = 5, height = 7.75)
par(mfrow = c(5, 2), mar = c(0,0,0,0), oma = c(0,.5,2,.5))
d_ply(rrdat.ext.by.class, "class", function(x){
plot.map.mollweide(x, grid.object = global_45x14, set.par.mar = FALSE)
mtext(unique(x$class), side = 3, line = -1.15, col = "grey35")
})
dev.off()

# now do it with a 22x7 grid:
#### put this (and above) into separate file
## and do the same thing with the raw data:
rrdat.22x7 <- ddply(data.finite, "MatchTaxon", function(x) {
  print(paste("Calculating", as.character(unique(x$MatchTaxon)), "richness"))
  grid.ext.data.2(x, grid.object = global_22x7, combine.type = "number", return.events = FALSE)
})
names(rrdat.22x7)[4] <- "richness"
uerates <- data.finite[!duplicated(data.finite[,c("MatchTaxon")]), c("MatchTaxon", "class", "ext")]
rrdat.with.rates.22x7 <- merge(rrdat.22x7, uerates, all = TRUE)
rrdat.ext.by.class.22x7 <- ddply(rrdat.with.rates.22x7, c("class", "PID", "SID"), summarize, Z = sum(richness * ext)/sum(richness))
pdf("../fig/mar-ext-moll-raw-sphere-2-22x7-8shift-by-class.pdf", width = 5, height = 7.75)
par(mfrow = c(5, 2), mar = c(0,0,0,0), oma = c(0,.5,2,.5))
d_ply(rrdat.ext.by.class.22x7, "class", function(x){
plot.map.mollweide(x, grid.object = global_22x7, set.par.mar = FALSE)
mtext(unique(x$class), side = 3, line = -1.15, col = "grey35")
})
dev.off()
####
#and 22x7 for the all classes:
temp <- grid.ext.data.2(data.finite, grid.object = global_22x7)
pdf("../fig/mar-ext-moll-22x7-8shift.pdf", width = 3, height = 1.6)
plot.map.mollweide(temp, grid.object = global_22x7)
dev.off()


#####
# build the richness, extinction rate, halpern, velocity of cc data
# frame:
# (this will take a while)
source("create.ext.halpern.velocity.df.R")
# or load it from here:
load("../data/global_45x14_ext_rich_vel_hlp_20120607.rda")

