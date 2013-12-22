# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jun 07, 2012
# Last modified: Jul 03, 2012
# Purpose:       Have a quick go at overlaying points onto Spalding et
# al. ecoregions etc.
# ====================================================================

load("../data/data.finite.rda")
require(maptools)

er <- readShapePoly("../data/MEOW2/meow_ecos.shp")
#plot(er, zcol=1,axes=TRUE, border="gray",las = 1,pbg="white")

# test it:
#halifax.dat <- cbind(-63.5744,44.6479)
#pts <- SpatialPoints(halifax.dat)
#over(pts, er)


pts <- SpatialPoints(data.finite[,c("longitude", "latitude")])
pts.over <- over(pts, er)

# merge:
d.eco <- cbind(data.finite, pts.over)

# bring in correct polygon version of marine mammal data:

# source("create.marine.mammal.spalding.data.R")
d.eco$use <- NULL
d.eco$latitude <- NULL
d.eco$longitude <- NULL
d.eco <- d.eco[!duplicated(d.eco), ]
d.eco <- d.eco[-which(is.na(d.eco$ECO_CODE)), ] # not in an ecoregion

load("../data/mm.eco.w.taxonomy.ext.rda")
d.eco <- d.eco[-which(as.character(d.eco$MatchTaxon) %in% as.character(unique(mm.eco.w.taxonomy.ext$MatchTaxon))), ]

d.eco <- merge(d.eco, mm.eco.w.taxonomy.ext, all = TRUE)

#save(d.eco, file = "../data/data.finite.with.ecoregions.20120607.rda")
save(d.eco, file = "../data/data.finite.with.ecoregions.20120702.rda")


#####
# new plots as of July 2 2012:
# SA


library(plyr)
#d.eco.realm.ext <- ddply(subset(d.eco, class != "Mammalia"), "RLM_CODE", function(x) {
 #unique.taxa.poly <- x[!duplicated(x[,c("taxon"),]), ]
 #data.frame(mean.ext = mean(unique.taxa.poly$ext), median.ext = median(unique.taxa.poly$ext), sd.ext = sd(unique.taxa.poly$ext), taxa.diversity = nrow(unique.taxa.poly))
#})  

d.eco.prov.jack <- ddply(subset(d.eco, !class %in% "Mammalia"), "PROV_CODE", function(x) {
  data.frame(no.Bivalvia = mean(subset(x, !class %in% "Bivalvia")$ext),
             no.Echinoidea = mean(subset(x, !class %in% "Echinoidea")$ext), 
             no.Gastopoda = mean(subset(x, !class %in% "Gastropoda")$ext),
             no.Anthozoa = mean(subset(x, !class %in% "Anthozoa")$ext),
             no.Polythalamea = mean(subset(x, !class %in% "Polythalamea")$ext),
             no.Elasmobranchii = mean(subset(x, !class %in% "Elasmobranchii")$ext),
             #no.Mammalia = mean(subset(x, !class %in% "Mammalia")$ext),
             no.Malacostraca = mean(subset(x, !class %in% "Malacostraca")$ext),
             no.Rhynchonellata = mean(subset(x, !class %in% "Rhynchonellata")$ext),
             no.Reptilia = mean(subset(x, !class %in% "Reptilia")$ext))
})
d.eco.prov.indiv <- ddply(subset(d.eco, !class %in% "Mammalia"), "PROV_CODE", function(x) {
  data.frame(only.Bivalvia = mean(subset(x, class %in% "Bivalvia")$ext),
             only.Echinoidea = mean(subset(x, class %in% "Echinoidea")$ext), 
             only.Gastopoda = mean(subset(x, class %in% "Gastropoda")$ext),
             only.Anthozoa = mean(subset(x, class %in% "Anthozoa")$ext),
             only.Polythalamea = mean(subset(x, class %in% "Polythalamea")$ext),
             only.Elasmobranchii = mean(subset(x, class %in% "Elasmobranchii")$ext),
             #only.Mammalia = mean(subset(x, class %in% "Mammalia")$ext),
             only.Malacostraca = mean(subset(x, class %in% "Malacostraca")$ext),
             only.Rhynchonellata = mean(subset(x, class %in% "Rhynchonellata")$ext),
             only.Reptilia = mean(subset(x, class %in% "Reptilia")$ext))
})

er@data$ID <- 1:nrow(er@data) # SUPER important!! Keep this in order for plotting!
er@data <- merge(er@data, d.eco.prov.jack, all = FALSE)
er@data <- merge(er@data, d.eco.prov.indiv, all = FALSE)
## very important! re-order:
er@data <- er@data[order(er@data$ID), ]

#pdf("../fig/ecoregions/ecorealm mean extinction risk without mammals.pdf", width = 10, height = 4)
spplot(er, zcol = c("no.Bivalvia", 
                    "no.Echinoidea", 
                    "no.Gastopoda", 
                    "no.Anthozoa", 
                    "no.Polythalamea", 
                    "no.Elasmobranchii", 
                    #"no.Mammalia", 
                    "no.Malacostraca", 
                    "no.Rhynchonellata", 
                    "no.Reptilia"), col.regions = smooth.pal(brewer.pal(9, "YlOrRd"), 10))
#dev.off()    
spplot(er, zcol = c("no.Bivalvia"), col.regions = smooth.pal(brewer.pal(9, "YlOrRd"), 10))
#dev.off()    

spplot(er, zcol = c("only.Bivalvia", 
                    "only.Echinoidea", 
                    "only.Gastopoda", 
                    "only.Anthozoa", 
                    "only.Polythalamea", 
                    "only.Elasmobranchii", 
                    #"only.Mammalia", 
                    "only.Malacostraca", 
                    "only.Rhynchonellata", 
                    "only.Reptilia"), col.regions = smooth.pal(brewer.pal(9, "YlOrRd"), 10))
#dev.off()    

library(ggplot2)
gpclibPermit() # required for fortify method

#temp.dat <- er@data[,c("PROV_CODE", "PROVINCE", "Lat_Zone", "no.Bivalvia")]
#temp.dat <- temp.dat[!duplicated(temp.dat), ]
#temp.dat$id <- 1:nrow(temp.dat)
er@data$id <- rownames(er@data)
#er@data <- er@data[order(er@data$ECO_CO
er.fort <- fortify(er, region = "ECO_CODE")
names(er.fort)[7] <- "ECO_CODE"
#names(er.fort)[7] <- "PROV_CODE"
#er.fort <- merge(er.fort, er@data)
er.fort.df <- join(er.fort, er@data, by="ECO_CODE")

ggplot(er.fort.df) + aes(long, lat, group = group, fill = no.Bivalvia) + geom_polygon()


#####
#d.eco.int <- ddply(d.eco, "RLM_CODE", function(x) {
  #data.frame(sample.intensity = nrow(x))
#})

#d.eco.int.prov <- ddply(d.eco, "PROV_CODE", function(x) {
  #data.frame(sample.intensity.prov = nrow(x))
#})

er@data$ID <- 1:nrow(er@data) # SUPER important!! Keep this in order for plotting!

pdf("../fig/REALM.pdf", width = 10, height = 4)
spplot(er, zcol = "REALM", col.regions = brewer.pal(12, "Set3"))
dev.off()

#pdf("../fig/eco.realm.raw.sampling.intensity.log", width = 10, height = 4)
#spplot(er, zcol = "sample.intensity.log", col.regions = smooth.pal(brewer.pal(9, "YlOrRd"), 3))
#dev.off()

#pdf("../fig/eco.prov.raw.sampling.intensity.log", width = 10, height = 4)
#spplot(er, zcol = "sample.intensity.prov.log", col.regions = smooth.pal(brewer.pal(9, "YlOrRd"), 3))
#dev.off()

## now try paleo extinction risk * diversity minus mammals
#er@data$ID <- 1:nrow(er@data) # SUPER important!! Keep this in order for plotting!
#er@data <- merge(er@data, na.omit(d.eco.int))
#er@data <- merge(er@data, na.omit(d.eco.int.prov))

d.eco.realm.ext <- ddply(subset(d.eco, class != "Mammalia"), "RLM_CODE", function(x) {
 unique.taxa.poly <- x[!duplicated(x[,c("taxon"),]), ]
 data.frame(mean.ext = mean(unique.taxa.poly$ext), median.ext = median(unique.taxa.poly$ext), sd.ext = sd(unique.taxa.poly$ext), taxa.diversity = nrow(unique.taxa.poly))
})
er@data <- merge(er@data, d.eco.realm.ext, all = FALSE)
## very important! re-order:
er@data <- er@data[order(er@data$ID), ]

pdf("../fig/ecoregions/ecorealm mean extinction risk without mammals.pdf", width = 10, height = 4)
spplot(er, zcol = c("mean.ext"), col.regions = smooth.pal(brewer.pal(9, "YlOrRd"), 10))
dev.off()    

#er@data <- merge(er@data, na.omit(d.eco.int))
#er@data <- merge(er@data, na.omit(d.eco.int.prov))

#er@data$sample.intensity.log <- log(er@data$sample.intensity)
#er@data$sample.intensity.prov.log <- log(er@data$sample.intensity.prov)

## very important! re-order:
#er@data <- er@data[order(er@data$ID), ]

library(RColorBrewer)

pdf("../fig/REALM.pdf", width = 10, height = 4)
spplot(er, zcol = "REALM", col.regions = brewer.pal(12, "Set3"))
dev.off()

#pdf("../fig/eco.realm.raw.sampling.intensity.log", width = 10, height = 4)
#spplot(er, zcol = "sample.intensity.log", col.regions = smooth.pal(brewer.pal(9, "YlOrRd"), 3))
#dev.off()

#pdf("../fig/eco.prov.raw.sampling.intensity.log", width = 10, height = 4)
#spplot(er, zcol = "sample.intensity.prov.log", col.regions = smooth.pal(brewer.pal(9, "YlOrRd"), 3))
#dev.off()

## now try paleo extinction risk * diversity minus mammals
#er@data$ID <- 1:nrow(er@data) # SUPER important!! Keep this in order for plotting!
#er@data <- merge(er@data, na.omit(d.eco.int))
#er@data <- merge(er@data, na.omit(d.eco.int.prov))

d.eco.realm.ext <- ddply(subset(d.eco, class != "Mammalia"), "RLM_CODE", function(x) {
 unique.taxa.poly <- x[!duplicated(x[,c("taxon"),]), ]
 data.frame(mean.ext = mean(unique.taxa.poly$ext), median.ext = median(unique.taxa.poly$ext), sd.ext = sd(unique.taxa.poly$ext), taxa.diversity = nrow(unique.taxa.poly))
})
er@data <- merge(er@data, d.eco.realm.ext, all = FALSE)
## very important! re-order:
er@data <- er@data[order(er@data$ID), ]

pdf("../fig/ecoregions/ecorealm mean extinction risk without mammals.pdf", width = 10, height = 4)
spplot(er, zcol = c("mean.ext"), col.regions = smooth.pal(brewer.pal(9, "YlOrRd"), 10))
dev.off()
pdf("../fig/ecoregions/ecorealm median extinction risk without mammals.pdf", width = 10, height = 4)
spplot(er, zcol = c("median.ext"), col.regions = smooth.pal(brewer.pal(9, "YlOrRd"), 10))
dev.off()
pdf("../fig/ecoregions/ecorealm standard deviation extinction risk without mammals.pdf", width = 10, height = 4)
spplot(er, zcol = c("sd.ext"), col.regions = smooth.pal(brewer.pal(9, "YlOrRd"), 10))
dev.off()
pdf("../fig/ecoregions/ecorealm taxa diversity without mammals.pdf", width = 10, height = 4)
spplot(er, zcol = c("taxa.diversity"), col.regions = smooth.pal(brewer.pal(9, "YlOrRd"), 10))
dev.off()

# by province:
d.eco.province.ext <- ddply(subset(d.eco, class != "Mammalia"), "PROV_CODE", function(x) {
 unique.taxa.poly <- x[!duplicated(x[,c("taxon"),]), ]
 data.frame(mean.ext.prov = mean(unique.taxa.poly$ext), median.ext.prov = median(unique.taxa.poly$ext), sd.ext.prov = sd(unique.taxa.poly$ext), taxa.diversity.prov = nrow(unique.taxa.poly))
})

er@data <- merge(er@data, d.eco.province.ext, all = TRUE)
er@data <- er@data[!is.na(er@data$PROV_CODE), ] # this should remove 1 row... those without an associated province
## very important! re-order:
er@data <- er@data[order(er@data$ID), ]

pdf("../fig/ecoregions/ecoprovince mean extinction risk without mammals.pdf", width = 10, height = 4)
spplot(er, zcol = c("mean.ext.prov"), col.regions = smooth.pal(brewer.pal(9, "YlOrRd"), 10))
dev.off()
pdf("../fig/ecoregions/ecoprovince median extinction risk without mammals.pdf", width = 10, height = 4)
spplot(er, zcol = c("median.ext.prov"), col.regions = smooth.pal(brewer.pal(9, "YlOrRd"), 10))
dev.off()
pdf("../fig/ecoregions/ecoprovince standard deviation extinction risk without mammals.pdf", width = 10, height = 4)
spplot(er, zcol = c("sd.ext.prov"), col.regions = smooth.pal(brewer.pal(9, "YlOrRd"), 10))
dev.off()
pdf("../fig/ecoregions/ecoprovince taxa diversity without mammals.pdf", width = 10, height = 4)
spplot(er, zcol = c("taxa.diversity.prov"), col.regions = smooth.pal(brewer.pal(9, "YlOrRd"), 10))
dev.off()

########
# bring in halpern and climate data:
# use the grid celled version for now
load("../data/halpern.grid.45x14.poly.rda")
halpern.grid.45x14.poly <- as.data.frame(halpern.grid.45x14.poly)
halpern.pts <- SpatialPoints(halpern.grid.45x14.poly[,c("X", "Y")])
halpern.pts.over <- over(halpern.pts, er)
halpern.eco <- cbind(halpern.grid.45x14.poly, halpern.pts.over)
halpern.eco <- na.omit(halpern.eco)
# take only one instance of each cell per RLM or province... all from each cell
# are the same
halpern.eco.prov.no.dup <- halpern.eco[!duplicated(halpern.eco[, c("PID_SID", "PROV_CODE")]), ]
halpern.eco.rlm.no.dup <- halpern.eco[!duplicated(halpern.eco[, c("PID_SID", "RLM_CODE")]), ]
# reduce the variou
halpern.eco.sum.prov <- ddply(halpern.eco.prov.no.dup, "PROV_CODE", summarize, mean.halpern.prov = mean(Z))
halpern.eco.sum.rlm <- ddply(halpern.eco.rlm.no.dup, "RLM_CODE", summarize, mean.halpern.rlm = mean(Z))

er@data$ID <- 1:nrow(er@data) # SUPER important!! Keep this in order for plotting!
er@data <- merge(er@data, halpern.eco.sum.prov, all = TRUE)
er@data <- merge(er@data, halpern.eco.sum.rlm, all = TRUE)
er@data <- er@data[order(er@data$ID), ] ## very important! re-order

pdf("../fig/ecoregions/halpern from 45x14 grid by ecoprovince.pdf", width = 10, height = 4)
spplot(er, zcol = "mean.halpern.prov", col.regions = smooth.pal(brewer.pal(9, "YlOrRd"), 3))
dev.off()
pdf("../fig/ecoregions/halpern from 45x14 grid by ecorealm.pdf", width = 10, height = 4)
spplot(er, zcol = "mean.halpern.rlm", col.regions = smooth.pal(brewer.pal(9, "YlOrRd"), 3))
dev.off()

# bring in the velocity data:
vc <- read.csv("../data/velocity-of-climate-change/Velocity.csv", stringsAsFactors = FALSE)
vc.pts <- SpatialPoints(vc[,c("x", "y")])
vc.pts.over <- over(vc.pts, er)
vc.eco <- cbind(vc, vc.pts.over)
vc.eco <- na.omit(vc.eco)
vc.eco.sum.prov <- ddply(vc.eco, "PROV_CODE", summarize, mean.vc.prov = mean(velocity))
vc.eco.sum.rlm <- ddply(vc.eco, "RLM_CODE", summarize, mean.vc.rlm = mean(velocity))

# log transform
# TODO - note, there might be some land getting into the ecoregions
vc.eco.sum.prov[vc.eco.sum.prov$mean.vc.prov > 0, "mean.vc.prov"] <- log(abs(vc.eco.sum.prov[vc.eco.sum.prov$mean.vc.prov > 0, "mean.vc.prov"]))
vc.eco.sum.prov[vc.eco.sum.prov$mean.vc.prov < 0, "mean.vc.prov"] <- -log(abs(vc.eco.sum.prov[vc.eco.sum.prov$mean.vc.prov < 0, "mean.vc.prov"]))

vc.eco.sum.rlm[vc.eco.sum.rlm$mean.vc.rlm > 0, "mean.vc.rlm"] <- log(abs(vc.eco.sum.rlm[vc.eco.sum.rlm$mean.vc.rlm > 0, "mean.vc.rlm"]))
vc.eco.sum.rlm[vc.eco.sum.rlm$mean.vc.rlm < 0, "mean.vc.rlm"] <- -log(abs(vc.eco.sum.rlm[vc.eco.sum.rlm$mean.vc.rlm < 0, "mean.vc.rlm"]))

er@data <- merge(er@data, vc.eco.sum.prov, all = TRUE)
er@data <- merge(er@data, vc.eco.sum.rlm, all = TRUE)
er@data <- er@data[order(er@data$ID), ] ## very important! re-order
# log transform:

# velocity of climate change plots:
pdf("../fig/ecoregions/velocity climate change logged by ecoprovince.pdf", width = 10, height = 4)
spplot(er, zcol = "mean.vc.prov", col.regions = rev(smooth.pal(brewer.pal(9, "RdBu"), 20))[-c(1:35)])
dev.off()
pdf("../fig/ecoregions/velocity climate change logged by ecorealm.pdf", width = 10, height = 4)
spplot(er, zcol = "mean.vc.rlm", col.regions = rev(smooth.pal(brewer.pal(9, "RdBu"), 20))[-c(1:70)])
dev.off()

save(er, file = "../data/ecoregion.data.with.ext.halpern.climate.rda")

lat_zone_df <- data.frame(Lat_Zone = c("Polar", "Temperate", "Tropical"), Lat_Zone_Order = c(3,2,1))
er@data <- merge(er@data, lat_zone_df)
er@data <- transform(er@data, Lat_Zone_reversed = reorder(Lat_Zone, Lat_Zone_Order))
er@data$Lat_Zone <- er@data$Lat_Zone_reversed
er@data$Lat_Zone_Order <- NULL
er@data$Lat_Zone_reversed <- NULL
er@data <- er@data[order(er@data$ID), ] ## very important! re-order

library(ggplot2)
p <- ggplot(er@data, aes(mean.ext.prov, mean.halpern.prov)) + geom_point(aes(col = Lat_Zone, size= taxa.diversity.prov)) 
ggsave("../fig/ecoregions/mean halpern (gridded) vs mean extinction rate by ecoprovince.pdf")
p <- ggplot(er@data, aes(mean.ext, mean.halpern.rlm)) + geom_point(aes(col = Lat_Zone, size= taxa.diversity)) 
ggsave("../fig/ecoregions/mean halpern (gridded) vs mean extinction rate by ecorealm.pdf")

p <- ggplot(er@data, aes(mean.ext.prov, mean.vc.prov)) + geom_point(aes(col = Lat_Zone, size= taxa.diversity.prov)) 
ggsave("../fig/ecoregions/mean climate change velocity vs mean extinction rate by ecoprovince.pdf")
p <- ggplot(er@data, aes(mean.ext, mean.vc.rlm)) + geom_point(aes(col = Lat_Zone, size= taxa.diversity)) 
ggsave("../fig/ecoregions/mean climate change velocity vs mean extinction rate by ecorealm.pdf")

p <- ggplot(er@data, aes(mean.ext.prov, mean.vc.prov)) + geom_point(aes(col = REALM), cex  = 3) + scale_colour_manual(values = brewer.pal(12, "Set3"))
ggsave("../fig/ecoregions/mean climate change velocity vs mean extinction rate grouped by ecoprovince by REALMs identified.pdf")

p <- ggplot(er@data, aes(mean.ext.prov, mean.halpern.prov)) + geom_point(aes(col = REALM), cex  = 3) + scale_colour_manual(values = brewer.pal(12, "Set3"))
ggsave("../fig/ecoregions/mean halpern (gridded) vs mean extinction rate grouped by ecoprovince by REALMs identified.pdf")


