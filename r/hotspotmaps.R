# Created by:    Sean C. Anderson
# Created:       Feb 28, 2013
# Last modified: Feb 28, 2013
# Purpose:       try hotspot maps with halpern, burrows, and ext risk

# create the dataset to map:
rm(list = ls())
source("Differnce extinction risk and other map layers.R")
# see er.df

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  # hues = seq(25, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

cols <- paste(gg_color_hue(3), "", sep = "")
cols[2] <- gg_color_hue(8)[7]

ext.risk.thresh <- as.numeric(quantile(er.df$ext.risk, probs = 0.8))
halp.thresh <- as.numeric(quantile(er.df$Halpern, probs = 0.8, na.rm = TRUE))
bur.thresh <- as.numeric(quantile(er.df$Burrows, probs = 0.8, na.rm = TRUE))
er.df <- transform(er.df, ext.hot = ifelse(ext.risk >= ext.risk.thresh, TRUE, FALSE), halp.hot = ifelse(Halpern >= halp.thresh, TRUE, FALSE), bur.hot = ifelse(Burrows >= bur.thresh, TRUE, FALSE))

er.df <- transform(er.df, base.col = ifelse(bur.hot == FALSE & halp.hot == FALSE & ext.hot == FALSE, "grey85", NA))

#er.df.ext.hot <- subset(er.df, ext.hot == TRUE)
#unionSpatialPolygons(


#er.df.joined <- er.df[!duplicated(er.df[,c("long", "lat")]), c("long", "lat", "ext.hot", "halp.hot", "bur.hot", "base.col")]
pdf("hotspots_test_1.pdf", width = 7, height = 4)
par(mar = c(3,0,0, 0), oma= c(0,0,0,0))
with(er.df, plot(long, lat, type = "n", col = "grey90", lwd = 0.5, axes = FALSE, xlab = "", ylab = "", asp = 1))
d_ply(er.df, "group", function(x) {
  #with(x, polygon(long, lat, border = NA, col = "grey85", lwd = .5))
  #with(x, polygon(long, lat, border = c(NA, cols[1])[ext.hot+1], lwd = 1.5))
  with(x, polygon(long, lat, border = NA, col =  c(NA, cols[1])[ext.hot+1], lwd = 1.5))
  #with(x, polygon(long, lat, border = NA, col =  c(NA, cols[2])[halp.hot+1], lwd = 1.5))
  #with(x, polygon(long, lat, border = NA, col =  c(NA, cols[3])[bur.hot+1], lwd = 1.5))
  with(x, polygon(long, lat, border = c(NA, "grey30")[halp.hot+1], lwd = 1.2))
  with(x, polygon(long, lat, border = NA, col = c(NA, "grey50")[bur.hot+1], lwd = 1.5, density = c(NA, 25)[bur.hot+1], angle = 45))
})
d_ply(land.fort, "group", function(x) {
  with(x, polygon(long, lat, border = NA, lwd = 0.5, col = "grey77"))
})
#legend("bottomright", fill = cols, legend = c("Ext", "Halpern", "Burrows"), bty = "n", col = cols)
par(xpd = NA)
legend(120, -90, fill = c(cols[1],NA, "grey50"), col = c(cols[1], "grey20", "grey50"), legend = c("Ext", "Halpern", "Burrows"), bty = "n", density = c(NA, NA, 25), angle = c(NA, NA, 45), border = c(cols[1], "grey20", NA))
dev.off()

er.df$col <- NA
er.df[er.df$ext.hot == TRUE, "col"] <- cols[1]
er.df[er.df$halp.hot == TRUE & !is.na(er.df$halp.hot), "col"] <- cols[3]
er.df[er.df$halp.hot == TRUE & !is.na(er.df$halp.hot) & er.df$ext.hot == TRUE, "col"] <- cols[2]

pdf("hotspots_test_2.pdf", width = 7, height = 4)
par(mar = c(3,0,0, 0), oma= c(0,0,0,0))
with(er.df, plot(long, lat, type = "n", col = "grey90", lwd = 0.5, axes = FALSE, xlab = "", ylab = "", asp = 1))
# ext risk
d_ply(er.df, "group", function(x) {
  with(x, polygon(long, lat, border = NA, col =  col, lwd = 1.5))
})
# halp
#d_ply(er.df, "group", function(x) {
  #with(x, polygon(long, lat, border = NA, col = , lwd = 1.2))
#})
# burr
d_ply(er.df, "group", function(x) {
  with(x, polygon(long, lat, border = NA, col = c(NA, "grey50")[bur.hot+1], lwd = 1.5, density = c(NA, 25)[bur.hot+1], angle = 45))
})
# land
d_ply(land.fort, "group", function(x) {
  with(x, polygon(long, lat, border = NA, lwd = 0.5, col = "grey77"))
})
#legend("bottomright", fill = cols, legend = c("Ext", "Halpern", "Burrows"), bty = "n", col = cols)
par(xpd = NA)
legend(120, -90, fill = c(cols[1],cols[3], "grey50"), col = c(cols[1], cols[3], "grey50"), legend = c("Ext", "Halpern", "Burrows"), bty = "n", density = c(NA, NA, 25), angle = c(NA, NA, 45), border = c(cols[1], cols[3], NA))
dev.off()

require(mapproj)
library(RColorBrewer)
col.pal <- paste(brewer.pal(9, "YlOrRd"), 90, sep = "")
er.df$col.pal.ext.risk <- col.pal[findInterval(er.df$ext.risk, seq(min(er.df$ext.risk), max(er.df$ext.risk), length.out = 10))]
er.df.mol <- mapproject(list(x = er.df$long, y = er.df$lat), proj = "mollweide")
er.df.m <- er.df
er.df.m$x <- er.df.mol$x
er.df.m$y <- er.df.mol$y

land.fort.mol <- mapproject(list(x = land.fort$long, y = land.fort$lat), proj = "mollweide")
land.fort.m <- land.fort
land.fort.m$x <- land.fort.mol$x
land.fort.m$y <- land.fort.mol$y

#source("map2.R")
cols <- paste(gg_color_hue(3), "", sep = "")
pdf("hotspots_test_3.pdf", width = 7, height = 4)
par(mar = c(0,0,0, 0), oma= c(0,0,0,0))
#with(er.df.m, plot(x, y, type = "n", col = "grey90", lwd = 0.5, axes = FALSE, xlab = "", ylab = ""))
map("world", proj = "mollweide",  col = "grey77", fill = TRUE, lwd = .3, border = c(0, 0), wrap = FALSE, resolution = 0, xlim = c(-178, 178), plot = TRUE)
# ext
d_ply(er.df.m, "group", function(x) {
  with(x, polygon(x, y, border = NA, col =  c(NA, cols[1])[ext.hot+1], lwd = 1.5))
})
# halp
d_ply(er.df.m, "group", function(x) {
  with(x, polygon(x, y, border = c(NA, "grey30")[halp.hot+1], lwd = 1.3))
})
# burr
d_ply(er.df.m, "group", function(x) {
  with(x, polygon(x, y, border = NA, col = c(NA, "grey40")[bur.hot+1], lwd = 1.5, density = c(NA, 23)[bur.hot+1], angle = 45))
})
# land
#d_ply(land.fort.m, "group", function(x) {
  #with(x, polygon(x, y, border = NA, lwd = 0.5, col = "grey77"))
#})
#map("world", proj = "", mar = c(0, 0, 0, 0), col = "#AECCB2", 
map("world", proj = "", mar = c(0, 0, 0, 0), col = "grey77", 
  fill = TRUE, lwd = .3, border = c(0, 0), wrap = FALSE, 
  resolution = 0, xlim = c(-178, 178), plot = TRUE, add = TRUE)
#}
par(xpd = NA)
legend(120, -90, fill = c(cols[1],NA, "grey50"), col = c(cols[1], "grey20", "grey50"), legend = c("Ext", "Halpern", "Burrows"), bty = "n", density = c(NA, NA, 25), angle = c(NA, NA, 45), border = c(cols[1], "grey20", NA))
dev.off()

#### with underlying gradient
pdf("hotspots_test_4.pdf", width = 7, height = 4)
par(mar = c(0,0,0, 0), oma= c(0,0,0,0))
#with(er.df.m, plot(x, y, type = "n", col = "grey90", lwd = 0.5, axes = FALSE, xlab = "", ylab = ""))
map("world", proj = "mollweide",  col = "grey77", fill = TRUE, lwd = .3, border = c(0, 0), wrap = FALSE, resolution = 0, xlim = c(-178, 178), plot = TRUE)
# ext
d_ply(er.df.m, "group", function(x) {
  with(x, polygon(x, y, border = NA, col = col.pal.ext.risk, lwd = 1.5))
})
# halp
d_ply(er.df.m, "group", function(x) {
  with(x, polygon(x, y, border = c(NA, "grey20")[halp.hot+1], lwd = 1.5))
})
# burr
d_ply(er.df.m, "group", function(x) {
  with(x, polygon(x, y, border = NA, col = c(NA, "grey20")[bur.hot+1], lwd = 1.5, density = c(NA, 23)[bur.hot+1], angle = 45))
})
# land
#d_ply(land.fort.m, "group", function(x) {
  #with(x, polygon(x, y, border = NA, lwd = 0.5, col = "grey77"))
#})
#map("world", proj = "", mar = c(0, 0, 0, 0), col = "#AECCB2", 
map("world", proj = "", mar = c(0, 0, 0, 0), col = "grey69", 
  fill = TRUE, lwd = .3, border = c(0, 0), wrap = FALSE, 
  resolution = 0, xlim = c(-178, 178), plot = TRUE, add = TRUE)
#}
par(xpd = NA)
legend(120, -90, fill = c(cols[1],NA, "grey50"), col = c(cols[1], "grey20", "grey50"), legend = c("Ext", "Halpern", "Burrows"), bty = "n", density = c(NA, NA, 25), angle = c(NA, NA, 45), border = c(cols[1], "grey20", NA))
dev.off()








##### ignore below...

#plot(er)

er.df.ext.risk <- er.df[,c("ext.risk", "PROV_CODE", "ext.hot")]
er.df.ext.risk <- er.df.ext.risk[!duplicated(er.df.ext.risk), ]
er@data <- join(er@data, er.df.ext.risk)
er@data <- er@data[order(er@data$id), ]
#plot(er)
junk <- unionSpatialPolygons(SpP = er, IDs = er@data$ext.hot)

plot(junk)
j.f <- fortify(junk, region = "id")
j.f <- join(j.f, er@data, by = "id")
ggplot(j.f, aes(long, lat, group = group))  + geom_polygon(aes(fill = ext.hot))




joined.ext.provs <- dlply(er@data, "RLM_CODE", function(x) {
  regions_to_join <- list()
# go through each region poly and gather it
# then we'll join them by realm
  for(i in 1:nrow(x)) {
    regions_to_join[[i]] <-  slot(er, "polygons")[[x$ID[i]]]
  }
  realm_with_many_regions <- SpatialPolygons(regions_to_join)
# now join them where possible:
  realm_union_regions <- unionSpatialPolygons(SpP = realm_with_many_regions, IDs = rep(1, length(realm_with_many_regions)))
  realm_union_regions
})

er.ext.joined <- unionSpatialPolygons(SpP = er, IDs = rep(1, length(realm_with_many_regions)))


#####

