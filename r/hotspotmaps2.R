# Created by:    Sean C. Anderson
# Created:       Feb 28, 2013
# Last modified: May 20, 2013
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

require(mapproj)
library(maps)
library(RColorBrewer)
#col.pal <- paste(brewer.pal(9, "YlOrRd"), 90, sep = "")
col.pal <- paste(brewer.pal(9, "YlOrRd"), "", sep = "")
source("smooth.pal.R")
col.pal <- smooth.pal(col.pal)
col.pal <- col.pal[-c(1:7)]
library(vcd)
col.pal <- (terrain_hcl(80))
er.df$col.pal.ext.risk <- col.pal[findInterval(er.df$ext.risk, seq(min(er.df$ext.risk), max(er.df$ext.risk), length.out = length(col.pal)+1))]
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

#####
pdf("hotspots_test_8.pdf", width = 7, height = 4)
par(mar = c(0,0,0, 0), oma= c(0,0,0,0))
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
map("world", proj = "", mar = c(0, 0, 0, 0), col = "grey69", 
  fill = TRUE, lwd = .3, border = c(0, 0), wrap = FALSE, 
  resolution = 0, xlim = c(-178, 178), plot = TRUE, add = TRUE)
par(xpd = NA)
legend(120, -90, fill = c(cols[1],NA, "grey50"), col = c(cols[1], "grey20", "grey50"), legend = c("Ext", "Halpern", "Burrows"), bty = "n", density = c(NA, NA, 25), angle = c(NA, NA, 45), border = c(cols[1], "grey20", NA))
dev.off()

### with both hatches:
pdf("hotspots_test_5.pdf", width = 7, height = 4)
par(mar = c(0,0,0, 0), oma= c(0,0,0,0))
map("world", proj = "mollweide",  col = "grey77", fill = TRUE, lwd = .3, border = c(0, 0), wrap = FALSE, resolution = 0, xlim = c(-178, 178), plot = TRUE)
# ext
d_ply(er.df.m, "group", function(x) {
  with(x, polygon(x, y, border = NA, col = col.pal.ext.risk, lwd = 1.5))
})
# halp
d_ply(er.df.m, "group", function(x) {
  with(x, polygon(x, y, border = NA, col = c(NA, "white")[halp.hot+1], lwd = 2.0, density = c(NA, 10)[halp.hot+1], angle = -45))
})
#d_ply(er.df.m, "group", function(x) {
  #with(x, polygon(x, y, border = c(NA, "grey20")[halp.hot+1], lwd = 1.5))
#})
# burr
d_ply(er.df.m, "group", function(x) {
  with(x, polygon(x, y, border = NA, col = c(NA, "grey15")[bur.hot+1], lwd = 1.8, density = c(NA, 14)[bur.hot+1], angle = 45))
})
# land
map("world", proj = "", mar = c(0, 0, 0, 0), col = "grey69", 
  fill = TRUE, lwd = .3, border = c(0, 0), wrap = FALSE, 
  resolution = 0, xlim = c(-178, 178), plot = TRUE, add = TRUE)
par(xpd = NA)
legend(120, -90, fill = c(cols[1],NA, "grey50"), col = c(cols[1], "grey20", "grey50"), legend = c("Ext", "Halpern", "Burrows"), bty = "n", density = c(NA, NA, 25), angle = c(NA, NA, 45), border = c(cols[1], "grey20", NA))
dev.off()

# HCL colours:
#col.pal <- paste(brewer.pal(9, "YlOrRd"), "", sep = "")
#col.pal.hcl <- rev(gg_color_hue(25))
#er.df$col.pal.ext.risk.hcl <- col.pal.hcl[findInterval(er.df$ext.risk, seq(min(er.df$ext.risk), max(er.df$ext.risk), length.out = 26))]
#er.df.mol <- mapproject(list(x = er.df$long, y = er.df$lat), proj = "mollweide")
#er.df.m <- er.df
#er.df.m$x <- er.df.mol$x
#er.df.m$y <- er.df.mol$y

pdf("hotspots_test_7.pdf", width = 7, height = 4)
par(mar = c(0,0,0, 0), oma= c(0,0,0,0))
map("world", proj = "mollweide",  col = "grey77", fill = TRUE, lwd = .3, border = c(0, 0), wrap = FALSE, resolution = 0, xlim = c(-178, 178), plot = TRUE)
# ext
d_ply(er.df.m, "group", function(x) {
  with(x, polygon(x, y, border = "grey60", col = col.pal.ext.risk, lwd = 1.2))
})
# halp
d_ply(er.df.m, "group", function(x) {
  with(x, polygon(x, y, border = c(NA, "grey15")[halp.hot+1], lwd = 1.8))
})
# burr
d_ply(er.df.m, "group", function(x) {
  with(x, polygon(x, y, border = NA, col = c(NA, "grey20")[bur.hot+1], lwd = 1.6, density = c(NA, 18)[bur.hot+1], angle = 45))
})
# land
map("world", proj = "", mar = c(0, 0, 0, 0), col = "grey69", 
  fill = TRUE, lwd = .3, border = c(0, 0), wrap = FALSE, 
  resolution = 0, xlim = c(-178, 178), plot = TRUE, add = TRUE)
par(xpd = NA)
legend(120, -90, fill = c(cols[1],NA, "grey50"), col = c(cols[1], "grey20", "grey50"), legend = c("Ext", "Halpern", "Burrows"), bty = "n", density = c(NA, NA, 25), angle = c(NA, NA, 45), border = c(cols[1], "grey20", NA))
dev.off()
