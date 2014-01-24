# Created by:    Sean C. Anderson
# Created:       Feb 28, 2013
# Last modified: Jan 23, 2014
# Purpose:       Global hotspot maps with halpern, burrows, and ext risk
#
# create the dataset to map:
load("../data/prov_SpatialPolygons.rda") # needed to draw joined provinces
load("../data/by.prov.all.rda")

library(maptools)
gpclibPermit()
er <- readShapePoly("../data/MEOW2/meow_ecos.shp")
er@data$id = rownames(er@data)
er.points = fortify(er, region = "id")
er.df <- join(er.points, er@data, by = "id")

er.df.all <- join(er.df, by.prov.all, by = "PROV_CODE")

## SETUP:
today <- format(Sys.time(), "%Y-%m-%d")
iterations <- 31
id <- "mean-log"
##

library(maps)
library(mapproj)
library(RColorBrewer)

# some polygons to patch the world map
# good grief; drew these with locator()
source("patches.r")

# not really used any more:
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  # hues = seq(25, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

# Create a box/oval to outline the map:
N <- 100
x.s <- seq(-180, 180, length.out = N)
y.s <- seq(-90, 90, length.out = N)
square <- data.frame(x = c(x.s, rep(x.s[N], N), rev(x.s), rep(x.s[1],
      N)), y = c(rep(y.s[1], N), y.s, rep(y.s[N], N), rev(y.s)))

oval <- mapproject(list(x = square$x, y = square$y), proj = "mollweide")

cols <- paste(gg_color_hue(3), "", sep = "")
cols[2] <- gg_color_hue(8)[7]

er.df.all$ext.plot <- log(er.df.all$mean.ext)

ext.plot.thresh <- as.numeric(quantile(er.df.all$ext.plot, probs = 0.8))
halp.thresh <- as.numeric(quantile(er.df.all$Halpern, probs = 0.8, na.rm = TRUE))
bur.thresh <- as.numeric(quantile(er.df.all$Burrows, probs = 0.8, na.rm = TRUE))
er.df <- transform(er.df.all, ext.hot = ifelse(ext.plot >= ext.plot.thresh, TRUE, FALSE), halp.hot = ifelse(Halpern >= halp.thresh, TRUE, FALSE), bur.hot = ifelse(Burrows >= bur.thresh, TRUE, FALSE))

### remove provinces with fewer than a specified number of genera:
er.df <- drop.levels(subset(er.df,er.df$N.gen >=Min.Prov.Genera))

er.df <- transform(er.df, base.col = ifelse(bur.hot == FALSE & halp.hot == FALSE & ext.hot == FALSE, "grey85", NA))
er.df$col <- NA
er.df[er.df$ext.hot == TRUE, "col"] <- cols[1]
er.df[er.df$halp.hot == TRUE & !is.na(er.df$halp.hot), "col"] <- cols[3]
er.df[er.df$halp.hot == TRUE & !is.na(er.df$halp.hot) & er.df$ext.hot == TRUE, "col"] <- cols[2]

#col.pal <- paste(brewer.pal(9, "YlOrRd"), 90, sep = "")
col.pal <- brewer.pal(9, "YlOrRd")
er.df$col.pal.ext.plot <- col.pal[findInterval(er.df$ext.plot, seq(min(er.df$ext.plot)-0.00001, max(er.df$ext.plot) + 0.00001, length.out = 10))]
er.df.mol <- mapproject(list(x = er.df$long, y = er.df$lat))
er.df.m <- er.df
er.df.m$x <- er.df.mol$x
er.df.m$y <- er.df.mol$y

land.fort.mol <- mapproject(list(x = land.fort$long, y = land.fort$lat))
land.fort.m <- land.fort
land.fort.m$x <- land.fort.mol$x
land.fort.m$y <- land.fort.mol$y

cols <- paste(gg_color_hue(3), "", sep = "")

### and the plotting:
filename <- paste("hotspots", today, "iter", iterations, id, sep = "-")
pdf(paste0("../figs/", filename, ".pdf"), width = 7, height = 4)
par(mar = c(0,0,0, 0), oma= c(0,0,0,0))
map("world", proj = "mollweide",  col = "grey69", fill = TRUE, lwd = 0.9, myborder = c(0, 0), wrap = FALSE, resolution = 0, xlim = c(-178, 178), plot = TRUE, border = "grey69", type = "n")

# grey background:
polygon(oval, col = "grey92", border = NA, lwd = 1.2)

# ext
d_ply(er.df.m, "group", function(x) {
  with(x, polygon(x, y, border = col.pal.ext.plot, col = col.pal.ext.plot, lwd = .8))
})

# halp
#d_ply(er.df.m, "group", function(x) {
#with(x, polygon(x, y, border = c(NA, "grey20")[halp.hot+1], lwd = 1.5))
#})

# Plot province borders only; Halpern
d_ply(er.df.m, "PROV_CODE", function(x) {
  PROV <- unique(x$PROV_CODE)
  prov.dat <- fortify(prov_SpatialPolygons[[PROV]])
  prov.dat.m <- mapproject(list(x = prov.dat$long, y = prov.dat$lat))
  prov.dat$x <- prov.dat.m$x
  prov.dat$y <- prov.dat.m$y
  halp.hot <- unique(subset(er.df, PROV_CODE == PROV)$halp.hot)
  if(length(halp.hot) > 1) warnings("More than one Halpern value per province")

  d_ply(prov.dat, "group", function(z) {
    with(z, polygon(x, y, border = c(NA, "grey20")[halp.hot+1], lwd = 2.2))
})

})

# burr
d_ply(er.df.m, "group", function(x) {
  with(x, polygon(x, y, border = NA, col = c(NA, "grey20")[bur.hot+1],
      lwd = 1.5, density = c(NA, 23)[bur.hot+1], angle = 45))
})

# land
#d_ply(land.fort.m, "group", function(x) {
#with(x, polygon(x, y, border = NA, lwd = 0.5, col = "grey77"))
#})
map("world", proj = "", mar = c(0, 0, 0, 0), col = "grey64", fill =
  TRUE, lwd = .95, myborder = c(0, 0), border = "grey64", wrap =
  FALSE, resolution = 0, xlim = c(-178, 178), plot = TRUE, add = TRUE)

# patches:
with(ant.patch, polygon(x, y, col = "grey64", border = FALSE))
with(russia.patch, polygon(x, y, col = "white", border = FALSE))

# oval:
lines(oval, col = "grey64", lwd = 2.8)

# legend
par(xpd = NA)
legend(1.4, -.75, fill = c(col.pal[6],NA, "grey50"), col = c(col.pal[6], "black", "black"), legend = c("Paleo", "Halpern", "Burrows"), bty = "n", density = c(NA, NA, 25), angle = c(NA, NA, 45), border = c(NA, "black", NA), box.lwd = c(0, 3, 1))

# text printing at bottom
#text(-1,-1.12,filename, pos = 4)

dev.off()

