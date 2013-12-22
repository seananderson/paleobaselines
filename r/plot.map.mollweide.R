plot.map.mollweide <- function# Plot a global map of marine extinction risk. 
### Plot a global map of marine extinction risk binned by cell using a Mollweide equal-area projection. 
(data,
### A data frame that comes from grid.ext.data.2().
### Will have the columns PID, SID, and Z.
grid.object,
### A list object with the elements: longitude, latitude, and grid.
### longitude and latitude should be vectors with the relevant
### intersection longitudes and latitudes. grid should be a data frame
### created by makeGrid() in the PBSmapping package using those
### longitude and latitude values. The resulting data frame for the
### grid element will have the columns: PID, SID, POS, X, and Y.
land.fill.col = "grey50",
### The shading colour of the land.
set.par.mar = TRUE,
### Shrink in the margins or leave the par settings alone?
poly.smoothness = 7,
# Affects how smooth the curves of the polygon cells are. Bigger
# numbers indicate more interpolation points to the cell borders and
# smoother lines. Anything above 5 seems hard to detect visually.
# Bigger numbers will result in more processing time.
col.scale = smooth.pal(brewer.pal(9, "YlOrRd"))
# The colour scale for the cells.
){

require(maps)
require(mapproj)
require(mapdata)
require(PBSmapping)
require(RColorBrewer)
require(data.table) # faster than plyr and easier to read than base

long <- grid.object$longitude
lat <- grid.object$latitude

this.grid <- grid.object$grid
this.grid <- transform(this.grid, PID_SID = paste(PID, SID, sep = "_"))
 
this.grid <- merge(this.grid, data, all = TRUE)
upper.ext.rate <- max(data$Z, na.rm = TRUE)
lower.ext.rate <- min(data$Z, na.rm = TRUE)
n.col.scale <- length(col.scale)
this.grid$col <- as.character(col.scale[findInterval(this.grid$Z, seq(lower.ext.rate,upper.ext.rate, length.out = n.col.scale))])
#browser()

# expand grid to polygon so the sides can be round on a different map
# projection:
n <- poly.smoothness 

create.poly.cell <- function(X, Y, col, n = poly.smoothness) {
# This function has been optimized for speed.
  cell <- data.frame(X = rep(999, n*4), Y = rep(999, n*4), col = rep("blank", n*4))
  min.Y <- min(Y)
  min.X <- min(X)
  max.X <- max(X)
  max.Y <- max(Y)
  x.seq <- seq(min.X, max.X, length.out = n)
  y.seq <- seq(min.Y, max.Y, length.out = n)
  cell[, 1] <- c(x.seq, rep(max.X, n), x.seq[n:1], rep(min.X, n))
  cell[, 2] <- c(rep(min.Y, n), y.seq, rep(max.Y, n), y.seq[n:1])
  cell[, 3] <- col
	cell
}
dtb <- data.table(this.grid, key="PID_SID")
out <- dtb[, create.poly.cell(X, Y, col), by=list(PID_SID)]


#browser()
#out$X <- out$X - 50
#browser()
co_poly <- mapproject(list(x = out$X, y = out$Y), proj = "mollweide")


# plot map:
if(set.par.mar) 
  par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0.2))

map2("world", proj = "mollweide",  col = land.fill.col, fill = TRUE, lwd = .3, border = c(0, 0), wrap = FALSE, resolution = 0, xlim = c(-178, 178), plot = TRUE)

for (i in seq(1, nrow(out), n * 4)) {
	polygon(co_poly$x[i:(i + n * 4 - 1)], co_poly$y[i:(i + 
    n * 4 - 1)], border = out$col[i], col = out$col[i], lwd = 0.1)
}
map2("world", proj = "", mar = c(0, 0, 0, 0), col = land.fill.col, 
  fill = TRUE, lwd = .3, border = c(0, 0), wrap = FALSE, 
  resolution = 0, xlim = c(-178, 178), plot = TRUE, add = TRUE)
}

# Testing the code:
# TODO or if this is in package form, then this won't be required:
#source("map2.R") # custom function - removes ugly country borders
# fake data check:

#fake.uniform.data <- data.frame(MatchTaxon = rep("ignore", 2e4), taxon = rep("ignore", 2e4), latitude = runif(2e4, -87, 87), longitude = runif(2e4, -180, 180), ext = rep(1, 2e4))
#fake.uniform.data <- data.frame(MatchTaxon = rep("ignore", 2e2), taxon = rep("ignore", 2e2), latitude = runif(2e2, -87, 87), longitude = runif(2e2, -180, 180), ext = rep(1, 2e2))

#russia.1 <- data.frame(MatchTaxon = "Ignore", taxon = "Ignore", latitude = 51.83577752045248, longitude = 140.537109375, class = "Ignore.class", ext = 0.5, use = 1)
#celtic.sea <- data.frame(MatchTaxon = "Ignore", taxon = "Ignore", latitude = 48.22467264956519, longitude = -4.74609375, class = "Ignore.class", ext = 0.5, use = 1)
#sa.tip <- data.frame(MatchTaxon = "Ignore", taxon = "Ignore", latitude = -51.971345808851716, longitude = -74.970703125, class = "Ignore.class", ext = 0.5, use = 1)
#recife.dat <- data.frame(MatchTaxon = "Ignore", taxon = "Ignore", latitude = -9.622414142924791, longitude = -35.68359375,  class = "Ignore.class", ext = 0.5, use = 1)
#halifax.dat <- data.frame(MatchTaxon = "Ignore", taxon = "Ignore", latitude = 44.6479, longitude = -63.5744, class = "Ignore.class", ext = 0.5, use = 1)
#hobart.dat <- data.frame(MatchTaxon = "Ignore", taxon = "Ignore", latitude = -42.8806, longitude = 147.3250, class = "Ignore.class", ext = 0.5, use = 1)
#cape.town.dat <- data.frame(MatchTaxon = "Ignore", taxon = "Ignore", latitude = -33.9767, longitude = 18.4244, class = "Ignore.class", ext = 0.5, use = 1)
#city.dat <- rbind(celtic.sea, russia.1, sa.tip, recife.dat, halifax.dat, hobart.dat, cape.town.dat)

#load("~/Dropbox/NESCent-extinction/map/data/equal-area-grid/global_90x28.rda")
#load("~/Dropbox/NESCent-extinction/map/data/equal-area-grid/global_45x14.rda")

#temp4.large <- grid.ext.data.2(fake.uniform.data, grid.object = global_45x14)
#plot.map.mollweide(temp4.large, grid.object = global_45x14)

#temp4 <- grid.ext.data.2(fake.uniform.data, grid.object = global_90x28)
#plot.map.mollweide(temp4, grid.object = global_90x28)

#temp.city <- grid.ext.data.2(city.dat, grid.object = global_90x28)
#plot.map.mollweide(temp.city, grid.object = global_90x28)
#temp.city <- grid.ext.data.2(city.dat, grid.object = global_45x14)
#plot.map.mollweide(temp.city, grid.object = global_45x14)

#load("~/Dropbox/NESCent-extinction/map/experiment-with-projections/data.finite.rda")

#temp.fine <- grid.ext.data.2(data.finite, grid.object = global_90x28)
#pdf("~/Desktop/mar-ext-moll-90x28.pdf", width = 3, height = 1.6)
#plot.map.mollweide(temp.fine, grid.object = global_90x28)
#dev.off()

#temp <- grid.ext.data.2(data.finite, grid.object = global_45x14)
#pdf("~/Desktop/mar-ext-moll-45x14.pdf", width = 3, height = 1.6)
#plot.map.mollweide(temp, grid.object = global_45x14)
#dev.off()

#temp <- grid.ext.data.2(fake.uniform.data, grid.object = global_45x14)
#plot.map.mollweide(temp, grid.object = global_45x14)

