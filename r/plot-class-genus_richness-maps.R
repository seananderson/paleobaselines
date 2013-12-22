# Created by:    Sean C. Anderson
# Created:       May 20, 2013
# Last modified: May 23, 2013
# Purpose:       Plot class-level extinction risk maps

# Set your working directory to the "r" folder
# Get the data by running
##rm(list = ls())
##source("Spalding.Map.FINAL.R")

load("~/Dropbox/nescent_extinction_map/Final data/by.prov.classes.rda")
by.prov.classes <- by.prov.classes

## SETUP:
#type <- "shift"  # shift or scale the colours?
type <- "scale"  # shift or scale the colours?
today <- format(Sys.time(), "%Y-%m-%d")
iterations <- 31
id <- "mean-log"
PDF <- FALSE
##

# Load libraries:
library(maps)
library(mapproj)
library(RColorBrewer)

# some polygons to patch the world map
source("patches.r")

# Pull in the eco province data:
er <- readShapePoly("../data/MEOW2/meow_ecos.shp")
er@data$id = rownames(er@data)
er.points = fortify(er, region = "id")
er.df <- join(er.points, er@data, by = "id")
er.df <- join(er.df, by.prov.classes, by = "PROV_CODE")


# is N.gen province specific?
# ddply(er.df, c("class", "PROVINCE"), summarize, n = length(order))
# gastro - 10
# turtles - > 2

# First scale by class for colour scale
# choose value to plot
er.df <- transform(er.df, log.N.gen = (mean_provs))

# figure out reasonable range:
p <- ggplot(er.df, aes(factor(class), log.N.gen)) + geom_boxplot() + coord_flip() + xlab("") + ylab("Mean log(extinction risk value)")
ggsave("class-risk-maps-boxplot-of-mean-log-ext-risk.pdf", width = 4.1, height = 4)
# ddply(er.df, "class", summarize, min = min(log.N.gen), max =
# max(log.N.gen), d = max - min, mean = mean(log.N.gen))

# Add a colour mapping for the risk:
col.pal <- brewer.pal(9, "YlOrRd")

if(type == "shift"){
  # new sliding version:
  # mean + .8, mean - .8 (on log scale)
  # outside boundaries, turned to max or min
  er.df <- ddply(er.df, "class", transform, mean.mean.ext =
    mean(log.N.gen))
  er.df <- ddply(er.df, "class", transform, lower.col.cut =
    mean.mean.ext - 0.8, upper.col.cut = mean.mean.ext + 0.8)
  # new sliding scale version:
  er.df <- ddply(er.df, "class", transform, ext.sliding = log.N.gen
    - mean.mean.ext)
  # crop the extremes:
  er.df$ext.sliding[er.df$ext.sliding <= -0.8] <- -0.7999
  er.df$ext.sliding[er.df$ext.sliding >= 0.8] <- 0.79999
  er.df$col.pal.ext.risk <- col.pal[findInterval(er.df$ext.sliding,
    seq(-0.8, 0.8, length.out = 10))]

} else {
  # original 0 to 1 version:

# first squash the tails to 2.5 and 97.5 quantiles:
  # er.df <- ddply(er.df, "class", transform, q.l = as.numeric(quantile(log.N.gen, 0.025, na.rm = TRUE)), q.u = as.numeric(quantile(log.N.gen, 0.975, na.rm = TRUE)))
  # er.df$log.N.gen.compressed <- NA
  # er.df$log.N.gen.compressed[er.df$log.N.gen < er.df$q.l] <-
    #er.df$q.l[er.df$log.N.gen < er.df$q.l]
  # er.df$log.N.gen.compressed[er.df$log.N.gen > er.df$q.u] <-
    #er.df$q.u[er.df$log.N.gen > er.df$q.u]
    
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  er.df <- ddply(er.df, "class", transform, log.N.gen.01 =
    range01(log.N.gen))
  er.df <- ddply(er.df, "class", transform, lower.col.cut = min(log.N.gen),
    upper.col.cut = max(log.N.gen))
  # old 0-1 version:
  er.df$col.pal.ext.risk <- col.pal[findInterval(er.df$log.N.gen.01,
    seq(-0.0001, 1.0001, length.out = 10))]
}

# Project the genus richness data:
er.df.mol <- mapproject(list(x = er.df$long, y = er.df$lat), proj =
  "mollweide")
er.df.m <- er.df
er.df.m$x <- er.df.mol$x
er.df.m$y <- er.df.mol$y

# Project the map:
land.fort.mol <- mapproject(list(x = land.fort$long, y =
    land.fort$lat), proj = "mollweide")
land.fort.m <- land.fort
land.fort.m$x <- land.fort.mol$x
land.fort.m$y <- land.fort.mol$y

# Create a box/oval to outline the map:
N <- 100
x.s <- seq(-180, 180, length.out = N)
y.s <- seq(-90, 90, length.out = N)
square <- data.frame(x = c(x.s, rep(x.s[N], N), rev(x.s), rep(x.s[1],
      N)), y = c(rep(y.s[1], N), y.s, rep(y.s[N], N), rev(y.s)))

oval <- mapproject(list(x = square$x, y = square$y), proj = "mollweide")

er.df.m <- ddply(er.df.m, "class", transform, med.mean.ext =
  -mean(log.N.gen, na.rm = TRUE))

# And make the maps:
filename <- paste("class-genus_richness-maps", today, "iter", iterations, id, type, sep = "-")
if(PDF) {
  pdf(paste0(filename, ".pdf"), width = 6, height = 6.1)
} else {
  png(paste0(filename, ".png"), width = 6, height = 6.1, units = "in", res = 250)
}

# layout:
mw <- 88
mg <- 1
kw <- 3
kg <- 5
N <- 8*2

lo <- matrix(ncol = mw*2 + kw*2 + mg*2 + kg*2, nrow = 4)
for(row.i in seq(1, 4)) {
  i <- (row.i - 1) * 4 + 1
  lo[row.i, ] <- 
  c(rep(i+0, mw), # map width
    rep(N+i+0, mg), # map gap
    rep(i+1, kw), # key width
    rep(N+i+1, kg), # key gap
    rep(i+2, mw), # map width
    rep(N+i+2, mg), # map gap
    rep(i+3, kw), # key width
    rep(N+i+3, kg)) # key gap
}
layout(lo)

par(mar = c(0,0,0, 0), oma= c(0,0,1,1)) 
par(cex = 0.5)
par(tck = -0.15)
par(mgp = c(3, 0.35, 0))
source("col-box-key.r")

d_ply(er.df.m, "med.mean.ext",
  function(class.dat) {

    # Set up a blank map:
    map("world", proj = "mollweide",  col = "grey69", fill = TRUE, lwd =
      0.9, myborder = c(0, 0), type = "n", wrap = FALSE, resolution = 0,
      xlim = c(-178, 178), plot = TRUE, border = "grey69", mar = c(0,
        0, 0, 0)) 

    #lines(oval, col = "grey60", lwd = 1.2)
    polygon(oval, col = "grey85", border = NA, lwd = 1.2)

    # Add the eco provinces:
    d_ply(class.dat, "group", function(class.group.dat) {
      with(class.group.dat, polygon(x, y, border = NA, col =
          col.pal.ext.risk, lwd = 1.5))
      })

    # Add the land on top:
    map("world", proj = "", mar = c(0, 0, 0, 0), col = "grey60", fill =
      TRUE, lwd = .55, myborder = c(0, 0), border = "grey60", wrap =
      FALSE, resolution = 0, xlim = c(-178, 178), plot = TRUE, add =
      TRUE)

    # patches:
    with(ant.patch, polygon(x, y, col = "grey60", border = FALSE))
    with(russia.patch, polygon(x-0.01, y, col = "white", border = FALSE))

    # border:
    lines(oval, col = "grey60", lwd = 1.2)

    # Label each panel:
    label <- unique(class.dat$class)
    if(label == "Malacostraca") label <- "Decapoda"
    mtext(label, line = -1.0, cex = 0.8, col = "grey30")
    usr <- par("usr")

    # Circles showing absolute ext. risk with area:
    #par(xpd = NA)
    #symbols(usr[2] * 0.75, usr[4] * 0.90, circles =
    #sqrt(exp(mean(class.dat$mean.ext, na.rm = TRUE))/pi) * 0.6, add =
    #TRUE, inches = FALSE, fg = "grey55", bg = "grey55")
    #par(xpd = FALSE)


    # Colour key:
    par(las = 1)
    col.regions <- with(class.dat, seq(unique(lower.col.cut),
        unique(upper.col.cut), length.out = 10))
    loc.limits <- with(class.dat, c(min(log.N.gen), max(log.N.gen)))
    add_locator <- ifelse(type == "shift", TRUE, FALSE)

    col_box_key(col.pal = col.pal, limits = c(min(er.df$log.N.gen),
        max(er.df$log.N.gen)), width = .3, col.regions =
      col.regions, N = 10, bg = "grey85", border.col = "grey60", at =
      log(c(0.05, 0.1, 0.2, 0.4)), at.labels = c(0.05, 0.1, 0.2, 0.4),
      #log(c(0.01, 0.02, 0.05, 0.1, 0.2)), at.labels = c(0.01,0.02,  0.05, 0.1, 0.2),
      add_locator = add_locator, loc_limits =
      loc.limits, loc_col = "black", loc_width = 2)

  })

# text printing at bottom
par(xpd = NA)
mtext(filename, side = 1, line = -2, outer = TRUE)

dev.off()

