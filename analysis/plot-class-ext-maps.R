# Plot class-level extinction risk maps

# --------------------- SETUP:
id <- "mean-log"
PDF <- TRUE
# find order and then set for all plots:
# mrisk <- dplyr::summarise(group_by(by.prov.classes, class), mean = mean(mean.ext))
# dplyr::arrange(mrisk, -mean)
plot_order <- data.frame(class = c("Mammalia", "Elasmobranchii",
  "Anthozoa", "Gastropoda","Bivalvia", "Echinoidea"),
  plot_order = 1:6)
# y-axis labels:
plot_type <- "ext" # other options are "OBIS_records" and "N.gen"
#plot_type <- "OBIS_records"
#plot_type <- "N.gen"

# ----------------- end of setup
add_default_axis4 <- FALSE

# Load libraries:
library(maps)
library(mapproj)
library(RColorBrewer)

# some polygons to patch the world map
source("patches.r")

load("../data/by.prov.classes.rda")

# read in land map:
land <- readShapePoly("../data/110m-land/110m_land.shp")
land.fort <- fortify(land)

# Pull in the eco province data:
er <- readShapePoly("../data/MEOW2/meow_ecos.shp")
er@data$id = rownames(er@data)
er.points = ggplot2::fortify(er, region = "id")
er.df <- plyr::join(er.points, er@data, by = "id")
er.df <- plyr::join(er.df, by.prov.classes, by = "PROV_CODE")
er.df <- gdata::drop.levels(subset(er.df,er.df$N.gen >= 5)) # TODO NOTE this number

# is N.gen province specific?
# ddply(er.df, c("class", "PROVINCE"), summarize, n = length(order))
# gastro - 10
# turtles - > 2

# First scale the median extinction risk by class for colour scale
# purposes:
if(plot_type == "ext") {
  yrange.all <- c(0.002, 0.01, 0.05, 0.1, 0.2, 0.5, 1)
  er.df <- transform(er.df, value.to.plot = mean.ext)
}
if(plot_type == "OBIS_records") {
  er.df <- transform(er.df, value.to.plot = log(OBIS_records))
  yrange.all <- c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1) * 20000
}
if(plot_type == "N.gen") {
  er.df <- transform(er.df, value.to.plot = log(N.gen))
  yrange.all <- c(0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1) * 500
}

# Add a colour mapping for the risk:
col.pal <- brewer.pal(9, "YlOrRd")

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
er.df <- ddply(er.df, "class", transform, value.to.plot.01 =
    range01(value.to.plot))
er.df <- ddply(er.df, "class", transform, lower.col.cut = min(value.to.plot),
  upper.col.cut = max(value.to.plot))
# old 0-1 version:
er.df$col.pal.ext.risk <- col.pal[findInterval(er.df$value.to.plot.01,
  seq(-0.0001, 1.0001, length.out = 10))]

# Project the extinction-risk (er) data:
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
    -mean(mean.ext, na.rm = TRUE))

er.df.m <- plyr::join(er.df.m, plot_order)

# And make the maps:
filename <- paste("class-risk-maps", id, plot_type, sep = "-")
if(PDF) {
  pdf(paste0("../figs/", filename, ".pdf"), width = 6.25, height = 4.8)
} else {
  png(paste0("../figs/", filename, ".png"), width = 6.25, height = 4.8, units =
      "in", res = 250)
}

# layout:
mw <- 88 # map width
mg <- 1 # map gap
kw <- 3 # key width
kg <- 5 # key gap
nrow <- 3
N <- nrow*2*2

lo <- matrix(ncol = mw*2 + kw*2 + mg*2 + kg*2, nrow = nrow)
for(row.i in seq(1, nrow)) {
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

par(mar = c(0,0,0, 0), oma = c(0,0,1,3.5))
par(cex = 0.5)
par(tck = -0.15)
par(mgp = c(3, 0.35, 0))

ii <<- 0

d_ply(er.df.m, "plot_order",
  function(class.dat) {
    ii <<- ii + 1

    # Set up a blank map:
    map("world", proj = "mollweide",  col = "grey69", fill = TRUE, lwd =
        0.9, myborder = c(0, 0), type = "n", wrap = FALSE, resolution = 0,
      xlim = c(-178, 178), plot = TRUE, border = "grey69", mar = c(0,
        0, 0, 0))

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

    mtext(substitute(paste(phantom("g"), bold(let), " ", lab, phantom("g")),
      list(let = LETTERS[ii], lab = as.character(label))),
      line = -1.2, cex = 0.8, col = "grey30")

    #mtext(label, line = -1.0, cex = 0.8, col = "grey30")
    usr <- par("usr")

    # Colour key:
    par(las = 1)
    col.regions <- with(class.dat, seq(unique(lower.col.cut),
      unique(upper.col.cut), length.out = 10))
    loc.limits <- with(class.dat, c(min(value.to.plot), max(value.to.plot)))
    add_locator <- FALSE

    ll <- exp(min(er.df$value.to.plot))
    uu <- exp(max(er.df$value.to.plot))
    yrange <- yrange.all[yrange.all >= ll & yrange.all <= uu]

    col_box_key(col.pal = col.pal, limits = c(min(er.df$value.to.plot),
      max(er.df$value.to.plot)), width = .3, col.regions =
        col.regions, N = 10, bg = "grey85", border.col = "grey60", at =
        log(yrange), at.labels = yrange,
      add_locator = add_locator, loc_limits =
        loc.limits, loc_col = "black", loc_width = 2)
    if(add_default_axis4) axis(4)

  })

mtext("Intrinsic extinction probability", side = 4, outer = TRUE, line = 2.0, las = 0, cex = 0.8, col = "grey30")

dev.off()
