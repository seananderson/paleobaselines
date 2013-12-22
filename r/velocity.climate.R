# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jan 30, 2012
# Last modified: Feb 05, 2012
# Purpose:       try reading in velocity of climate change data and
# plotting it
# ====================================================================

load("~/Desktop/data.finite.rda")
out <- grid.ext.data(data.finite, combine.type = "mean", return.events = FALSE)

vc <- read.csv("~/Dropbox/NESCent-extinction/map/data/velocity-of-climate-change/Velocity.csv", stringsAsFactors = FALSE)

names(vc)[c(1, 2, 9)] <- c("X_COORD", "Y_COORD", "HumanImp")
#source("grid.ext.data.R")
source("grid.hal.data.R")
vc.grid <- grid.hal.data(vc)
names(vc.grid)[3] <- "Z"

#vc.grid$Z[vc.grid$Z > 0] <- log(vc.grid$Z[vc.grid$Z > 0])
#vc.grid$Z[vc.grid$Z < 0] <- -log(abs(vc.grid$Z[vc.grid$Z < 0]))

require(RColorBrewer)
p1 <- brewer.pal(9, "RdBu")
p1 <- smooth.pal(p1, n = 5) # smooth the palette
breaks <- c(-200, 0, 5, 10, 20, 50, 100, 200, 500, 1000)
p1 <- rev(brewer.pal(length(breaks), "RdBu"))
source("plot.ext.map.R")
pdf("velocity-climate-median.pdf", width = 5.75, height = 3)
plot.ext.map(vc.grid, log.col.scale = FALSE, pal = p1, breaks = breaks)
dev.off()



