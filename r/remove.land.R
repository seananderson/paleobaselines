# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jan 27, 2012
# Last modified: Jan 27, 2012
# Purpose:       find and remove cells with land
# ====================================================================

data(worldLL)
require(PBSmapping)
source("eq.area.grid.R")
library(plyr)
grid <- eq.area.grid()
plt <- c(0.05,0.95,0.05,0.95)
par(xpd = FALSE)
pdf("land-cell-map.pdf", width = 8, height = 4)
plotMap(worldLL, col="grey70", bg=NA, border = NA, tck=-0.015, mgp=c(2,.5,0),projection = "LL", cex=1.2,  axes = FALSE, xlab = "",plt = plt,  ylab = "", xlim = c(-20, 340))
abline(v = unique(grid$X), h = unique(grid$Y), lwd = 0.5)
par(xpd = NA)
d_ply(grid, "SID", function(x) {text( -35, min(unique(x$Y)) + diff(unique(x$Y))/2,unique(x$SID), pos = 4, cex = 0.6, col = "red")})
d_ply(grid, "PID", function(x) {text( min(unique(x$X)) + diff(unique(x$X))/2,95,unique(x$PID), pos = 1, cex = 0.6, col = "red")})
dev.off()
# now find cells with entirely land...

land.cells <- read.csv("land.cells.txt", comment.char = "#", header = F, col.names = c("PID", "SID"))
pdf("land-cell-map-marked-cells.pdf", width = 8, height = 4)
par(xpd = F)
plotMap(worldLL, col="grey70", bg=NA, border = NA, tck=-0.015, mgp=c(2,.5,0),projection = "LL", cex=1.2,  axes = FALSE, xlab = "",plt = plt,  ylab = "", xlim = c(-20, 340))
abline(v = unique(grid$X), h = unique(grid$Y), lwd = 0.5)
par(xpd = NA)
d_ply(grid, "SID", function(x) {text( -35, min(unique(x$Y)) + diff(unique(x$Y))/2,unique(x$SID), pos = 4, cex = 0.6, col = "grey50")})
d_ply(grid, "PID", function(x) {text( min(unique(x$X)) + diff(unique(x$X))/2,95,unique(x$PID), pos = 1, cex = 0.6, col = "grey50")})
for(i in 1:nrow(land.cells)) {
  current.cell <- subset(grid, PID == land.cells[i,"PID"] & SID == land.cells[i,"SID"])
  with(current.cell, rect(X[1], Y[1], X[3], Y[3], col = "#FF000050", border = F))
}
dev.off()


# check if removing Halpern 0s works:
plotMap(worldLL, col="grey70", bg=NA, border = NA, tck=-0.015, mgp=c(2,.5,0),projection = "LL", cex=1.2,  axes = FALSE, xlab = "",plt = plt,  ylab = "", xlim = c(-20, 340))
# out is from halpern.threat.index.R
with(subset(out, Z == 0), points(X, Y, col = "#FF000040", pch = 19))

hal.zeros <- subset(hal.out, Z_hlp == 0)

plotMap(worldLL, col="grey70", bg=NA, border = NA, tck=-0.015, mgp=c(2,.5,0),projection = "LL", cex=1.2,  axes = FALSE, xlab = "",plt = plt,  ylab = "", xlim = c(-20, 340))
abline(v = unique(grid$X), h = unique(grid$Y), lwd = 0.5)
par(xpd = NA)
d_ply(grid, "SID", function(x) {text( -35, min(unique(x$Y)) + diff(unique(x$Y))/2,unique(x$SID), pos = 4, cex = 0.6, col = "grey50")})
d_ply(grid, "PID", function(x) {text( min(unique(x$X)) + diff(unique(x$X))/2,95,unique(x$PID), pos = 1, cex = 0.6, col = "grey50")})
for(i in 1:nrow(land.cells)) {
  current.cell <- subset(grid, PID == hal.zeros[i,"PID"] & SID == hal.zeros[i,"SID"])
  with(current.cell, rect(X[1], Y[1], X[3], Y[3], col = "#FF000050", border = F))
}

#pdf("climap-core.pdf", width = 8, height = 4)
#plotMap(worldLL, col="grey70", bg=NA, border = NA, tck=-0.015, mgp=c(2,.5,0),projection = "LL", cex=1.2,  axes = FALSE, xlab = "",plt = plt,  ylab = "", xlim = c(-20, 340))
#q <- read.csv("~/Downloads/CLIMAPCoreLatLong.csv", header = F)
#q$V2[q$V2 < 0] <- q$V2[q$V2 < 0] + 360
#with(q, points(V2, V1, pch = 19, col = "#FF000080"))
#dev.off()

