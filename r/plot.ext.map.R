# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jan 26, 2012
# Last modified: Jan 30, 2012
# Purpose:       Plot a map of something binned by cell.
# ====================================================================

plot.ext.map <- function# Plot a global map with extinction risk binned by cell.
### Create a global map with extinction risk plotted in a grid. 
(data,
### The pdata with X, Y, Z, and EID columns. Can be created a function such as grid.ext.data().
add.halpern = FALSE,
### Combine Halpern risk with paleo risk? You will also need to
plot.only.halpern = FALSE,
### Plot the Halpern dataset on its own.
latent.risk = FALSE,
### Plot Halpern - intrinsic risk.
log.col.scale = TRUE,
### Whether the colour scale should be log transformed.
col.range = NULL,
### A numeric vector of two values if you want to specify the lower
### and upper limits of extinction probability for the colour palette:
### \code{c(lower, upper)}.
col.key = TRUE,
### \code{TRUE} or \code{FALSE} whether a colour key should be added.
col.key.cex = 0.7,
### The \code{cex} value for the text in the colour key.
land.col = "grey65",
### The colour of land on the map. Defaults to a dark grey.
plot.land = TRUE,
### Plot land? You might want to exclude it to uncover data issues.
pal = "red",
breaks = NULL
##details<< The grid is an equal-area grid.
){

  source("eq.area.grid.R")
  grid <- eq.area.grid()
  pdata <- data
require(PBSmapping)
require(RColorBrewer)

#p1 <- brewer.pal(9, "YlOrRd")
#p1 <- pal
#p1 <- smooth.pal(p1, n = 5) # smooth the palette

# can't have negative values for the colour scale
# need to adjust this later in the colour key
col.scale.shift <- 0 # a default value
#if(min(d2$ext) < 0) {
  #col.scale.shift <- abs(min(d2$ext, na.rm = TRUE))
  #d2$ext <- d2$ext + col.scale.shift
#}
if(is.null(col.range)) col.range <- range(pdata$Z, na.rm = TRUE)

if(is.null(col.range)) {
  lower.ext <- min(pdata$Z, na.rm = TRUE)
  upper.ext <- max(pdata$Z, na.rm = TRUE)
} else {
  lower.ext <- col.range[1]
  upper.ext <- col.range[2]
}

if(is.null(breaks)) {

###
lower.col.fudge <- max(pdata$Z, na.rm = TRUE) * 1e-5
if(log.col.scale)
  brks <- c(0, exp(seq(log(lower.ext + lower.col.fudge), log(upper.ext), length.out = 39)))
else
  brks <- seq(lower.ext, upper.ext, length.out = 39)

# if all ext are the same then create a fake colour range to avoid an
# error:
if(length(unique(brks)) == 1) brks <- seq(brks[1]*.5, brks[1] * 2, length.out = 39)
} else { # breaks were specified, so use them
  brks <- breaks 
}
#browser()

# any above the defined upper colour limit should be made the same
# colour as the upper limit:
pdata$Z[pdata$Z > upper.ext] <- upper.ext

lbrks <- length(brks) 
pdata <- makeProps(pdata, brks, "col", p1) 
#par(mar = c(1,1,1,1), oma = c(1,1,1,1))
#browser()
data(worldLL)
plt <- c(0.1,1,0,1)
plotMap(worldLL, col="grey", bg="white", border = "white", tck=-0.015, mgp=c(2,.5,0), cex=1.2, axes = FALSE, plt = plt, type = "n", xlab = "", ylab = "", projection = "LL", xlim = c(-20, 340)) 
addPolys(grid, polyProps=pdata, border = FALSE)
par(new = TRUE)
if(plot.land == TRUE){
plotMap(worldLL, col=land.col, bg=NA, border = NA, tck=-0.015, mgp=c(2,.5,0),projection = "LL", cex=1.2,  axes = FALSE, xlab = "",plt = plt,  ylab = "", xlim = c(-20, 340)) 
}


 if(col.key){
   col.range - col.scale.shift
   n <- length(p1)
   y.rects <- seq(par("usr")[3], par("usr")[4], length.out = n + 1)
   x.rects <- c(par("usr")[1] - 12, par("usr")[1] - 4)
   par(xpd = NA)
    for(i in 1:n){rect(x.rects[1], y.rects[i], x.rects[2], y.rects[i+1], border = NA, col = p1[i])}

    mid.y.rects <- y.rects[1:n] + diff(range(y.rects))/n/2
    
    segments(x.rects[1]-2, mid.y.rects[1], x.rects[1], mid.y.rects[1], col = "grey50")
    segments(x.rects[1]-2, mid.y.rects[n], x.rects[1], mid.y.rects[n], col = "grey50")

    text(x.rects[1]+3, mid.y.rects[1], labels = round(col.range[1], 1), col = "grey50", pos = 2, cex = col.key.cex)
    text(x.rects[1]+3, mid.y.rects[n], labels = round(col.range[2], 1), col = "grey50", pos = 2, cex = col.key.cex)
par(xpd = FALSE)
}
}

