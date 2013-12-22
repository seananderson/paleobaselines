map.ext <- structure(function# Plot a global map with extinction risk binned by cell.
### Create a global map with extinction risk plotted in a grid. 
(data,
### A data.frame containing columns named "taxon", "longitude",
### "latitude", and "ext". These columns can be in any order and
### the data.frame can contain other columns (they are ignored).
plot.type = c("mean", "median", "variance", "min", "max", "number"),
### The type of plot to make. "number" plots the number of unique taxa per cell.
add.halpern = FALSE,
### Combine Halpern risk with paleo risk? You will also need to
### specify the Halpern et al. data.
halpern.data = NULL,
### The Halpern et al. data. Should have columns of X_COORD, Y_COORD,
### and HumanImp. The X_COORD should range from -180 to 180 degrees.
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
plot.land = TRUE
### Plot land? You might want to exclude it to uncover data issues.
##details<< The grid is an equal-area grid.
){
if (sum(c("taxon", "longitude", "latitude", "ext") %in% names(data)) != 4)
stop("You need to have columns named taxon, longitude, latitude, and ext")

require(PBSmapping)
require(RColorBrewer)

type <- match.arg(plot.type)

p1 <- brewer.pal(9, "YlOrRd")
p1 <- smooth.pal(p1, n = 5) # smooth the palette

d2 <- data[,c("taxon", "longitude","latitude","ext")]
names(d2) <- c("taxon", "X", "Y", "ext")

#browser()
#d2[, "X"] <- d2[, "X"] + 360
d2$X[d2$X < 0] <- d2$X[d2$X < 0] + 360

d2[d2$X > 340, "X"] <- d2[d2$X > 340,"X" ] - 360
d2$EID <- 1:nrow(d2)
events <- d2
events <- as.EventData(events, projection = "LL")
long <- seq(-20, 340, 8)
lat <- c(-74.8168184, -55.6358741, -43.3017621, -33.1076902, -23.9906811, -15.4835508, -7.3156408, 0.7042382,  8.7380536,  16.9503945, 25.5418333,  34.8057747,  45.2693150,  58.2170000,  81.7595726)
grid <- makeGrid(x = long, y = lat, projection = "LL")

# remove those outside of our equal area grid - can't go right to
# poles
#browser()
events <- subset(events, Y < max(lat) & Y > min(lat))

# remove duplicate taxa:
events <- transform(events, X.int = sort(grid$X)[findInterval(X, sort(grid$X))], Y.int =  sort(grid$Y)[findInterval(Y, sort(grid$Y))])
events <- events[!duplicated(events[,c("taxon", "X.int", "Y.int")]), ]

locData <- findPolys(events, grid, maxRows = 1e+07) 
events$Z <- events$ext
pdata <- switch(type, 
  mean =  combineEvents(events, locData, FUN=function(x) mean(x, na.rm = TRUE)),
  median = combineEvents(events, locData, FUN=function(x) median(x, na.rm = TRUE)) ,
  min = combineEvents(events, locData, FUN=function(x) min(x, na.rm = TRUE)) ,
  max = combineEvents(events, locData, FUN=function(x) max(x, na.rm = TRUE)) ,
  variance = combineEvents(events, locData, FUN=function(x) var(x, na.rm = TRUE)) ,
  number = combineEvents(events, locData, FUN=function(x) length(x)))


### bring in Halpern data:
if(add.halpern | plot.only.halpern) {
hlp <- halpern.data
hlp <- hlp[,c("X_COORD", "Y_COORD", "HumanImp")]
hlp$X_COORD <- hlp$X_COORD + 180
hlp$X_COORD <- hlp$X_COORD + 180
hlp$X_COORD[hlp$X_COORD > 360] <- hlp$X_COORD[hlp$X_COORD > 360] - 360
hlp$X_COORD[hlp$X_COORD > 340] <- hlp$X_COORD[hlp$X_COORD > 340] - 360
names(hlp) <- c("X", "Y", "Z")
hlp$EID <- 1:nrow(hlp)
hlp <- as.EventData(hlp, projection = "LL") 
locData.hlp <- findPolys(hlp, grid, maxRows = 1e+07) 
pdata.hlp <- combineEvents(hlp, locData.hlp, FUN=function(x) mean(x, na.rm = TRUE))
names(pdata.hlp)[3] <- "Z_hlp"
pdata2 <- merge(pdata, pdata.hlp, all = TRUE)
#browser()


if(plot.only.halpern)
  pdata2 <- transform(pdata2, Z_combined = Z_hlp)
if(latent.risk) {
  pdata2$Z <- pdata2$Z / max(pdata2$Z)
  pdata2$Z_hlp <- pdata2$Z_hlp / max(pdata2$Z_hlp)
  pdata2 <- transform(pdata2, Z_combined = Z_hlp - Z)
  }
else{
  ## try scaling both risks to a max of 1 to put them on the same scale:
  #pdata2$Z_hlp <- pdata2$Z_hlp/max(pdata2$Z_hlp)
  #pdata2$Z <- pdata2$Z/max(pdata2$Z)
  pdata2 <- transform(pdata2, Z_combined = Z * Z_hlp)
}

pdata2 <- pdata2[,c("PID", "SID", "Z_combined")]
names(pdata2)[3] <- "Z"
pdata <- pdata2
}

### histogram line:
#plot(ddply(pdata, "SID", summarize, mean(Z))[14:1,c(2,1)], type = "l")

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

###
lower.col.fudge <- max(pdata$Z, na.rm = TRUE) * 1e-5
if(log.col.scale)
  brks <- c(0, exp(seq(log(lower.ext + lower.col.fudge), log(upper.ext), length.out = 39)))
else
  brks <- seq(lower.ext, upper.ext, length.out = 39)

# if all ext are the same then create a fake colour range to avoid an
# error:
if(length(unique(brks)) == 1) brks <- seq(brks[1]*.5, brks[1] * 2, length.out = 39)

# any above the defined upper colour limit should be made the same
# colour as the upper limit:
pdata$Z[pdata$Z > upper.ext] <- upper.ext

lbrks <- length(brks) 
pdata <- makeProps(pdata, brks, "col", p1) 
#par(mar = c(1,1,1,1), oma = c(1,1,1,1))
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
,ex=function(){
dat <-
structure(list(taxon = c("Thyasira", "Caryocorbula", "Corbula", 
"Cardita", "Teredo", "Mimachlamys", "Venericardia", "Zenatia", 
"Hiatella", "Placopecten", "Thyasira", "Cyclochlamys", "Nucula", 
"Aulacomya", "Calyptogena", "Astarte", "Parvicardium", "Astarte", 
"Parvicardium", "Heteranomia", "Nuculana", "Timoclea", "Arcuatula", 
"Saxidomus", "Conchocele", "Dacrydium", "Mytilus", "Mytilus", 
"Meiocardia", "Laevichlamys", "Crassostrea", "Rangia", "Arctica", 
"Kurtiella", "Scintilla", "Limaria", "Phaxas", "Chlamys", "Kurtiella", 
"Tellina"), longitude = c(0.933132, -86.5596, 4.10188, 151.555, 
-74.17, 151.215, 173.485, 174.117, -1.07343, -68.3321, -5.18197, 
150.25, -5.32628, 17.9821, 139.225, 10.2689, -73.833, -0.5, -73.25, 
-5.2364, -79.017, -5.34569, 39.3962, -131.33, 144.082, 10.7456, 
-3.553, -5.1043, 128.133, 113.433, -4.53333, -88.9385, -68, 5.50056, 
98.4074, -86.8768, -5.18197, 178.417, 3.03778, 0.322747), latitude = c(60.7409, 
30.4191, 51.6887, -21.405, 39.81, -20.985, -42.7717, -40.9833, 
54.5717, 41.3524, 51.6556, -36.25, 51.6516, -33.1429, 35.0015, 
54.3586, 40.367, 55.25, 39.017, 58.2177, 32.167, 52.5719, -4.66149, 
53.52, 54.446, 67.5659, 50.5194, 56.693, -3.25, -24.5166, 48.3, 
23.2548, 40.5, 54.2686, 7.80319, 29.9289, 51.6556, -18.133, 56.15, 
50.3618), ext = c(0.128367707, 0.114078495, 0.114078495, 0.123402468, 
0.123843769, 0.160904827, 0.123402468, 0.146479058, 0.090395346, 
0.160904827, 0.128367707, 0.139563446, 0.117593928, 0.14322149, 
0.279329609, 0.1762707, 0.145665148, 0.1762707, 0.145665148, 
0.135330263, 0.130702145, 0.150861858, 0.14322149, 0.150861858, 
0.128367707, 0.14322149, 0.14322149, 0.14322149, 0.186014067, 
0.160904827, 0.118857316, 0.146479058, 0.138985407, 0.232897154, 
0.335245474, 0.17032078, 0.158690366, 0.160904827, 0.232897154, 
0.153990146)), .Names = c("taxon", "longitude", "latitude", "ext"
), row.names = c(NA, -40L), class = "data.frame")
map.ext(dat)
}
)
