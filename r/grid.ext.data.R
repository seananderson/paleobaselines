# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jan 26, 2012
# Last modified: Jun 01, 2012
# Purpose:       shape the extinction data
# ====================================================================

grid.ext.data <- function# Plot a global map with extinction risk binned by cell.
### Create a global map with extinction risk plotted in a grid. 
(data,
### A data.frame containing columns named "taxon", "longitude",
### "latitude", and "ext". These columns can be in any order and
### the data.frame can contain other columns (they are ignored).
combine.type = c("mean", "median", "variance", "min", "max", "number"),
### The type of plot to make. "number" plots the number of unique taxa per cell.
return.events = FALSE
){
if (sum(c("taxon", "longitude", "latitude", "ext") %in% names(data)) != 4)
stop("You need to have columns named taxon, longitude, latitude, and ext")

type <- match.arg(combine.type)

d2 <- data[,c("taxon", "longitude","latitude","ext")]
names(d2) <- c("taxon", "X", "Y", "ext")

d2$X[d2$X < 0] <- d2$X[d2$X < 0] + 360

d2[d2$X > 340, "X"] <- d2[d2$X > 340,"X" ] - 360
d2$EID <- 1:nrow(d2)
events <- d2
require(PBSmapping)
events <- as.EventData(events, projection = "LL")

source("eq.area.grid.R")
grid <- eq.area.grid()
# remove those outside of our equal area grid - can't go right to
# poles
lat <- c(-74.8168184, -55.6358741, -43.3017621, -33.1076902, -23.9906811, -15.4835508, -7.3156408, 0.7042382,  8.7380536,  16.9503945, 25.5418333,  34.8057747,  45.2693150,  58.2170000,  81.7595726)
events <- subset(events, Y < max(lat) & Y > min(lat))

# remove duplicate taxa:
browser()
events <- transform(events, X.int = sort(grid$X)[findInterval(X, sort(grid$X))], Y.int =  sort(grid$Y)[findInterval(Y, sort(grid$Y))])
events <- events[!duplicated(events[,c("taxon", "X.int", "Y.int")]), ]

#browser()
locData <- findPolys(events, grid, maxRows = 1e+07) 
events$Z <- events$ext
pdata <- switch(type, 
  mean =  combineEvents(events, locData, FUN=function(x) mean(x, na.rm = TRUE)),
  median = combineEvents(events, locData, FUN=function(x) median(x, na.rm = TRUE)) ,
  min = combineEvents(events, locData, FUN=function(x) min(x, na.rm = TRUE)) ,
  max = combineEvents(events, locData, FUN=function(x) max(x, na.rm = TRUE)) ,
  variance = combineEvents(events, locData, FUN=function(x) var(x, na.rm = TRUE)) ,
  number = combineEvents(events, locData, FUN=function(x) length(x)))
if(return.events) {
  # now adding in the PID, EID, and SID
  events <- merge(events, locData[,c("EID", "PID", "SID")])
  events
}
else pdata
}


