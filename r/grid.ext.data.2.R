# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jan 26, 2012
# Last modified: Jun 05, 2012
# Purpose:       shape the extinction data. goes from -180 to 180
# ====================================================================

grid.ext.data.2 <- function# Plot a global map with extinction risk binned by cell.
### Create a global map with extinction risk plotted in a grid. 
(data,
### A data.frame containing columns named "taxon", "class", "longitude",
### "latitude", and "ext". These columns can be in any order and
### the data.frame can contain other columns (they are ignored).
combine.type = c("mean", "median", "variance", "min", "max", "number"),
### The type of plot to make. "number" plots the number of unique taxa per cell.
grid.object,
### A list object with the elements: longitude, latitude, and grid.
### longitude and latitude should be vectors with the relevant
### intersection longitudes and latitudes. grid should be a data frame
### created by makeGrid() in the PBSmapping package using those
### longitude and latitude values. The resulting data frame for the
### grid element will have the columns: PID, SID, POS, X, and Y.
shift_amount = -8,
### Shift longitudes so the land map lines up with the coordinates?
### Need to figure out exactly what's going on here, but about 8 seems
### to work via trial and error.
return.events = FALSE
### Return events data from PBSmapping? Can be useful for further data
### analysis.
){
if (sum(c("taxon", "longitude", "latitude", "ext", "class") %in% names(data)) != 5)
stop("You need to have columns named taxon, class, longitude, latitude, and ext")

type <- match.arg(combine.type)

d2 <- data[,c("class", "taxon", "longitude","latitude","ext")]

#browser()
## cells need to be shifted to the left to match the land map
## checked by known landmarks
shift_longitude <- function(input_long, shift_amount = shift_amount, left_cutoff = -180) {
  output_long <- input_long + shift_amount
  output_long[output_long < left_cutoff] <- output_long[output_long < left_cutoff] + 360
  output_long
}

#browser()
d2$longitude <- shift_longitude(d2$longitude, shift_amount = shift_amount)
names(d2) <- c("class", "taxon", "X", "Y", "ext")

d2$EID <- 1:nrow(d2)
events <- d2
require(PBSmapping)
events <- as.EventData(events, projection = "LL")

grid <- grid.object$grid

# remove those outside of our equal area grid - can't go right to poles
lat <- grid.object$latitude
long <- grid.object$longitude

# shouldn't actually do anything, but there's anything outside the
# borders, the findInterval bit below will crash
# could cut data if we're not covering all latitude or longitude with
# the grid, for example
events <- subset(events, Y < max(lat) & Y > min(lat))
events <- subset(events, X < max(long) & X > min(long))

# remove duplicate taxa:
events <- transform(events, X.int = sort(grid$X)[findInterval(X, sort(grid$X), rightmost.closed = FALSE)], Y.int =  sort(grid$Y)[findInterval(Y, sort(grid$Y), rightmost.closed = FALSE)])

events <- events[!duplicated(events[,c("class", "taxon", "X.int", "Y.int")]), ]

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



