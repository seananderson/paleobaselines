# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Sep 06, 2012
# Last modified: Sep 21, 2012
# Purpose:       Bound the modern occurrence data by a range box, fill
# in grid cells, and then find which Spalding regions are intersected
# by those boxes.
# ====================================================================

load("../data/data.finite.rda")
require(plyr)

# start by removing everything but min and max points
# these will be redundant

# important: sort first by longitude:
data.finite <- data.finite[order(data.finite$taxon, data.finite$longitude), ]

#"grid it here"
#d <- data.finite[1:29919,]
d <- data.finite

# a 1x1 cell grid:
#grid <- expand.grid(longitude = -180:180, latitude = -90:90)
#gr <- data.frame(longitude = -180:180, latidue = -90:90)
long <- seq(-180,180, 2)
lat <- seq(-90,90,2)

d <- transform(d, long.g = long[findInterval(longitude, long, rightmost.closed = FALSE)], lat.g =  lat[findInterval(latitude, lat, rightmost.closed = FALSE)])

# now remove duplicate grid cells:

d <- d[!duplicated(d[,c("MatchTaxon", "taxon", "class", "ext", "long.g", "lat.g")]),]

# what's the maximum gap to fill in longitude degrees?
X <- 90


# now fill in cells based on bounding boxes:
d2 <- ddply(d, "taxon", .fun = function(x) {
           n.row <- nrow(x)
           long.to.fill <- c()
# check it from -180 to 180
  if(sum(diff(x$long.g) > X) == 0) { # all gaps are smaller than X
    #print("fill from min to max longitude")
    long.to.fill <- seq(x$long.g[1], x$long.g[n.row]) # we can use indexing like this because we already sorted by longitude
  }
  else {
    #"there's a big gap"
    #print("fill all those with gaps less than X")
    for(i in 1:(nrow(x)-1)) {
      if(abs(x$long.g[i] - x$long.g[i+1]) <= X) {
        long.to.fill <- c(long.to.fill, seq(x$long.g[i], x$long.g[i+1]))
      }
    }
  }
# now check it around the wrap:
  wrapped.long.g <- x$long.g
  wrapped.long.g[wrapped.long.g < 0] <- wrapped.long.g[wrapped.long.g < 0] + 360
  wrapped.long.g <- sort(wrapped.long.g)
  long.to.fill.wrapped <- c()

  if(sum(diff(wrapped.long.g) > X) == 0) { # all wrapped gaps smaller than X
    #print("fill from min to max (wrapped)")
    long.to.fill.wrapped <- seq(wrapped.long.g[1], wrapped.long.g[n.row]) # we can use indexing like this because we already sorted by longitude
  }
  else {
    #"there's a big gap"
    #print("fill all those with gaps less than X (wrapped)")
    #"keeping track of the fact that we need to subtract 180 degrees"
    for(i in 1:(length(wrapped.long.g)-1)) {
      if(abs(wrapped.long.g[i] - wrapped.long.g[i+1]) <= X) {
        long.to.fill.wrapped <- c(long.to.fill.wrapped, seq(wrapped.long.g[i], wrapped.long.g[i+1]))
      }
    }
  }
# now unwrap those wrapped longitudes:
  long.to.fill.wrapped[long.to.fill.wrapped > 180] <- long.to.fill.wrapped[long.to.fill.wrapped > 180] - 360

# and take the union of the original longitudes and the filled
# longitudes:
  all.long.g <- union(long.to.fill.wrapped, long.to.fill)
  all.long.g <- union(all.long.g, x$long.g) # in case something was lost, need to check if this is necessary or redundant

# and fill all latitudes:
  all.lat.g <- seq(min(x$lat.g), max(x$lat.g))

# and build the output data.frame:
  expand.grid(MatchTaxon = x$MatchTaxon[1], taxon = x$taxon[1], class = x$class[1], ext = x$ext[1], longitude = all.long.g, latitude = all.lat.g)
})
           
