# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Sep 06, 2012
# Last modified: Sep 07, 2012
# Purpose:       Bound the modern occurrence data by a range box, fill
# in grid cells.
# This version uses data.table to be much faster.
# ====================================================================

load("../data/data.finite.rda")

# important: sort first by longitude:
data.finite <- data.finite[order(data.finite$taxon, data.finite$longitude), ]

#"grid it here"
d <- data.finite
# a 2x2 cell grid:
long <- seq(-180,180, 2)
lat <- seq(-90,90,2)

d <- transform(d, long.g = long[findInterval(longitude, long, rightmost.closed = FALSE)], lat.g =  lat[findInterval(latitude, lat, rightmost.closed = FALSE)])

# now remove duplicate grid cells:
d <- d[!duplicated(d[,c("MatchTaxon", "taxon", "class", "ext", "long.g", "lat.g")]),]

# now fill in cells based on bounding boxes:
fill.bounding.boxes <- function(long.g, lat.g, X = 90, MatchTaxon = NA, this.class = NA, ext = NA) {
           n.row <- length(long.g)
           long.to.fill <- c()
# check it from -180 to 180
  if(sum(diff(long.g) > X) == 0) { # all gaps are smaller than X
    #print("fill from min to max longitude")
    long.to.fill <- seq(long.g[1], long.g[n.row]) # we can use indexing like this because we already sorted by longitude
  }
  else {
    #"there's a big gap"
    #print("fill all those with gaps less than X")
    for(i in 1:(n.row-1)) {
      if(abs(long.g[i] - long.g[i+1]) <= X) {
        long.to.fill <- c(long.to.fill, seq(long.g[i], long.g[i+1]))
      }
    }
  }
# now check it around the wrap:
  wrapped.long.g <- long.g
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
  all.long.g <- union(all.long.g, long.g) # in case something was lost, need to check if this is necessary or redundant

# and fill all latitudes:
  all.lat.g <- seq(min(lat.g), max(lat.g))

# and build the output data.frame:
  out.df <- expand.grid(longitude = all.long.g, latitude = all.lat.g)
  out.df$class = this.class
  out.df$MatchTaxon = MatchTaxon
  out.df$ext = ext
  out.df
}
           
require(data.table) # super fast split apply combine
dtb <- data.table(d)
out <- dtb[, fill.bounding.boxes(long.g = long.g, lat.g = lat.g, MatchTaxon = MatchTaxon[1], this.class = class[1], ext = ext[1]), by=list(taxon)]

data.finite.filled <- as.data.frame(out)

save(data.finite.filled, file = "../data/data.finite.filled.rda")

