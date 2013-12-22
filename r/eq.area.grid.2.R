# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jan 26, 2012
# Last modified: Feb 07, 2012
# Purpose:       Generate an equal area grid. This version is from -180
# to 180 longitude.
# ====================================================================
eq.area.grid.2 <- function() {
  long <- seq(-180, 180, 8)
require(PBSmapping)
lat <- c(-74.8168184, -55.6358741, -43.3017621, -33.1076902, -23.9906811, -15.4835508, -7.3156408, 0.7042382,  8.7380536,  16.9503945, 25.5418333,  34.8057747,  45.2693150,  58.2170000,  81.7595726)
grid <- makeGrid(x = long, y = lat, projection = "LL")
grid
}



