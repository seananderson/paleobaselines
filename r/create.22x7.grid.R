# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jun 01, 2012
# Last modified: Jun 01, 2012
# Purpose:       take the 45x14 global eq area grid and make it 22x7
# ====================================================================

lat <- global_45x14$latitude[seq(1, length(global_45x14$latitude), 2)] 
long <- global_45x14$longitude[seq(1, length(global_45x14$longitude), 2)]
this.grid <- makeGrid(x = long, y = lat, projection = "LL")
global_22x7 <- list(longitude = long, latitude = lat, grid = this.grid)

