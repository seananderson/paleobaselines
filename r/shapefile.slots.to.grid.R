shapefile.slots.to.grid <- function# Convert a shapefile with a polygon grid to a PBSmapping grid.
### Take a shapefile that contains a grid as a polygon and return the longitude and latitude coordinates as well as a grid object from and for use in PBSmapping.
(filename
### The shapefile. Leave off the .shp file extention.
) 
{
### get underlying polygon grid from slots of a shape file
	require(maptools)
	require(PBSmapping)
	x <- readShapePoly(filename, delete_null_obj = TRUE)
	out <- data.frame(x1 = rep(NA, length(slot(x, "polygons"))), 
		x2 = NA, y1 = NA, y2 = NA)
	for (i in 1:length(slot(x, "polygons"))) {
		poly.coord <- slot(slot(slot(x, "polygons")[[i]], 
			"Polygons")[[1]], "coords")
		range.x <- range(poly.coord[, 1])
		range.y <- range(poly.coord[, 2])
		out[i, 1] <- range.x[1]
		out[i, 2] <- range.x[2]
		out[i, 3] <- range.y[1]
		out[i, 4] <- range.y[2]
	}
	long <- sort(unique(c(out$x1, out$x2)))
	lat <- sort(unique(c(out$y1, out$y2)))
	grid <- makeGrid(x = long, y = lat, projection = "LL")
	list(longitude = long, latitude = lat, grid = grid)
### A list object with three elements: a vector of longitudes, a vector of latitudes, and a grid of class PolySet (a data frame) from and for use in PBSmapping.
}
