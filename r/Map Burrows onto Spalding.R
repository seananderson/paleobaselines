
load("../data/Burrows.rda")

require(maptools)

er <- readShapePoly("../data/MEOW2/meow_ecos.shp")
#plot(er, zcol=1,axes=TRUE, border="gray",las = 1,pbg="white")

pts <- SpatialPoints(Burrows[,c("longitude", "latitude")])
pts.over <- over(pts, er)


# merge:
Burrows.Spalding <- na.omit(cbind(Burrows, pts.over))

save(Burrows.Spalding,file = "Burrows.Spalding.rda")