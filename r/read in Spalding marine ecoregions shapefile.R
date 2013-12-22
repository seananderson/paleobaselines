
require(maptools)

ecoregions.mp <- readShapePoly("/Users/Seth/Dropbox/nescent_extinction_map/data/MEOW2/meow_ecos.shp")

res <- getinfo.shape("/Users/Seth/Dropbox/nescent_extinction_map/data/MEOW2/meow_ecos.shp")
str(res)
plot(ecoregions.mp, zcol=1,axes=TRUE, border="gray",las = 1,pbg="white")