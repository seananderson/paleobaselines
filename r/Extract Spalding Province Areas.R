er <- readShapePoly("~/Dropbox/nescent_extinction_map/data/MEOW2/meow_ecos.shp")
er@data$id = rownames(er@data)
data <- slot(er, "data")


ecoregion_areas <- read.csv("~/Dropbox/nescent_extinction_map/data/Spalding_region_areas.csv",header = TRUE,stringsAsFactors = FALSE)

areas <- merge(data,ecoregion_areas,by="id")

prov.areas <- function(df) sum(df$area)

Prov.Areas <- ddply(areas,.(PROV_CODE),prov.areas)
colnames(Prov.Areas) <- c("PROV_CODE","Area")
write.table(Prov.Areas,"Spalding province areas.csv",sep=",")