# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jul 02, 2012
# Last modified: Jul 03, 2012
# Purpose:       try ggplot plotting and fortify df
# Modified by Seth Finnegan Aug. 16 2012
# ====================================================================
library(maptools)
library(gdata)
library(ggplot2)
gpclibPermit()
data <- load("/Users/Seth/Dropbox/nescent_extinction_map/data/interpolated_provinces.rda")
d.eco.filled <- extEST
land <- readShapePoly("/Users/Seth/Dropbox/nescent_extinction_map/data/110m-land/110m_land.shp")
land.fort <- fortify(land)



extEST <- read.csv("/Users/Seth/Dropbox/nescent_extinction_map/data/Risk estimates by stage.csv",header = TRUE)
extEST <- drop.levels(subset(extEST,extEST$Use==1))


extNEW <- extEST$MeanRisk
New.Ex.Est <- data.frame(extNEW,genus)
colnames(New.Ex.Est) <- c("new.ext","genus")
d.eco.filled <- drop.levels(unique(merge(d.eco.filled,New.Ex.Est,by = "genus",all.x = FALSE)))
d.eco.filled <- drop.levels(unique(data.frame(d.eco.filled$class,d.eco.filled$group,d.eco.filled$genus,d.eco.filled$new.ext, d.eco.filled$PROV_CODE,d.eco.filled$RLM_CODE)))
colnames(d.eco.filled) <- c("class","MatchTaxon","genus","new.ext","PROV_CODE","RLM_CODE")
require(maptools)
require(ggplot2)
require(plyr)

er <- readShapePoly("/Users/Seth/Dropbox/nescent_extinction_map/data/MEOW2/meow_ecos.shp")
er@data$id = rownames(er@data)
er.points = fortify(er, region = "id")
er.df <- join(er.points, er@data, by = "id")

### select "RLM_CODE" to plot realms,"PROV_CODE" to plot provinces
mean.ext.by.prov <- ddply(drop.levels(subset(d.eco.filled, d.eco.filled$class == "Bivalvia")), "PROV_CODE", summarize, mean.prov.ext = mean(new.ext))
N.by.prov <- ddply(drop.levels(subset(d.eco.filled, d.eco.filled$class == "Bivalvia")), "PROV_CODE", summarize, N.prov.ext = length(new.ext))
quartz("map",13,7)
er.df$mean.prov.ext <- NULL
mean.ext.by.prov <- merge(mean.ext.by.prov,N.by.prov,by = "PROV_CODE")
er.df <- join(er.df, mean.ext.by.prov, by = "PROV_CODE")
er.df <- drop.levels(subset(er.df,er.df$N.prov.ext > 10))


ggplot(er.df) + aes(long, lat, group=group,fill=mean.prov.ext) + geom_polygon() + scale_fill_gradient(low = "yellow", high = "red",name = "Mean extinction risk") + geom_polygon(data = map_data("world"), aes(long, lat, group = group), fill = "black") + opts(panel.background = theme_rect(colour="darkgray", size =1, fill = "white"))  + opts(panel.grid.major = theme_blank()) + opts(panel.grid.minor = theme_blank()) + ylab(expression("Latitude")) + xlab(expression("Longitude")) + coord_equal(ylim = c(-85,90),xlim = c(-180,180))
