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
### run garbage collection to free up memory before plotting
gc()
data <- load("/Users/Seth/Dropbox/nescent_extinction_map/data/interpolated_provinces_hybrid.rda")
d.eco.filled <- d.eco.final
land <- readShapePoly("/Users/Seth/Dropbox/nescent_extinction_map/data/110m-land/110m_land.shp")
land.fort <- fortify(land)



extEST <- read.csv("/Users/Seth/Dropbox/nescent_extinction_map/data/Risk estimates by stage.csv",header = TRUE)
genus <- extEST$genus
extNEW <- extEST$MeanRisk
New.Ex.Est <- data.frame(extNEW,genus)
colnames(New.Ex.Est) <- c("new.ext","genus")
d.eco.filled <- drop.levels(unique(merge(d.eco.filled,New.Ex.Est,by = "genus",all.x = FALSE)))
d.eco.filled <- drop.levels(unique(data.frame(d.eco.filled$class,d.eco.filled$group,d.eco.filled$genus,d.eco.filled$new.ext, d.eco.filled$PROV_CODE,d.eco.filled$RLM_CODE)))
d.eco.filled <- na.omit(d.eco.filled)
colnames(d.eco.filled) <- c("class","MatchTaxon","genus","new.ext","PROV_CODE","RLM_CODE")
require(maptools)
require(plyr)
N.Prov <- function(df)length(df$new.ext)
by.Gen <- ddply(d.eco.filled,.(genus),N.Prov)
d.eco.filled <- merge(d.eco.filled,by.Gen,by="genus")




er <- readShapePoly("/Users/Seth/Dropbox/nescent_extinction_map/data/MEOW2/meow_ecos.shp")
er@data$id = rownames(er@data)
er.points = fortify(er, region = "id")
er.df <- join(er.points, er@data, by = "id")

### select "RLM_CODE" to plot realms,"PROV_CODE" to plot provinces

mean.ext <- function(df)mean(df$new.ext)
N.gen <- function(df)length(df$new.ext)
Gen.range <- function(df)mean(df$V1)
by.prov.classes <- ddply(d.eco.filled,.(class,PROV_CODE),each(mean.ext,N.gen,Gen.range))
by.prov.all <- ddply(d.eco.filled,.(PROV_CODE),each(mean.ext,N.gen,Gen.range))
### Use by.prov.all for composite, by.prov.classes for classes
by.prov <- by.prov.all


quartz("map",13,7)
er.df$Gen.range <- NULL
er.df <- join(er.df, by.prov, by = "PROV_CODE")
er.df <- drop.levels(subset(er.df,er.df$N.gen > 1))
###er.df <- drop.levels(subset(er.df,er.df$class == "Anthozoa"))

ggplot(er.df) + aes(long, lat, group=group,fill=Gen.range) + geom_polygon() + scale_fill_gradient(high="red",low = "yellow",name = "Mean Gen.Prov.") + geom_polygon(data = map_data("world"), aes(long, lat, group = group), fill = "black") + theme(panel.background = element_rect(colour="darkgray", size =1, fill = "white"))  + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) + ylab(expression("Latitude")) + xlab(expression("Longitude")) + coord_equal(ylim = c(-85,90),xlim = c(-180,180)) 

####+ facet_wrap(~ class, ncol=3) + opts(strip.background = theme_rect(col=NA,fill=NA))

####+ scale_fill_gradient2(high="red",mid = "orange",low= "yellow",midpoint = 0, name = "Mean extinction risk")
####+ scale_fill_gradient2(low="blue",high="red",mid = "white",midpoint = 0,name = "Difference")