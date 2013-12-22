# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jul 02, 2012
# Last modified: Oct 10, 2012
# Purpose:       try ggplot plotting and fortify df
# Modified by Seth Finnegan Aug. 16 2012
# ====================================================================
library(maptools)
library(gdata)
library(ggplot2)
gpclibPermit()


###choose province occurrence dataset."interpolated_provs_alt_all_taxa.rda" has central and western indo-pacific merged
data <- load("~/Dropbox/nescent_extinction_map/Final data/interpolated_provs_alt_all_taxa.rda")
d.eco.filled <- interpolated_provs_alt_all_taxa
land <- readShapePoly("~/Dropbox/nescent_extinction_map/data/110m-land/110m_land.shp")
land.fort <- fortify(land)


#### read in risk estimates; choose whether to use merged ("merged_indopacific") or unmerged ("unaltered") range estimates
extEST <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Risk estimates for plotting FINAL.csv",header = TRUE)

genus <- extEST$genus
extNEW <- extEST$Neogene_gbm
group <- extEST$group
class <- extEST$class
New.Ex.Est <- na.omit(data.frame(extNEW,genus,group,class))
colnames(New.Ex.Est) <- c("new.ext","genus","group","class")
d.eco.filled <- drop.levels(unique(merge(d.eco.filled,New.Ex.Est,by = "genus",all.x = FALSE)))
##d.eco.filled <- drop.levels(unique(data.frame(d.eco.filled$class,d.eco.filled$group,d.eco.filled$genus,d.eco.filled$new.ext, d.eco.filled$PROV_CODE,d.eco.filled$RLM_CODE)))
colnames(d.eco.filled) <- c("genus","PROV_CODE","new.ext","group","class")
require(maptools)
require(ggplot2)
require(plyr)

er <- readShapePoly("~/Dropbox/nescent_extinction_map/data/MEOW2/meow_ecos.shp")
er@data$id = rownames(er@data)
er.points = fortify(er, region = "id")
er.df <- join(er.points, er@data, by = "id")

### select "RLM_CODE" to plot realms,"PROV_CODE" to plot provinces

mean.ext <- function(df)mean(df$new.ext)
N.gen <- function(df)length(df$new.ext)
by.prov.classes <- ddply(d.eco.filled,.(class,PROV_CODE),each(mean.ext,N.gen))
by.prov.all <- ddply(d.eco.filled,.(PROV_CODE),each(mean.ext,N.gen))
### Use by.prov.all for composite, by.prov.classes for classes
by.prov <- by.prov.classes

quartz("map",13,7)
er.df$mean.ext <- NULL
er.df <- join(er.df, by.prov, by = "PROV_CODE")
er.df <- drop.levels(subset(er.df,er.df$N.gen > 1))
er.df <- drop.levels(subset(er.df,er.df$class == "Elasmobranchii"))

ggplot(er.df) + aes(long, lat, group=group,fill=N.gen) + geom_polygon() + scale_fill_gradient(low = "yellow", high = "red",name = "Mean ext. risk") + geom_polygon(data = map_data("world"), aes(long, lat, group = group), fill = "black") + opts(panel.background = theme_rect(colour="darkgray", size =1, fill = "white"))  + opts(panel.grid.major = theme_blank()) + opts(panel.grid.minor = theme_blank()) + ylab(expression("Latitude")) + xlab(expression("Longitude")) + coord_equal(ylim = c(-85,90),xlim = c(-180,180))

all_genus_prov <- expand.grid(genus = unique(sort(d.eco.filled$genus)), PROV_CODE =  sort(unique(d.eco.filled$PROV_CODE)))

d.eco.filled2 <- merge(all_genus_prov, d.eco.filled, all.x = TRUE)
d.eco.filled2$present <- 0
d.eco.filled2$present[!is.na(d.eco.filled2$group)] <- 1

er.df.genus.sharks <- join(er.df, d.eco.filled2[,c("genus", "PROV_CODE", "present")])

