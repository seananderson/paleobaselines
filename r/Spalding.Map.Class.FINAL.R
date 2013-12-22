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


###choose province occurrence dataset."interpolated_provs_alt_all_taxa.rda" has central and western indo-pacific merged
data <- load("~/Dropbox/nescent_extinction_map/Final data/interpolated_provs_all_taxa.rda")
d.eco.filled <- interpolated_provs_all_taxa
land <- readShapePoly("~/Dropbox/nescent_extinction_map/data/110m-land/110m_land.shp")
land.fort <- fortify(land)

#### read in risk estimates; choose whether to use merged ("merged_indopacific") or unmerged ("unaltered") range estimates
extEST <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Neogene_ext_risk_estimates.csv",header = TRUE,stringsAsFactors = FALSE)
#extEST <- drop.levels(subset(extEST,extEST$min.occs==1))
Neogene.gbm.mean <- extEST$gbmEx_Neogene_Model7
extNEW <- Neogene.gbm.mean
genus <- extEST$genus
group <- extEST$group
class <- extEST$class
occupancy <- extEST$occupancy
New.Ex.Est <- na.omit(data.frame(extNEW,occupancy,genus,group,class))



Class_Mean<- function(df) mean(df$extNEW)
Class_StDev <- function(df) sd(df$extNEW)
Class_Meds <- ddply(New.Ex.Est,.(class),each(Class_Mean,Class_StDev))
colnames(Class_Meds) <- c("class","class_mean","class_sd")
New.Ex.Est <- merge(New.Ex.Est,Class_Meds,by="class")
ext.class.z <- ((New.Ex.Est$extNEW - New.Ex.Est$class_mean)/New.Ex.Est$class_sd)
New.Ex.Est <- data.frame(New.Ex.Est,ext.class.z)


colnames(New.Ex.Est) <- c("class","new.ext","occupancy","genus","group","class_mean","class_sd","ext.class.z")
d.eco.filled <- drop.levels(unique(merge(d.eco.filled,New.Ex.Est,by = "genus",all.x = FALSE)))
##d.eco.filled <- drop.levels(unique(data.frame(d.eco.filled$class,d.eco.filled$group,d.eco.filled$genus,d.eco.filled$new.ext, d.eco.filled$PROV_CODE,d.eco.filled$RLM_CODE)))
colnames(d.eco.filled) <- c("genus","PROV_CODE","class","new.ext","occupancy","group","class_mean","class_sd","ext.class.z")
# remove mammals if desired
#d.eco.filled <- drop.levels(subset(d.eco.filled,d.eco.filled$class != "Mammalia"))
require(maptools)
require(ggplot2)
require(plyr)

er <- readShapePoly("~/Dropbox/nescent_extinction_map/data/MEOW2/meow_ecos.shp")
er@data$id = rownames(er@data)
er.points = fortify(er, region = "id")
er.df <- join(er.points, er@data, by = "id")



### select "RLM_CODE" to plot realms,"PROV_CODE" to plot provinces

mean.ext <- function(df)median(df$ext.class.z)
stdev.ext <- function(df)sd(df$ext.class.z)
N.gen <- function(df)length(df$ext.class.z)
mean.occ <- function(df)mean(df$occupancy)
by.prov.classes <- ddply(d.eco.filled,.(class,PROV_CODE),each(mean.ext,N.gen,mean.occ))
by.prov.all <- ddply(d.eco.filled,.(PROV_CODE),each(mean.ext,N.gen,mean.occ))
### Use by.prov.all for composite, by.prov.classes for classes
by.prov <- by.prov.classes


#by.prov <- drop.levels(subset(by.prov,by.prov$class != "iMammaliai"))

Prov.Areas <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Spalding province areas.csv",header = TRUE,stringsAsFactors = FALSE)

by.prov <- merge(by.prov,Prov.Areas,by="PROV_CODE")

write.table(by.prov,"Province_mean_ext.csv",sep = ",")

#p <- ggplot(by.prov,aes(mean.occ,mean.ext, colour=log(Area)))
#p + geom_point(size = 4)





#qplot(by.prov$mean.occ,by.prov$mean.ext,colour =by.prov$class) + ylab(expression("Mean ext. risk")) + xlab(expression("Mean. genus occupancy"))

Foundations of Paleoecology
scheduled April 29, 2013 from 4:00 PM to 6:00 PM
Location: Fishbowl

quartz("map",13,7)
er.df$mean.ext <- NULL
er.df <- join(er.df, by.prov, by = "PROV_CODE")
er.df <- drop.levels(subset(er.df,er.df$N.gen > 1))
#er.df <- drop.levels(subset(er.df,er.df$class == "Testudines"))

ggplot(er.df) + aes(long, lat, group=group,fill=mean.ext) + geom_polygon() + scale_fill_gradient(low = "yellow", high = "red",name = "Mean ext. risk") + geom_polygon(data = map_data("world"), aes(long, lat, group = group), fill = "black") + opts(panel.background = theme_rect(colour="darkgray", size =1, fill = "white"))  + opts(panel.grid.major = theme_blank()) + opts(panel.grid.minor = theme_blank()) + ylab(expression("Latitude")) + xlab(expression("Longitude")) + coord_equal(ylim = c(-85,90),xlim = c(-180,180)) + facet_wrap(~ class)