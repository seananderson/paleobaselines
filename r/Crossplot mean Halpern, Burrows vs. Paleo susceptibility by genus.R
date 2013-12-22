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
d.eco.filled2 <- d.eco.filled
require(maptools)
require(plyr)

er <- readShapePoly("/Users/Seth/Dropbox/nescent_extinction_map/data/MEOW2/meow_ecos.shp")
er@data$id = rownames(er@data)
er.points = fortify(er, region = "id")
er.df <- join(er.points, er@data, by = "id")

### select "RLM_CODE" to plot realms,"PROV_CODE" to plot provinces

mean.ext <- function(df)mean(df$new.ext)
N.gen <- function(df)length(df$new.ext)
by.prov.classes <- ddply(d.eco.filled,.(class,PROV_CODE),each(mean.ext,N.gen))
by.prov.all <- ddply(d.eco.filled,.(PROV_CODE),each(mean.ext,N.gen))
### Use by.prov.all for composite, by.prov.classes for classes
by.prov <- by.prov.all

###read in Halpern and Burrows layers
Impacts <- read.csv("/Users/Seth/Dropbox/nescent_extinction_map/data/Halpern_Burrows_by_Spalding.csv",header = TRUE)
Impacts <- data.frame(Impacts$PROV_CODE,Impacts$Mean_Halpern_Hybrid,Impacts$Mean_Burrows_Hybrid)
colnames(Impacts) <- c("PROV_CODE","Halpern","Burrows")

HalBurPaleo <- merge(Impacts,d.eco.filled2,by="PROV_CODE")
HalBurPaleo <- na.omit(HalBurPaleo)

mean.ext <- function(df)mean(df$new.ext)
mean.Hal <- function(df)mean(df$Halpern)
mean.Bur <- function(df)mean(df$Burrows)

by.genus <- ddply(HalBurPaleo,.(class,genus),each(mean.ext,mean.Hal,mean.Bur))
class <- by.genus$class
Halpern <- by.genus$mean.Hal
Susceptibility <- by.genus$mean.ext
Burrows <- by.genus$mean.Bur

by.class <- data.frame(class,Halpern,Susceptibility,Burrows)
by.class <- na.omit(by.class)

mean.ext <- function(df)mean(df$Susceptibility)
mean.Hal <- function(df)mean(df$Halpern)
mean.Bur <- function(df)mean(df$Burrows)

by.class2 <- ddply(by.class,.(class),each(mean.ext,mean.Hal,mean.Bur))


p <- ggplot()
p + geom_hline(yintercept = mean(by.class$Halpern)) + geom_vline(xintercept = mean(by.class$Susceptibility)) + geom_point(data=by.class,aes(Susceptibility,Halpern,colour=class),alpha=.3,pch=1,size =1) + scale_colour_brewer(palette="Set1",name = "Class") + geom_point(data=by.class2,aes(mean.ext,mean.Hal,colour=class),alpha=1,pch=16,size =5) + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) + theme(panel.background = element_rect(colour="black", size =1, fill = "white")) + ylab(expression("Mean anthropogenic impact")) + xlab(expression("Extinction susceptibility")) + theme(axis.title.x = element_text(size=16)) + theme(axis.title.y = element_text(size=16, angle = 90)) 


+ scale_y_continuous(limits = c(-5,5))+ scale_x_continuous(limits = c(-5,5))


