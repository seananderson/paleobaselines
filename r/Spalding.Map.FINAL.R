# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jul 02, 2012
# Last modified: May 21, 2013
# Purpose:       try ggplot plotting and fortify df
# Modified by Seth Finnegan Aug. 16 2012
# ====================================================================
library(maptools)
library(gdata)
library(ggplot2)
library(plyr)
gpclibPermit()

### load province occupancy file and subset depending on whether using raw or interpolated ranges
load("~/Dropbox/nescent_extinction_map/Final data/Modern_Province_Occupancy.rda")
d.eco.filled <- Modern_Province_Occupancy

### make breakdown of subtaxa by province
#Gen_rich <- function(df) length(unique(df$genus))
#Relative.Genus.Richness <- ddply(d.eco.filled,.(class,group,PROV_CODE),each(Gen_rich))
## save table of genus richness
#write.table(Relative.Genus.Richness,"~/Dropbox/nescent_extinction_map/Final data/Genus richness by group and province.csv",sep=",")


d.eco.filled <- drop.levels(subset(Modern_Province_Occupancy,Modern_Province_Occupancy$Ranges == Input_ranges))
drops <- c("Ranges")
d.eco.filled <- d.eco.filled[,!(names(d.eco.filled) %in% drops)]



land <- readShapePoly("~/Dropbox/nescent_extinction_map/data/110m-land/110m_land.shp")
land.fort <- fortify(land)

### Import model predictions, output from "Set model predictions for maps.R"

source("~/Dropbox/nescent_extinction_map/r/Set model predictions for maps.R")
load("~/Dropbox/nescent_extinction_map/Final data/Risk_predictions.rda")
extEST <- Use.model
head(extEST)

###make grid showing mean risk as function of two variables
#extEST$extratropical <- ifelse(extEST$min.lat >= 30,1,ifelse(extEST$max.lat <= 30,0,NA))
extEST$extratropical <- ifelse(extEST$min.lat >= 30,1,0)
bin_mean <- function(df) median(log(df$use.risk))
bin_n <- function(df) length(log(df$use.risk))
binned <- ddply(extEST,.(class,extratropical,occupancy),each(bin_mean,bin_n))
binned <- drop.levels(subset(binned,binned$bin_n > 1))
pdf()
p1 <- ggplot(binned,aes(extratropical,occupancy,fill=bin_mean)) + geom_tile() + scale_fill_gradient2(low="green2",mid="yellow",high="red",midpoint=-2) + xlab(expression("")) + ylab(expression("Occupancy (prop. max.)")) + opts(axis.text.x = theme_text(size=10)) + opts(axis.text.y = theme_text(size=10))+ opts(panel.background = theme_rect(colour="black", size =1, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank()) + theme(strip.background = element_rect(fill = "white")) + facet_wrap(~class,ncol=4) + geom_vline(xintercept=.5)
print(p1)
ggsave(p1,file="~/Dropbox/nescent_extinction_map/r/grid.plot.tropical.vs.occupancy.vs.ext_risk.pdf",width=8,height=7)
dev.off()


Neogene.gbm.mean <- extEST$use.risk
extNEW <- Neogene.gbm.mean
genus <- extEST$genus
group <- extEST$group
class <- extEST$class
occupancy <- extEST$occupancy
richness <- extEST$richness 
great.circle <- extEST$great.circle 
occurrences <- extEST$occurrences 
lat.range <- extEST$lat_range 
min.lat <- extEST$min.lat
max.lat <- extEST$max.lat
mean.lat <- extEST$mean.lat
New.Ex.Est <- na.omit(data.frame(extNEW,genus,group,class,occupancy,richness,great.circle,occurrences,lat.range,min.lat,max.lat,mean.lat))

# ddply(New.Ex.Est, "class", summarize, n = length(class))
           #class   n
#1       Anthozoa 132
#2       Bivalvia 441
#3     Echinoidea 118
#4 Elasmobranchii 105
#5   Foraminifera 374
#6     Gastropoda 646
#7   Malacostraca 330
#8       Mammalia  55
#9     Testudines  15

colnames(New.Ex.Est) <- c("new.ext","genus","group","class","occupancy","richness","great.circle","occurrences","lat.range","min.lat","max.lat","mean.lat")
d.eco.filled <- drop.levels(unique(merge(d.eco.filled,New.Ex.Est,by = "genus",all.x = FALSE)))
##d.eco.filled <- drop.levels(unique(data.frame(d.eco.filled$class,d.eco.filled$group,d.eco.filled$genus,d.eco.filled$new.ext, d.eco.filled$PROV_CODE,d.eco.filled$RLM_CODE)))
#colnames(d.eco.filled) <- c("genus","PROV_CODE","new.ext","group","class","occupancy","richness","great.circle","occurrences","lat.range","min.lat","max.lat","mean.lat")
# remove mammals if desired
d.eco.filled <- drop.levels(subset(d.eco.filled,d.eco.filled$class != "Foraminifera"))

# ddply(d.eco.filled, "class", summarize, n = length(class))
           #class    n
#1       Anthozoa 2635
#2       Bivalvia 6601
#3     Echinoidea 1671
#4 Elasmobranchii 2099
#5     Gastropoda 8572
#6   Malacostraca 3930
#7       Mammalia 1353
#8     Testudines  219

require(maptools)
require(ggplot2)
require(plyr)

er <- readShapePoly("~/Dropbox/nescent_extinction_map/data/MEOW2/meow_ecos.shp")
er@data$id = rownames(er@data)
er.points = fortify(er, region = "id")
er.df <- join(er.points, er@data, by = "id")

head(d.eco.filled)


### select "RLM_CODE" to plot realms,"PROV_CODE" to plot provinces

# NOTE It's called "mean.ext" but is actually the median:
mean.ext <- function(df)mean(log(df$new.ext))
median.ext <- function(df)median(log(df$new.ext))
N.gen <- function(df)length(df$new.ext)
mean.occupancy <- function(df)mean(df$occupancy)
mean.occurrences <- function(df)mean(df$occurrences)
mean.great.circle <- function(df)mean(df$great.circle)
mean.lat.range <- function(df)mean(df$lat.range)
mean.richness <- function(df)mean(df$richness)
mean.min.lat <- function(df)mean(df$min.lat)
mean.max.lat <- function(df)mean(df$max.lat)
mean.mean.lat <- function(df)mean(df$mean.lat)

by.prov.classes <- ddply(d.eco.filled,.(class,PROV_CODE),each(mean.ext,median.ext,N.gen,mean.occupancy,mean.occurrences,mean.great.circle,mean.lat.range,mean.richness,mean.min.lat,mean.max.lat,mean.mean.lat))
by.prov.all <- ddply(d.eco.filled,.(PROV_CODE),each(mean.ext,median.ext,N.gen,mean.occupancy,mean.occurrences,mean.great.circle,mean.lat.range,mean.richness,mean.min.lat,mean.max.lat,mean.mean.lat))

class.use <- rep(plot.value,length(by.prov.classes[,1]))
all.use <- rep(plot.value,length(by.prov.all[,1]))
     
by.prov.classes$ext.plot <- ifelse(class.use == "median",by.prov.classes$median.ext,by.prov.classes$mean.ext)
by.prov.all$ext.plot <- ifelse(all.use == "median",by.prov.all$median.ext,by.prov.all$mean.ext)

Prov.Areas <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Spalding province areas.csv",header = TRUE,stringsAsFactors = FALSE)
Prov.Sampling <- read.csv("~/Dropbox/nescent_extinction_map/Final data/OBIS.sampling.and.richness.csv",header = TRUE,stringsAsFactors = FALSE)

Prov.Data <- merge(Prov.Sampling,Prov.Areas,by=c("Zone","PROV_CODE"),all.x=TRUE)
     
by.prov.classes <- merge(by.prov.classes,Prov.Data,by=c("class","PROV_CODE"))
by.prov.all <- merge(by.prov.all,Prov.Data,by="PROV_CODE")  


###read in Halpern and Burrows layers
Impacts <- read.csv("~/Dropbox/nescent_extinction_map/data/Halpern_Burrows_by_Spalding.csv",header = TRUE)
Impacts <- data.frame(Impacts$PROV_CODE,scale(Impacts$Mean_Halpern_Province),scale(Impacts$Mean_Burrows_Province))
colnames(Impacts) <- c("PROV_CODE","Halpern","Burrows")
by.prov.all <- merge(by.prov.all,Impacts,by = "PROV_CODE")
by.prov.all <- drop.levels(subset(by.prov.all,by.prov.all$class=="all"))
by.prov.classes <- merge(by.prov.classes,Impacts,by = "PROV_CODE")


#er.df.classes <- join(er.df, by.prov.classes, by = "PROV_CODE")
#er.df.all <- join(er.df, by.prov.all, by = "PROV_CODE")

save(by.prov.classes, file = "~/Dropbox/nescent_extinction_map/Final data/by.prov.classes.rda")
save(by.prov.all, file = "~/Dropbox/nescent_extinction_map/Final data/by.prov.all.rda")
#save(er.df.classes, file = "~/Dropbox/nescent_extinction_map/Final data/er.df.classes.rda")
#save(er.df.all, file = "~/Dropbox/nescent_extinction_map/Final data/er.df.all.rda")

by.prov.classes$Lat_Zone <- as.factor(ifelse(by.prov.classes$Zone=="Tropical","Tropical","Extratropical"))

png()
p1 <- ggplot(by.prov.classes,aes(log(OBIS_records),ext.plot)) + geom_point(size=2,pch=1) + geom_smooth(method="lm") + xlab(expression("log OBIS records")) + ylab(expression("Log mean extinction susceptibility")) + opts(axis.text.x = theme_text(size=6)) + opts(axis.text.y = theme_text(size=6))+ opts(panel.background = theme_rect(colour="black", size =1, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank()) + facet_wrap(~class,ncol=2,scales="free") + theme(strip.background = element_rect(fill = "white")) + scale_colour_manual(values=c("blue","red"))

print(p1)
ggsave(p1,file="~/Dropbox/nescent_extinction_map/r/crossplot.of.sampling.vs.mean.risk.png",width=7,height=7)
dev.off()



png()
p1 <- ggplot(by.prov.classes,aes(mean.occupancy,ext.plot)) + geom_point(size=2,pch=1) + geom_smooth(method="lm") + xlab(expression("mean occupancy")) + ylab(expression("Log mean extinction susceptibility")) + opts(axis.text.x = theme_text(size=6)) + opts(axis.text.y = theme_text(size=6))+ opts(panel.background = theme_rect(colour="black", size =1, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank()) + facet_wrap(~class,ncol=2,scales="free") + theme(strip.background = element_rect(fill = "white")) + scale_colour_manual(values=c("blue","red"))

print(p1)
ggsave(p1,file="~/Dropbox/nescent_extinction_map/r/crossplot.of.mean.occupancy.vs.mean.risk.png",width=7,height=7)
dev.off()

png()
p1 <- ggplot(by.prov.classes,aes(mean.richness,ext.plot)) + geom_point(size=2,pch=1) + geom_smooth(method="lm") + xlab(expression("mean richness")) + ylab(expression("Log mean extinction susceptibility")) + opts(axis.text.x = theme_text(size=6)) + opts(axis.text.y = theme_text(size=6))+ opts(panel.background = theme_rect(colour="black", size =1, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank()) + facet_wrap(~class,ncol=2,scales="free") + theme(strip.background = element_rect(fill = "white")) + scale_colour_manual(values=c("blue","red"))

print(p1)
ggsave(p1,file="~/Dropbox/nescent_extinction_map/r/crossplot.of.mean.richness.vs.mean.risk.png",width=7,height=7)
dev.off()

png()
p1 <- ggplot(by.prov.classes,aes(mean.great.circle,ext.plot)) + geom_point(size=2,pch=1) + geom_smooth(method="lm") + xlab(expression("mean great circle distance")) + ylab(expression("Log mean extinction susceptibility")) + opts(axis.text.x = theme_text(size=6)) + opts(axis.text.y = theme_text(size=6))+ opts(panel.background = theme_rect(colour="black", size =1, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank()) + facet_wrap(~class,ncol=2,scales="free") + theme(strip.background = element_rect(fill = "white")) + scale_colour_manual(values=c("blue","red"))

print(p1)
ggsave(p1,file="~/Dropbox/nescent_extinction_map/r/crossplot.of.mean.great.circle.vs.mean.risk.png",width=7,height=7)
dev.off()

png()
p1 <- ggplot(by.prov.classes,aes(mean.lat.range,ext.plot)) + geom_point(size=2,pch=1) + geom_smooth(method="lm") + xlab(expression("mean latitudinal range")) + ylab(expression("Log mean extinction susceptibility")) + opts(axis.text.x = theme_text(size=6)) + opts(axis.text.y = theme_text(size=6))+ opts(panel.background = theme_rect(colour="black", size =1, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank()) + facet_wrap(~class,ncol=2,scales="free") + theme(strip.background = element_rect(fill = "white")) + scale_colour_manual(values=c("blue","red"))

print(p1)
ggsave(p1,file="~/Dropbox/nescent_extinction_map/r/crossplot.of.mean.lat.range.vs.mean.risk.png",width=7,height=7)
dev.off()

     
png()
p1 <- ggplot(by.prov.classes,aes(log(OBIS_records),ext.plot)) + geom_point(size=2,pch=1) + geom_smooth(method="lm") + xlab(expression("mean species richness")) + ylab(expression("Log mean extinction susceptibility")) + opts(axis.text.x = theme_text(size=6)) + opts(axis.text.y = theme_text(size=6))+ opts(panel.background = theme_rect(colour="black", size =1, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank()) + facet_wrap(~class,ncol=2,scales="free") + theme(strip.background = element_rect(fill = "white")) + scale_colour_manual(values=c("blue","red"))

print(p1)
ggsave(p1,file="~/Dropbox/nescent_extinction_map/r/crossplot.of.genus.richness.vs.mean.risk.png",width=7,height=7)
dev.off()

png()
p1 <- ggplot(by.prov.classes,aes(mean_endemic,ext.plot)) + geom_point(size=2,pch=1) + geom_smooth(method="lm") + xlab(expression("proportion endemic genera")) + ylab(expression("Log mean extinction susceptibility")) + opts(axis.text.x = theme_text(size=6)) + opts(axis.text.y = theme_text(size=6))+ opts(panel.background = theme_rect(colour="black", size =1, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank()) + facet_wrap(~class,ncol=2,scales="free") + theme(strip.background = element_rect(fill = "white")) + scale_colour_manual(values=c("blue","red"))

print(p1)
ggsave(p1,file="~/Dropbox/nescent_extinction_map/r/crossplot.of.prop.endemic.vs.mean.risk.png",width=7,height=7)
dev.off()





#png()
#p2 <- ggplot(d.eco.filled,aes(new.ext,colour=class))+ geom_density(fill=NA) + geom_vline(xintercept=.2,size=.25,colour#="red") + facet_wrap(~PROV_CODE,ncol=8,scales = 'free_y') 
#print(p2)
#ggsave(p2,file="~/Dropbox/nescent_extinction_map/r/risk.histograms.by.province.png",width=10,height=7)
#dev.off()

# Make bocxplots by class and province


#stat_sum_line <- function(fun, geom="point", ...) {
#  stat_summary(fun.y=fun, colour="black", geom=geom, size = 1, ...)
#}

#png()
#p2 <- ggplot(d.eco.filled.zone,aes(class,log(new.ext),group=class,fill=class)) + geom_rect(data = d.eco.filled.zone,aes(colour = Zone),fill="white",size=1.5,xmin = -Inf,xmax = Inf,ymin = -Inf,ymax = Inf) + geom_violin(size=.1) + facet_wrap(~PROV_CODE,ncol=8) + coord_flip() + opts(panel.background = theme_rect(colour="black", size =1, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank()) + scale_colour_manual(values=c("blue","green3","red")) + stat_sum_line(median)
#print(p2)
#ggsave(p2,file="~/Dropbox/nescent_extinction_map/r/risk.boxplots.by.province.png",width=8.5,height=11)
#dev.off()

# Make bocxplots by and province
#PROV_CODE <- seq(1,62,by=1)
#Zone <- c("polar",rep("temperate",10),rep("tropical",33),rep("temperate",14),rep("polar",4))
#Lat_Zones <- data.frame(PROV_CODE,Zone)
#d.eco.filled.zone <- merge(d.eco.filled,Lat_Zones,by="PROV_CODE")

#stat_sum_single <- function(fun, geom="point", ...) {
#  stat_summary(fun.y=fun, colour="black", geom=geom, size = 2, ...)
#}

#png()
#p2 <- ggplot(d.eco.filled.zone,aes(PROV_CODE,log(new.ext),group=PROV_CODE,fill=Zone))+ geom_violin(outlier.size=.75,size=.25) +  coord_flip() + scale_fill_manual(values=c("cyan","palegreen","gold")) + stat_sum_single(median)
#print(p2)
#ggsave(p2,file="~/Dropbox/nescent_extinction_map/r/risk.boxplots.by.province.all.png",width=5,height=8)
#dev.off()

#png()
#p2 <- ggplot(d.eco.filled.zone,aes(PROV_CODE,log(new.ext),group=PROV_CODE,fill=Zone))+ geom_violin(outlier.size=.75,size=.25,colour=NA) + scale_fill_manual(values=c("blue","green3","red")) + stat_sum_single(median)
#print(p2)
#ggsave(p2,file="~/Dropbox/nescent_extinction_map/r/risk.boxplots.by.province.all.png",width=8,height=5)
#dev.off()


#png()
#p2 <- ggplot(d.eco.filled.zone,aes(log(new.ext),group=PROV_CODE,colour=Zone))+ geom_density(alpha#=.01) + scale_colour_manual(values=c("blue","green3","red")) + facet_wrap(~PROV_CODE,ncol=8) 
#print(p2)
#ggsave(p2,file="~/Dropbox/nescent_extinction_map/r/risk.boxplots.by.province.all.png",width=10,height=8)
#dev.off()

