library(ggplot2)
library(plyr)
library(pls)
require("maps") 
world <- data.frame(map("world", plot=FALSE)[c("x","y")]) 
world <- fortify(world)
load("/Users/Seth/Dropbox/nescent_extinction_map/data/global_45x14_ext_rich_vel_hlp_20120605.rda")
d <- d.45x14


d$taxon <- NULL
CellID <- paste(d$PID,d$SID)
d <- data.frame(d,CellID)
d <- unique(d)
data <- d
data <- subset(data,data$class != "Mammalia" & data$class != "Reptilia" & data$class != "Elasmobranchii")
BelongsToClass <- ifelse(data$class == "Rhynchonellata",data$rich,0)
CellID <- paste(data$PID,data$SID)
data <- data.frame(data,CellID)
PID <- data$PID
SID <- data$SID
long <- data$long
lat <- data$lat
class <- data$class
rich <- data$rich
ext <- data$ext*rich
vel <- data$vel
hlp <- data$hlp
Taxon <- data$MatchTaxon
CellID <- as.factor(data$CellID)
MinDepth <- data$depth_min
MeanDepth <- data$depth_mean
MaxDepth <- data$depth_max
data <- data.frame(PID,SID,long,lat,ext,rich,vel,hlp,Taxon,CellID,MinDepth,MeanDepth,MaxDepth,class,BelongsToClass)



Lat <- function(df) min(df$lat)
Long <- function(df) mean(df$long)
MaxD <- function(df) mean(df$MaxDepth)
MeanD <- function(df) mean(df$MeanDepth)
Hlp <- function(df) mean(df$hlp)
Vel <- function(df) mean(df$vel)
Ext <- function(df) sum(df$ext)
Rich <- function(df) sum(df$rich)
ClassRich <- function(df) sum(df$BelongsToClass)
byCell <- ddply(data,.(PID,SID),each(Lat,Long,Hlp,Vel,Ext,Rich,MaxD,MeanD,ClassRich))
PID <- byCell$PID
SID <- byCell$SID
extmean <- byCell$Ext/byCell$Rich
MaxD <- byCell$MaxD
MeanD <- byCell$MeanD
byCell <- subset(byCell, byCell$MaxD >= -200 & extmean <5 & byCell$Rich >50)
PID <- byCell$PID
SID <- byCell$SID
MaxD <- byCell$MaxD
MeanD <- byCell$MeanD
Long <- byCell$Long
Lat <- byCell$Lat
ClassRich <- byCell$ClassRich
Rich <- byCell$Rich
ClassPropRich <- ClassRich/Rich
Ext <- byCell$Ext
Vel <- byCell$Vel
Hlp <- byCell$Hlp
MeanRisk <- Ext/Rich
VelTran <- abs(Vel)
MaxD <- byCell$MaxD
ExtStd <- scale(MeanRisk)
HlpStd <- scale(Hlp)
LogVelTran <- log10(VelTran)
HlpVel <- scale(Hlp)+scale(LogVelTran)
HlpVelExt <- scale(Hlp)+scale(LogVelTran)+scale(MeanRisk)
Z <- MeanRisk
midp <- mean(Hlp)
load("/Users/Seth/Dropbox/nescent_extinction_map/data/equal_area_grid/global_45x14.rda")
source("/Users/Seth/Dropbox/nescent_extinction_map/r/map2.R") 
source("/Users/Seth/Dropbox/nescent_extinction_map/r/col.key.R")
source("/Users/Seth/Dropbox/nescent_extinction_map/r/smooth.pal.R")
source("/Users/Seth/Dropbox/nescent_extinction_map/r/grid.ext.data.2.R")
source("/Users/Seth/Dropbox/nescent_extinction_map/r/plot.map.mollweide.R")

df <- data.frame(PID,SID,Z)
plot.map.mollweide(df, grid.object = global_45x14)



qplot(HlpVel,HlpVelExt,colour = abs(Lat),size = 4, alpha = .9) + scale_colour_gradient2(mid="yellow",high= "green2",low = "red", midpoint = 30)+geom_abline()+ opts(panel.background = theme_rect(colour="darkgray", size =1, fill = "white")) + opts(panel.grid.major = theme_blank()) + opts(panel.grid.minor = theme_blank()) 



