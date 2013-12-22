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
data <- subset(data,data$class != "AMammalia")
CellID <- paste(data$PID,d$SID)
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
data <- data.frame(PID,SID,long,lat,ext,rich,vel,hlp,Taxon,CellID,MinDepth,MeanDepth,MaxDepth,class)



Lat <- function(df) min(df$lat)
Long <- function(df) mean(df$long)
MaxD <- function(df) mean(df$MaxDepth)
MeanD <- function(df) mean(df$MeanDepth)
Hlp <- function(df) mean(df$hlp)
Vel <- function(df) mean(df$vel)
Ext <- function(df) sum(df$ext)
Rich <- function(df) sum(df$rich)

byCell <- ddply(data,.(CellID),each(Lat,Long,Hlp,Vel,Ext,Rich,MaxD,MeanD))
extmean <- byCell$Ext/byCell$Rich
MaxD <- byCell$MaxD
MeanD <- byCell$MeanD
byCell <- subset(byCell, byCell$MaxD >= -200000 & extmean < 5 & byCell$Rich >1)
MaxD <- byCell$MaxD
MeanD <- byCell$MeanD
Long <- byCell$Long
Lat <- byCell$Lat
Rich <- byCell$Rich
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
PlotVar <- MeanRisk
midp <- mean(PlotVar)

#### make plot
p <- ggplot()

p + geom_tile(aes(mapping = byCell,Long,Lat, fill=PlotVar)) + scale_fill_gradient2(mid="yellow",high= "red",low = "green2", midpoint = midp)+ opts(panel.background = theme_rect(colour="darkgray", size =1, fill = "white")) + opts(panel.grid.major = theme_blank()) + opts(panel.grid.minor = theme_blank()) 
last_plot() + geom_path(aes(mapping=world,world[,1]-10,world[,2]),fill = "black") + stat_contour(aes(mapping = data.frame(Long,Lat,PlotVar),Long,Lat,z=PlotVar),colour = "blue")


#### + stat_contour(aes(mapping = byCell,x=Long,y=Lat,z=PlotVar))

###+ geom_point(pch=16, alpha =1,size =7)  
### + scale_fill_gradient2(mid="white",high= "red",low = "blue", midpoint = midp)


##p <- ggplot(byCell,aes(Hlp,log(VelTran), colour = log(MeanRisk), size = log(Rich)))
###p + geom_point(pch=16, alpha =.7)  + scale_colour_gradient2(mid="yellow",high= "orange",low = "green", midpoint = -3)


###+ opts(panel.background = theme_rect(colour="darkgray", size =1, fill = "white")) 



####+ facet_wrap(~ MatchTaxon, ncol=12)

