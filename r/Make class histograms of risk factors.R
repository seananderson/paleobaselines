library(ggplot2)
library(plyr)
library(pls)
load("/Users/Seth/Dropbox/nescent_extinction_map/data/global_22x7_ext_rich_vel_hlp_20120601.rda")
d <- d.45x14
data <- d

### create layer and paleo rate dataframes to be standardized
cells <- data.frame(paste(d$PID,d$SID),d$hlp,d$vel)
taxa <- data.frame(d$MatchTaxon,d$ext)

### drop redundant rows and standardize classes and layers, create merge table to be matched to original data
uniqueCells <- unique(cells)
uniqueTaxa <- unique(taxa)
CellID <- uniqueCells[,1]
hlp <- uniqueCells[,2]
vel <- uniqueCells[,3]
MatchTaxon <- uniqueTaxa[,1]
ext <- uniqueTaxa[,2]
hlpStd <- scale(hlp)
velStd <- scale(log(abs(vel)))
extStd <- scale(log(ext+.1))

VelHlp <- data.frame(CellID,hlpStd,velStd)
Ext <- data.frame(MatchTaxon,extStd)


### define variables in original matrix
CellID <- paste(data$PID,data$SID)
data <- data.frame(data,CellID)
EID <- data$EID
PID <- data$PID
SID <- data$SID
long <- data$long
lat <- data$lat
rich <- data$rich
ext <- data$ext
weights <- ext*rich
vel <- data$vel
hlp <- data$hlp
MatchTaxon <- data$MatchTaxon
CellID <- as.factor(data$CellID)
class <- data$class
genus <- data$taxon

### merge with lookup tables of standardized rates and layers
data <- data.frame(CellID,Taxon,hlp,vel,ext,lat,long,PID,SID,class,MatchTaxon,genus)
head(data)


data3 <- merge(data,VelHlp)


### take averages of all grid cells occupied by each genus

Vel <- data3$velStd
Hlp <- data3$hlpStd
Hlp_Vel <- Vel+Hlp




data4 <- data.frame(CellID,Taxon,rich,PID,SID,class,MatchTaxon,genus,Hlp_Vel,Vel,Hlp)

Vel <- function(df) mean(df$Vel)
Occupancy <- function(df) length(df$Vel)
Hlp <- function(df) mean(df$Hlp)
HlpVel <- function(df) mean(df$Hlp_Vel)
byGenus <- ddply(data4,.(class,MatchTaxon,genus),each(HlpVel,Vel,Hlp,Occupancy))
data5 <- merge(byGenus,Ext)
class <- data5$class
HlpVel <- scale(data5$HlpVel)
Vel <- scale(data5$Vel)
Hlp <- scale(data5$Hlp)
Occupancy <- data5$Occupancy
ext <- data5$extStd


HlpExt <- Hlp + ext
HlpVelExt <- HlpVel + ext
VelExt <- Vel + ext
data5 <- data.frame(class,HlpVel,HlpVelExt,Hlp,HlpExt,Vel,VelExt,Occupancy)


p <- ggplot(data5,aes(Vel,VelExt)) 
p + geom_point() + facet_wrap(~ class, ncol=2) + geom_abline()



### make pdf plots
p <- ggplot(data5) + geom_vline(xintercept=0,size = .25)
p + geom_density(aes(HlpVelExt,fill = "HlpVelExt",alpha = .3)) + geom_density(aes(HlpVel,fill = "HlpVel",alpha = .3)) + facet_wrap(~ class,scales = "free_y",ncol=2)  + coord_cartesian(xlim= c(-2,5))

qplot(Vel,Hlp, alpha = .5, colour = class, pch = class) + geom_smooth(method = "lm")+ scale_shape_manual(values = c(1:10))



### + geom_density(aes(Hlp,fill = "Hlp",alpha = .3))
### +geom_density(aes(Vel,fill = "Vel",alpha = .3))




