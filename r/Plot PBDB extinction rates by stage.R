library(ggplot2)
library(plyr)
library(gdata)

load("~/Dropbox/nescent_extinction_map/Final data/Occs.09.30.with.midcells.rda")
PaleoDB <- PaleoDB
head(PaleoDB)
#data <- subset(PaleoDB,PaleoDB$slc==72 | PaleoDB$slc==73)
data <- subset(PaleoDB,PaleoDB$slc > 68)
Lat <- data$Y_mid
Long <- data$X_mid
genus <- data$occurrence.genus_name

### make map of all Neogene occurrences 

mapping <- data.frame(genus,Lat,Long)
colnames(mapping) <- c("genus","Lat","Long")
#mapping <- unique(mapping)

occs <- function(df) length(df$genus)
occ_dist <- ddply(mapping,.(Lat,Long),each(occs))

p <- ggplot(occ_dist,aes(Long,Lat,fill=log10(occs)))
p + geom_tile() + scale_fill_gradient2(low="green3",mid="yellow",high="red",midpoint=2)

### make by-stage maps of genus richness and extinction risk

load("~/Dropbox/nescent_extinction_map/Final data/Occs.09.30.with.midcells.rda")

data <- subset(PaleoDB,PaleoDB$slc==70 | PaleoDB$slc==70)

Lat <- data$Y_mid
Long <- data$X_mid
genus <- data$occurrence.genus_name


mapping <- data.frame(genus,Lat,Long)
colnames(mapping) <- c("genus","Lat","Long")
mapping <- unique(mapping)

plotdata <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Modern and PaleoDB ranges FINAL 2.0.csv",header = TRUE,stringsAsFactors = FALSE)

plotdata <- drop.levels(subset(plotdata,plotdata$use==1  & plotdata$Interval_Name=="Middle Miocene"))

plotgenus <- plotdata$genus
plotclass <- plotdata$class
plotEx <- plotdata$Extinct.in.stage

plotdata <- data.frame(plotclass,plotgenus,plotEx)
colnames(plotdata) <- c("class","genus","Ex")

data <- merge(mapping,plotdata,by="genus",all.x=FALSE)

mean_Ex <- function(df) mean(df$Ex)
N_gen <- function(df) length(df$Ex)
ex_rates <- ddply(data,.(Lat,Long),each(mean_Ex,N_gen))

ex_rates <- drop.levels(subset(ex_rates,ex_rates$N_gen >= 25))
p <-ggplot(ex_rates,aes(log10(N_gen),log10(mean_Ex+.01),colour=abs(Lat)))
p + geom_point(size=4) + scale_colour_gradient2(low="red",mid="yellow",high="green3",midpoint=45) + geom_smooth(method="lm")

#plot genus richness
p <- ggplot(ex_rates,aes(Long,Lat,fill=log10(N_gen)))
p + geom_tile() + coord_cartesian(xlim=c(-180,180),ylim=c(-90,90)) + scale_fill_gradient2(low="green2",mid="yellow",high="red",midpoint=2)

#plot extinction rate
p <- ggplot(ex_rates,aes(Long,Lat,fill=mean_Ex))
p + geom_tile() + coord_cartesian(xlim=c(-180,180),ylim=c(-90,90)) + scale_fill_gradient2(low="green2",mid="yellow",high="red",midpoint=.1)