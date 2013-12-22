ext <- read.csv("/Users/Seth/Dropbox/nescent_extinction_map/data/Risk estimates by stage.csv",header = TRUE)
head(ext)
require(ggplot2)
class <- ext$class
logOcc <- log10(ext$occupancy+1)
Risk <- ext$NeogeneMeanRisk 
SE <- ext$NeogeneSE
limits <- aes(ymax = Risk + SE, ymin=Risk - SE) 

p <- ggplot(ext,aes(logOcc,Risk,colour=class))
p + geom_errorbar(limits, width=0,alpha=.5) + geom_point(alpha=.6,size=3) + scale_colour_brewer(palette="Set1",name = "Class") + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) + theme(panel.background = element_rect(colour="black", size =1, fill = "white")) + ylab(expression("Extinction susceptibility")) + xlab(expression(paste(Log[10], ,occupancy))) + theme(axis.title.x = element_text(size=16)) + theme(axis.title.y = element_text(size=16, angle = 90))