require(gdata)
data <- read.csv("~/Dropbox/Rasmussen and Harper brach database/Rasmussen Harper Database Processed.csv",header=TRUE)
head(data)
#data <-drop.levels(subset(data,data$Single_occurrence==0))
data$LatBin <- data$MaxDepth
#data$LatBin <- ceiling(data$MinLat/10)*10

Prop.Ext <- function(df) mean(df$Ex)
gen <- function(df) length(df$Ex)
RichnessEx <- ddply(data,.(stage,LatBin),each(Prop.Ext,gen))
RichnessEx <- drop.levels(subset(RichnessEx,RichnessEx$gen >= 4))
RichnessEx$stage <- factor(RichnessEx$stage, levels=c("E Caradoc","M Caradoc","U Caradoc","Pusgillian", "Cautleyan", "Rawtheyan", "Hirnantian", "Rhuddanian", "Aeronian", "Telychian"))

quartz()
p <- ggplot(RichnessEx,aes(stage,LatBin,fill = Prop.Ext))
p + geom_tile()+ opts(axis.text.x = theme_text(size=10,angle=-90)) + opts(axis.text.y = theme_text(size=10)) + xlab(expression("stage")) + ylab(expression("Abs. Lat. Range"))