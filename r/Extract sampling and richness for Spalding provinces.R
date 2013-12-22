
# Created by:    Sean C. Anderson
# Created:       Feb 06, 2012
# Last modified: Feb 06, 2013
# Purpose:       Overlay the equal area grid on the raw occurrence
#                data as a sensitivity test for our interpolation
#                method.
require(plyr)
require(maptools)
require(ggplot2)
occsdata <- load("../data/composite.occ2.rda")
composite.occ2 <- composite.occ2
head(composite.occ2)


er <- readShapePoly("../data/MEOW2/meow_ecos.shp")
#plot(er, zcol=1,axes=TRUE, border="gray",las = 1,pbg="white")


pts <- SpatialPoints(composite.occ2[,c("longitude", "latitude")])
pts.over <- over(pts, er)

# merge:
d.eco.sampling <- cbind(composite.occ2, pts.over)
head(d.eco.sampling)
d.eco.sampling$localities <- as.factor(paste(d.eco.sampling$latitude,d.eco.sampling$longitude))

provs <- function(df) length(unique(df$PROV_CODE))
localities <- function(df) length(unique(df$localities))
  
gen_endemicity <- ddply(d.eco.sampling,.(genus),each(provs,localities),.progress="text")
endemic <- ifelse(gen_endemicity$provs == 1,1,0)
gen_endemicity <- data.frame(gen_endemicity,endemic)
colnames(gen_endemicity) <- c("genus","provs","localities","endemic")

d.eco.sampling <- merge(d.eco.sampling,gen_endemicity,by="genus")
### fix weird Tropical/Temperate split for province 17
#d.eco.sampling[d.eco.sampling$PROV_CODE == 17, "Lat_Zone"] <- "Tropical"
d.eco.sampling  <- na.omit(d.eco.sampling)
Lat_Zone <-ifelse(d.eco.sampling$PROV_CODE == 17,"Tropical",as.character(d.eco.sampling$Lat_Zone))
d.eco.sampling$Lat_Zone <- as.factor(Lat_Zone)
OBIS.genus.occs <- data.frame(d.eco.sampling$group,d.eco.sampling$genus,d.eco.sampling$provs, d.eco.sampling$localities.y)
colnames(OBIS.genus.occs) <- c("group","genus","provinces","localities")

OBIS.genus.occs <- unique(OBIS.genus.occs)
write.table(OBIS.genus.occs,"~/Dropbox/nescent_extinction_map/Final data/OBIS.genus.occs.csv",sep=",")


sampling <- function(df) length(df$genus)
richness <- function(df) length(unique(df$genus))
mean_provs <- function(df) {
  gen <- data.frame(df$genus,df$provs,df$endemic)
  colnames(gen) <- c("genus","provs","endemic")
  unique_gen <- unique(gen)
  mean(unique_gen$provs)}
mean_endemic <- function(df) {
  gen <- data.frame(df$genus,df$provs,df$endemic)
  colnames(gen) <- c("genus","provs","endemic")
  unique_gen <- unique(gen)
  mean(unique_gen$endemic)}


OBIS.sampling <- ddply(d.eco.sampling,.(group,Lat_Zone,PROV_CODE),each(sampling,richness,mean_provs,mean_endemic),.progress="text")
colnames(OBIS.sampling) <- c("class","lat_zone","PROV_CODE","records","genera","mean_provs","mean_endemic")


OBIS.sampling.all <- ddply(d.eco.sampling,.(Lat_Zone,PROV_CODE),each(sampling,richness,mean_provs,mean_endemic),.progress="text")
colnames(OBIS.sampling.all) <- c("lat_zone","PROV_CODE","records","genera","mean_provs","mean_endemic")
OBIS.sampling.all$class <- rep("all",length(OBIS.sampling.all[,1]))

OBIS.sampling.and.richness <- rbind(OBIS.sampling,OBIS.sampling.all)
write.table(OBIS.sampling.and.richness,"~/Dropbox/nescent_extinction_map/Final data/OBIS.sampling.and.richness.csv",sep=",")

#OBIS.all <- subset(OBIS.sampling.and.richness,OBIS.sampling.and.richness$class == "all")
OBIS.all <- na.omit(drop.levels(subset(OBIS.sampling.and.richness,OBIS.sampling.and.richness$class != "Foraminifera")))

pdf()
p <- ggplot(OBIS.all,aes(log10(records),mean_endemic)) + geom_point(size=2,pch=16) + geom_smooth(method="lm") + geom_point(data=OBIS.all,aes(log10(records),mean_endemic,colour=lat_zone),size=2,pch=16) + xlab(expression("Log10 records")) + ylab(expression("Proportion endemic genera")) + opts(axis.text.x = theme_text(size=6)) + opts(axis.text.y = theme_text(size=6))+ opts(panel.background = theme_rect(colour="black", size =1, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank()) + facet_wrap(~class,ncol=3,scales="free") + theme(strip.background = element_rect(fill = "white"))
print(p)

ggsave(p,file="~/Dropbox/nescent_extinction_map/r/crossplot.records.vs.prop.endemic.pdf",width=8,height=5.75)
dev.off()
