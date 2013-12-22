# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jul 02, 2012
# Last modified: Jul 03, 2012
# Purpose:       try ggplot plotting and fortify df
# Modified by Seth Finnegan Sep. 5 2012
# ====================================================================
library(maptools)
library(gdata)
library(ggplot2)
library(untb)
gpclibPermit()
d.eco <- read.csv("/Users/Seth/Dropbox/nescent_extinction_map/data/N records per genus per province.csv",header=TRUE)
land <- readShapePoly("/Users/Seth/Dropbox/nescent_extinction_map/data/110m-land/110m_land.shp")
land.fort <- fortify(land)


extEST <- read.csv("/Users/Seth/Dropbox/nescent_extinction_map/data/Risk estimates by stage.csv",header = TRUE)
genus <- extEST$genus
extNEW <- extEST$MeanRisk
New.Ex.Est <- data.frame(extNEW,genus)
colnames(New.Ex.Est) <- c("new.ext","genus")
d.eco <- drop.levels(unique(merge(d.eco,New.Ex.Est,by.x = "genus",all.x = FALSE)))
d.eco <- drop.levels(unique(data.frame(d.eco$class,d.eco$MatchCat,d.eco$genus,d.eco$new.ext, d.eco$PROV_CODE,d.eco$RLM_CODE,d.eco$records)))
colnames(d.eco) <- c("class","MatchTaxon","genus","new.ext","PROV_CODE","RLM_CODE","records")
require(maptools)
require(ggplot2)
require(plyr)

genus2 <- d.eco$genus
Prov <- d.eco$PROV_CODE
recs <- d.eco$records
temp2 <- unique(data.frame(genus2,Prov,recs))
NumProv <- function(df)length(df$Prov)
NumRec  <- function(df)sum(df$recs)
Ranges <- ddply(temp2,.(genus2),each(NumProv,NumRec))
Endemic <- data.frame(Ranges$genus2,ifelse(Ranges$NumProv == 1,1,0),Ranges$NumRec,Ranges$NumProv)
colnames(Endemic) <- c("genus","endemic","recs","N.provinces")
Endemic <- subset(Endemic,Endemic$recs > 1)
d.eco <- merge(d.eco,Endemic,by="genus")

p <- ggplot(d.eco, aes(round(log(records),0),N.provinces,group=round(log(records))))
p + geom_jitter(alpha =.4) + geom_boxplot()

er <- readShapePoly("/Users/Seth/Dropbox/nescent_extinction_map/data/MEOW2/meow_ecos.shp")
er@data$id = rownames(er@data)
er.points = fortify(er, region = "id")
er.df <- join(er.points, er@data, by = "id")

### select "RLM_CODE" to plot realms,"PROV_CODE" to plot provinces

mean.ext <- function(df)mean(df$new.ext)
N.gen <- function(df)length(df$new.ext)
N.rec <- function(df)sum(df$records)
N.endemic <- function(df)sum(df$endemic)
Simpson <- function(df)simpson(df$records)



plotdata <- drop.levels(subset(d.eco, d.eco$class != "iAnthozoa"))
by.prov <- ddply(plotdata,.(PROV_CODE),each(mean.ext,N.gen,N.rec,N.endemic,Simpson))
Prop.Endemic <- by.prov$N.endemic/by.prov$N.gen
GeneraPerRecord <- by.prov$N.gen/by.prov$N.rec
by.prov <- data.frame(by.prov,Prop.Endemic,GeneraPerRecord)
SampleCorrected <- lm(mean.ext ~ log10(N.gen)+log10(N.rec)+(log10(N.gen)*log10(N.rec)),by.prov)
NormRisk <- data.frame(SampleCorrected$residuals)
colnames(NormRisk) <- c("NormRisk")
by.prov <- data.frame(by.prov,NormRisk)
quartz("map",13,7)
er.df$N.rec <- NULL
er.df <- join(er.df, by.prov, by = "PROV_CODE")
er.df <- drop.levels(subset(er.df,er.df$N.gen > 0))


ggplot(er.df) + aes(long, lat, group=group,fill=NormRisk) + geom_polygon() + scale_fill_gradient(low = "yellow", high = "red",name = "Risk residuals") + geom_polygon(data = map_data("world"), aes(long, lat, group = group), fill = "black") + theme(panel.background = element_rect(colour="darkgray", size =1, fill = "white"))  + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) + ylab(expression("Latitude")) + xlab(expression("Longitude")) + coord_equal(ylim = c(-85,90),xlim = c(-180,180))

quartz()
p <- ggplot(by.prov,aes(log10(N.rec),NormRisk,label=PROV_CODE))
p + geom_text() + geom_smooth(method = "lm") + coord_cartesian(xlim = c(-.005,.065))


plot(by.prov$mean.ext,by.prov$NormRisk)

plot(log(by.prov$GeneraPerRecord),by.prov$mean.ext)

