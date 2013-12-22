# define function for extracting geographic range parameters

genbybin <- function(df){
#   write.table(unique(as.character(df$clgen1)), "genus.fail.txt")
  
  if(length(unique(na.omit(df$paleolatdec)))>2){
    events <- data.frame(EID=1:length(df$paleolatdec), X =
      as.numeric(as.vector(df$paleolngdec)),
                         Y=as.numeric(as.vector(df$paleolatdec)))
    events <- as.EventData(na.omit(events), projection='UTM')
    fc <- findCells(events, polys)
    eac <-length(unique(subset(fc,select=c(PID,SID)))[,1])
    
    lats <- data.frame(events, Z = events$Y) # take within cell lats and long
    lons <- data.frame(events, Z = events$X)
    Y <- combineEvents(lats, findPolys(lats, polys), mean)
    X <- combineEvents(lons, findPolys(lons, polys), mean)
    gcd <-max(rdist.earth(cbind(X$Z, Y$Z), miles=F))
    max.lat <- (max(Y$Z))
    min.lat <- (min(Y$Z))}else{    
      eac=1
      gcd=NA
      max.lat = (max(df$paleolatdec))
      min.lat = (min(df$paleolatdec))
    }
  
  class <- unique(as.character(df$class))
  order <- unique(as.character(df$order_name))
  family <- unique(as.character(df$family))
  clgen <- unique(as.character(df$clgen1))
  genus <- as.character(unique(df$occurrence.genus_name))
  bin <- unique(df$slc)
  rich <- length(unique(df$occurrence.species_name))
  FA <- unique(df$FA)
  LA <- unique(df$LA)
  
  life.habit <-  unique(as.character(df$life_habit))[1]
  taxon.environment <- unique(as.character(df$taxon_environment))[1]
  loco <- unique(as.character(df$locomotion))[1]
  diet <- unique(as.character(df$diet1))[1]
  clone <- unique(as.character(df$clonal))[1]
  eyes <- unique(as.character(df$vision))[1]
  
  trop <- table(df$latnow)
  pol <- table(df$polarnow)
  
  if(sum(trop)>3){
    trop <- table(df$latnow)
    sam <- subset(sampling, slc == unique(df$slc))
    vect = c(trop[2], trop[1], sam[,2], sam[,3])
    vectalt = c(trop[1], trop[2], sam[,3], sam[,2])
    vecnt = c(trop[1], trop[2], sam[,3], sam[,2])
    
    trop.affin.bayes <- ifelse(trop[2]>=trop[1], affin.bayes(vect), 1-affin.bayes(vectalt))
    
    trop.affin.binom <- affin.binom(vect)[2]
    ntrop.affin.binom <- affin.binom(vecnt)[2]
    
    vecp <- c(pol[2], pol[1], sam[,4], sam[,5])
    vecpalt <- c(pol[1], pol[2], sam[,5], sam[,4])
    vecnp <- c(pol[1], pol[2], sam[,5], sam[,4])
    
    polar.affin.bayes <- ifelse(pol[2]>=pol[1], affin.bayes(vecp), 1-affin.bayes(vecpalt))
      
    polar.affin.binom <- affin.binom(vecp)[2]
    npolar.affin.binom <- affin.binom(vecnp)[2]} else{
    
      polar.affin.bayes <- NA
      npolar.affin.binom <- NA
      polar.affin.binom <- NA
      trop.affin.bayes <- NA
      trop.affin.binom <- NA
      ntrop.affin.binom <- NA
      
    }
  out <- data.frame(clgen, class, order, family, genus, bin, FA, LA, rich, life.habit, taxon.environment, loco, diet, clone, eyes, tr.occs = trop[2], ntr.occs = trop[1], polar.occs = pol[2], npolar.occs = pol[1], trop.affin.bayes, trop.affin.binom, ntrop.affin.binom, polar.affin.bayes, polar.affin.binom, npolar.affin.binom, eac, gcd, max.lat, min.lat)
}

library(fields)
library(plyr)
library(PBSmapping)
library(gdata)
source("affinity.bayes.R")
source("affinity.binom.R")
tscale<-read.table("PBDB_stages.csv",header=TRUE,sep=',')

load("~/Dropbox/nescent_extinction_map/data/equal_area_grid/global_45x14.rda")


LatSeq <- global_45x14$latitude
LongSeq <- global_45x14$longitude
polys<- makeGrid (x=round(LongSeq,0), y=round(LatSeq,0),projection="UTM")


mydata<-read.csv('~/Dropbox/nescent_extinction_map/data/Occs.09.30.csv', header=TRUE, sep=',')

mydata<-subset(mydata,is.na(mydata$slc)==FALSE)

FA<-tapply(mydata$slc,mydata$clgen,min,na.rm=TRUE)
clgen<-as.character(names(FA))
#names(clgen)<-'clgen'
FA<-as.integer(FA)
FA1<-(cbind(clgen,FA))
FA1<-FA1[-c(1),]
data1<-merge(mydata,FA1,by.x='clgen',by.y="clgen",all.y=TRUE)

LA<-tapply(mydata$slc,mydata$clgen,max,na.rm=TRUE)
clgen<-names(LA)
#names(clgen)<-'clgen'
LA<-as.integer(LA)
LA1<-(cbind(clgen,LA))
LA1<-LA1[-c(1),]
mydata<-merge(data1,LA1,by.x='clgen',by.y="clgen",all.y=TRUE)


tmp.names <- as.character(mydata$order_name[which(mydata$order_name == "Carnivora" | mydata$order_name == "Cetacea" | mydata$order_name == "Sirenia" | mydata$order_name == "Testudines")])

tmp <- which(mydata$order_name == "Carnivora" | mydata$order_name == "Cetacea" | mydata$order_name == "Sirenia" | mydata$order_name == "Testudines")

mydata[tmp, ]$class_name 
mydata$class_name[tmp] <- tmp.names


data <- mydata
data<-subset(data,is.na(data$slc)==FALSE)
# fill is a dummy variable to have complete ends of lines

preflith<-c("p","np")
preflat<-c("t","nt")

# makes a unique genus level id, don't want to get algae and brachs mixed up
valid.cl <- unique(data$class)
clgens<-as.character()
for(i in 1:length(valid.cl))
{
  x<-subset(data,class==valid.cl[i])
  y<-paste(x$class, x$occurrence.genus_name)
  clgens<-append(clgens,y)
}
clgen1<- paste(data$class, data$occurrence.genus_name)
data<-cbind(clgen1,data)

#sampling of latitudinal occurrences
tp <- table(data$latnow, data$slc)
pp <- table(data$polarnow, data$slc)

sampling <- data.frame(slc = 61:73, t = tp[2,], nt = tp[1,], p = pp[2,], np = pp[1,])

source("process.funct.R")
  
bybin <- ddply(data, .(clgen1, slc), genbybin, .progress="text")

coral.groups <- numeric(length = nrow(bybin))
group1 <- "Acroporidae"
group2 <- "Faviidae"
group3 <- "Other"

coral.groups[which(bybin$family == group1)] <- "group1"
coral.groups[which(bybin$family == group2)] <- "group2"
coral.groups[which(bybin$family != group1 & bybin$family != group2 & bybin$order == "Scleractinia")] <- "other"
coral.groups[which(bybin$order != "Scleractinia")] <- NA

data.out <- data.frame(bybin, coral.groups = coral.groups)

write.csv(data.out, "new.map.data.paleo.csv", row.names=FALSE)
write.csv(data.out, "~/Dropbox/nescent_extinction_map/new.map.data.paleo.csv", row.names=FALSE)



