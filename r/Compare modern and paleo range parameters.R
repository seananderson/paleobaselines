library(randomForest)
library(matrixStats)
require(verification)
library(gdata)
library(party)
library(plyr)
library(Hmisc)

load("~/Dropbox/nescent_extinction_map/Final data/standardized.predictors.Cenozoic.OBIS.rda")
data <- standardized.cenozoic

#data <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Modern and PaleoDB ranges FINAL 2.0.csv",header = TRUE,stringsAsFactors = FALSE)

# > unique(data$Interval_Name)
#  [1] "Danian"              "Selandian-Thanetian" "Ypresian"            "Lutetian"           
#  [5] "Bartonian"           "Priabonian"          "Rupelian"            "Chattian"           
#  [9] "Lower Miocene"       "Middle Miocene"      "Upper Miocene"       "Pliocene"           
# [13] "Pleistocene"         "Spalding_raw"        "Spalding_merged"    

### select first interval to analyze; take subset if desired

data1 <- drop.levels(subset(data,data$stage == "Plio-Pleistocene"))


#data1 <- drop.levels(subset(data,data$Interval_Name == "Lower Miocene" & data$occurrences > 1))

#c("Lower Miocene", "Middle Miocene", "Upper Miocene", "Pliocene", "Pleistocene")))

### select interval to analyze with forest model from first interval
data2 <- drop.levels(subset(data,data$stage == "Modern"))
## limit analyses to groups shared between both intervals

newdata <- merge(data1,data2,by="genus",all.x=FALSE,all.y=FALSE)
quartz()
p <- ggplot(newdata,aes(lat.range.y,lat.range.x,colour=class.x ))
p + geom_point() + geom_smooth(method="lm") + geom_abline()

p <- ggplot(data,aes(richness,colour=class))
p + geom_density() + facet_wrap(~stage)



Latrange.x <- newdata$MaxLat.x-newdata$MinLat.x
Latrange.y <- newdata$MaxLat.y-newdata$MinLat.y

newdata <- cbind(newdata,Latrange.x,Latrange.y)

p <- ggplot(newdata,aes(Latrange.x,Latrange.y))
p + geom_point(pch=1) + geom_smooth(method="lm") + coord_equal() + geom_abline()


model <- lm(MinLat.x ~ MinLat.y,data=newdata)
summary(model)



