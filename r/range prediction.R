
load("~/Dropbox/nescent_extinction_map/Final data/Env.data.rda")
Env.data <- Env.data 
head(Env.data)
LatLong <- as.factor(paste(Env.data$CenterLat,Env.data$CenterLong))
Env.data <- data.frame(Env.data,LatLong)
testdata <- read.csv("~/Dropbox/nescent_extinction_map/data/testdata.csv",header = TRUE)
LatLong <- as.factor(paste(testdata$latitude,testdata$longitude))
testdata <- data.frame(testdata,LatLong)


trainingdata <- merge(testdata,Env.data, by = "LatLong")
length(trainingdata)
Env.factors <- data.frame(trainingdata[,8:27])
points <- data.matrix(trainingdata$latitude,trainingdata$longitude)

bc <- bioclim(Env.factors,points)