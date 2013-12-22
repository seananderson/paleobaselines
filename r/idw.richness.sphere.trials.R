# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       May 30, 2012
# Last modified: May 31, 2012
# Purpose:       Smooth the richness occurrence data with inverse
# distance weighting.
# ====================================================================




require(plyr)
require(gstat)

# [r]aw [r]ichness [dat]a
rrdat <- ddply(data.finite, "MatchTaxon", function(x) {
  print(paste("Calculating", as.character(unique(x$MatchTaxon)), "richness"))
  grid.ext.data.2(x, grid.object = global_45x14, combine.type = "number", return.events = FALSE)
})

## try giving it lat and long instead of PID and SID:
load("../data/global_45x14_grid_PID_SID.rda")
rrdat <- merge(rrdat, global_45x14_grid_PID_SID, all = TRUE)
names(rrdat)[6:7] <- c("long", "lat")

# [s]mooth [r]ichness [dat]a
srdat <- ddply(rrdat, "MatchTaxon", function(x) {

print(paste("Smoothing", as.character(unique(x$MatchTaxon))))

# TODO hard coded for 45x14 right now
#predict.loc <- expand.grid(seq(1, 45), seq(1, 14))
predict.loc <- expand.grid(global_45x14$longitude, global_45x14$latitude)
names(predict.loc) <- c("long", "lat")

# This miraculously converts the objects to the required class.
# Annoying non-standard way of writing the code for the sp package.
#gridded(predict.loc) = ~PID+SID
#coordinates(x) = ~PID+SID
# just 9 points on a grid:

#x <- c(1,1,1,2,2,2,3,3,3)
#y <- c(1,2,3,1,2,3,1,2,3)
#xy <- cbind(x,y)
#S <- SpatialPoints(xy)
#class(S)
#plot(S)
#gridded(S) <- TRUE
predict.loc.SpatialPoints <- SpatialPoints(predict.loc)
gridded(predict.loc.SpatialPoints) <- TRUE

#gridded(predict.loc) = ~long+lat
coordinates(x) = ~long+lat

# idp is the inverse distance weighting power
# default is idp = 2
# bigger numbers make the weighting fall off faster
# i.e. more localized smoothing
#x.idw <- idw(log(Z)~1, x, predict.loc, idp = 4)
x <- subset(rrdat, MatchTaxon == "BenthicForaminifera")
x[which(x$long == -180),"Z"] <- 1000 # fake data for testing
x.idw <- idw(log(Z)~1, locations = ~long+lat, data = x, newdata = predict.loc)

names(x.idw)[3] <- "Z"
x.idw$Z <- exp(x.idw$Z)
x.idw$var1.var <- NULL
x.idw$type <- "idw"
x$type <- "raw"
junk <- merge(x, x.idw, all = TRUE)
ggplot(junk, aes(long, lat)) + geom_point(aes(colour = Z), cex = 7) + facet_wrap(~type) + scale_fill_gradient(low = "white", high = "steelblue", breaks = seq(0, 50, 1))

############################
# example from r help list from gstat author:
world = expand.grid(long=seq(-177.5,177.5,5),lat=seq(-87.5,87.5,5))
world.sp = SpatialPixels(SpatialPoints(world,CRS("+proj=longlat")))
plot(world.sp,axes=T)
pts=data.frame(long=runif(100,-180,180),lat=runif(100,-90,90),val=rnorm(100))
coordinates(pts)=~long+lat
proj4string(pts)=CRS("+proj=longlat")
points(pts,col='red')
# inverse distance interpolation on the sphere:
idw.out = idw(val~1,pts,world.sp)
image(idw.out, axes = TRUE, ylim = c(-90,90))
points(pts, pch=3)

# now try it with our data:
x <- subset(rrdat, MatchTaxon == "BenthicForaminifera")
x[which(x$long == -180),"Z"] <- 300 # fake data for testing
x$lat <- round(x$lat, 3) # for some reason, some aren't "exactly" the same

predict.loc <- expand.grid(global_45x14$longitude, global_45x14$latitude)
names(predict.loc) <- c("long", "lat")
predict.loc$lat <- round(predict.loc$lat, 3)

coordinates(predict.loc)=~long+lat
proj4string(predict.loc)=CRS("+proj=longlat")

coordinates(x)=~long+lat
proj4string(x)=CRS("+proj=longlat")

idw.out = idw(log(Z)~1,x,predict.loc)

#idw.out$var1.pred

# plot it:
x <- subset(rrdat, MatchTaxon == "BenthicForaminifera")
x$lat <- round(x$lat, 3) # for some reason, some aren't "exactly" the same
x[which(x$long == -180),"Z"] <- 300 # fake data for testing
x$type <- "raw"
predict.loc <- expand.grid(global_45x14$longitude, global_45x14$latitude)
names(predict.loc) <- c("long", "lat")
predict.loc$lat <- round(predict.loc$lat, 3)

smoothed <- data.frame(long = predict.loc$long, lat = predict.loc$lat, Z = exp(idw.out$var1.pred), type = "idw")

x <- x[,c("long", "lat", "Z", "type")]

junk <- rbind(smoothed, x)
#junk <- merge(x, smoothed, all = TRUE)

ggplot(junk, aes(long, lat)) + geom_point(aes(colour = Z), cex = 7) + facet_wrap(~type) + scale_fill_gradient(low = "white", high = "steelblue", breaks = seq(0, 50, 1))




4

# NO!!
#predict.loc.sp = SpatialPixels(SpatialPoints(predict.loc,CRS("+proj=longlat")))



############################




# TODO hard coded for 45x14 right now
# Need to recreate the object because gridded() above
# changed the class.
predict.loc <- expand.grid(seq(1, 45), seq(1, 14))
names(predict.loc) <- c("PID", "SID")

out <- data.frame(PID = predict.loc$PID, SID = predict.loc$SID, richness = exp(x.idw["var1.pred"]$var1.pred))
out
}
)

