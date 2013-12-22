# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       May 30, 2012
# Last modified: Jun 04, 2012
# Purpose:       Smooth the richness occurrence data with inverse
# distance weighting. This time do it spherically with great circle
# distance.
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

# switch to mid points
mid.points <- ddply(global_45x14_grid_PID_SID, c("PID", "SID"), function(x){
  X.mid <- min(c(x[x$POS == 2,"X"], x[x$POS == 1,"X"])) + abs(x[x$POS == 1, "X"] - x[x$POS == 2, "X"])/2
  Y.mid <- min(c(x[x$POS == 3,"Y"], x[x$POS == 1,"Y"])) + abs(x[x$POS == 1, "Y"] - x[x$POS == 3, "Y"])/2
  data.frame(long.mid = X.mid, lat.mid = Y.mid)
})

rrdat <- merge(rrdat, mid.points, all = TRUE)

# [s]mooth [r]ichness [dat]a
srdat <- ddply(rrdat, "MatchTaxon", function(x) {

print(paste("Smoothing", as.character(unique(x$MatchTaxon))))
               #browser()

# TODO hard coded for 45x14 right now
predict.loc <- expand.grid(sort(unique(mid.points$long.mid)), sort(unique(mid.points$lat.mid)))
names(predict.loc) <- c("long.mid", "lat.mid")
predict.loc.sp <- predict.loc # because sp package functions will change class
x.sp <- x # temporary object because the sp functions will change the class

coordinates(predict.loc.sp)=~long.mid+lat.mid
proj4string(predict.loc.sp)=CRS("+proj=longlat")

coordinates(x.sp)=~long.mid+lat.mid
proj4string(x.sp)=CRS("+proj=longlat")

idw.out = idw(log(Z)~1,x.sp,predict.loc.sp)

predict.loc$ID <- 1:nrow(predict.loc)
predict.loc.w.PID.SID <- merge(predict.loc, mid.points, all = TRUE)
# now bring back to original order!!!
predict.loc.w.PID.SID <- predict.loc.w.PID.SID[order(predict.loc.w.PID.SID$ID), ]

out <- data.frame(PID = predict.loc.w.PID.SID$PID, SID = predict.loc.w.PID.SID$SID, richness = exp(idw.out$var1.pred))
out
})

