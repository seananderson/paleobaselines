# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       May 29, 2012
# Last modified: May 30, 2012
# Purpose:       Experiment with kriging the taxonomic richness data.
# This code will be moved to more appropriate files later.
# ====================================================================

out2 <- ddply(data.finite, "MatchTaxon", function(x) {
  grid.ext.data.2(x, grid.object = global_45x14, combine.type = "number", return.events = FALSE)
})

j <- subset(out2, MatchTaxon == "az")


require(geoR)
az.geo <- as.geodata(j[,2:4])

# get the variogram to feed to the kriging:
v <- variog(az.geo)
vf <- variofit(v)
#vf <- likfit(az.geo)

# points to predict on
predict.loc <- expand.grid(seq(1, 45), seq(1, 14))
names(predict.loc) <- c("PID", "SID")

# the kriging itself
az.geo.krig <- krige.conv(az.geo, locations = predict.loc, krige = krige.control(obj.m = vf))

# visualize it:
#image(az.geo.krig)

# combine back into the same format as we started with:
junk <- data.frame(PID = predict.loc$PID, SID = predict.loc$SID, Z = az.geo.krig$predict)
junk$type <- "kriged"
j$MatchTaxon <- NULL
j$type <- "raw"

d <- merge(junk, j, all = TRUE)
#ggplot(d, aes(PID, SID)) + geom_tile(aes(fill = Z)) + facet_wrap(~type) + scale_fill_gradient(low = "white", high = "steelblue", breaks = seq(0, 50, 2))
ggplot(d, aes(PID, SID)) + geom_tile(aes(fill = Z)) + facet_wrap(~type) + scale_fill_gradient(low = "white", high = "steelblue", breaks = seq(0, 50, 1))


### gam version
library(mgcv)
m <- gam(Z~s(PID, SID), data = j2)
p.m <- predict(m, newdata = predict.loc)
j3 <- data.frame(PID = predict.loc$PID, SID = predict.loc$SID, Z = p.m, type = "gam")
d <- merge(d, j3, all = TRUE)

ggplot(d, aes(PID, SID)) + geom_tile(aes(fill = Z)) + facet_wrap(~type) + scale_fill_gradient(low = "white", high = "steelblue", breaks = seq(0, 50, 1))

### gstat version:
library(gstat)
j5 <- subset(out2, MatchTaxon == "z")

coordinates(j5) = ~PID+SID
gridded(predict.loc) = ~PID+SID
#m <- vgm(1, "Exp", 800, 1)
#v <- variogram(log(Z) ~ 1, data = j5)

j5.idw <- krige(Z~1, j5, predict.loc)
predict.loc <- expand.grid(seq(1, 45), seq(1, 14))
names(predict.loc) <- c("PID", "SID")
junk5 <- data.frame(PID = predict.loc$PID, SID = predict.loc$SID, Z = j5.idw["var1.pred"]$var1.pred, type = "idw kriged")
j5$MatchTaxon <- NULL
j5$type <- "raw"

d2 <- merge(j5, junk5, all = TRUE)
ggplot(d2, aes(PID, SID)) + geom_tile(aes(fill = Z)) + facet_wrap(~type) + scale_fill_gradient(low = "white", high = "steelblue", breaks = seq(0, 50, 1))


#> x <- krige(log(zinc) ~ 1, meuse, meuse.grid, 
   #model = v, nmax = 40, nmin = 20, maxdist = 1000,
   #block = c(40,40))



# now with ordinary kriging:
j6 <- subset(out2, MatchTaxon == "Neogastropoda")
j6 <- subset(out2, MatchTaxon == "Cidaroida")
j6 <- subset(out2, MatchTaxon == "Arbacioida")
j6 <- subset(out2, MatchTaxon == "Neritoina")
coordinates(j6) = ~PID+SID
gridded(predict.loc) = ~PID+SID

#m <- vgm(1, "Exp", 800, 1)

v <- variogram(log(Z) ~ 1, data = j6)
m <- fit.variogram(v, vgm(1, "Exp", 2, 1))
j6.ord.krig <- krige(log(Z) ~ 1, j6, predict.loc, model = m)

predict.loc <- expand.grid(seq(1, 45), seq(1, 14))
names(predict.loc) <- c("PID", "SID")
junk6 <- data.frame(PID = predict.loc$PID, SID = predict.loc$SID, Z = exp(j6.ord.krig["var1.pred"]$var1.pred), type = "ord. kriged")
j6$MatchTaxon <- NULL
j6$type <- "raw"

d6 <- merge(j6, junk6, all = TRUE)
ggplot(d6, aes(PID, SID)) + geom_tile(aes(fill = log(Z))) + facet_wrap(~type) + scale_fill_gradient(low = "white", high = "steelblue", breaks = seq(0, 2, 0.5))


gridded(predict.loc) = ~PID+SID
j6.idw <- krige(log(Z)~1, j6, predict.loc)
predict.loc <- expand.grid(seq(1, 45), seq(1, 14))
names(predict.loc) <- c("PID", "SID")
junk6 <- data.frame(PID = predict.loc$PID, SID = predict.loc$SID, Z = exp(j6.idw["var1.pred"]$var1.pred), type = "idw")
j6$MatchTaxon <- NULL
j6$type <- "raw"

d6 <- merge(d6, junk6, all = TRUE)
ggplot(d6, aes(PID, SID)) + geom_tile(aes(fill = log(Z))) + facet_wrap(~type) + scale_fill_gradient(low = "white", high = "steelblue", breaks = seq(0, 2, 0.5))

########
#######
# try for each taxon:

library(plyr)
#d_ply(out2, "MatchTaxon", function(x) {
for(i in unique(out2$MatchTaxon)) {

x <- subset(out2, MatchTaxon == i)

#browser()
      if(nrow(x > 200)) {
        print(unique(x$MatchTaxon))
        print("yes")
        browser()
coordinates(x) = ~PID+SID
gridded(predict.loc) = ~PID+SID

v <- variogram(log(Z) ~ 1, data = x)
m <- fit.variogram(v, vgm(1, "Exp", 2, 1))
x.ord.krig <- krige(log(Z) ~ 1, x, predict.loc, model = m)

predict.loc <- expand.grid(seq(1, 45), seq(1, 14))
names(predict.loc) <- c("PID", "SID")
junk6 <- data.frame(PID = predict.loc$PID, SID = predict.loc$SID, Z = exp(x.ord.krig["var1.pred"]$var1.pred), type = "ord. kriged")
x$MatchTaxon <- NULL
x$type <- "raw"

d6 <- merge(x, junk6, all = TRUE)
#ggplot(d6, aes(PID, SID)) + geom_tile(aes(fill = log(Z))) + facet_wrap(~type) + scale_fill_gradient(low = "white", high = "steelblue", breaks = seq(0, 2, 0.5))

gridded(predict.loc) = ~PID+SID
x.idw <- krige(log(Z)~1, x, predict.loc)
predict.loc <- expand.grid(seq(1, 45), seq(1, 14))
names(predict.loc) <- c("PID", "SID")
junk6 <- data.frame(PID = predict.loc$PID, SID = predict.loc$SID, Z = exp(x.idw["var1.pred"]$var1.pred), type = "idw")
x$MatchTaxon <- NULL
x$type <- "raw"

d6 <- merge(d6, junk6, all = TRUE)
p <- ggplot(d6, aes(PID, SID)) + geom_tile(aes(fill = log(Z))) + facet_wrap(~type) + scale_fill_gradient(low = "white", high = "steelblue", breaks = seq(0, 2, 0.5))
ggsave(filename = paste("../fig/", unique(x$MatchTaxon), "krig-test.pdf", sep = ""), width = 7, height = 4)
      } 
}

