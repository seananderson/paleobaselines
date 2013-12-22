# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       May 31, 2012
# Last modified: Jun 01, 2012
# Purpose:       Experiment with soap-film smoothing of richness data
# ====================================================================

 For reprojection of
sp-objects you can use the spTransform function (rgdal).

> Can I work with gstat with lat/long data? 
Ehm, no/yes. If you wait for a few days to get gstat version 0.9-35 
(just uploaded to CRAN), it does use great circle distances for distance 
evaluations if data are longlat. (i.e., they evaluate FALSE when passed 
to is.projected())

Make sure that for e.g. kriging you use a variogram 
model that is positive definite on the sphere; gstat does not check 
this. It seems that the exponentional model should work. Be aware that 
this is a mostly untested domain, for gstat at least. (You may want to 
look at package fields as an alternative).

# S. Wood example:
## Fit a soap film smooth to the noisy data
#bnd = boundary
b <- gam(z~s(x,y,k=c(30,15),bs="so",xt=list(bnd=bnd)),knots=knots)
plot(b) ## default plot
vis.gam(b,plot.type="contour") ## prettier version

## much prettier plot, based on prediction non a fine grid.
## Note: block.size parameter set to -1 to avoid splitting 
## prediction data into smaller chunks (which would involve
## duplication of PDE solving).
fv <- predict(b,newdata=data.frame(x=xx,y=yy),block.size=-1)
image(xm,yn,matrix(fv,100,100),xlab="x",ylab="y")
contour(xm,yn,matrix(fv,100,100),add=TRUE)
lines(bnd[[1]],lwd=2);lines(bnd[[2]],lwd=2)
