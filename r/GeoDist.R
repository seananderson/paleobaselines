#-------------------------------------------
#
# Function geodist()
#
# (c) Simon Joly, Massey University, 2008
#
#-------------------------------------------

geodist <- function(data, col.swap = FALSE)

# The function geo.dist computes a matrix of geographic distance from a table of latitudes
# and longitude in decimal degrees. The geographic distance is calculated takes into
# account the curvature of the earth, using the law of cosines, which is only strictly
# valid on a sphere. Note that the length of the earth radius used to calculate the distance between
# two points is the mean radius between these two points.
#
#
# Parameters:
#
# data:       A data frame containning the lat and long of several sites in decimal degrees
# col.swap:   A boolean variable saying whether the columns of the matrix should be interchange.
#               By default, the function assumes that latitudes are in the first column. If
#               longitudes are in the furst column, you should use col.swap = TRUE.
#
# Notes: - The function assumes that latitudes are in the first column
#        - For the estimations, the radius of the earth is averaged between the 
#            two latitudes compared
#        - Distances are returned in kilometers (km)
#

{
  mat <- mat.or.vec(nrow(data),nrow(data))
  for (i in 1:nrow(data)) {
    for (j in 1:i) {
      if (i==j) {
        mat[i,i]=0
        next
        }
      if (col.swap==FALSE) mat[i,j]=getdistance(data[i,1],data[i,2],data[j,1],data[j,2])
	else mat[i,j]=getdistance(data[i,2],data[i,1],data[j,2],data[j,1])
      }
    }
  colnames(mat) <- rownames(data)
  rownames(mat) <- rownames(data)
  mat <- as.dist(mat)
  return(mat)
}

#-------------------------------------------
#
# Function getdistance()
#
# Simon Joly, Massey University, July 2008
#
#-------------------------------------------

getdistance <- function(lat1,lon1,lat2,lon2)

# Parameters:  the latitude and longitude of two points between which one wants to calculate 
#              the distance, in kilometers

{
  if ( (lat1==lat2) && (lon1==lon2)) return(0)

  #Converting degrees to radians
  DE2RA = 0.01745329252
  lat1 = lat1*DE2RA;
  lon1 = lon1*DE2RA;
  lat2 = lat2*DE2RA;
  lon2 = lon2*DE2RA;

  # a: earth radius at the equator
  a=6378.137
  # b: earth radius at the poles
  b=6356.7523

  d = sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2)*cos(lon1 - lon2)
  mean.lat=(lat1+lat2)/2
  earth.radius=sqrt( ( (a^2*cos(mean.lat))^2+(b^2*sin(mean.lat))^2) / ((a*cos(mean.lat))^2+(b*sin(mean.lat))^2) )
  return(earth.radius * acos(d))
  }