# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jun 07, 2012
# Last modified: Jun 07, 2012
# Purpose:       I had the projection a bit off before... move the
# longitudes back 8 degrees east. Only needed to be run once. From now
# on the rest of the code will generate correct files. This just saved
# me re-running the gridding code on the bathymetry data.
# ====================================================================

load("~/Dropbox/nescent_extinction_map/data/global_45x14_ext_rich_vel_hlp_20120605.rda")
d.45x14$PID <- d.45x14$PID + 1
d.45x14$PID[d.45x14$PID == 46] <- 1

d.45x14$long <- d.45x14$long + 8
d.45x14$long[d.45x14$long == 184] <- -176

save(d.45x14, file = "../data/global_45x14_ext_rich_vel_hlp_20120607.rda")

