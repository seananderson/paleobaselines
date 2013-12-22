# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jan 26, 2012
# Last modified: Feb 07, 2012
# Purpose:       Take the Halpern data and shape it for mapping. This
# version shifts back to -180, 180 longitude coordinates.
# ====================================================================

grid.hal.data.2 <- function(halpern.data) {
  #browser()
  require(PBSmapping)
  source("eq.area.grid.2.R")
  grid <- eq.area.grid.2()
hlp <- halpern.data
hlp <- hlp[,c("X_COORD", "Y_COORD", "HumanImp")]
#hlp$X_COORD <- hlp$X_COORD + 180
#hlp$X_COORD <- hlp$X_COORD + 180
#browser()
## need to figure out why the cells are off by one - re-grid the data?
hlp$X_COORD[hlp$X_COORD <= -176] <- hlp$X_COORD[hlp$X_COORD <= -176] + 360
hlp$X_COORD <- hlp$X_COORD - 8
#hlp$X_COORD[hlp$X_COORD > 360] <- hlp$X_COORD[hlp$X_COORD > 360] - 360
#hlp$X_COORD[hlp$X_COORD > 340] <- hlp$X_COORD[hlp$X_COORD > 340] - 360
#browser()
names(hlp) <- c("X", "Y", "Z")
hlp$EID <- 1:nrow(hlp)
hlp <- as.EventData(hlp, projection = "LL") 
locData.hlp <- findPolys(hlp, grid, maxRows = 1e+07) 
pdata.hlp <- combineEvents(hlp, locData.hlp, FUN=function(x) mean(x, na.rm = TRUE))
names(pdata.hlp)[3] <- "Z_hlp"
pdata.hlp
}



