# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jan 26, 2012
# Last modified: Jan 30, 2012
# Purpose:       Take the Halpern data and shape it for mapping
# ====================================================================

grid.hal.data <- function(halpern.data) {
  require(PBSmapping)
  source("eq.area.grid.R")
  grid <- eq.area.grid()
hlp <- halpern.data
hlp <- hlp[,c("X_COORD", "Y_COORD", "HumanImp")]
hlp$X_COORD <- hlp$X_COORD + 180
hlp$X_COORD <- hlp$X_COORD + 180
hlp$X_COORD[hlp$X_COORD > 360] <- hlp$X_COORD[hlp$X_COORD > 360] - 360
hlp$X_COORD[hlp$X_COORD > 340] <- hlp$X_COORD[hlp$X_COORD > 340] - 360
names(hlp) <- c("X", "Y", "Z")
hlp$EID <- 1:nrow(hlp)
hlp <- as.EventData(hlp, projection = "LL") 
locData.hlp <- findPolys(hlp, grid, maxRows = 1e+07) 
pdata.hlp <- combineEvents(hlp, locData.hlp, FUN=function(x) mean(x, na.rm = TRUE))
names(pdata.hlp)[3] <- "Z_hlp"
pdata.hlp
}



