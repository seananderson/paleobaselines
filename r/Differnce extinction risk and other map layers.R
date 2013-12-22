
# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jul 02, 2012
# Last modified: May 22, 2013
# Purpose:       try ggplot plotting and fortify df
# Modified by Seth Finnegan Aug. 16 2012
# ====================================================================
library(maptools)
library(gdata)
library(ggplot2)
gpclibPermit()

### load province occupancy file and subset depending on whether using raw or interpolated ranges
load("~/Dropbox/nescent_extinction_map/Final data/Modern_Province_Occupancy.rda")
d.eco.filled <- Modern_Province_Occupancy

d.eco.filled <- drop.levels(subset(Modern_Province_Occupancy,Modern_Province_Occupancy$Ranges == Input_ranges))
drops <- c("Ranges")
d.eco.filled <- d.eco.filled[,!(names(d.eco.filled) %in% drops)]


land <- readShapePoly("~/Dropbox/nescent_extinction_map/data/110m-land/110m_land.shp")
land.fort <- fortify(land)

### load province risk estimates previously generated in "Spalding.Map.FINAL.R"
load("~/Dropbox/nescent_extinction_map/Final data/by.prov.all.rda")
by.prov <- by.prov.all

###read in Halpern and Burrows layers
Impacts <- read.csv("~/Dropbox/nescent_extinction_map/data/Halpern_Burrows_by_Spalding.csv",header = TRUE)
Impacts <- data.frame(Impacts$PROV_CODE,scale(Impacts$Mean_Halpern_Province),scale(Impacts$Mean_Burrows_Province))
colnames(Impacts) <- c("PROV_CODE","Halpern","Burrows")
by.prov <- merge(by.prov,Impacts,by = "PROV_CODE")

er.df <- join(er.df, by.prov, by = "PROV_CODE")

