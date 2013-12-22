data <- load("~/Dropbox/nescent_extinction_map/Final data/genus_grid_merged.rda")
OBIS.genera <- datamerged
genus.Matches <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Modern genera matchtaxa.csv", header = TRUE)
OBIS.for.plot <- merge(OBIS.genera,genus.Matches,by.x = "genus",all.x = FALSE)
latnow <- ifelse(OBIS.for.plot$lat_mid < 30,"t","nt")
polarnow <- ifelse(OBIS.for.plot$lat_mid > 60,"p","np")
OBIS.for.plot <- data.frame(OBIS.for.plot,latnow,polarnow)
save(OBIS.for.plot,file = "OBIS.for.plot.rda")
