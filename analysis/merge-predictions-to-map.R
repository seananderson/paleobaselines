# Take ext. risk predictions and merge them onto modern Spalding et al. map
#
library(maptools)
library(gdata)
library(ggplot2)
library(plyr)
gpclibPermit()

# read in the modern genus-occupancy data:
# was:
# load("~/Dropbox/nescent_extinction_map/Final data/Modern_Province_Occupancy.rda")
d.eco.filled <- readRDS("../data/modern-province-occupancy.rds")
#d.eco.filled <- drop.levels(subset(d.eco.filled,d.eco.filled$class != "Foraminifera"))
Input_ranges <- "Interpolated"
d.eco.filled <- drop.levels(subset(d.eco.filled,d.eco.filled$Ranges == Input_ranges))
d.eco.filled$Ranges <- NULL

# bring in the modern predictions:
modern <- readRDS("../data/modern-predictions.rds")

# merge occurence data with risk predictions:
d.eco.filled <- merge(d.eco.filled, modern, by = "genus", all.x = FALSE)

# and calculate mean values by class-province or for the provinces overall:

by.prov.classes <- ddply(d.eco.filled, .(class, PROV_CODE), summarize, mean.ext
  = mean(mean_observed), median.ext = median(mean_observed), N.gen = length(unique(genus)),
  mean.occupancy = mean(occupancy), mean.occurrences = mean(occurrences),
  mean.min.lat = mean(min.lat), mean.max.lat = mean(max.lat), mean.mean.lat =
  mean(mean.lat), mean.gcd = mean(great.circle), mean.lat.range =
  mean(lat.range), mean.richness = mean(richness))
by.prov.classes <- subset(by.prov.classes, mean.ext < 0.03) # TODO note testing
# ggplot(by.prov.classes, aes(mean.ext)) + geom_histogram() + facet_wrap(~class)

by.prov.all <- ddply(d.eco.filled, .(PROV_CODE), summarize, mean.ext
  = mean(mean_observed), median.ext = median(mean_observed), N.gen = length(unique(genus)),
  mean.occupancy = mean(occupancy), mean.occurrences = mean(occurrences),
  mean.min.lat = mean(min.lat), mean.max.lat = mean(max.lat), mean.mean.lat =
  mean(mean.lat), mean.gcd = mean(great.circle), mean.lat.range =
  mean(lat.range), mean.richness = mean(richness))

by.prov.all2 <- ddply(by.prov.classes, .(PROV_CODE), summarize, mean.ext =
  mean(mean.ext))

#check <- ddply(d.eco.filled, c("class","PROV_CODE"), summarize, n = length(pred))
#dcast(check, PROV_CODE ~ class, value.var = "n")


# read in province area measurements and OBIS sampling data:
Prov.Areas <- read.csv("../data/spalding-province-areas.csv", header = TRUE,
  stringsAsFactors = FALSE)
Prov.Sampling <- read.csv("../data/obis.sampling.and.richness.csv", header =
  TRUE, stringsAsFactors = FALSE)
Prov.Data <- merge(Prov.Sampling, Prov.Areas, by = c("Zone", "PROV_CODE"),
  all.x = TRUE)
by.prov.classes <- merge(by.prov.classes, Prov.Data, by = c("class", "PROV_CODE"))
by.prov.all <- merge(by.prov.all, Prov.Data, by = "PROV_CODE")

# read in Halpern et al. and Burrows et al. layers
Impacts <- read.csv("../data/Halpern_Burrows_by_Spalding.csv", header = TRUE)
Impacts <- data.frame(Impacts$PROV_CODE, scale(Impacts$Mean_Halpern_Province),
  scale(Impacts$Mean_Burrows_Province))
colnames(Impacts) <- c("PROV_CODE", "Halpern", "Burrows")
by.prov.all <- merge(by.prov.all, Impacts, by = "PROV_CODE")
by.prov.all <- drop.levels(subset(by.prov.all, by.prov.all$class=="all"))
by.prov.classes <- merge(by.prov.classes, Impacts, by = "PROV_CODE")
by.prov.all$log_OBIS_records <- log(by.prov.all$OBIS_records)
by.prov.classes$log_OBIS_records <- log(by.prov.classes$OBIS_records)

by.prov.classes$Lat_Zone <- as.factor(ifelse(by.prov.classes$Zone=="Tropical","Tropical","Extratropical"))
by.prov.all$Lat_Zone <- as.factor(ifelse(by.prov.all$Zone=="Tropical","Tropical","Extratropical"))

save(by.prov.classes, file = "../data/by.prov.classes.rda")
save(by.prov.all, file = "../data/by.prov.all.rda")
#saveRDS(er.df, file = "../data/ecoregion-df.rds")
#saveRDS(land.fort, file = "../data/land-df.rds")

