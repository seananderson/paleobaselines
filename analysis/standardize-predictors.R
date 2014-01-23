library(Hmisc)
library(gdata)
library(matrixStats)

# specify values for data standardization:
Min.Lat.Bin <- 10
Max.Lat.Bin <- 10
Lat.Range.Bin <- 10
Mean.Lat.Bin <- 10
Mean.Lat.Zone.Bin <- .5
Tropical.Only.Bin <- 1
Great.Circle.Bin <- 2000
# choose number of bins to use for standardizing richness, occurrences, occupancy
Num_Bins <- 10
# choose number of quantiles to use for standardizing extinction risk, if desired (see below)
Num_risk_quantiles <- 10
# choose "Interpolated" to use within-realm province interpolated data, otherwise choose "Raw"
Input_ranges <- "Interpolated"
Min_PBDB_Occurrences <- 2
Min_Modern_Occurrences <- 2
# Minimum number of genera for province to be plotted on Burrows and Halpern crossplots
Min.Prov.Genera <- 100
# Minimum duration (in stages) for a genera included: set "1" to include singletons, and "2" to include them
Minimum_Duration <- 1
# choose the minimum number of equal-area grid cells occupied for inclusion
Min_Occupancy <- 1
# choose the minimum number of PaleoDB localities for inclusion
Min_PBDB_Loc <- 1
# choose the minimum number of OBIS localities for inclusion
Min_OBIS_Loc <- 1
# choose the minimum number of Spalding provinces occupied for inclusion (applies only to modern genera)
Min_Prov <- 1
# set minimum great circle distance for inclusion (single occurrence PBDB genera assigned great.circle = 200)
Min_gcd <- 1

standardize_data <- function(x) {

  message(paste("Standardizing", x, "interval"))
  mod_data <- drop.levels(subset(data, data$Interval_Name %in% x))

  data1 <- mod_data
  stage_name <- data1$Interval_Name[1]
  stage_top <- data1$stage_top[1]

  ### create factors to analyze from prediction interval, round if desired
  genus <- data1$genus
  class <- data1$class
  group <- data1$MatchTaxon

  max.rich <- max(round(log(data1$richness), 0))
  richness <- round(log(data1$richness), 0)/max.rich

  max.occupancy <- max(round(log(data1$eac), 0))
  occupancy <- round(log(data1$eac), 0)/max.occupancy

  max.occurrences <- max(round(log(data1$occurrences), 0))
  occurrences <- round(log(data1$occurrences), 0)/max.occurrences

  gcd <- data1$gcd_corrected
  great.circle <- ceiling(gcd/Great.Circle.Bin)*Great.Circle.Bin
  lats <- data.frame(abs(data1$MinLat), abs(data1$MaxLat))

  #paleolats & longs rounded
  min.lat <- ceiling(apply(lats, 1, min)/Min.Lat.Bin)*Min.Lat.Bin
  max.lat <- ceiling(apply(lats, 1, max)/Max.Lat.Bin)*Max.Lat.Bin
  lat.range <- max.lat-min.lat
  mean.lat <- round(abs(data1$mean_lat)/Mean.Lat.Bin)*Mean.Lat.Bin
  mean.lat.zone <- round(data1$mean_lat_zone/Mean.Lat.Zone.Bin)*Mean.Lat.Zone.Bin
  tropical_only <- ifelse(max.lat > 30, 0, 1)

  Ex <- data1$Extinct.in.stage

  stage <- rep(stage_name, length(Ex))
  stage_top <- rep(stage_top, length(Ex))

  data1factors <- drop.levels(na.omit(data.frame(stage, stage_top, class, group,
        genus, richness, occupancy, occurrences, min.lat, max.lat, lat.range,
        mean.lat, mean.lat.zone, great.circle, tropical_only, Ex)))

  return(list(stand_data = data1factors))
}

### select whether to use within-realm interpolated or non-interpolated OBIS ranges
data <- readRDS("../data/modern-paleodb-ranges.rds")

data <- drop.levels(subset(data, data$use==1 & data$occurrences >= Min_PBDB_Occurrences &  data$OBIS_occurrences >= Min_Modern_Occurrences & data$Num_Stage >= Minimum_Duration & data$class != "Foraminifera" & (data$OBIS_Ranges == Input_ranges | data$OBIS_Ranges == 0)))

### merge in mean lats -a posteriori fix, should eventually be included in Simpson script
mean.lats <- readRDS("../data/genus-mean-occurence-lats.rds")

data <- merge(data, mean.lats)

stage_names <- list()
for(i in 1:length(unique(data$Interval_Name))) {
  stage_names[[i]] <- unique(data$Interval_Name)[i]
}

out <- lapply(stage_names, function(x) standardize_data(x))

data.out <- list()
for(i in 1:length(out)) {
  data.out[[i]] <- out[[i]]$stand_data
}

standardized.cenozoic <- do.call("rbind", data.out)
saveRDS(standardized.cenozoic,
  file = "../data/standardized-predictors-cenozoic-obis.rds")

