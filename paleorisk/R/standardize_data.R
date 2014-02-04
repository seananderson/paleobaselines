#' Standardize paleontological and contemporary datasets
#'
#' @param dat The data frame to standardize.
#' @param interval_name The geologic stage name to work with.
#' @param min_lat_bin TODO
#' @param max_lat_bin TODO
#' @param lat_range_bin TODO
#' @param mean_lat_bin TODO
#' @param mean_lat_zone_bin TODO
#' @param tropical_only_bin
#' @param great_circle_bin
#' @param num_bins Number of bins to use for standardizing richness,
#' occurrences, occupancy.
#' @param num_risk_quantiles Number of quantiles to use for standardizing
#' extinction risk, if desired.
#' @param input_ranges Either \code{"Interpolated"} or \code{"Raw"}.
#' @param min_pbdb_occurrences Minimum number of genera per province of PBDB
#' occurrences.
#' @param min_modern_occurrences Minimum number of genera per province of
#' contemporary occurrences.
#' @param minimum_duration Minimum duration (in stages) for a genera included:
#' set "1" to include singletons, and "2" to include them.
#'
#' @return
#' A standardized data frame.
#'
#' @export

standardize_data <- function(dat, interval_name, min_lat_bin = 10, max_lat_bin = 10,
  lat_range_bin = 10, mean_lat_bin = 10, mean_lat_zone_bin = 0.5,
  tropical_only_bin = 1, great_circle_bin = 2000, num_bins = 10,
  num_risk_quantiles = 10, input_ranges = "Interpolated",
  min_pbdb_occurrences = 2, min_modern_occurrences = 2,
  minimum_duration = 1) {

  message(paste("Standardizing", interval_name, "interval"))

  data <- gdata::drop.levels(subset(dat, dat$use==1 & dat$occurrences >=
      min_pbdb_occurrences & dat$OBIS_occurrences >= min_modern_occurrences &
      dat$Num_Stage >= minimum_duration & dat$class != "Foraminifera" &
      (dat$OBIS_Ranges == input_ranges | dat$OBIS_Ranges == 0)))

  mod_data <- gdata::drop.levels(subset(data, data$Interval_Name %in% interval_name))

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
  great.circle <- ceiling(gcd/great_circle_bin)*great_circle_bin
  lats <- data.frame(abs(data1$MinLat), abs(data1$MaxLat))

  #paleolats & longs rounded
  min.lat <- ceiling(apply(lats, 1, min)/min_lat_bin)*min_lat_bin
  max.lat <- ceiling(apply(lats, 1, max)/max_lat_bin)*max_lat_bin
  lat.range <- max.lat-min.lat
  mean.lat <- round(abs(data1$mean_lat)/mean_lat_bin)*mean_lat_bin
  mean.lat.zone <- round(data1$mean_lat_zone/mean_lat_zone_bin)*mean_lat_zone_bin
  tropical_only <- ifelse(max.lat > 30, 0, 1)

  Ex <- data1$Extinct.in.stage

  stage <- rep(stage_name, length(Ex))
  stage_top <- rep(stage_top, length(Ex))

  stand_dat <- gdata::drop.levels(na.omit(data.frame(stage, stage_top, class, group,
        genus, richness, occupancy, occurrences, min.lat, max.lat, lat.range,
        mean.lat, mean.lat.zone, great.circle, tropical_only, Ex)))


  return(stand_dat)
}
