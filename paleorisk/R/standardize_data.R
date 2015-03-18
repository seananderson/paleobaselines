#' Standardize paleontological and contemporary datasets
#'
#' @param dat The data frame to standardize.
#' @param min_lat_bin TODO
#' @param max_lat_bin TODO
#' @param lat_range_bin TODO
#' @param mean_lat_bin TODO
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

standardize_data <- function(dat, min_lat_bin = 10, max_lat_bin = 10,
  lat_range_bin = 10, mean_lat_bin = 10,
  tropical_only_bin = 1, great_circle_bin = 2000, num_bins = 10,
  min_modern_occurrences = 2) {

  if(unique(dat$Interval_Name[1]) %in% c("Modern_merged", "Modern_raw")) {
    cull.taxa <- c("Bivalvia", "Gastropoda")
    dat$cull.val <- ifelse(dat$class %in% cull.taxa, min_modern_occurrences, 1)
    dat <- dat[(dat$occurrences >= dat$cull.val), ]
    data1  <- droplevels(dat)
  } else {
    data1  <- droplevels(dat)
  }

  stage_name <- data1$Interval_Name[1]
  stage_top <- data1$stage_top[1]

  ### create factors to analyze from prediction interval, round if desired
  genus <- data1$genus
  class <- data1$class
  group <- data1$MatchTaxon

  max.rich <- max(log(data1$richness))
  richness <- round(log(data1$richness)/max.rich, 1)

  max.occupancy <- max(log(data1$eac))
  occupancy <- round(log(data1$eac)/max.occupancy, 1)

  max.occurrences <- max(log(data1$occurrences))
  occurrences <- round(log(data1$occurrences)/max.occurrences, 1)

  gcd <- data1$gcd
  great.circle <- round(gcd/great_circle_bin)*great_circle_bin
  lats <- data.frame(abs(data1$MinLat), abs(data1$MaxLat))

  #paleolats & longs rounded
  min.lat <- round(apply(lats, 1, min)/min_lat_bin)*min_lat_bin
  max.lat <- round(apply(lats, 1, max)/max_lat_bin)*max_lat_bin
  lat.range <- max.lat-min.lat
  mean.lat <- round(abs(data1$mean_lat)/mean_lat_bin)*mean_lat_bin
  tropical_only <- ifelse(max.lat > 30, 0, 1)

  Ex <- data1$Extinct.in.stage

  stage <- rep(stage_name, length(Ex))
  stage_top <- rep(stage_top, length(Ex))

  stand_dat <- droplevels(data.frame(stage, stage_top, class, group,
        genus, richness, occupancy, occurrences, min.lat, max.lat, lat.range,
        mean.lat, great.circle, tropical_only, Ex, prop_comp = data1$prop_comp,
        single_obs = data1$single_occ_any_stage, singleton = data1$Singleton))

  return(stand_dat)
}
