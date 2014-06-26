#' Standardize paleontological and contemporary datasets
#'
#' @param dat The data frame to standardize.
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

standardize_data <- function(dat, interval, great_circle_bin = 2000,
  num_bins = 10, min_modern_occurrences = 2, min_pbdb_occurrences = 1,
  modern_no_cull_classes = c("Mammalia", "Elasmobranchii")) {

  mod.dat <- dat[(dat$stage_top == 0),]
  fossil.dat <- dat[(dat$stage_top > 0),]

  mod.dat.cull <- mod.dat[!mod.dat$class %in% modern_no_cull_classes, ]
  mod.dat.no.cull <- mod.dat[mod.dat$class %in% modern_no_cull_classes, ]

  mod.dat.cull <- mod.dat.cull[(mod.dat.cull$occurrences >= min_modern_occurrences), ]
  mod.dat <- rbind(mod.dat.cull, mod.dat.no.cull)

  fossil.dat <- fossil.dat[(fossil.dat$occurrences >= min_pbdb_occurrences),]

  data1 <- rbind(mod.dat,fossil.dat)

  data1 <- subset(data1, Interval_Name == interval)

  stage_name <- data1$Interval_Name[1]
  stage_top <- data1$stage_top[1]

  ### create factors to analyze from prediction interval
  genus <- data1$genus
  class <- data1$class
  group <- data1$MatchTaxon

  max.rich <- max(log(data1$richness))
  richness <- round((log(data1$richness)/max.rich),1)

  max.occupancy <- max(log(data1$eac))
  occupancy <- round((log(data1$eac)/max.occupancy ),1)

  max.occurrences <- max(log(data1$occurrences))
  occurrences <- round((log(data1$occurrences)/max.occurrences ),1)

  gcd <- data1$gcd
  great.circle <- round((gcd/great_circle_bin),0)*great_circle_bin
  lats <- data.frame(abs(data1$MinLat), abs(data1$MaxLat))

  #paleolats & longs
  min.lat <- apply(lats, 1, min)
  max.lat <- apply(lats, 1, max)
  lat.range <- max.lat-min.lat
  mean.lat <- abs(data1$mean_lat)

  min.lat <- floor(min.lat / num_bins)*num_bins
  max.lat <- ceiling(max.lat / num_bins)*num_bins
  lat.range <- round(lat.range/num_bins)*num_bins
  mean.lat <- round(mean.lat/num_bins)*num_bins

  Ex <- data1$Extinct.in.stage

  stage <- rep(stage_name, length(Ex))
  stage_top <- rep(stage_top, length(Ex))

  stand_dat <- gdata::drop.levels(na.omit(data.frame(stage, stage_top, class, group,
        genus, richness, occupancy, occurrences, min.lat, max.lat, lat.range,
        mean.lat, great.circle, Ex)))

  return(stand_dat)
}
