\name{map_hotspots}
\alias{map_hotspots}
\title{Make global intrinsic extinction probability hotspot maps}
\usage{
map_hotspots(er_dat, min_prov_genera = 50, hotspot_thresh = 0.8,
  col_pal = RColorBrewer::brewer.pal(9, "YlOrRd"), hotspots = TRUE)
}
\arguments{
  \item{er_dat}{A data frame with the data to plot. Should
  have \code{\link[ggplot2]{fortify}} data from a
  \code{\link[maptools]{readShapePoly}} version of the
  EcoRegion data. Also should have columns named
  \code{Halpern}, \code{Burrows}, \code{log_OBIS_records},
  \code{N.gen}, \code{mean.ext}. \code{mean.ext} should
  contain *log*-transformed extinction probability.}

  \item{min_prov_genera}{The minimum contemporary genera
  per province required to plot.}

  \item{hotspot_thresh}{Percentile threshold to declare a
  contemporary hotspot.}

  \item{col_pal}{The colour palette to plot with. Should be
  approximately a vector of length 9.}

  \item{hotspots}{Show the contemporary hotspots?}
}
\description{
This function plots overall intrinsic extinction
probability and can add hotspots of contemporary risk.
}

