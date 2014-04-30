\name{standardize_data}
\alias{standardize_data}
\title{Standardize paleontological and contemporary datasets}
\usage{
standardize_data(dat, min_lat_bin = 10, max_lat_bin = 10,
  lat_range_bin = 10, mean_lat_bin = 10, tropical_only_bin = 1,
  great_circle_bin = 2000, num_bins = 10, num_risk_quantiles = 10,
  input_ranges = "Interpolated", min_occurrences = 2,
  minimum_duration = 1)
}
\arguments{
  \item{dat}{The data frame to standardize.}

  \item{min_lat_bin}{TODO}

  \item{max_lat_bin}{TODO}

  \item{lat_range_bin}{TODO}

  \item{mean_lat_bin}{TODO}

  \item{tropical_only_bin}{}

  \item{great_circle_bin}{}

  \item{num_bins}{Number of bins to use for standardizing
  richness, occurrences, occupancy.}

  \item{num_risk_quantiles}{Number of quantiles to use for
  standardizing extinction risk, if desired.}

  \item{input_ranges}{Either \code{"Interpolated"} or
  \code{"Raw"}.}

  \item{min_pbdb_occurrences}{Minimum number of genera per
  province of PBDB occurrences.}

  \item{min_modern_occurrences}{Minimum number of genera
  per province of contemporary occurrences.}

  \item{minimum_duration}{Minimum duration (in stages) for
  a genera included: set "1" to include singletons, and "2"
  to include them.}
}
\value{
A standardized data frame.
}
\description{
Standardize paleontological and contemporary datasets
}

