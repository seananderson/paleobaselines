\name{fit_interval_gbm}
\alias{fit_interval_gbm}
\title{Fit gbm models across paleo intervals}
\usage{
fit_interval_gbm(dat, stage_train, stage_test, interaction.depth = 1,
  n.trees = 300, shrinkage = 0.1, use_weights = FALSE, ...)
}
\arguments{
  \item{dat}{The data to work with. Should have columns
  named \code{stage}, \code{Ex} (0 or 1 for not extinct or
  extinct), as well as the predictors: \code{richness +
  occupancy + occurrences + min.lat + max.lat + lat.range +
  mean.lat + great.circle + class + group}.}

  \item{stage_train}{The paleo stage to train the model
  on.}

  \item{stage_test}{The paleo stage to test on.}

  \item{shrinkage}{The \code{shrinkage} argument to pass to
  \code{\link[gbm]{gbm}}.}

  \item{interaction.depth}{The \code{interaction.depth}
  argument to pass to \code{\link[gbm]{gbm}}.}

  \item{n.trees}{The \code{n.trees} argument to pass to
  \code{\link[gbm]{gbm}}.}

  \item{...}{Anything else to pass to
  \code{\link[gbm]{gbm}}.}
}
\description{
Fit gbm models across paleo intervals
}

