\name{validate_gbm}
\alias{validate_gbm}
\title{Validate and calibrate models}
\usage{
validate_gbm(dat, test_fraction = 0.5, threshold = 0.5, shrinkage = 0.1,
  interaction.depth = 1, n.trees = 300, bin = 0.1, smote = FALSE,
  use_weights = FALSE, classification_stats = FALSE, ...)
}
\arguments{
  \item{dat}{The input data.}

  \item{test_fraction}{The fraction of the data to reserve
  for testing.}

  \item{threshold}{The cutoff to use in binary
  classification statistics.}

  \item{shrinkage}{The \code{shrinkage} argument to pass to
  \code{\link[gbm]{gbm}}.}

  \item{interaction.depth}{The \code{interaction.depth}
  argument to pass to \code{\link[gbm]{gbm}}.}

  \item{n.trees}{The \code{n.trees} argument to pass to
  \code{\link[gbm]{gbm}}.}

  \item{bin}{The bin size to split up the predictions into
  for validation and calibration.}

  \item{smote}{A logical argument: should the function
  implement SMOTE algorithm for unbalanced classification?
  Implemented via the \code{\link[DMwR]{SMOTE}} function.}

  \item{use_weights}{A logical argument: should the GBM be
  fit with weights assigned based on extinction frequency.
  Use only \code{SMOTE = TRUE} or \code{use_weights =
  TRUE}.}

  \item{classification_stats}{Should the function return
  classification statistics (sensitivity, specificity,
  Kappa)?}

  \item{...}{Anything else to pass to
  \code{\link[gbm]{gbm}}.}
}
\value{
A list. If \code{classification_stats = TRUE} then the list
will have elements: summaries_class (classification
summaries by taxonomic class), summaries_all
(classification summaries overall), pred (predictions), and
bin_validation (a data frame summarizing the bin validation
routine).

If \code{classification_stats = FALSE} then the list will
only have the prediction and bin validation data frames.
}
\description{
This function fits \code{\link[gbm]{gbm}} benoulli models
and returns observation statistics within bins for use with
validation and calibration.
}

