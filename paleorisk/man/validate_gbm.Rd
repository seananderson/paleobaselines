\name{validate_gbm}
\alias{get_weights}
\alias{smote}
\alias{summarize_val_test}
\alias{validate_gbm}
\title{Validate and calibrate models}
\usage{
validate_gbm(dat, test_fraction = 0.5, threshold = 0.5, shrinkage = 0.1,
  interaction.depth = 1, n.trees = 300, bin = 0.05, smote = FALSE,
  use_weights = FALSE, classification_stats = FALSE, ...)

get_weights(x)

smote(x, perc.over = 800, perc.under = 100)

summarize_val_test(x)
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

  \item{x}{A vector of 0s and 1s with 0s representing
  non-extinctions and 1s representing extinctions.}
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

A list with extiction and survival weights to be passed to
a model.
}
\description{
This function fits \code{\link[gbm]{gbm}} benoulli models
and returns observation statistics within bins for use with
validation and calibration.

Get model weights

SMOTE our data

Summarize a validation test
}
\examples{
get_weights(c(0, 0, 1))
}

