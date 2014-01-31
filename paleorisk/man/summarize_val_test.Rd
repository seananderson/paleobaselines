\name{summarize_val_test}
\alias{summarize_val_test}
\title{Summarize a validation test}
\usage{
summarize_val_test(x, type = c("glm", "quantile"))
}
\arguments{
  \item{x}{A data frame that has a column named
  \code{obs_ext_prob}. This column should contain 0s and 1s
  (or factors with two levels) that can be modelled as a
  binomial process.}

  \item{type}{One of either \code{"glm"} for mean and 95%
  likelihood-profile-based confidence intervals or
  \code{"quantile"} for median and 50% percentile intervals
  (interquartile range).}
}
\value{
A data frame containing the columns \code{mean_observed}
(the calibration value), \code{l} (the lower confidence
interval), \code{u} (the upper confidence interval), and
\code{median_sample_n} (the median number of samples in
each model, i.e. in each cross-validation).
}
\description{
Summarize a validation test
}

