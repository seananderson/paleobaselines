\name{assign_bins}
\alias{assign_bins}
\title{Assign bins}
\usage{
assign_bins(x, bin = 0.025, lower = 0, upper = 1)
}
\arguments{
  \item{x}{A vector to bin.}

  \item{bin}{The bin size.}

  \item{lower}{The lower bound.}

  \item{upper}{The upper bound.}
}
\description{
Assign bins to a vector based on a lower and upper limit at
a bin size
}
\examples{
assign_bins(runif(10))
}

