\name{get_weights}
\alias{get_weights}
\title{Get model weights}
\usage{
get_weights(x)
}
\arguments{
  \item{x}{A vector of 0s and 1s with 0s representing
  non-extinctions and 1s representing extinctions.}
}
\value{
A list with extiction and survival weights to be passed to
a model.
}
\description{
Get model weights
}
\examples{
get_weights(c(0, 0, 1))
}

