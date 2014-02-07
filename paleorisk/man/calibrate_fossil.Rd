\name{calibrate_fossil}
\alias{calibrate_fossil}
\title{Calibrate the fossil data}
\usage{
calibrate_fossil(dat)
}
\arguments{
  \item{dat}{The input data. Should be output from
  \code{\link{standardize_data}} over multiple stages.}
}
\description{
This function attempts to calibrate the fossil data to
account for undersamping compared to the contemporary
period by using the data from the Plio-Pleistocene as a
model. For each class-range parameter combination it takes
the genera found in both the modern and Plio-Pleistocene
datasets and regresses the former on the latter. It then
uses this regression to calibrate the data.
}
