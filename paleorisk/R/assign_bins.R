#' Assign bins
#' 
#' Assign bins to a vector based on a lower and upper limit at a bin size
#' 
#' @param x A vector to bin.
#' @param bin The bin size.
#' @param lower The lower bound.
#' @param upper The upper bound.
#' 
#' @export
#' @examples
#' assign_bins(runif(10))

assign_bins <- function(x, bin = 0.025, lower = 0, upper = 1) {
  # create vector of cuts to bin probabilities into:
  # assign the mid value from each bin:
  prob_cuts <- seq(lower, upper, bin)
  prob_bin_middles <- prob_cuts + diff(prob_cuts[1:2])/2
  # one less middle than cut:
  prob_bin_middles <- prob_bin_middles[-length(prob_bin_middles)]
  # in case we're on the border:
  prob_cuts[1] <- -0.00001
  prob_cuts[length(prob_cuts)] <- 1.00001
  binned <- prob_bin_middles[findInterval(x, prob_cuts)]
  binned
}
