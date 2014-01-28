#' Get binary summary statistics
#'
#' This function returns some binary classification summary statistics: 
#' sensitivity, specificity, and Kappa.
#' 
#' @param obs A vector of binary observations (0 or 1).
#' @param pred_prob The predicted probability.
#' @param threshold The threshold above which to classify an observation as a 1.
#' 
#' @export
#' @examples 
#' get_summary_stats(obs = rbinom(10, 1, 0.5), pred_prob = runif(10), 0.5)

get_summary_stats <- function(obs, pred_prob, threshold) {
  binary_labels <- obs == 1
  tp <- sum((pred_prob > threshold) & binary_labels)
  sensitivity <- tp / sum(binary_labels)
  tn <- sum((pred_prob <= threshold) & (!binary_labels))
  specificity <- tn / sum(!binary_labels)
  predicted_Ex <- as.numeric(pred_prob > threshold)
  k <- irr::kappa2(cbind(predicted_Ex, obs))$value
  data.frame(sensitivity, specificity, k)
}
