#' Validate and calibrate models
#' 
#' This function fits \code{\link[gbm]{gbm}} benoulli models and returns 
#' observation statistics within bins for use with validation and calibration.
#' 
#' @param dat The input data.
#' @param test_fraction The fraction of the data to reserve for testing.
#' @param threshold The cutoff to use in binary classification statistics.
#' @param shrinkage The \code{shrinkage} argument to pass to 
#'   \code{\link[gbm]{gbm}}.
#' @param interaction.depth The \code{interaction.depth} argument to pass to 
#'   \code{\link[gbm]{gbm}}.
#' @param n.trees The \code{n.trees} argument to pass to \code{\link[gbm]{gbm}}.
#' @param bin The bin size to split up the predictions into for validation and 
#'   calibration.
#' @param smote A logical argument: should the function implement SMOTE 
#'   algorithm for unbalanced classification? Implemented via the 
#'   \code{\link[DMwR]{SMOTE}} function.
#' @param use_weights A logical argument: should the GBM be fit with weights 
#'   assigned based on extinction frequency. Use only \code{SMOTE = TRUE} or 
#'   \code{use_weights = TRUE}.
#' @param classification_stats Should the function return classification 
#'   statistics (sensitivity, specificity, Kappa)?
#' @param ... Anything else to pass to \code{\link[gbm]{gbm}}.
#'   
#' @export
#' 
#' @return A list. If \code{classification_stats = TRUE} then the list will have
#' elements: summaries_class (classification summaries by taxonomic class), 
#' summaries_all (classification summaries overall), pred (predictions), and 
#' bin_validation (a data frame summarizing the bin validation routine).
#' 
#' If \code{classification_stats = FALSE} then the list will only have the 
#' prediction and bin validation data frames.

validate_gbm <- function(dat, test_fraction = 0.5, threshold = 0.5,
  shrinkage = 0.1, interaction.depth = 1, n.trees = 300, bin = 0.025, 
  smote = FALSE, use_weights = FALSE, classification_stats = FALSE, ...) {
  
  if(smote & use_weights) stop("Use only one of smote and use_weights.")
  
  N <- nrow(dat)
  N_test <- round(N * test_fraction)
  N_train <- N - N_test
  
  scrambled_rows <- sample(1:N, N)
  dat_train <- dat[scrambled_rows[1:N_train], ]
  
  if(test_fraction > 0) { # we're cross-validating
    dat_test <- dat[scrambled_rows[(N_train+1):N], ]
  } else { # no cross-validating
    dat_test <- dat_train
  }
  
  if(smote) {
    dat_train <- smote(dat_train)
  }
  # just in case these already exist:
  dat_test$gbm_pred <- NULL
  
  if(use_weights) {    
    obs <- nrow(dat_train)
    exes <- sum(dat_train$Ex)
    ExFreq <- (exes/obs)
    SurFreq <- (1-(exes/obs))
    MaxFreq <- max(ExFreq,SurFreq)
    ExWeight <- (1/(ExFreq/MaxFreq))
    SurWeight <- (1/(SurFreq/MaxFreq))
    weights <- ifelse(dat_train$Ex==1, ExWeight, SurWeight)
  } else {
    weights <- rep(1, nrow(dat_train))
  }
  
  # model fitting:
  if(use_weights) {
  m <- gbm::gbm(Ex ~ richness + occupancy + occurrences + min.lat + max.lat +
      lat.range + mean.lat + great.circle + class + group, data =
      dat_train, n.trees = n.trees, interaction.depth = interaction.depth,
    distribution = "bernoulli", shrinkage = shrinkage, weights = weights, ...)
  } else {
    m <- gbm::gbm(Ex ~ richness + occupancy + occurrences + min.lat + max.lat +
        lat.range + mean.lat + great.circle + class + group, data =
        dat_train, n.trees = n.trees, interaction.depth = interaction.depth,
      distribution = "bernoulli", shrinkage = shrinkage, ...)
  }
  
  dat_train$gbm_pred <- gbm::predict.gbm(m, n.trees = n.trees, type = "response")
  dat_test$gbm_pred <- gbm::predict.gbm(m, n.trees = n.trees, type = "response",
    newdata = dat_test)
  
  # calculate average observed extinction probability for each bin
  # for validation:
  dat_test$gbm_pred_binned <- assign_bins(dat_test$gbm_pred, bin = bin)
  bin_validation <- plyr::ddply(dat_test, c("class", "gbm_pred_binned"),
    plyr::summarize, obs_ext_prob =  mean(Ex), sample_n = length(Ex))
  
  if(classification_stats) {
    summaries_class <- plyr::ddply(dat_test, "class", function(x) {
      get_summary_stats(x$Ex, x$gbm_pred, threshold = threshold)
    })
    
    summaries_all <- get_summary_stats(dat_test$Ex, dat_test$gbm_pred,
      threshold = threshold)
    
    return(list(stats_class = summaries_class, stats_all = summaries_all,
      pred = dat_test, bin_validation = bin_validation))
  } else {
    return(list(pred = dat_test, bin_validation = bin_validation))
  }
}
