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
#' @rdname validate_gbm

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
     w <- get_weights(dat_train$Ex)
     weights <- ifelse(dat_train$Ex == 1, w$ex_weight, w$sur_weight)
   } else {
     weights <- rep(1, nrow(dat_train))
   }
  
  # model fitting:
  m <- gbm::gbm(Ex ~ richness + occupancy + occurrences + min.lat + max.lat +
      lat.range + mean.lat + great.circle + class + group, data =
      dat_train, n.trees = n.trees, interaction.depth = interaction.depth,
    distribution = "bernoulli", shrinkage = shrinkage, weights = weights, ...)
  
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

#' Get model weights
#'
#' @param x A vector of 0s and 1s with 0s representing non-extinctions and 1s
#'   representing extinctions.
#' @return A list with extiction and survival weights to be passed to a model.
#' @examples
#' get_weights(c(0, 0, 1))
#' @export
#' @rdname validate_gbm

get_weights <- function(x) {
  obs <- length(x)
  exes <- sum(x)
  ExFreq <- (exes/obs)
  SurFreq <- (1-(exes/obs))
  MaxFreq <- max(ExFreq,SurFreq)
  ExWeight <- (1/(ExFreq/MaxFreq))
  SurWeight <- (1/(SurFreq/MaxFreq))
  list(ex_weight = ExWeight, sur_weight = SurWeight)
} 

#' SMOTE our data
#' @rdname validate_gbm

smote <- function(x, perc.over = 800, perc.under = 100) {
  x$Ex <- as.factor(x$Ex)
  x <- DMwR::SMOTE(Ex ~ ., data = x[,c("class", "group", 
    "great.circle", "mean.lat", "lat.range", "max.lat", "min.lat", 
    "occurrences", "occupancy", "richness", "Ex")], 
    perc.over = perc.over, perc.under = perc.under)
  x$Ex <- as.numeric(as.character(x$Ex))
  x
}

#' Summarize a validation test
#'
#' @export
#' @rdname validate_gbm

summarize_val_test <- function(x) {
  m <- stats::glm(obs_ext_prob ~ 1, family = binomial, data = x,
    weights = sample_n)
  int <- boot::inv.logit(coef(m)[[1]])
  cis <- boot::inv.logit(confint(m))
  median_sample_n <- median(x$sample_n)
  data.frame(mean_observed = int, l = cis[[1]], u = cis[[2]],
    median_sample_n = median_sample_n)
}