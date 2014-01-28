#' Fit gbm models across paleo intervals
#' 
#' @param dat The data to work with. Should have columns named \code{stage},
#' \code{Ex} (0 or 1 for not extinct or extinct), as well as the predictors:
#' \code{richness + occupancy + occurrences + min.lat + max.lat + lat.range + 
#' mean.lat + great.circle + class + group}.
#' @param stage_train The paleo stage to train the model on.
#' @param stage_test The paleo stage to test on.
#' @param shrinkage The \code{shrinkage} argument to pass to
#' \code{\link[gbm]{gbm}}.
#' @param interaction.depth The \code{interaction.depth} argument to pass to
#' \code{\link[gbm]{gbm}}.
#' @param n.trees The \code{n.trees} argument to pass to
#' \code{\link[gbm]{gbm}}.
#' @param ... Anything else to pass to \code{\link[gbm]{gbm}}.
#' 
#' @export

fit_interval_gbm <- function(dat, stage_train, stage_test, interaction.depth = 1,
  n.trees = 300, shrinkage = 0.1, ...) {
  message(paste("Predicting", stage_test, "from", stage_train))
  dat_train <- dat[dat$stage == stage_train, ]
  dat_test  <- dat[dat$stage == stage_test,  ]
  # cross-stage model:
  m_test <- gbm(Ex ~ richness + occupancy + occurrences + min.lat +
      max.lat + lat.range + mean.lat + great.circle + class + group,
    data = dat_train, n.trees = n.trees, interaction.depth = interaction.depth,
    distribution = "bernoulli", shrinkage = shrinkage, ...)
  # test stage self model:
  m_self <- gbm::gbm(Ex ~ richness + occupancy + occurrences + min.lat +
      max.lat + lat.range + mean.lat + great.circle + class + group,
    data = dat_test, n.trees = n.trees, interaction.depth = interaction.depth,
    distribution = "bernoulli", shrinkage = shrinkage, ...)
  pred <- gbm::predict.gbm(m_test, newdata = dat_test, type = "response",
    n.trees = n.trees)
  pred_self <- gbm::predict.gbm(m_self, type = "response", n.trees = n.trees)
  data.frame(stage_train, stage_test, pred, pred_self,
    class = dat_test$class)
}
