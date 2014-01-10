# Take the gbm models and calibrate them as in Thorson et al. 2012 CJFAS
# Start by sourcing the file "Produce gbm models for prediction2.R"

# To add calibrated probabilities to any dataset you need to:
# 1. Have sourced this file, which creates the function assign_bins(), the
#    object pred_cal (calibrated predictions), and a version of preds2 that has
#    the calibrated predictions.
# 2. To make this work on a new dataset, you need to bin the predicted
#    probabilities using assign_bins() into a new column named "extinct_binned".
# 3. Then, join() or merge() the dataset with pred_cal (which will match by
#    interval, class, and extinct_binned).
# Or, find some other way of joining the two for cross-interval predictions.
#
# See the file binned-prediction-vs-observed-gbms.pdf for a plot of the
# calibration method.

preds2$extinct <- as.numeric(preds2$extinct)
preds2$survive <- as.numeric(preds2$survive)

library(ggplot2)
# p <- ggplot(preds2, aes(survive, outcome)) + geom_point(alpha = .1, position = position_jitter(height = 0.1)) + facet_grid(interval~class) + xlab("Predicted probability of survival")
# ggsave("prediction-vs-observed-gbms.pdf", width = 13, height = 7)

assign_bins <- function(x, bin = 0.05, lower = 0, upper = 1) {
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

preds2$extinct_binned <- assign_bins(preds2$extinct)

# ggplot(preds2, aes(extinct, extinct_binned)) + geom_point(alpha = .1) + facet_grid(interval~class)

preds2$extinct_numeric <- ifelse(preds2$outcome == "extinct", 1, 0)

library(plyr)
pred_cal <- ddply(preds2, c("class", "interval", "extinct_binned"), function(x) {
  data.frame(obs_extinct_prob =  mean(x$extinct_numeric))
})

# if using glm() instead of mean()
# pred_cal <- subset(pred_cal, obs_extinct_prob < 1)

p <- ggplot(pred_cal, aes(extinct_binned, obs_extinct_prob)) + geom_point(alpha
  = .9) + facet_grid(interval~class) +
geom_abline(intercept = 0, slope = 1, col = "blue", alpha = 0.4) +
xlab("Predicted extinction probability") +
ylab("Mean observed extinction probability")
ggsave("binned-prediction-vs-observed-gbms.pdf", width = 11, height = 7)

# p <- ggplot(pred_cal, aes(extinct_binned, obs_extinct_prob, colour = class)) +
# geom_point(alpha = .9) + facet_wrap(~interval) + geom_abline(intercept = 0,
#   slope = 1, col = "blue", alpha = 0.4)
# ggsave("binned-prediction-vs-observed-gbms-colour.pdf", width = 8, height = 5)

# p <- ggplot(preds2, aes(extinct_binned, extinct_numeric)) + geom_point(alpha =
#   0.3, position = position_jitter(height = 0.1)) + facet_grid(interval~class) +
# geom_abline(intercept = 0, slope = 1)
# ggsave("binned-prediction-vs-observed-jitter-gbms.pdf", width = 11, height = 7)

pred_cal <- plyr::rename(pred_cal, c("obs_extinct_prob" = "calibrated_ext_prob"))
preds2 <- plyr::join(preds2, pred_cal)

