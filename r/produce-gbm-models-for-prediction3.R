# This version aims to reproduce the same output as "Produce gbm models for
# prediction2.R" but with greatly simplified modelling code to facilitate:
# 1. Calibrating the predictions with observed data (on the whole Neogene at
# once) and possibly doing this on an continuous (interpolated) scale.
# 2. Cross-validating the predictions from calibrated and uncalibrated models to
# gauge how well these models are performing at out-of-sample prediction.
#
# SA 20140116


load("../Final data/standardized.predictors.Cenozoic.OBIS.rda")

stcen <- standardized.cenozoic
stcen <- subset(stcen, class != "Testudines")
stcen <- gdata::drop.levels(subset(stcen,stcen$stage_top < 22 &
    stcen$stage_top != 0))

# library(gbm)
#
# m <- gbm(Ex ~ richness + occupancy + occurrences + min.lat + max.lat +
#   lat.range + mean.lat + great.circle + class + group, data = stcen,
#   n.trees = 200, interaction.depth = 3, distribution = "bernoulli", shrinkage
#   = 0.01)
#
# stcen$gbm_pred <- predict(m, n.trees = 200, type = "response")
# stcen$gbm_pred_binned <- assign_bins(stcen$gbm_pred, bin = 0.03)
#
# pred_cal <- ddply(stcen, c("class", "gbm_pred_binned"), function(x) {
#   data.frame(obs_ext_prob =  mean(x$Ex))
#   m <- glm(Ex~1, family = binomial, data = x)
#   int <- boot::inv.logit(coef(m)[[1]])
#   cis <- boot::inv.logit(confint(m))
#   data.frame(obs_ext_prob = int, l = cis[[1]], u = cis[[2]], .n = nrow(x))
#   })
# ggplot(pred_cal, aes(gbm_pred_binned, obs_ext_prob)) + geom_pointrange(aes(ymin
#     = ltheme(aspect.ratio=1), ymax = u)) + facet_wrap(~class) +
# geom_abline(intercept = 0, slope = 1)

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

fit_models <- function(dat, test_fraction = 0.5, threshold = 0.5, calibrate =
  FALSE, calibration_lm_plot = FALSE, shrinkage = 0.1, interaction.depth = 3,
  n.trees = 200, bin = 0.02) {

  require(gbm)

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

  # just in case:
  dat_test$gbm_pred_calibrated <- NULL
  dat_test$gbm_pred <- NULL

  # model fitting:
  m <- gbm(Ex ~ richness + occupancy + occurrences + min.lat + max.lat +
    lat.range + mean.lat + great.circle + class + group, data = dat_train,
    n.trees = n.trees, interaction.depth = interaction.depth, distribution =
    "bernoulli", shrinkage = shrinkage)

  dat_train$gbm_pred <- predict(m, n.trees = n.trees, type = "response")
  dat_test$gbm_pred <- predict(m, n.trees = n.trees, type = "response", 
    newdata = dat_test)

  # calculate average observed extinction probability for each bin 
  # for validation:

  dat_test$gbm_pred_binned <- assign_bins(dat_test$gbm_pred, bin = bin)
  bin_validation <- ddply(dat_test, c("class", "gbm_pred_binned"), summarize,
    obs_ext_prob =  mean(Ex), sample_n = length(Ex))

  if(calibrate) {
    # bin the predicted extinction probabilities
    dat_train$gbm_pred_binned <- assign_bins(dat_train$gbm_pred, bin = 0.02)

    # find mean observed extinction probability
    pred_cal <- ddply(dat_train, c("class", "gbm_pred_binned"), summarize,
      obs_ext_prob =  mean(Ex))

    # fit calibration models:
    cal_lms <- dlply(pred_cal, "class", function(x) {
      m_cal <- lm(obs_ext_prob ~ gbm_pred_binned, data = x)
      m_cal
    })
    names(cal_lms) <- unique(pred_cal$class)

    # visual check:
    if(calibration_lm_plot) {
      par(mfrow = c(2, 4))
      for(i in 1:7) {
        with(subset(pred_cal, class == unique(pred_cal$class)[i]),
          plot(gbm_pred_binned, obs_ext_prob, xlim = c(0.05, 0.2), 
            ylim = c(0, 0.55), main = unique(pred_cal$class)[i], 
            xlab = "Predicted probability", 
            ylab = "Observed probability", yaxs = "i"))
        abline(a = 0, b = 1)
        abline(a = cal_lms[[i]], col = "red")
      }
    }

    # now calibrate the testing data with the models:
    dat_test <- ddply(dat_test, "class", function(x) {
      cal_mod <- cal_lms[[unique(x$class)]]
      p <- predict(cal_mod, newdata = data.frame(gbm_pred_binned = x$gbm_pred))
      data.frame(x, gbm_pred_calibrated = p)
          })

    # visual check:
    # par(mfrow = c(2, 4))
    # for(i in 1:7) {
    #   with(subset(dat_test, class == unique(dat_train$class)[i]),
    #     plot(gbm_pred, gbm_pred_calibrated, xlim = c(0.05, 0.2), ylim = c(0, 0.55), main
    #       = unique(pred_cal$class)[i], xlab = "Predicted probability", ylab =
    #       "Observed probability", yaxs = "i"))
    #   abline(a = 0, b = 1)
    #   abline(a = cal_lms[[i]], col = "red")
    # }
    # require(ggplot2)
    # ggplot(dat_test, aes(gbm_pred_calibrated, Ex)) + geom_point(alpha = 0.2) + facet_wrap(~class)
    # ggplot(dat_test, aes(gbm_pred, Ex)) + geom_point(alpha = 0.2) + facet_wrap(~class)

    dat_test$gbm_pred <- NULL
    dat_test <- plyr::rename(dat_test, c("gbm_pred_calibrated" = "gbm_pred"))
  }

  summaries_class <- ddply(dat_test, "class", function(x) {
    get_summary_stats(x$Ex, x$gbm_pred, threshold = threshold)
          })

  summaries_all <- get_summary_stats(dat_test$Ex, dat_test$gbm_pred, 
    threshold = threshold)

  if(calibrate) {
    return(list(cal_lms = cal_lms, stats = out, pred = dat_test,
        bin_validation = bin_validation))
  } else {
    return(list(stats_class = summaries_class, stats_all = summaries_all,
        pred = dat_test, bin_validation = bin_validation))
  }
}

library(plyr)
library(ggplot2)


# test gbm parameters for out-of-sample prediction:
val_test <- rdply(500, fit_models(stcen, test_fraction = .5, 
  bin = 0.025, shrinkage = 0.1, interaction.depth = 1,
  n.trees = 200)$bin_validation)

val_test_summarized <- ddply(val_test, c("class", "gbm_pred_binned"), 
  function(x) {
      m <- glm(obs_ext_prob ~ 1, family = binomial, data = x, 
        weights = sample_n)
      int <- boot::inv.logit(coef(m)[[1]])
      cis <- boot::inv.logit(confint(m))
      median_sample_n <- median(x$sample_n)
      data.frame(mean_observed = int, l = cis[[1]], u = cis[[2]], 
        median_sample_n = median_sample_n)
    })
p <- ggplot(val_test_summarized, 
  aes(gbm_pred_binned, mean_observed)) + 
  geom_point(data = val_test, aes(gbm_pred_binned, obs_ext_prob), 
    alpha = 0.1) +
  geom_linerange(aes(ymin = l, ymax = u), colour = "#4E9DF0") + 
  geom_point(aes(size = median_sample_n), colour = "#4E9DF0") +
  facet_wrap(~class) + geom_abline(intercept = 0, slope = 1) +
  scale_size(trans = "log10") + coord_fixed(ratio = 1) +
  ylim(0, 1) + xlim(0, 1) + 
  xlab("Out-of-sample GBM-predicted extinction probability") +
  ylab("Observed extinction probability") +
  labs(size = "Median sample #")


ggsave("bin-predicted-vs-observed-cross-validation-0.5-testing-bin-0.025-mean-by-bin-shrinkage-0.1-int-depth-1-ntrees-200.pdf", width = 8.5, height = 8)



########################
no_cal <- rdply(20, fit_models(stcen, test_fraction = 0, calibrate = FALSE, threshold = 0.5)$stats_all)
(no_cal_cv <- rdply(50, fit_models(stcen, test_fraction = .1, calibrate = FALSE, threshold = 0.5)$stats_all))
j <- fit_models(stcen, test_fraction = 0, calibrate = FALSE, threshold = 0.5)
plot(sapply(seq(0.1, 0.8, length.out = 100), function(x) sum(j$pred$gbm_pred > x)));abline(h = sum(j$pred$Ex))

cutoffs <- seq(0.1, 0.8, length.out = 300)
observed_Ex_n <- sum(j$pred$Ex)
pred_Ex_n <- sapply(cutoffs, function(x) sum(j$pred$gbm_pred > x))
matching_thresh <- min(cutoffs[pred_Ex_n < observed_Ex_n])

(no_cal_cv_0.5 <- rdply(50, fit_models(stcen, test_fraction = .1, calibrate = FALSE, threshold = .5)$stats_all))
(no_cal_cv_0.26 <- rdply(50, fit_models(stcen, test_fraction = .1, calibrate = FALSE, threshold = matching_thresh)$stats_all))
par(mfrow = c(2, 1))
hist(no_cal_cv_0.5$k, xlim = c(0, 0.5))
hist(no_cal_cv_0.26$k, xlim = c(0, 0.5))

(no_cal_cv_class_0.3 <- rdply(20, fit_models(stcen, test_fraction = 0, calibrate = FALSE, threshold = .3)$stats_class))
library(ggplot2)
ggplot(no_cal_cv_class_0.3, aes(class, k)) + geom_boxplot()

repeated_fits_test <- rdply(20, fit_models(stcen, test_fraction = 0, calibrate = FALSE, threshold = .3, shrinkage = 0.01)$pred)
j2 <- ddply(repeated_fits_test, c(".n", "group"), summarize, mean_gbm_pred = mean(gbm_pred))
p <- ggplot(j2, aes(group, mean_gbm_pred)) + geom_boxplot() + coord_flip()
p
ggsave("repeated-mean-group-predictions-shrinkage-0.01.pdf", width = 7, height = 7)

repeated_fits_test_cv <- rdply(50, fit_models(stcen, test_fraction = 1/3, calibrate = FALSE, threshold = .3, shrinkage = 0.01)$pred)
j2 <- ddply(repeated_fits_test_cv, c(".n", "group"), summarize, mean_gbm_pred = mean(gbm_pred))
p <- ggplot(j2, aes(group, mean_gbm_pred)) + geom_boxplot() + coord_flip()
ggsave("repeated-mean-group-predictions-cv-shrinkage-0.01-n.trees-1000.pdf", width = 7, height = 7)



#yes_cal <- rdply(20, fit_models(stcen, test_fraction = 0.33, calibrate = TRUE, threshold = 0.2)$stats)
#no_cal$calibrated <- FALSE
#yes_cal$calibrated <- TRUE
#cal_test <- rbind(no_cal, yes_cal)

#library(ggplot2)
#ggplot(cal_test, aes(class, k)) + geom_boxplot(aes(colour = calibrated))
#ggplot(cal_test, aes(class, sensitivity)) + geom_boxplot(aes(colour = calibrated))
#ggplot(cal_test, aes(class, specificity)) + geom_boxplot(aes(colour = calibrated))

ggplot(no_cal, aes(class, k)) + geom_boxplot()
ggplot(no_cal, aes(class, sensitivity)) + geom_boxplot()
ggplot(no_cal, aes(class, specificity)) + geom_boxplot()

junk <- fit_models(stcen, test_fraction = 0, calibrate = TRUE, threshold = 0.2, calibration_lm_plot = TRUE)$pred
junk_sum <- ddply(junk, "class", summarize, m = mean(Ex))

ggplot(junk, aes(class, gbm_pred)) + geom_boxplot() + geom_point(data = junk_sum, aes(class, m), colour = "blue", size = 10)

junk2 <- fit_models(stcen, test_fraction = 0, calibrate = FALSE, threshold = 0.2)$pred
junk_sum2 <- ddply(junk2, "class", summarize, m = mean(Ex))

ggplot(junk2, aes(class, gbm_pred)) + geom_boxplot() + geom_point(data = junk_sum2, aes(class, m), colour = "blue", size = 10)

j1 <- ddply(junk, "class", summarize, m = mean(gbm_pred))
j2 <- ddply(junk2, "class", summarize, m = mean(gbm_pred))
plot(j1$m, j2$m)

j <- fit_models(stcen, test_fraction = 0, calibrate = TRUE, threshold = 0.2,
  calibration_lm_plot = TRUE)



1

###########################


 require(caret)

 require(doMC)
 registerDoMC(4)

 fitControl <- trainControl(method = "repeatedCV", number = 10, repeats = 4,
   returnResamp = "all", classProbs = TRUE, allowParallel = TRUE)
#
 ext <- as.factor(ifelse(stcen$Ex == 1,"extinct","survive"))
#
 gbm_fit <- train(x = stcen[,c("richness", "occupancy", "occurrences", "min.lat",
                               "max.lat", "lat.range", "mean.lat", "great.circle", "class", "group")],
                  y = ext,
                  method = "gbm",
                  trControl = fitControl,
                  metric = "Kappa",
                  verbose = FALSE,
                  tuneGrid = expand.grid(.interaction.depth = c(3),
                                         .n.trees = c(200),
                                         .shrinkage = c(0.1)))
#
