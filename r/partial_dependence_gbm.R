# 
#  partial_dependence_gbm.R
#  Look at something like a partial dependence plot for the gbm model.
#  
#  Created by Sean C. Anderson on 2012-10-11.
# 

# OK, let's build the predictive model averaged across recent intervals:



# fitControl <- trainControl(method = "repeatedcv", number = 10, 
#   repeats = 5, returnResamp = "all", classProbs = TRUE, 
#   summaryFunction = twoClassSummary)


# c("Lower Miocene", "Middle Miocene", "Upper Miocene", "Pliocene", "Pleistocene")


# gbmFit1 <- train(data1factors, ext,
#                   method = "gbm",
#                   trControl = fitControl,
#                   metric = "Kappa",
#                   verbose = FALSE, 
#                   weights = weights)j

# From running "Caret run multiple models for interval 1 and predict
# interval 2.R":
load("../data/gbm_dat.rda")
# Contains: data1factors (training data from one paleo tim period), data2factors
# (modern data to predict on), and gbmFit1.

# Now, build a dataset to look at the dependence of the model on each of model
# components.

# First, let's start with richness. We'll set it to each of the (4) levels and
# get a distribution of extinction probabilities.

# As output, we want a data.frame that looks like this: variable_name | 
# mean_ext_prob | median_ext_prob | l_quart_ext_prob | u_quart_ext_prob

# An example for one column:
# partial_probs_richness <- ldply(1:4, function(x) {
#   current_dat <- data2factors
#   current_dat$richness <- x
#   p <- predict(gbmFit1, newdata = current_dat, type = "prob")
#   mean_ext_prob <- mean(p$extinct)
#   quants_ext_prob <- quantile(p$extinct, probs = c(0.25, 0.5, 0.75))
#   return(data.frame(mean_ext_prob = mean_ext_prob, median_ext_prob = quants_ext_prob[2], l_quart_ext_prob = quants_ext_prob[1], u_quart_ext_prob = quants_ext_prob[3], value = x))
# })
# partial_probs_richness$variable <- "richness"

# Now do it for all variables at once:
get_partial_prob <- function(model_name, predict_data, col_num, 
  variable_name) {
  ldply(sort(unique(predict_data[, col_num])), function(x) {
    current_dat <- predict_data
    current_dat[, col_num] <- x
    p <- predict(model_name, newdata = current_dat, type = "prob")
    mean_ext_prob <- mean(p$extinct)
    quants_ext_prob <- quantile(p$extinct, 
      probs = c(0.25, 0.375, 0.5, 0.625, 0.75))
    return(data.frame(
      mean_ext_prob    = mean_ext_prob, 
      quant_ext_prob_1 = quants_ext_prob[1], 
      quant_ext_prob_2 = quants_ext_prob[2],
      quant_ext_prob_3 = quants_ext_prob[3], 
      quant_ext_prob_4 = quants_ext_prob[4],
      quant_ext_prob_5 = quants_ext_prob[5], 
      value            = x, 
      variable         = variable_name))
  })
}

partial_probs <- list()
for (i in 3:ncol(data2factors)) {
  partial_probs[[i-2]] <- get_partial_prob(model_name = gbmFit1, 
    predict_data = data1factors, col_num = i, variable_name =
      names(data2factors)[i])

}

partial_probs <- do.call("rbind", partial_probs)

q <- ggplot(partial_probs, aes(x = value, y = quant_ext_prob_3)) + 
  facet_wrap(~variable, scales = "free_x") + geom_line() + xlab("Variable value") + ylab("Predicted extinction probability (partial)")
q <- q + geom_ribbon(aes(ymin = quant_ext_prob_1, ymax = quant_ext_prob_5), 
    fill = "#00000030")
q <- q + geom_ribbon(aes(ymin = quant_ext_prob_2, ymax = quant_ext_prob_4), 
    fill = "#00000030")

print(q)

# ================================
# = Now try with lines per class =
# ================================
get_partial_prob_class <- function(model_name, predict_data, col_num, 
  variable_name) {
  ldply(sort(unique(predict_data[, col_num])), function(x) {
    current_dat <- predict_data
    current_dat[, col_num] <- x
  junk <-  ldply(unique(predict_data$class), function(y) {
      p <- predict(model_name, newdata = subset(current_dat, class == y), type = "prob")
    mean_ext_prob <- mean(p$extinct)
    quants_ext_prob <- quantile(p$extinct, 
      probs = c(0.25, 0.375, 0.5, 0.625, 0.75))
    return(data.frame(
      mean_ext_prob    = mean_ext_prob, 
      quant_ext_prob_1 = quants_ext_prob[1], 
      quant_ext_prob_2 = quants_ext_prob[2],
      quant_ext_prob_3 = quants_ext_prob[3], 
      quant_ext_prob_4 = quants_ext_prob[4],
      quant_ext_prob_5 = quants_ext_prob[5], 
      value            = x, 
      variable         = variable_name,
      class_name       = y
      ))
          })
          junk
  })
}

partial_probs <- list()
for (i in 3:ncol(data2factors)) {
  partial_probs[[i-2]] <- get_partial_prob_class(model_name = gbmFit1, 
    predict_data = data1factors, col_num = i, variable_name =
      names(data2factors)[i])

}

partial_probs <- do.call("rbind", partial_probs)

q <- ggplot(partial_probs, aes(x = value, y = quant_ext_prob_3, colour = class_name)) + 
  facet_wrap(~variable, scales = "free_x") + geom_line(lwd = 1) + xlab("Variable value") + ylab("Predicted extinction probability (partial)")
 q <- q + geom_ribbon(aes(ymin = quant_ext_prob_2, ymax = quant_ext_prob_4, fill = class_name),  linetype = 0, alpha = 0.1) + coord_trans(y = "log")
#     fill = "#00000030")
# q <- q + geom_ribbon(aes(ymin = quant_ext_prob_2, ymax = quant_ext_prob_4), 
#     fill = "#00000030")

print(q)