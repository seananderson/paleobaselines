# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Sep 14, 2012
# Last modified: Sep 14, 2012
# Purpose:       Create a partial dependence plot for a ctree object
#                from the package party.
# ====================================================================

# example for testing:
#require(party)
#airq <- subset(airquality, !is.na(Ozone))
#airct <- ctree(Ozone ~ ., data = airq,  controls = ctree_control(maxsurrogate = 3))
#column_number_to_predict_on <- 3

partial_dependence_ctree <- function(ctree_data, ctree_object, column_number_to_predict_on) {
# Only written for continuous variables at this point.
require(party)
out <- sapply(1:nrow(ctree_data), function(i) {
  newdata <- ctree_data
  newdata[ , column_number_to_predict_on] <- rep(newdata[i , column_number_to_predict_on], nrow(ctree_data))
  predicted_mean_response <- mean(as.numeric(predict(ctree_object, newdata = newdata, type = "prob")))
  return(y = predicted_mean_response)
}) 
out
}

# example:
#partial_dependence_ctree(ctree_data = airq, ctree_object = airct, column_number_to_predict_on = 3)
