require(gdata)
require(plyr)
require(gbm)
require(caret)
require(ggplot2)

# load predictors:
load("~/Dropbox/nescent_extinction_map/Final data/standardized.predictors.Cenozoic.OBIS.rda")
data <- standardized.cenozoic

Modern <- drop.levels(subset(data,data$stage == "Modern"))
Neogene <- drop.levels(subset(data,data$stage_top > 20 & data$stage_top != 0 ))
Plio_Pleistocene <- drop.levels(subset(data,data$stage == "Plio-Pleistocene"))
Upper_Miocene <- drop.levels(subset(data,data$stage == "Upper Miocene"))
Middle_Miocene <- drop.levels(subset(data,data$stage == "Middle Miocene"))
Lower_Miocene <- drop.levels(subset(data,data$stage == "Lower Miocene"))

Modern$interval <- "Modern"
Neogene$interval <- "Neogene"
Plio_Pleistocene$interval <- "Plio_Pleistocene"
Upper_Miocene$interval <- "Upper_Miocene"
Middle_Miocene$interval <- "Middle_Miocene"
Lower_Miocene$interval <- "Lower_Miocene"

data_to_predict <- rbind(Modern, Neogene, Plio_Pleistocene, Upper_Miocene,
  Middle_Miocene, Lower_Miocene)

## remove extraneous variables and set "group" to factor
drops <- c("stage","stage_top","genus","Ex")
data_to_predict <- data_to_predict[,!(names(data_to_predict) %in% drops)]
data_to_predict$group <- as.factor(data_to_predict$group)
data_to_predict$class <- as.factor(data_to_predict$class)

load("~/Dropbox/nescent_extinction_map/Final data/caret.gbm.model.sets.rda")
models <- caret.gbm.model.sets
intervals <- names(models)
#c("Lower_Miocene", "Middle_Miocene", "Upper_Miocene", "Plio_Pleistocene","Neogene")

cross_predict <- function(interval_to_predict, model_interval, model_list) {
  message(paste("Predicting from", model_interval, "based on", interval_to_predict))
  this_model <- model_list[[model_interval]]
  other_model <- model_list[[interval_to_predict]]
  other_data_to_predict <- data_to_predict[data_to_predict$interval ==
    interval_to_predict, ]
  model_pred <- predict(other_model, newdata = other_data_to_predict, type =
    "prob")$gbm$extinct
  prediction <- predict(this_model, newdata = other_data_to_predict, type =
    "prob")$gbm$extinct
  predictor <- rep(model_interval, length(prediction))
  predicted <- rep(interval_to_predict, length(predictor))
  data.frame(class = other_data_to_predict$class, predictor, model_pred,
    predicted, prediction)
}

Prediction.matrix <- ldply(intervals, function(i) {
     ldply(intervals, function(j) {
    cross_predict(i, j, model_list = models)
    })})

Prediction.matrix$model_pred_bin <- assign_bins(Prediction.matrix$model_pred)
Prediction.matrix$prediction_bin <- assign_bins(Prediction.matrix$prediction)

save(Prediction.matrix,file="~/Dropbox/nescent_extinction_map/Final data/Prediction.matrix.rda")

# TODO SEAN: I commented out some lines temporarily here on 20140113
# losing all data here: (didn't run)
# Prediction.matrix <- drop.levels(subset(Prediction.matrix,Prediction.matrix$predictor != "Neogene" & Prediction.matrix$predicted != "Neogene"))
#
# same.model <- ifelse(Prediction.matrix$predictor==Prediction.matrix$predicted,1,0)
# Prediction.matrix <- data.frame(Prediction.matrix,same.model)
# Prediction.matrix <- drop.levels(subset(Prediction.matrix,Prediction.matrix$same.model==0))

# Prediction.matrix$predictor <- factor(Prediction.matrix$predictor, levels=c("Plio_Pleistocene","Upper_Miocene","Middle_Miocene","Lower_Miocene"))
# Prediction.matrix$predicted <- factor(Prediction.matrix$predicted, levels=c("Lower_Miocene","Middle_Miocene","Upper_Miocene","Plio_Pleistocene"))

### create separate predictor and prediction interval calibrated risk data frames to merge
Predicted_cal1 <- pred_cal
Predicted_cal1$interval[Predicted_cal1$interval == "Cenozoic"] <- "Neogene"
Predicted_cal1 <- plyr::rename(Predicted_cal1, c("extinct_binned" = "model_pred_bin", "calibrated_ext_prob" = "model_pred_calibrated"))

Predicted_cal2 <- pred_cal
Predicted_cal2$interval[Predicted_cal2$interval == "Cenozoic"] <- "Neogene"
Predicted_cal2 <- plyr::rename(Predicted_cal2, c("extinct_binned" = "prediction_bin", "calibrated_ext_prob" = "prediction_calibrated"))

#colnames(Predicted_cal) <- c("class","predicted","prediction_bin","prediction_cal")
#colnames(Predictor_cal) <- c("class","predictor","prediction_bin","model_pred_cal")

# Predicted_cal <- drop.levels(subset(Predicted_cal,Predicted_cal$predicted !="Cenozoic"))
# Predictor_cal <- drop.levels(subset(Predictor_cal,Predictor_cal$predictor !="Cenozoic"))

Prediction.matrix2 <- rename(Prediction.matrix, c("predictor" = "interval"))
Prediction.matrix2 <- join(Prediction.matrix2,Predicted_cal1)
Prediction.matrix2 <- rename(Prediction.matrix2, c("interval" = "predictor"))
Prediction.matrix2 <- rename(Prediction.matrix2, c("predicted" = "interval"))
Prediction.matrix2 <- join(Prediction.matrix2, Predicted_cal2)
Prediction.matrix2 <- rename(Prediction.matrix2, c("interval" = "predicted"))

#head(Prediction.matrix3)
#Prediction.matrix2 <- drop.levels(subset(Prediction.matrix,Prediction.matrix$class=="Elasmobranchii"))

# uncalibrated predictions:
ggplot(Prediction.matrix,aes(log(prediction),log(model_pred),colour=class)) + geom_abline(size=.35,linetype=1,colour="black") + facet_grid(predicted ~ predictor,scales="free") + geom_point(size=1.25,pch=16,alpha=.6)

# binned but not calibrated:
ggplot(Prediction.matrix,aes(log(prediction_bin),log(model_pred_bin),colour=class)) + geom_abline(size=.35,linetype=1,colour="black") + facet_grid(predicted ~ predictor,scales="free") + geom_point(size=1.25,pch=16,alpha=.6, position = position_jitter())

# look at calibration factors:
p <- ggplot(pred_cal, aes(extinct_binned, calibrated_ext_prob)) + geom_line() + geom_point() +facet_grid(class~interval)
ggsave("calibration-factors.pdf", width = 9, height = 9)


# calibrated:
ggplot(Prediction.matrix2,aes(log(model_pred_calibrated+0.01),log(prediction_calibrated + 0.01),colour=class))+ facet_grid(predicted ~ predictor) + geom_jitter(size=1.25,pch=16,alpha=.6) + theme(aspect.ratio=1) + geom_smooth(data=Prediction.matrix2,aes(log(model_pred_calibrated + 0.01), log(prediction_calibrated + 0.01)), method="lm",colour="black") + geom_abline(intercept = 0, slope = 1, col = "black", alpha = 0.4, lty = 2)

ggplot(Prediction.matrix2,aes(log(model_pred+0.01),log(prediction + 0.01),colour=class))+ facet_grid(predicted ~ predictor) + geom_point(size=1.25,pch=16,alpha=.6, position = position_jitter()) + theme(aspect.ratio=1) + geom_smooth(data=Prediction.matrix2,aes(log(model_pred + 0.01), log(prediction + 0.01)), method="lm",colour="black") + geom_abline(intercept = 0, slope = 1, col = "black", alpha = 0.4, lty = 2)


ggplot(Prediction.matrix2,aes(model_pred, model_pred_calibrated, colour=class)) + facet_grid(predicted ~ predictor,scales="free") + geom_point(size=1.25,pch=16,alpha=.6) + geom_abline(intercept = 0, slope = 1, col = "black", alpha = 0.4) + scale_y_continuous(limits=c(0, 1)) + scale_x_continuous(limits=c(0, 1))

####------------------------------------------------------------

png()
p <- ggplot(Prediction.matrix3,aes(prediction,model_pred,colour=class)) + geom_abline(size=.35,linetype=1,colour="black") + facet_grid(predicted ~ predictor,scales="free") + geom_jitter(size=1.25,pch=16,alpha=.6) + scale_colour_manual(values=c("red","blue","seagreen3","cyan2","orange","darkorchid2","magenta3","lightsalmon2"))+ xlab(expression("Risk, predicted by other models")) + ylab(expression("Risk, prediction interval model")) + opts(axis.text.x = theme_text(size=6)) + opts(axis.text.y = theme_text(size=6))+ opts(panel.background = theme_rect(colour="black", size =1, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank())  + theme(strip.background = element_rect(fill = "white")) + theme(aspect.ratio=1) + geom_smooth(data=Prediction.matrix3,aes(prediction,model_pred), method="lm",colour="black")
print(p)

ggsave(p,file="~/Dropbox/nescent_extinction_map/r/crossplot.matrix.of.Cenozoic.model.predictions.png",width=7,height=7)
dev.off()



Spear_Cor <- function(df) {
	new.pred <- log(df$prediction_cal)
	model.pred <- log(df$model_pred_cal)
	scale(new.pred)
	scale(model.pred)
	SpearRsq <- cor(new.pred,model.pred,method="spearman")^2
}

N_genera <- function(df) length(df$prediction_cal)
PredMatrix <- ddply(Prediction.matrix3,.(class,predictor,predicted),each(N_genera,Spear_Cor))
colnames(PredMatrix) <- c("Class","Model.Interval","Prediction.Model.Interval","N_genera","Rsq")

p <- ggplot(PredMatrix,aes(Rsq,group=Class))
p + geom_histogram(binwidth=.05) + facet_wrap(~Class)
Rsq_mod <- ifelse(PredMatrix$Rsq > .80,NA,PredMatrix$Rsq)
Rsq_mod <- round(Rsq_mod*10,1)/10
median(na.omit(Rsq_mod))
PredMatrix <- data.frame(PredMatrix,Rsq_mod)

PredMatrix$Prediction.Model.Interval <- factor(PredMatrix$Prediction.Model.Interval, levels=c("Plio_Pleistocene","U_Miocene","M_Miocene","L_Miocene"))
PredMatrix$Model.Interval <- factor(PredMatrix$Model.Interval, levels=c("Plio_Pleistocene","U_Miocene","M_Miocene","L_Miocene"))

png()
p2 <- ggplot(PredMatrix,aes(Model.Interval,Prediction.Model.Interval,fill=Rsq_mod,label=Rsq_mod)) + geom_tile() + scale_fill_gradient(low="lightblue",high="blue") + geom_text() + coord_equal() + xlab(expression("Predictor Interval")) + ylab(expression("Predicted Interval")) + opts(panel.background = theme_rect(colour="white", size =.1, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank())
print(p2)
ggsave(file="~/Dropbox/nescent_extinction_map/r/Spearman.Rsquared.matrix.of.Cenozoic.model.predictions.png",width=7,height=7)
dev.off()
