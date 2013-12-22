require(gdata)
require(plyr)
require(gbm)
require(caret)
require(ggplot2)

# load predictors:
data <- load("~/Dropbox/nescent_extinction_map/Final data/standardized.predictors.Cenozoic.OBIS.rda")  
data <- standardized.cenozoic 


Modern <- drop.levels(subset(data,data$stage == "Modern"))
Neogene <- drop.levels(subset(data,data$stage_top < 22))
Plio_Pleistocene <- drop.levels(subset(data,data$stage == "Plio-Pleistocene"))
U_Miocene <- drop.levels(subset(data,data$stage == "Upper Miocene"))
M_Miocene <- drop.levels(subset(data,data$stage == "Middle Miocene"))
L_Miocene <- drop.levels(subset(data,data$stage == "Lower Miocene"))

Plio_Pleistocene_classes <- Plio_Pleistocene$class
U_Miocene_classes <- U_Miocene$class
M_Miocene_classes <- M_Miocene$class
L_Miocene_classes <- L_Miocene$class
Neogene_classes <- Neogene$class
Modern_classes <- Modern$class

## remove extraneous variables and set "group" to factor

 drops <- c("stage","stage_top","genus","Ex")

Modern <- Modern[,!(names(Modern) %in% drops)]
Neogene <- Neogene[,!(names(Neogene) %in% drops)]
Plio_Pleistocene <- Plio_Pleistocene[,!(names(Plio_Pleistocene) %in% drops)]
U_Miocene <- U_Miocene[,!(names(U_Miocene) %in% drops)]
M_Miocene <- M_Miocene[,!(names(M_Miocene) %in% drops)]
L_Miocene <- L_Miocene[,!(names(L_Miocene) %in% drops)]

Modern$group <- as.factor(Modern$group)
Neogene$group <- as.factor(Neogene$group)
Plio_Pleistocene$group <- as.factor(Plio_Pleistocene$group)
U_Miocene$group <- as.factor(U_Miocene$group)
M_Miocene$group <- as.factor(M_Miocene$group)
L_Miocene $group <- as.factor(L_Miocene $group)

Modern$class <- as.factor(Modern$class)
Neogene$class <- as.factor(Neogene$class)
Plio_Pleistocene$class <- as.factor(Plio_Pleistocene$class)
U_Miocene$class <- as.factor(U_Miocene$class)
M_Miocene$class <- as.factor(M_Miocene$class)
L_Miocene $class <- as.factor(L_Miocene $class)

Modern_groups <- levels(Modern$group)
Neogene_groups <- levels(Neogene$group)
Plio_Pleistocene_groups <- levels(Plio_Pleistocene$group)
U_Miocene_groups <- levels(U_Miocene$group)
M_Miocene_groups <- levels(M_Miocene$group)
L_Miocene_groups <- levels(L_Miocene$group)



load("~/Dropbox/nescent_extinction_map/Final data/caret.gbm.model.sets.rda")
models <- caret.gbm.model.sets
names(models) <- c("L_Miocene","M_Miocene","U_Miocene","Plio_Pleistocene","Neogene")

###Pleistocene
model <- models$Plio_Pleistocene
predict_model <- "Plio_Pleistocene"
predict_levels <- levels(Plio_Pleistocene$group)

prediction <- predict(model, newdata = Plio_Pleistocene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Plio_Pleistocene",length(predictor))
model_pred <- predict(models$Plio_Pleistocene, newdata = Plio_Pleistocene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- Plio_Pleistocene_classes
Plio_Pleis <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = U_Miocene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("U_Miocene",length(predictor))
model_pred <- predict(models$U_Miocene, newdata = U_Miocene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- U_Miocene_classes
U_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = M_Miocene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("M_Miocene",length(predictor))
model_pred <- predict(models$M_Miocene, newdata = M_Miocene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- M_Miocene_classes
M_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = L_Miocene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("L_Miocene",length(predictor))
model_pred <- predict(models$L_Miocene, newdata = L_Miocene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- L_Miocene_classes
L_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = Neogene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Neogene",length(predictor))
model_pred <- predict(models$Neogene, newdata = Neogene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- Neogene_classes
Neo <- data.frame(class,predictor,predicted,prediction,model_pred)

Plio_Pleistocene_preds <- rbind(Plio_Pleis,U_Mio,M_Mio,L_Mio,Neo)


###U_Miocene
model <- models$U_Miocene
predict_model <- "U_Miocene"

prediction <- predict(model, newdata = Plio_Pleistocene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Plio_Pleistocene",length(predictor))
model_pred <- predict(models$Plio_Pleistocene, newdata = Plio_Pleistocene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- Plio_Pleistocene_classes
Plio_Pleis <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = U_Miocene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("U_Miocene",length(predictor))
model_pred <- predict(models$U_Miocene, newdata = U_Miocene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- U_Miocene_classes
U_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = M_Miocene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("M_Miocene",length(predictor))
model_pred <- predict(models$M_Miocene, newdata = M_Miocene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- M_Miocene_classes
M_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = L_Miocene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("L_Miocene",length(predictor))
model_pred <- predict(models$L_Miocene, newdata = L_Miocene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- L_Miocene_classes
L_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = Neogene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Neogene",length(predictor))
model_pred <- predict(models$Neogene, newdata = Neogene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- Neogene_classes
Neo <- data.frame(class,predictor,predicted,prediction,model_pred)

U_Miocene_preds <- rbind(Plio_Pleis,U_Mio,M_Mio,L_Mio,Neo)

###M_Miocene
model <- models$M_Miocene
predict_model <- "M_Miocene"

prediction <- predict(model, newdata = Plio_Pleistocene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Plio_Pleistocene",length(predictor))
model_pred <- predict(models$Plio_Pleistocene, newdata = Plio_Pleistocene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- Plio_Pleistocene_classes
Plio_Pleis <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = U_Miocene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("U_Miocene",length(predictor))
model_pred <- predict(models$U_Miocene, newdata = U_Miocene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- U_Miocene_classes
U_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = M_Miocene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("M_Miocene",length(predictor))
model_pred <- predict(models$M_Miocene, newdata = M_Miocene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- M_Miocene_classes
M_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = L_Miocene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("L_Miocene",length(predictor))
model_pred <- predict(models$L_Miocene, newdata = L_Miocene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- L_Miocene_classes
L_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = Neogene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Neogene",length(predictor))
model_pred <- predict(models$Neogene, newdata = Neogene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- Neogene_classes
Neo <- data.frame(class,predictor,predicted,prediction,model_pred)

M_Miocene_preds <- rbind(Plio_Pleis,U_Mio,M_Mio,L_Mio,Neo)

###L_Miocene
model <- models$L_Miocene
predict_model <- "L_Miocene"

prediction <- predict(model, newdata = Plio_Pleistocene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Plio_Pleistocene",length(predictor))
model_pred <- predict(models$Plio_Pleistocene, newdata = Plio_Pleistocene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- Plio_Pleistocene_classes
Plio_Pleis <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = U_Miocene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("U_Miocene",length(predictor))
model_pred <- predict(models$U_Miocene, newdata = U_Miocene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- U_Miocene_classes
U_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = M_Miocene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("M_Miocene",length(predictor))
model_pred <- predict(models$M_Miocene, newdata = M_Miocene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- M_Miocene_classes
M_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = L_Miocene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("L_Miocene",length(predictor))
model_pred <- predict(models$L_Miocene, newdata = L_Miocene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- L_Miocene_classes
L_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = Neogene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Neogene",length(predictor))
model_pred <- predict(models$Neogene, newdata = Neogene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- Neogene_classes
Neo <- data.frame(class,predictor,predicted,prediction,model_pred)

L_Miocene_preds <- rbind(Plio_Pleis,U_Mio,M_Mio,L_Mio,Neo)

###Neogene
model <- models$Neogene
predict_model <- "Neogene"

prediction <- predict(model, newdata = Plio_Pleistocene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Plio_Pleistocene",length(predictor))
model_pred <- predict(models$Plio_Pleistocene, newdata = Plio_Pleistocene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- Plio_Pleistocene_classes
Plio_Pleis <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = U_Miocene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("U_Miocene",length(predictor))
model_pred <- predict(models$U_Miocene, newdata = U_Miocene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- U_Miocene_classes
U_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = M_Miocene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("M_Miocene",length(predictor))
model_pred <- predict(models$M_Miocene, newdata = M_Miocene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- M_Miocene_classes
M_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = L_Miocene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("L_Miocene",length(predictor))
model_pred <- predict(models$L_Miocene, newdata = L_Miocene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- L_Miocene_classes
L_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = Neogene, type="prob")
str(prediction)
prediction <- prediction$gbm$extinct
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Neogene",length(predictor))
model_pred <- predict(models$Neogene, newdata = Neogene, type="prob")
str(model_pred)
model_pred <- model_pred$gbm$extinct
class <- Neogene_classes
Neo <- data.frame(class,predictor,predicted,prediction,model_pred)

Neogene_preds <- rbind(Plio_Pleis,U_Mio,M_Mio,L_Mio,Neo)

Prediction.matrix <- rbind(Plio_Pleistocene_preds,U_Miocene_preds,M_Miocene_preds,L_Miocene_preds,Neogene_preds)

save(Prediction.matrix,file="~/Dropbox/nescent_extinction_map/Final data/Prediction.matrix.rda")

Prediction.matrix <- drop.levels(subset(Prediction.matrix,Prediction.matrix$predictor != "Neogene" & Prediction.matrix$predicted != "Neogene"))

same.model <- ifelse(Prediction.matrix$predictor==Prediction.matrix$predicted,1,0)
Prediction.matrix <- data.frame(Prediction.matrix,same.model)
Prediction.matrix <- drop.levels(subset(Prediction.matrix,Prediction.matrix$same.model==0))

Prediction.matrix$predictor <- factor(Prediction.matrix$predictor, levels=c("Plio_Pleistocene","U_Miocene","M_Miocene","L_Miocene"))
Prediction.matrix$predicted <- factor(Prediction.matrix$predicted, levels=c("L_Miocene","M_Miocene","U_Miocene","Plio_Pleistocene"))

Prediction.matrix2 <- drop.levels(subset(Prediction.matrix,Prediction.matrix$class=="Elasmobranchii"))

png()
p <- ggplot(Prediction.matrix,aes(log(prediction),log(model_pred),colour=class)) + geom_abline(size=.35,linetype=1,colour="black") + facet_grid(predicted ~ predictor,scales="free") + geom_point(size=1.25,pch=16,alpha=.6) + scale_colour_manual(values=c("red","blue","seagreen3","cyan2","orange","darkorchid2","magenta3","lightsalmon2"))+ xlab(expression("Log risk, predicted by other models")) + ylab(expression("Log risk, prediction interval model")) + opts(axis.text.x = theme_text(size=6)) + opts(axis.text.y = theme_text(size=6))+ opts(panel.background = theme_rect(colour="black", size =1, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank())  + theme(strip.background = element_rect(fill = "white")) + theme(aspect.ratio=1) + geom_smooth(data=Prediction.matrix,aes(log(prediction),log(model_pred)),method="lm",colour="black") 
print(p)

ggsave(p,file="~/Dropbox/nescent_extinction_map/r/crossplot.matrix.of.Cenozoic.model.predictions.png",width=7,height=7)
dev.off()



Spear_Cor <- function(df) {
	new.pred <- log(df$prediction)
	model.pred <- log(df$model_pred)
	scale(new.pred)
	scale(model.pred)
	SpearRsq <- cor(new.pred,model.pred,method="spearman")^2
}

N_genera <- function(df) length(df$prediction)
PredMatrix <- ddply(Prediction.matrix,.(class,predictor,predicted),each(N_genera,Spear_Cor))
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