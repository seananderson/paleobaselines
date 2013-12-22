require(gdata)
require(plyr)
require(gbm)
require(caret)
require(ggplot2)

# load predictors:
data <- load("~/Dropbox/nescent_extinction_map/Final data/standardized.predictors.Cenozoic.OBIS.rda")  
data <- standardized.cenozoic 


### set predictor sets per stage
Neogene <- drop.levels(subset(data,data$stage_top < 22))
Pleistocene <- drop.levels(subset(data,data$stage == "Pleistocene"))
Pliocene <- drop.levels(subset(data,data$stage == "Pliocene"))
U_Miocene <- drop.levels(subset(data,data$stage == "Upper Miocene"))
M_Miocene <- drop.levels(subset(data,data$stage == "Middle Miocene"))
L_Miocene <- drop.levels(subset(data,data$stage == "Lower Miocene"))

Pleistocene_classes <- Pleistocene$class
Pliocene_classes <- Pliocene$class
U_Miocene_classes <- U_Miocene$class
M_Miocene_classes <- M_Miocene$class
L_Miocene_classes <- L_Miocene$class
Neogene_classes <- Neogene$class

### remove extraneous variables and set "group" to factor

 drops <- c("stage","stage_top","genus","class","Ex")

 
Neogene <- Neogene[,!(names(Neogene) %in% drops)]
Pleistocene <- Pleistocene[,!(names(Pleistocene) %in% drops)]
Pliocene <- Pliocene[,!(names(Pliocene) %in% drops)]
U_Miocene <- U_Miocene[,!(names(U_Miocene) %in% drops)]
M_Miocene <- M_Miocene[,!(names(M_Miocene) %in% drops)]
L_Miocene <- L_Miocene[,!(names(L_Miocene) %in% drops)]

Neogene$group <- as.factor(Neogene$group)
Pleistocene$group <- as.factor(Pleistocene$group)
Pliocene$group <- as.factor(Pliocene$group)
U_Miocene$group <- as.factor(U_Miocene$group)
M_Miocene$group <- as.factor(M_Miocene$group)
L_Miocene $group <- as.factor(L_Miocene $group)




load("~/Dropbox/nescent_extinction_map/Final data/caret.gbm.models.rda")
models <- caret.gbm.models
names(models) <- c("Pleistocene","Pliocene","U_Miocene","M_Miocene","L_Miocene","Neogene")


###Pleistocene
model <- models$Pleistocene
predict_model <- "Pleistocene"

prediction <- predict(model, newdata = Pleistocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Pleistocene",length(predictor))
model_pred <- predict(models$Pleistocene, newdata = Pleistocene, type="response", n.trees = models$Pleistocene$n.trees)
class <- Pleistocene_classes
Pleis <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = Pliocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Pliocene",length(predictor))
model_pred <- predict(models$Pliocene, newdata = Pliocene, type="response", n.trees = models$Pliocene$n.trees)
class <- Pliocene_classes
Plio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = U_Miocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("U_Miocene",length(predictor))
model_pred <- predict(models$U_Miocene, newdata = U_Miocene, type="response", n.trees = models$U_Miocene$n.trees)
class <- U_Miocene_classes
U_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = M_Miocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("M_Miocene",length(predictor))
model_pred <- predict(models$M_Miocene, newdata = M_Miocene, type="response", n.trees = models$M_Miocene$n.trees)
class <- M_Miocene_classes
M_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = L_Miocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("L_Miocene",length(predictor))
model_pred <- predict(models$L_Miocene, newdata = L_Miocene, type="response", n.trees = models$L_Miocene$n.trees)
class <- L_Miocene_classes
L_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = Neogene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Neogene",length(predictor))
model_pred <- predict(models$Neogene, newdata = Neogene, type="response", n.trees = models$Neogene$n.trees)
class <- Neogene_classes
Neo <- data.frame(class,predictor,predicted,prediction,model_pred)

Pleistocene_preds <- rbind(Pleis,Plio,U_Mio,M_Mio,L_Mio,Neo)


###Pliocene
model <- models$Pliocene
predict_model <- "Pliocene"

prediction <- predict(model, newdata = Pleistocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Pleistocene",length(predictor))
model_pred <- predict(models$Pleistocene, newdata = Pleistocene, type="response", n.trees = models$Pleistocene$n.trees)
class <- Pleistocene_classes
Pleis <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = Pliocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Pliocene",length(predictor))
model_pred <- predict(models$Pliocene, newdata = Pliocene, type="response", n.trees = models$Pliocene$n.trees)
class <- Pliocene_classes
Plio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = U_Miocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("U_Miocene",length(predictor))
model_pred <- predict(models$U_Miocene, newdata = U_Miocene, type="response", n.trees = models$U_Miocene$n.trees)
class <- U_Miocene_classes
U_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = M_Miocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("M_Miocene",length(predictor))
model_pred <- predict(models$M_Miocene, newdata = M_Miocene, type="response", n.trees = models$M_Miocene$n.trees)
class <- M_Miocene_classes
M_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = L_Miocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("L_Miocene",length(predictor))
model_pred <- predict(models$L_Miocene, newdata = L_Miocene, type="response", n.trees = models$L_Miocene$n.trees)
class <- L_Miocene_classes
L_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = Neogene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Neogene",length(predictor))
model_pred <- predict(models$Neogene, newdata = Neogene, type="response", n.trees = models$Neogene$n.trees)
class <- Neogene_classes
Neo <- data.frame(class,predictor,predicted,prediction,model_pred)

Pliocene_preds <- rbind(Pleis,Plio,U_Mio,M_Mio,L_Mio,Neo)

###U_Miocene
model <- models$U_Miocene
predict_model <- "U_Miocene"

prediction <- predict(model, newdata = Pleistocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Pleistocene",length(predictor))
model_pred <- predict(models$Pleistocene, newdata = Pleistocene, type="response", n.trees = models$Pleistocene$n.trees)
class <- Pleistocene_classes
Pleis <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = Pliocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Pliocene",length(predictor))
model_pred <- predict(models$Pliocene, newdata = Pliocene, type="response", n.trees = models$Pliocene$n.trees)
class <- Pliocene_classes
Plio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = U_Miocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("U_Miocene",length(predictor))
model_pred <- predict(models$U_Miocene, newdata = U_Miocene, type="response", n.trees = models$U_Miocene$n.trees)
class <- U_Miocene_classes
U_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = M_Miocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("M_Miocene",length(predictor))
model_pred <- predict(models$M_Miocene, newdata = M_Miocene, type="response", n.trees = models$M_Miocene$n.trees)
class <- M_Miocene_classes
M_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = L_Miocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("L_Miocene",length(predictor))
model_pred <- predict(models$L_Miocene, newdata = L_Miocene, type="response", n.trees = models$L_Miocene$n.trees)
class <- L_Miocene_classes
L_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = Neogene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Neogene",length(predictor))
model_pred <- predict(models$Neogene, newdata = Neogene, type="response", n.trees = models$Neogene$n.trees)
class <- Neogene_classes
Neo <- data.frame(class,predictor,predicted,prediction,model_pred)

U_Miocene_preds <- rbind(Pleis,Plio,U_Mio,M_Mio,L_Mio,Neo)

###M_Miocene
model <- models$M_Miocene
predict_model <- "M_Miocene"

prediction <- predict(model, newdata = Pleistocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Pleistocene",length(predictor))
model_pred <- predict(models$Pleistocene, newdata = Pleistocene, type="response", n.trees = models$Pleistocene$n.trees)
class <- Pleistocene_classes
Pleis <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = Pliocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Pliocene",length(predictor))
model_pred <- predict(models$Pliocene, newdata = Pliocene, type="response", n.trees = models$Pliocene$n.trees)
class <- Pliocene_classes
Plio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = U_Miocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("U_Miocene",length(predictor))
model_pred <- predict(models$U_Miocene, newdata = U_Miocene, type="response", n.trees = models$U_Miocene$n.trees)
class <- U_Miocene_classes
U_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = M_Miocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("M_Miocene",length(predictor))
model_pred <- predict(models$M_Miocene, newdata = M_Miocene, type="response", n.trees = models$M_Miocene$n.trees)
class <- M_Miocene_classes
M_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = L_Miocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("L_Miocene",length(predictor))
model_pred <- predict(models$L_Miocene, newdata = L_Miocene, type="response", n.trees = models$L_Miocene$n.trees)
class <- L_Miocene_classes
L_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = Neogene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Neogene",length(predictor))
model_pred <- predict(models$Neogene, newdata = Neogene, type="response", n.trees = models$Neogene$n.trees)
class <- Neogene_classes
Neo <- data.frame(class,predictor,predicted,prediction,model_pred)

M_Miocene_preds <- rbind(Pleis,Plio,U_Mio,M_Mio,L_Mio,Neo)

###L_Miocene
model <- models$L_Miocene
predict_model <- "L_Miocene"

prediction <- predict(model, newdata = Pleistocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Pleistocene",length(predictor))
model_pred <- predict(models$Pleistocene, newdata = Pleistocene, type="response", n.trees = models$Pleistocene$n.trees)
class <- Pleistocene_classes
Pleis <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = Pliocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Pliocene",length(predictor))
model_pred <- predict(models$Pliocene, newdata = Pliocene, type="response", n.trees = models$Pliocene$n.trees)
class <- Pliocene_classes
Plio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = U_Miocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("U_Miocene",length(predictor))
model_pred <- predict(models$U_Miocene, newdata = U_Miocene, type="response", n.trees = models$U_Miocene$n.trees)
class <- U_Miocene_classes
U_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = M_Miocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("M_Miocene",length(predictor))
model_pred <- predict(models$M_Miocene, newdata = M_Miocene, type="response", n.trees = models$M_Miocene$n.trees)
class <- M_Miocene_classes
M_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = L_Miocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("L_Miocene",length(predictor))
model_pred <- predict(models$L_Miocene, newdata = L_Miocene, type="response", n.trees = models$L_Miocene$n.trees)
class <- L_Miocene_classes
L_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = Neogene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Neogene",length(predictor))
model_pred <- predict(models$Neogene, newdata = Neogene, type="response", n.trees = models$Neogene$n.trees)
class <- Neogene_classes
Neo <- data.frame(class,predictor,predicted,prediction,model_pred)

L_Miocene_preds <- rbind(Pleis,Plio,U_Mio,M_Mio,L_Mio,Neo)

###Neogene
model <- models$Neogene
predict_model <- "Neogene"

prediction <- predict(model, newdata = Pleistocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Pleistocene",length(predictor))
model_pred <- predict(models$Pleistocene, newdata = Pleistocene, type="response", n.trees = models$Pleistocene$n.trees)
class <- Pleistocene_classes
Pleis <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = Pliocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Pliocene",length(predictor))
model_pred <- predict(models$Pliocene, newdata = Pliocene, type="response", n.trees = models$Pliocene$n.trees)
class <- Pliocene_classes
Plio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = U_Miocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("U_Miocene",length(predictor))
model_pred <- predict(models$U_Miocene, newdata = U_Miocene, type="response", n.trees = models$U_Miocene$n.trees)
class <- U_Miocene_classes
U_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = M_Miocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("M_Miocene",length(predictor))
model_pred <- predict(models$M_Miocene, newdata = M_Miocene, type="response", n.trees = models$M_Miocene$n.trees)
class <- M_Miocene_classes
M_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = L_Miocene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("L_Miocene",length(predictor))
model_pred <- predict(models$L_Miocene, newdata = L_Miocene, type="response", n.trees = models$L_Miocene$n.trees)
class <- L_Miocene_classes
L_Mio <- data.frame(class,predictor,predicted,prediction,model_pred)

prediction <- predict(model, newdata = Neogene, type="response", n.trees = model$n.trees)
predictor <- rep(predict_model,length(prediction))
predicted <- rep("Neogene",length(predictor))
model_pred <- predict(models$Neogene, newdata = Neogene, type="response", n.trees = models$Neogene$n.trees)
class <- Neogene_classes
Neo <- data.frame(class,predictor,predicted,prediction,model_pred)

Neogene_preds <- rbind(Pleis,Plio,U_Mio,M_Mio,L_Mio,Neo)

Prediction.matrix <- rbind(Pleistocene_preds,Pliocene_preds,U_Miocene_preds,M_Miocene_preds,L_Miocene_preds,Neogene_preds)

save(Prediction.matrix,file="~/Dropbox/nescent_extinction_map/Final data/Prediction.matrix.rda")

Prediction.matrix <- drop.levels(subset(Prediction.matrix,Prediction.matrix$predictor != "Neogene" & Prediction.matrix$predicted != "Neogene"))


Prediction.matrix$predictor <- factor(Prediction.matrix$predictor, levels=c("Pleistocene","Pliocene","U_Miocene","M_Miocene","L_Miocene"))
Prediction.matrix$predicted <- factor(Prediction.matrix$predicted, levels=c("L_Miocene","M_Miocene","U_Miocene","Pliocene","Pleistocene"))

p <- ggplot(Prediction.matrix,aes(scale(log(prediction)),scale(log(model_pred)),colour=class)) + facet_grid(predicted ~ predictor)
p + geom_point(size=1,pch=1,alpha=.8) + xlab(expression("Predictor Interval")) + ylab(expression("Predicted Interval")) + scale_x_continuous(limits=c(-4,3)) + scale_y_continuous(limits=c(-4,3))

Spear_Cor <- function(df) {
	new.pred <- log(df$prediction)
	model.pred <- log(df$model_pred)
	scale(new.pred)
	scale(model.pred)
	SpearRsq <- cor(new.pred,model.pred,method="spearman")^2
}

N_genera <- function(df) length(df$prediction)
PredMatrix <- ddply(Prediction.matrix,.(predictor,predicted),each(N_genera,Spear_Cor))
colnames(PredMatrix) <- c("Model.Interval","Prediction.Model.Interval","N_genera","Rsq")
Rsq_mod <- ifelse(PredMatrix$Rsq > .99,NA,PredMatrix$Rsq)
Rsq_mod <- round(Rsq_mod*10,1)/10

PredMatrix <- data.frame(PredMatrix,Rsq_mod)


Prediction.matrix$predictor <- factor(Prediction.matrix$predictor, levels=c("Pleistocene","Pliocene","U_Miocene","M_Miocene","L_Miocene"))
Prediction.matrix$predicted <- factor(Prediction.matrix$predicted, levels=c("Pleistocene","Pliocene","U_Miocene","M_Miocene","L_Miocene"))

#Make matrix of correlations:

library(ggplot2)
p <- ggplot(PredMatrix,aes(Model.Interval,Prediction.Model.Interval,fill=Rsq_mod,label=Rsq_mod))
p + geom_tile() + scale_fill_gradientn(colours = topo.colors(10)) + geom_text()




### break down by classes


PredMatrix <- ddply(Prediction.matrix,.(predictor,predicted,class),each(N_genera,Spear_Cor))
colnames(PredMatrix) <- c("Model.Interval","Prediction.Model.Interval","class","N_genera","Rsq")
Rsq_mod <- ifelse(PredMatrix$Rsq > .99,NA,PredMatrix$Rsq)
Rsq_mod <- round(Rsq_mod*10,1)/10

PredMatrix <- data.frame(PredMatrix,Rsq_mod)

#Make matrix of correlations:

library(ggplot2)
p <- ggplot(PredMatrix,aes(Model.Interval,Prediction.Model.Interval,fill=Rsq_mod,label=Rsq_mod))
p + geom_tile() + scale_fill_gradientn(colours = topo.colors(10)) + geom_text()

#Make histograms of prediction correlations for classes

PredMatrix <- data.frame(PredMatrix,Rsq_mod)
use <- ifelse(PredMatrix$Model.Interval == PredMatrix$Prediction.Model.Interval,NA,1)
PredMatrix <- na.omit(data.frame(PredMatrix,use))

p <- ggplot(PredMatrix,aes(PredMatrix$Rsq,group=Model.Interval)) 
p + geom_histogram() + facet_wrap(~Model.Interval)



