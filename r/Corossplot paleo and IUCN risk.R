require(gdata)
require(plyr)

load("~/Dropbox/nescent_extinction_map/Final data/predictions.cenozoic.rda")
data <- data.frame(predictions.cenozoic)
head(data)

###select model trained on full Neogene or other models
Neogene.all <- drop.levels(subset(data,data$training_interval == "Cenozoic"))
Neogene.all$training_interval <- rep("Neogene.all",length(Neogene.all[,1]))
Plio_Pleistocene <- drop.levels(subset(data,data$training_interval == "Plio-Pleistocene"))
U_Miocene <- drop.levels(subset(data,data$training_interval == "Upper Miocene"))
M_Miocene <- drop.levels(subset(data,data$training_interval == "Middle Miocene"))
L_Miocene <- drop.levels(subset(data,data$training_interval == "Lower Miocene"))

###select stage models
other.models <- drop.levels(subset(data,data$training_interval != "Cenozoic"))

###average stage models -choose "risk" for raw estimates or "risk_quantile" for quantiles
model.mean <- function(df) mean(df$risk)
model.mean.quantile <- function(df) mean(df$risk_quantile)

Neogene.mean <- ddply(other.models,.(class,group,genus),each(model.mean,model.mean.quantile))

colnames(Neogene.mean) <- c("class","group","genus","risk","risk_quantile")
training_interval <- rep("Neogene.mean",length(Neogene.mean[,1]))
Neogene.mean <- data.frame(training_interval,Neogene.mean)

all.models <- rbind(Plio_Pleistocene,U_Miocene,M_Miocene,L_Miocene,Neogene.all,Neogene.mean)

Use.model <- drop.levels(subset(all.models,all.models$training_interval == Prediction_model))

Ext.Use <- rep(Extinction_metric,length(Use.model[,1]))

use.risk <- ifelse(Ext.Use=="Raw Risk",Use.model$risk,Use.model$risk_quantile)
Use.model <- data.frame(Use.model,use.risk)

quartz()
IUCN_data <- read.csv("~/Dropbox/nescent_extinction_map/Final data/IUCN_threat_for_paleo_comparison.csv",header = TRUE,stringsAsFactors = FALSE)
IUCN <- data.frame(IUCN_data$genus,IUCN_data$Mean_IUCN_Threat)
colnames(IUCN) <- c("genus","IUCN_threat")
Paleo_IUCN <- merge(Use.model,IUCN,by="genus")
p <- ggplot(Paleo_IUCN,aes(log(risk),IUCN_threat,colour=class))
p + geom_point(pch=1,size=3) + geom_smooth(method="lm")


