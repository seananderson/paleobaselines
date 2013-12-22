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


Neogene.mean <- ddply(other.models,.(class,group,genus,occupancy,occurrences,great.circle,richness,lat_range,min.lat,max.lat,mean.lat),each(model.mean,model.mean.quantile))
colnames(Neogene.mean) <- c("class","group","genus","occupancy","occurrences","great.circle","richness","lat_range","min.lat","max.lat","mean.lat","risk","risk_quantile")
training_interval <- rep("Neogene.mean",length(Neogene.mean[,1]))
Neogene.mean <- data.frame(training_interval,Neogene.mean)

all.models <- rbind(Plio_Pleistocene,U_Miocene,M_Miocene,L_Miocene,Neogene.all,Neogene.mean)

Use.model <- drop.levels(subset(all.models,all.models$training_interval == Prediction_model))
max.rank <- max(as.numeric(factor(Use.model$risk)))
Use.model$Risk.rank <- as.numeric(factor(Use.model$risk))/max.rank

Ext.Use <- rep(Extinction_metric,length(Use.model[,1]))


use.risk <- ifelse(Ext.Use=="Risk Rank",Use.model$Risk.rank,ifelse(Ext.Use == "Risk Quartile",Use.model$risk_quantile,Use.model$risk))
                   
Use.model <- data.frame(Use.model,use.risk)


save(Use.model,file="~/Dropbox/nescent_extinction_map/Final data/Risk_predictions.rda")


## keep deactivated unless you want to overwrite this file
#write.table(Use.model,"~/Dropbox/nescent_extinction_map/Final data/Modern_Risk_Predictions.csv",sep=",")
## keep deactivated except to add a new prediction column to previous spreadsheet
new.pred <- data.frame(Use.model$class,Use.model$genus,Use.model$use.risk)
colnames(new.pred) <- c("class","genus","use.risk.cull")
old.preds <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Modern_Risk_Predictions.csv",header=TRUE,stringsAsFactors=FALSE)


Predictions <- merge(old.preds,new.pred,by=c("class","genus"))
png()
p <- ggplot(Predictions,aes(log(use.risk),log(use.risk.cull),colour=class))+geom_point(pch=1) + geom_abline() + geom_smooth(method="lm")
print(p)
ggsave(p,file="~/Dropbox/nescent_extinction_map/r/compare.modern.predictions.png",width=8,height=7)
dev.off()




