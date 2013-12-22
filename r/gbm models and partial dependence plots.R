library(gbm)
library(ggplot2)
library(gdata)

#data <- read.csv("~/Dropbox/nescent_extinction_map/Final data/All_Neogene_Transformed.csv",header = TRUE,stringsAsFactors = FALSE)
data <- read.csv("~/Dropbox/nescent_extinction_map/Final data/All_Neogene_Transformed.csv",header = TRUE,stringsAsFactors = FALSE)


train_data <- drop.levels(subset(data,data$stage != "????"))

# ignore for now:
predict_data <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Modern_Transformed.csv",header = TRUE,stringsAsFactors = FALSE)

# only train data for the plot

# All_Cenozoic_Transformed

#unique(data$Interval_Name)
#  [1] "Danian"              "Selandian_Thanetian" "Ypresian"            "Lutetian"           
#  [5] "Bartonian"           "Priabonian"          "Rupelian"            "Chattian"           
#  [9] "Lower Miocene"       "Middle Miocene"      "Upper Miocene"       "Pliocene"           
# [13] "Pleistocene"         delete"Spalding_raw"        delete"Spalding_merged"    

# replace with "Modern"



genus <- predict_data$genus

stage_name <- "Neogene_ALL"

###get data into shape for model prediction -transform group into factor and remove extraneous variables

group <- as.factor(train_data$group)

drops <- c("stage","genus","group")

train_data <- train_data[,!(names(train_data) %in% drops)]

train_data <- data.frame(train_data,group)

group <- as.factor(predict_data$group)

predict_data <- predict_data[,!(names(predict_data) %in% drops)]

predict_data <- data.frame(predict_data,group)


### make vector of weights for train_data
obs <- length(train_data$Ex)
exes <- sum(train_data$Ex)
ExFreq <- (exes/obs)
SurFreq <- (1-(exes/obs))
MaxFreq <- max(ExFreq,SurFreq)
ExWeight <- 1/(ExFreq/MaxFreq)
SurWeight <- 1/(SurFreq/MaxFreq)
Weights <- ifelse(train_data$Ex==1,ExWeight,SurWeight)




Fossil.mod <- gbm(Ex ~ ., distribution="bernoulli", data=train_data, weights = Weights, n.trees=2000, shrinkage=0.01, cv.folds=5, verbose=FALSE)
#gbm.perf(Fossil.mod, plot.it=TRUE,method="cv")
preds <- data.frame(genus,predict(Fossil.mod, newdata = predict_data , type="response"))

write.table(preds,"~/Dropbox/nescent_extinction_map/Final data/Extinction_risk_predictions.csv",sep=",")

#save(Fossil.mod, file = "~/Dropbox/nescent_extinction_map/Final data/Neogene.gbm.model.rda")

#model <- load("~/Dropbox/nescent_extinction_map/Final data/Neogene.gbm.model.rda")
#Fossil.mod <- data$Fossil.mod


relative.influence(Fossil.mod, 1000)
#par(mfrow=c(2,4))
#plot(...)
one <- plot.gbm(Fossil.mod, i.var = 1, n.trees = Fossil.mod$n.trees, continuous.resolution = 100, return.grid = TRUE, type = "response")
two <- plot.gbm(Fossil.mod, i.var = 2, n.trees = Fossil.mod$n.trees, continuous.resolution = 100, return.grid = TRUE, type = "response")
three <- plot.gbm(Fossil.mod, i.var = 3, n.trees = Fossil.mod$n.trees, continuous.resolution = 100, return.grid = TRUE, type = "response")
four <- plot.gbm(Fossil.mod, i.var = 4, n.trees = Fossil.mod$n.trees, continuous.resolution = 100, return.grid = TRUE, type = "response")
five <- plot.gbm(Fossil.mod, i.var = 5, n.trees = Fossil.mod$n.trees, continuous.resolution = 100, return.grid = TRUE, type = "response")
six <- plot.gbm(Fossil.mod, i.var = 6, n.trees = Fossil.mod$n.trees, continuous.resolution = 100, return.grid = TRUE, type = "response")
seven <- plot.gbm(Fossil.mod, i.var = 7, n.trees = Fossil.mod$n.trees, continuous.resolution = 100, return.grid = TRUE, type = "response")

Group <- data.frame(rep("group",length(one[,1])),one)
colnames(Group) <- c("predictor","value","response")
stage <- rep(stage_name,length(Group[,1]))
Group <- data.frame(stage,Group)


two<- data.frame(rep("richness",length(two[,1])),two)
colnames(two) <- c("predictor","value","response")

three <- data.frame(rep("occupancy",length(three[,1])),three)
colnames(three) <- c("predictor","value","response")

four <- data.frame(rep("occurrences",length(four[,1])),four)
colnames(four) <- c("predictor","value","response")

five <- data.frame(rep("min.lat",length(five[,1])),five)
colnames(five) <- c("predictor","value","response")

six <- data.frame(rep("max.lat",length(six[,1])),six )
colnames(six ) <- c("predictor","value","response")

seven <- data.frame(rep("lat.range",length(seven[,1])),seven)
colnames(seven) <- c("predictor","value","response")

Others <- rbind(two,three,four,five,six,seven)
stage <- rep(stage_name,length(Others[,1]))
Others <- data.frame(stage,Others)

M_Miocene_Group <- Group
M_Miocene_Others <- Others


All_Neogene_Group
Pli_Group
Pleis_Group
U_Mio_Group
M_Mio_Group
L_Mio_Group
Chattian_Group
Rupelian_Group
Priabonian_Group 
Bartonian_Group
Lutetian_Group
Ypresian_Group
Selandian_Thanetian_Group 
Danian_Group
All_Cenozoic_Group

All_Neogene_Others
Pli_Others
Pleis_Others
U_Mio_Others
M_Mio_Others
L_Mio_Others
Chattian_Others
Rupelian_Others
Priabonian_Others
Bartonian_Others
Lutetian_Others
Ypresian_Others
Selandian_Thanetian_Others
Danian_Others
All_Cenozoic_Others


Group_Pred_Cenozoic <- rbind(Pli_Group,Pleis_Group,U_Mio_Group,M_Mio_Group,L_Mio_Group,All_Neogene_Group,Chattian_Group,Rupelian_Group,Priabonian_Group,Bartonian_Group,Lutetian_Group,Ypresian_Group,Selandian_Thanetian_Group,Danian_Group,All_Cenozoic_Group)

Other_Preds_Cenozoic <- rbind(Pli_Others,Pleis_Others,U_Mio_Others,M_Mio_Others,L_Mio_Others,All_Neogene_Others,Chattian_Others,Rupelian_Others,Priabonian_Others,Bartonian_Others,Lutetian_Others,Ypresian_Others,Selandian_Thanetian_Others,Danian_Others,All_Cenozoic_Others)


write.table(Group_Pred_Cenozoic,"~/Dropbox/nescent_extinction_map/Final data/Cenozoic_Group_gbm_dependence_by_stage.csv",sep=",")
write.table(Other_Preds_Cenozoic,"~/Dropbox/nescent_extinction_map/Final data/Cenozoic_Other_predictor_gbm_dependence_by_stage.csv",sep=",")

Group_Pred <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Cenozoic_Group_gbm_dependence_by_stage.csv",header = TRUE,stringsAsFactors = FALSE)
#Group_Pred <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Neogene_Group_gbm_dependence_by_stage.csv",header = TRUE,stringsAsFactors = FALSE)

Other_Preds <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Cenozoic_Other_predictor_gbm_dependence_by_stage.csv",header = TRUE,stringsAsFactors = FALSE)
#Other_Preds <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Neogene_Other_predictor_gbm_dependence_by_stage.csv",header = TRUE,stringsAsFactors = FALSE)





Group_Preds_All <- drop.levels(subset(Group_Pred,Group_Pred$stage=="All_Cenozoic"))
Other_Preds_All <- drop.levels(subset(Other_Preds,Other_Preds$stage=="All_Cenozoic"))

quartz(1,6.2,3.5)
p1 <- ggplot()
p1 + geom_point(data= Group_Pred, aes(x=value, y=response,colour=stage,group=stage),size=6,alpha=.6,pch="-") + ggtitle("Phylogenetic Group")+ scale_y_continuous(limits = c(0,.78)) + ylab(expression("")) + xlab(expression("")) + opts(axis.text.x = theme_text(size=6,angle=270)) + opts(axis.text.y = theme_text(size=6)) + opts(panel.background = theme_rect(colour="black", size =1, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank())  + geom_point(data= Group_Preds_All, aes(x=value, y=response,group=stage),size=3,colour="black") + scale_colour_manual(values=c("black","blue","forestgreen","turquoise3","darkorange","firebrick1"))

#scale_colour_manual(values=c("black","blue","forestgreen","gold1","darkorange","red"))
#scale_colour_manual(values=c("black","blue","forestgreen","turquoise3","darkorange","firebrick1"))


quartz(1,6.2,4)
p1 <- ggplot()
p1 + geom_line(data= Other_Preds, aes(x=value, y=response,colour=stage),size=.6,alpha=.6) + scale_y_continuous(limits = c(0,.12)) + ylab(expression("")) + xlab(expression("")) + opts(axis.text.x = theme_text(size=6)) + opts(axis.text.y = theme_text(size=6))+ opts(panel.background = theme_rect(colour="black", size =1, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank()) + geom_line(data= Other_Preds_All, aes(x=value, y=response),size=1,colour="black")+ facet_wrap(~predictor,scales = "free_x",ncol=3)+ opts(strip.background = theme_rect(fill = "white")) 

#scale_colour_manual(values=c("black","blue","forestgreen","turquoise3","darkorange","firebrick1")) 







quartz(1,10,4)
p2 <- ggplot(Other_Preds, aes(x=value, y=response,colour=stage)) 
p2 + geom_line(alpha=.7,size=1) + scale_y_continuous(limits = c(0,.125)) + 
ylab(expression("")) + xlab(expression("")) + opts(axis.text.x = theme_text(size=8,angle=0)) + opts(panel.background = theme_rect(colour="black", size =1, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank()) + scale_colour_brewer(palette="Set1") + facet_wrap(~predictor,scales = "free_x",ncol=3)+ opts(strip.background = theme_rect(fill = "white")) 






one <- data.frame(rep("group",length(one[,1])),one)
colnames(one) <- c("predictor","value","response")

two<- data.frame(rep("richness",length(two[,1])),two)
colnames(two) <- c("predictor","value","response")

three <- data.frame(rep("occupancy",length(three[,1])),three)
colnames(three) <- c("predictor","value","response")

four <- data.frame(rep("occurrences",length(four[,1])),four)
colnames(four) <- c("predictor","value","response")

five <- data.frame(rep("min.lat",length(five[,1])),five)
colnames(five) <- c("predictor","value","response")

six <- data.frame(rep("max.lat",length(six[,1])),six )
colnames(six ) <- c("predictor","value","response")

seven <- data.frame(rep("lat.range",length(seven[,1])),seven)
colnames(seven) <- c("predictor","value","response")




p1 <-  ggplot(one, aes(x=value, y=response)) + geom_point() + ggtitle("Group") + scale_y_continuous(limits = c(0,1)) + ylab(expression("")) + xlab(expression("")) + opts(axis.text.x = theme_text(size=8,angle=-25))

p2 <-  ggplot(two, aes(x=value, y=response)) + geom_line() + ggtitle("Richness") + scale_y_continuous(limits = c(0,0.12))+ ylab(expression("")) + xlab(expression(""))

p3 <- ggplot(three, aes(x=value, y=response)) + geom_line() + ggtitle("Occupancy")+ scale_y_continuous(limits = c(0,0.12))+ ylab(expression("")) + xlab(expression(""))

p4 <- ggplot(four, aes(x=value, y=response)) + geom_line() + ggtitle("Occurrences")+ scale_y_continuous(limits = c(0,0.12))+ ylab(expression("")) + xlab(expression(""))

p5 <- ggplot(five, aes(x=value, y=response)) + geom_line() + ggtitle("Minimum Latitude")+ scale_y_continuous(limits = c(0,0.12))+ ylab(expression("")) + xlab(expression(""))

p6 <- ggplot(six, aes(x=value, y=response)) + geom_line() + ggtitle("Maximum Latitude")+ scale_y_continuous(limits = c(0,0.12))+ ylab(expression("")) + xlab(expression(""))

p7 <- ggplot(seven, aes(x=value, y=response)) + geom_line() + ggtitle("Latitudinal Range")+ scale_y_continuous(limits = c(0,0.12))+ 
ylab(expression("")) + xlab(expression(""))


multiplot(p1, p2, p3, p4, p5, p6, p7, layout = matrix(c(1,1,1,2,3,4,5,6,7), 3, 3, byrow = TRUE))





