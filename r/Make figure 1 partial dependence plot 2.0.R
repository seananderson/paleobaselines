require(ggplot2)
require(gdata)

load("~/Dropbox/nescent_extinction_map/Final data/partial.groups.cenozoic.rda")
load("~/Dropbox/nescent_extinction_map/Final data/partial.others.cenozoic.rda")
groups <- drop.levels(subset(partial.groups.cenozoic,partial.groups.cenozoic$stage!="Cenozoic"))
others <- drop.levels(subset(partial.others.cenozoic,partial.others.cenozoic$stage!="Cenozoic"))


smoothed_value <- function(df) {
		ifelse(df$predictor == "richness" | df$predictor ==  "occupancy" | df$predictor ==  "occurrences",round(df$value,1), ifelse(df$predictor == "min.lat" | df$predictor ==  "max.lat" |  df$predictor ==  "lat.range" | df$predictor ==  "max.lat" | df$predictor ==  "mean.lat", round(df$value/10,0)*10,round(df$value/500,0)*500))
}
		
		
smoothed_others <- ddply(others,.(stage,predictor,value),each(smoothed_value))
others <- merge(others,smoothed_others)


Neogene_mean <- function(df) mean(df$response)
other_mean <- ddply(others,.(predictor,smoothed_value),each(Neogene_mean))
other_mean$stage <- rep("Neogene mean", length(other_mean[,1]))
colnames(other_mean) <- c("predictor","value","response","stage")

others <- data.frame(others$predictor,others$value,others$response,others$stage)
colnames(others) <- c("predictor","value","response","stage")

others <-rbind(others,other_mean)
others$weight <- ifelse(others$stage == "Neogene mean",2,1)


groups <- data.frame(as.factor(groups$value),groups$response,groups$stage)
colnames(groups) <- c("value","response","stage")

group_mean <- ddply(groups,.(value),each(Neogene_mean))
group_mean$stage <- rep("Neogene mean", length(group_mean[,1]))
colnames(group_mean) <- c("value","response","stage")

groups <- rbind(groups,group_mean)
groups$weight <- ifelse(groups$stage == "Neogene mean",2,1)

Group_Mean <- drop.levels(subset(groups,groups$stage=="Neogene mean"))
Other_Mean <- drop.levels(subset(others,others$stage=="Neogene mean"))

groups$stage <- factor(groups$stage, levels=c("Lower Miocene","Middle Miocene","Upper Miocene","Plio-Pleistocene","Neogene mean"))

png()
Group <- ggplot(groups, aes(value,response,colour=stage,group=stage)) + geom_line(size=.75) + geom_point() + scale_y_continuous(limits = c(0,1)) + ylab(expression("")) + xlab(expression("")) + opts(axis.text.x = theme_text(size=6,angle=270)) + opts(axis.text.y = theme_text(size=6)) + opts(panel.background = theme_rect(colour="black", size =1, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank()) + scale_colour_manual(values = c("red","orange","green2","blue","black"))+ geom_line(data= Group_Mean, aes(x=value, y=response,group=stage),size=1.1,colour="black")
print(Group)
ggsave(Group ,file="~/Dropbox/nescent_extinction_map/r/Group.partial.dependence.plot.png",width=6.7,height=2.9)
dev.off()

max.val <- max(partial.others.cenozoic$response)
min.val <- min(partial.others.cenozoic$response)

others$predictor <- factor(others$predictor, levels=c("occupancy","occurrences","richness","great.circle","max.lat","min.lat","mean.lat","lat.range"))
others$stage <- factor(others$stage, levels=c("Lower Miocene","Middle Miocene","Upper Miocene","Plio-Pleistocene","Neogene mean"))

png()
Other <- ggplot(others, aes(value,response,colour=stage,group=stage)) + geom_line(size=.75) + scale_y_continuous(limits = c(min.val-.05,max.val+.05)) + ylab(expression("")) + xlab(expression("")) + opts(axis.text.x = theme_text(size=6)) + opts(axis.text.y = theme_text(size=6))+ opts(panel.background = theme_rect(colour="black", size =1, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank())  + facet_wrap(~predictor,scales = "free_x",ncol=4)+ opts(strip.background = theme_rect(fill = "white")) + scale_colour_manual(values = c("red","orange","green2","blue","black")) + geom_line(data= Other_Mean, aes(x=value, y=response,group=stage),size=1.1,colour="black")
print(Other)
ggsave(Other,file="~/Dropbox/nescent_extinction_map/r/Other.partial.dependence.plot.png",width=6.7,height=3.5)
dev.off()

###compare group partial dependence with preservation rate
props <- read.csv("~/Dropbox/nescent_extinction_map/Final data/group.genus.prop.complete.csv",header=TRUE)
gaps <- function(df) mean(df$PropGap)
prop.gaps <- ddply(props,.(class,group),gaps)
prop.gaps$complete <- 1 - prop.gaps$V1
prop.gaps$value <- prop.gaps$group
comp.vs.dep. <- merge(prop.gaps,Group_Mean,by="value")

png()
p4 <- ggplot(comp.vs.dep.,aes(complete,response,colour=group)) + geom_point(size=4) + xlab(expression("mean Cenozoic completeness")) + ylab(expression("Partial risk dependence")) + opts(axis.text.x = theme_text(size=10)) + opts(axis.text.y = theme_text(size=10))+ opts(panel.background = theme_rect(colour="black", size =1, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank()) + opts(legend.text=theme_text(size=5)) + guides(col = guide_legend(nrow = 15))
print(p4)
ggsave(p4,file="~/Dropbox/nescent_extinction_map/r/completeness.vs.partial.dependence.png",width=8,height=5)
dev.off()

