require(ggplot2)
require(gdata)

load("~/Dropbox/nescent_extinction_map/Final data/partial.groups.cenozoic.rda")
load("~/Dropbox/nescent_extinction_map/Final data/partial.others.cenozoic.rda")
groups <- drop.levels(subset(partial.groups.cenozoic,partial.groups.cenozoic$stage!="Cenozoic"))
others <- drop.levels(subset(partial.others.cenozoic,partial.others.cenozoic$stage!="Cenozoic"))


Group_Preds_All <- drop.levels(subset(partial.groups.cenozoic,partial.groups.cenozoic$stage=="Cenozoic"))
Other_Preds_All <- drop.levels(subset(partial.others.cenozoic,partial.others.cenozoic$stage=="Cenozoic"))



png()
Group <- ggplot() + geom_point(data= partial.groups.cenozoic, aes(x=value, y=response,colour=stage,group=stage),size=6,alpha=.6,pch="-") + scale_y_continuous(limits = c(0,1)) + ylab(expression("")) + xlab(expression("")) + opts(axis.text.x = theme_text(size=6,angle=270)) + opts(axis.text.y = theme_text(size=6)) + opts(panel.background = theme_rect(colour="black", size =1, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank()) + geom_point(data= Group_Preds_All, aes(x=value, y=response,group=stage),size=3,colour="black",pch=18)
print(Group)
ggsave(Group ,file="~/Dropbox/nescent_extinction_map/r/Group.partial.dependence.plot.png",width=6.7,height=3.1)
dev.off()

max.val <- max(partial.others.cenozoic$response)
min.val <- min(partial.others.cenozoic$response)

png()
Other <- ggplot() + geom_line(data= partial.others.cenozoic, aes(x=value, y=response,colour=stage),size=.6,alpha=.6) + scale_y_continuous(limits = c(min.val-.05,max.val+.05)) + ylab(expression("")) + xlab(expression("")) + opts(axis.text.x = theme_text(size=6)) + opts(axis.text.y = theme_text(size=6))+ opts(panel.background = theme_rect(colour="black", size =1, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank())  + facet_wrap(~predictor,scales = "free_x",ncol=3)+ opts(strip.background = theme_rect(fill = "white")) + geom_line(data= Other_Preds_All, aes(x=value, y=response),size=1,colour="black")
print(Other)
ggsave(Other,file="~/Dropbox/nescent_extinction_map/r/Other.partial.dependence.plot.png",width=6.7,height=5)
dev.off()


### summarize means
#stat_sum_single <- function(fun, geom="point", ...) {
#  stat_summary(fun.y=fun, colour="black", geom=geom, size = .5, ...)
#  }  
#p  + stat_sum_single(mean, geom="line")
