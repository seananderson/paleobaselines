
by.prov.cull <- drop.levels(subset(by.prov.all,by.prov.all$N.gen >=Min.Prov.Genera))
head(by.prov.cull)
require(ggplot2)
quartz()
p <- ggplot(by.prov.cull,aes(Halpern,Burrows,colour= ext.plot,pch=Zone)) 
p + geom_point(size=3) + scale_colour_gradient2(low="green2",mid="yellow",high ="red",midpoint=-3.4)
dev.off()
## Halpern plot

png()
p1 <- ggplot(by.prov.cull,aes(Halpern,ext.plot,colour=Zone)) + geom_point(size=3,alpha=.7) + scale_colour_manual(values = c("blue","pink2","red")) + geom_point(data=by.prov.cull,aes(Halpern,ext.plot),colour="black",pch=1,size=3.2) + xlab(expression("Human Impact")) + ylab(expression("Extinction Susceptibility")) + opts(panel.background = theme_rect(colour="black", size =.5, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank()) + opts(axis.line = theme_segment(colour = "black"),panel.border = theme_blank(),panel.background = theme_blank()) 
print(p1)
ggsave(p1,file="~/Dropbox/nescent_extinction_map/r/Risk.vs.Halpern.png",width=4,height=2.6)
dev.off()

## Burrows plot
png()
p2 <- ggplot(by.prov.cull,aes(Burrows,ext.plot,colour=Zone)) + geom_point(size=3,alpha=.7) + scale_colour_manual(values = c("blue","pink2","red")) + geom_point(data=by.prov.cull,aes(Burrows,ext.plot),colour="black",pch=1,size=3.2) + xlab(expression("Velocity of Climate Change")) + ylab(expression("Extinction Susceptibility")) + opts(panel.background = theme_rect(colour="black", size =.5, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank()) + opts(axis.line = theme_segment(colour = "black"),panel.border = theme_blank(),panel.background = theme_blank()) 
print(p2)
ggsave(p2,file="~/Dropbox/nescent_extinction_map/r/Risk.vs.Burrows.png",width=4,height=2.6)
dev.off()
## Burrows vs. Halpern plot
png()
p3 <- ggplot(by.prov.cull,aes(Burrows,Halpern,colour=Zone)) + geom_point(size=3,alpha=.7) + scale_colour_manual(values = c("blue","pink2","red")) + geom_point(data=by.prov.cull,aes(Burrows,Halpern),colour="black",pch=1,size=3.2) + xlab(expression("Velocity of Climate Change")) + ylab(expression("Human Impact")) + opts(panel.background = theme_rect(colour="black", size =.5, fill = "white")) + opts(panel.grid.minor = theme_blank()) + opts(panel.grid.major = theme_blank()) + opts(axis.line = theme_segment(colour = "black"),panel.border = theme_blank(),panel.background = theme_blank()) 
print(p3)
ggsave(p3,file="~/Dropbox/nescent_extinction_map/r/Halpern.vs.Burrows.png",width=4,height=2.6)
dev.off()

#export table for additional plotting
write.table(by.prov.cull,"~/Dropbox/nescent_extinction_map/r/Provincial_risk.csv",sep=",")