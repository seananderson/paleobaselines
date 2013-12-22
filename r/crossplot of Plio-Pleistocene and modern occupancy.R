library(ggplot2)
CompOcc <- read.csv("/Users/Seth/Dropbox/nescent_extinction_map/data/Plio-Pleistocen vs modern occupancy.csv",header = TRUE)
PlioPleis <- CompOcc$Plio.Pleis_occ
Modern <- CompOcc$Modern_occ
class <- as.factor(CompOcc$Class)
data <- data.frame(PlioPleis,Modern,class)

p <- ggplot(data,aes(log(PlioPleis+1),log(Modern+1)))
p + geom_point(pch =1,size =3) + facet_wrap(~class,ncol=3) + theme(panel.background = element_rect(colour="darkgray", size =1, fill = "white"))  + theme(panel.grid.major = element_blank()) + theme(panel.grid.minor = element_blank()) + ylab(expression("Log modern occupancy")) + xlab(expression("Log Plio-Pleistocene occupancy"))