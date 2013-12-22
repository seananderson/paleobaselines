library(randomForest)
library(matrixStats)
require(verification)
library(gdata)
library(party)
library(plyr)
library(Hmisc)

data <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Modern and PaleoDB ranges FINAL.csv",header = TRUE,stringsAsFactors = FALSE)
data <- drop.levels(subset(data,data$OBIS_occurrences >= 30 & data$use==1 & data$gcd != 0 & data$stage_bottom !=0 & data$occurrences > 1 ))
head(data)

p <- ggplot(data,aes((gcd_corrected+10),group=class))
p + geom_density(fill="black")+facet_wrap(~class)