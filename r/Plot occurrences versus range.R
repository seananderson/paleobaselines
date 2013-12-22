library(randomForest)
library(matrixStats)
require(verification)
library(gdata)
library(party)
library(plyr)
library(Hmisc)

data <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Modern and PaleoDB ranges FINAL.csv",header = TRUE,stringsAsFactors = FALSE)
data <- drop.levels(subset(data,data$OBIS_occurrences >= 1 & data$use==1 & data$Interval_Name == "Spalding_raw" & data$occurrences != 99999 & data$occurrences < 100000000))
head(data)
occurrence_bin <- ceiling(data$occurrences/5)*5
histogram(log10(data$occurrences))


data <- cbind(data,occurrence_bin)

p <- ggplot(data,aes(occurrence_bin,gcd, group=occurrence_bin))
p + geom_point(size = 2, alpha = .5) + geom_boxplot()



 + scale_x_log10()