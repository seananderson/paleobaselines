provinces <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Province.mean.ext.min.occs.csv",header = TRUE,stringsAsFactors = FALSE)
provinces <- subset(provinces,provinces$N.gen >= 200)
p <- ggplot(provinces,aes(min.occurrences,mean.ext.standardized,group = as.factor(PROV_CODE),colour = as.factor(Latitude),size=N.gen))
p + geom_line(alpha =.4)