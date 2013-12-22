occs <- load("/Users/sethfinnegan/Dropbox/nescent_extinction_map/data/composite.occ2.rda")
head(occs)
occsdata <- composite.occ2
head(occsdata)
occurrences <- function(df) length(df$genus)
occ.count <- ddply(occsdata,.(genus),occurrences)
head(occ.count)
colnames(occ.count) <- c("genus","total_occurrences")
write.table(occ.count,"OBIS_genus_occurrences.csv",sep =",")