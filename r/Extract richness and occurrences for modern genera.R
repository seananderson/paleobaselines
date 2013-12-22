# Created by:    Sean C. Anderson
# Created:       Feb 06, 2012
# Last modified: Feb 06, 2013
# Purpose:       Overlay the equal area grid on the raw occurrence
#                data as a sensitivity test for our interpolation
#                method.


occsdata <- load("../data/composite.occ2.rda")
composite.occ2 <- composite.occ2
head(composite.occ2)


richness <- function(df) length(unique(df$species))
occurrences <- function(df) length(df$species)

outs <- ddply(composite.occ2,.(match,genus),each(richness,occurrences),.progress="text")
write.table(outs,"~/Dropbox/nescent_extinction_map/Final data/OBIS.species.richness.and.occurrences.csv",sep=",")