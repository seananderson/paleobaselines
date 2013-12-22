library(plyr)
data <- read.csv("/Users/Seth/Dropbox/nescent_extinction_map/data/N records per genus per province.csv",header = TRUE)
head(data)

extEST <- read.csv("/Users/Seth/Dropbox/nescent_extinction_map/data/Risk estimates by stage.csv",header = TRUE)
genus <- extEST$genus
####Choose which extinction risk estimate to map (e.g. pure taxon rates, mean taxon + range, Neogene mean taxon + range, Sepkoski-corrected Neogene mean taxon + range,etc.)
ext <- extEST$NeogeneMeanRisk

RateLookup <- data.frame(ext,genus)
newdata <- merge(RateLookup,data,by="genus")

meanGenus <- function(df) mean(df$ext)
Occs <- function(df) sum(df$records)
ddply(newdata,.(class,genus),each(meanGenus,Occs))
