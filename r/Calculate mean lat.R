load("~/Dropbox/nescent_extinction_map/Final data/Occs.09.30.with.midcells.rda")
load("~/Dropbox/nescent_extinction_map/data/genus_cells_no_interpolation.rda")
load("~/Dropbox/nescent_extinction_map/data/genus_cells.rda")


OBIS <- genus_cells
OBIS_no_interp <- genus_cells_no_interpolation
PaleoDB <- PaleoDB


Interval <- ifelse(PaleoDB$slc >= 72,72,PaleoDB$slc)
genus <- PaleoDB$occurrence.genus_name
Lat <- PaleoDB$Y_mid

PaleoDB <- data.frame(Interval,genus,Lat)

Interval <- rep("Modern",length(OBIS[,1]))
genus <- OBIS$genus
Lat <- OBIS$Y_mid

OBIS <- data.frame(Interval,genus,Lat)

Interval <- rep("Modern_No_Interp",length(OBIS_no_interp[,1]))
genus <- OBIS_no_interp$genus
Lat <- OBIS_no_interp$Y_mid

OBIS_no_interp <- data.frame(Interval,genus,Lat)

All_data <- rbind(PaleoDB,OBIS,OBIS_no_interp)


Lat_Zone <- ifelse(All_data$Lat>=60,3,ifelse(All_data$Lat>=30,2,1))

newdata <- data.frame(All_data,Lat_Zone)

newdata <- na.omit(newdata)

newdata <- unique(newdata)

mean_lat <- function(df) mean(abs(df$Lat))
mean_lat_zone <- function(df) mean(df$Lat_Zone)

news <- ddply(newdata,.(Interval,genus),each(mean_lat,mean_lat_zone))

colnames(news) <- c("slc","genus","mean_lat","mean_lat_zone")

write.table(news,"Genus mean occurrence lats by slc.csv",sep =",")






### select whether to use within-realm interpolated or non-interpolated OBIS ranges
data <- read.csv("~/Dropbox/nescent_extinction_map/Final data/Modern and PaleoDB ranges FINAL 2.0.csv",header = TRUE,stringsAsFactors = FALSE)
data <- drop.levels(subset(data,data$Interval_Name != "Modern"))
stagetop <- function(df) mean(df$stage_top)
data <- ddply(data,.(Interval_Name),stagetop)
fn <- order(data$V1, decreasing=TRUE)
data = data[fn,]

intervals <- data[,1]
slc <- seq(61,72,by=1)

stage <- data.frame(intervals,slc)


colnames(stage) <- c("Interval","slc")


lookup_table <- merge(news,stage,by="slc")



write.table(news,"Genus mean occurrence lats by slc.csv",sep =",")


