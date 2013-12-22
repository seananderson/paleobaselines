
data_Int<- load("~/Dropbox/nescent_extinction_map/Final data/interpolated_provs_all_taxa.rda")
data_Raw <- load("~/Dropbox/nescent_extinction_map/Final data/raw_provs_all_taxa.rda")

data_Int <- interpolated_provs_all_taxa
Ranges <- rep("Interpolated",length(data_Int[,1]))
Interpolated_ranges <- data.frame(data_Int,Ranges)


data_Raw <- raw_provs_all_taxa
Ranges <- rep("Raw",length(data_Raw[,1]))
Raw_ranges <- data.frame(data_Raw,Ranges) 

Modern_Province_Occupancy <- rbind(Interpolated_ranges,Raw_ranges)

save(Modern_Province_Occupancy, file = "~/Dropbox/nescent_extinction_map/Final data/Modern_Province_Occupancy.rda")