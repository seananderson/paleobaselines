####import genus grid cells for both raw spalding realms and for central and western Indo-Pacific merged, create a column of labels, and merge

load("~/Dropbox/nescent_extinction_map/Final data/genus_cells.rda")
Spalding_raw <- genus_cells
provinces <- rep("Spalding_raw",length(Spalding_raw[,1]))
Spalding_raw <- data.frame(Spalding_raw,provinces)
load("/Users/sethfinnegan/Dropbox/nescent_extinction_map/Final data/genus_cells_alt.rda")
Spalding_merged <- genus_cells_alt
provinces <- rep("Spalding_merged",length(Spalding_merged[,1]))
Spalding_merged <- data.frame(Spalding_merged,provinces)
genus_grid_merged <- rbind(Spalding_raw,Spalding_merged)
save(genus_grid_merged,file="genus_grid_merged.rda")