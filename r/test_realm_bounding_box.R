# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Sep 24, 2012
# Last modified: Sep 24, 2012
# Purpose:       test the realm_bouning_box.R output
# ====================================================================

require(plyr)
require(sp)
require(rgeos) # for overlays with 2 sets of polygons
require(ggplot2)
require(maptools)
gpclibPermit()
gc()
load("../data/interpolated_provs.rda")
load("../data/composite.occ2.rda")
composite.occ2 <- composite.occ2[!composite.occ2$group %in% c("Mammalia", "Elasmobranchii"), ] # these came from range maps; no interpolation necessary

# get a data frame with the unique taxonomic info:
#unique_taxa_info <- composite.occ2[,c("group", "match", "genus")][!duplicated(composite.occ2[,c("group", "match", "genus")]), c("group", "match", "genus")]
#row.names(unique_taxa_info) <- NULL

# now add the interpolated data back to the unique taxonomic info:
# d <- merge(interpolated_provs, unique_taxa_info, all.x = TRUE)
d <- interpolated_provs

# the ecoregion data:
er <- readShapePoly("~/Dropbox/nescent_extinction_map/data/MEOW2/meow_ecos.shp")
er@data$id = rownames(er@data)
er.points = fortify(er, region = "id")
er.df <- join(er.points, er@data, by = "id")

# now add the original data to provinces:
load("pts.over.cache.rda")
oc.df <- cbind(composite.occ2, pts.over)
# remove rows without an ecoregion, we aren't dealing with them:
oc.df <- oc.df[!is.na(oc.df$RLM_CODE), ]

# now try plotting some sampled genuses to make sure they look right:

all_genuses <- unique(interpolated_provs$genus)
genuses_sample <- all_genuses[sample(1:length(all_genuses), size = 9)]

pdf("../fig/province_interpolation_tests.pdf", width = 9, height = 7)
par(mfrow = c(3, 3), cex = 0.6, mar = c(0,0,4,0), oma = c(1,1,1,1))

# genuses to plot:

for(current_genus in genuses_sample) {
  inter_occurs.df <- data.frame(occurs = TRUE, PROV_CODE = subset(d, genus ==current_genus)$PROV_CODE)
  inter_does_not_occur.df <- data.frame(occurs = FALSE, PROV_CODE = unique(d$PROV_CODE)[!unique(d$PROV_CODE) %in% inter_occurs.df$PROV_CODE])
  inter_occur.df <- rbind(inter_occurs.df, inter_does_not_occur.df)
  inter_occur.df$occur_type <- "interpolated"
  
  raw_occurs.df <- data.frame(occurs = TRUE, PROV_CODE = subset(oc.df, genus ==current_genus)$PROV_CODE)
  raw_occurs.df <- raw_occurs.df[!duplicated(raw_occurs.df), ]
  raw_does_not_occur.df <- data.frame(occurs = FALSE, PROV_CODE = unique(d$PROV_CODE)[!unique(d$PROV_CODE) %in% raw_occurs.df$PROV_CODE])
  raw_occur.df <- rbind(raw_occurs.df, raw_does_not_occur.df)
  raw_occur.df$occur_type <- "raw"
  
  with(er.df, plot(long, lat, asp = 1,type = "n", xlim = c(-180, 180), ylim = c(-90, 90), xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab = ""));d_ply(er.df, "group", transform, polygon(long, lat, col = paste(rep(brewer.pal(8, "Set1"), 99)[RLM_CODE], "30", sep = ""), border = NA))
  box(col = "grey50")
  
  temp1 <- join(er.df, inter_occur.df)
  # interpolated
  d_ply(temp1, "group", transform, polygon(long, lat, col = ifelse(occurs, "NA", NA), border =ifelse(occurs, "grey10", NA)))
  
  temp2 <- join(er.df, raw_occur.df)
  # raw
  d_ply(temp2, "group", transform, polygon(long, lat, col = ifelse(occurs, "#00000050", NA), border =NA))
  
  # raw points
  with(subset(oc.df, genus == current_genus), points(longitude, latitude, pch = 4, col = "red", cex = 1))
  mtext(current_genus)
}

dev.off()
