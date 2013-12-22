# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Sep 29, 2012
# Last modified: Oct 10, 2012
# Purpose:       Merge the shark and mammal data (which Derek joined) 
#                with the interpolated province data from the rest.
#                20121009 - SA - updated to bring in revised data from
#                Derek that has all sharks
#                20121010 - SA - try with the buffer version of the
#                sharks
# ====================================================================

setwd("~/Dropbox/nescent_extinction_map/r")

#sharks <- read.csv("../data/SharkProvinceJoinNoBuffer_20121009.csv", stringsAsFactors = FALSE)[,c("PROV_CODE", "species")]
sharks <- read.csv("../data/SharkProvinceJoin2pt5DegreeBuffer_20121009.csv", stringsAsFactors = FALSE)[,c("PROV_CODE", "species")]
mar_mamm <- read.csv("../data/MarineMammalProvinceJoin2.csv", stringsAsFactors = FALSE)[,c("PROV_CODE", "BINOMIAL")]
names(mar_mamm) <- c("PROV_CODE", "species")

sharks$genus <- gsub("([a-zA-Z]+)_[a-zA-Z]+", "\\1", sharks$species)
mar_mamm$genus <- gsub("([a-zA-Z]+) [a-zA-Z]+", "\\1", mar_mamm$species)

sharks$species <- NULL
mar_mamm$species <- NULL

sharks <- sharks[!duplicated(sharks), ]
mar_mamm <- mar_mamm[!duplicated(mar_mamm), ]

#sharks$group <- "Mammalia"
#mar_mamm$group <- "Elasmobranchii"

# now merge it all together:
sharks_and_mamm <- rbind(sharks, mar_mamm)

# remove those rows without a genus
sharks_and_mamm <- subset(sharks_and_mamm, genus != "")

load("../data/interpolated_provs.rda")
load("../data/interpolated_provs_alt.rda")

# make sure RLM_CODE is removed from interpolated provs data frame
# in an older version it was there
interpolated_provs$RLM_CODE <- NULL
interpolated_provs_alt$RLM_CODE <- NULL

# add back the match column to sharks and mammals
#load("../data/composite.occ2.rda")
# first create a lookup dataframe with match and genus
#unique_taxa_info <- composite.occ2[,c("match", "genus")][!duplicated(composite.occ2[,c("match", "genus")]), c("match", "genus")]
#row.names(unique_taxa_info) <- NULL
# now merge:
#sharks_and_mamm <- merge(sharks_and_mamm, unique_taxa_info, all.x = TRUE)

#rm(composite.occ2)

# merge the sharks and mammals with the interpolated data
interpolated_provs_all_taxa <- rbind(interpolated_provs, sharks_and_mamm)
interpolated_provs_alt_all_taxa <- rbind(interpolated_provs_alt, sharks_and_mamm)

save(interpolated_provs_all_taxa, file = "../data/interpolated_provs_all_taxa.rda")
save(interpolated_provs_alt_all_taxa, file = "../data/interpolated_provs_alt_all_taxa.rda")

# DONE!
