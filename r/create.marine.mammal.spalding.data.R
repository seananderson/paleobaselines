# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jul 02, 2012
# Last modified: Jul 03, 2012
# Purpose:       Bring in occurrence data, taxonomic data, and
# extinction data, merge them, and manipulate them to match the other
# data.
# ====================================================================

require(maptools)
TaxonRates <- read.csv("~/Dropbox/NESCent-extinction/map/data/Cenozoic ordinal rates composite.csv", header = TRUE, stringsAsFactors = FALSE)
names(TaxonRates)[1] <- "MatchTaxon"
names(TaxonRates)[3] <- "ext"

mm.lookup <- read.csv("../data/Input_files/Mammalia.csv", stringsAsFactors = FALSE)
mm.lookup$latitude <- NULL
mm.lookup$longitude <- NULL
mm.lookup$Depth <- NULL
mm.lookup$Environment <- NULL
mm.lookup$Database <- NULL
mm.lookup$Download.date <- NULL
mm.lookup <- mm.lookup[!duplicated(mm.lookup), ]

mm.eco.shp <- readShapePoly("../data/marine_mammals_by_province/FullProvincesWithMarineMammals.shp")
mm.eco <- mm.eco.shp@data

#names(mm.lookup)[c(2, 3, 4, 5, 6)]
names(mm.eco)[13] <- "species"
mm.eco$species <- as.character(mm.eco$species)
mm.eco <- mm.eco[,c(4, 5, 6, 7, 8, 9,10,11,12,13)]

mm.eco.w.taxonomy <- merge(mm.eco, mm.lookup)

# NOTE remove some potentially land taxa?
mm.eco.w.taxonomy <- transform(mm.eco.w.taxonomy, MatchTaxon = ifelse(order=="Carnivora",order,suborder))

mm.eco.w.taxonomy.ext <- merge(mm.eco.w.taxonomy, TaxonRates[,c("MatchTaxon", "ext")])

names(mm.eco.w.taxonomy.ext)[which(names(mm.eco.w.taxonomy.ext) == "genus")] <- "taxon"

mm.eco.w.taxonomy.ext <- mm.eco.w.taxonomy.ext[,c("MatchTaxon", "taxon", "class", "ext", "ECO_CODE", "ECOREGION", "PROV_CODE", "PROVINCE", "RLM_CODE", "REALM", "ALT_CODE", "ECO_CODE_X", "Lat_Zone")]

save(mm.eco.w.taxonomy.ext, file = "../data/mm.eco.w.taxonomy.ext.rda")
