
# combine the 3 output files from the cluster
# together these files contain the ecoprovince data across the filled in grid cells

load("../data/d.eco.filled1.rda")
load("../data/d.eco.filled2.rda")
load("../data/d.eco.filled3.rda")

gc()
d.eco.filled <- rbind(d.eco.filled1, d.eco.filled2, d.eco.filled3)
rm(d.eco.filled1, d.eco.filled3, d.eco.filled3)
gc()

d.eco.filled <- d.eco.filled[!duplicated(d.eco.filled[,c("genus", "group", "match", "PROV_CODE", "PROVINCE", "RLM_CODE", "REALM", "ALT_CODE", "ECO_CODE_X", "Lat_Zone")]), c("genus", "group", "match", "PROV_CODE", "PROVINCE", "RLM_CODE", "REALM", "ALT_CODE", "ECO_CODE_X", "Lat_Zone")]

save(d.eco.filled, file = "../data/d.eco.filled.rda")

sink("d.eco.filled.head.txt")
head(d.eco.filled)
nrow(d.eco.filled)
sink()
