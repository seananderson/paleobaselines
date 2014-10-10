modern <- readRDS("../data/modern-predictions.rds")

# theme_set(theme_gray())

load("../data/by.prov.classes.rda")
er <- maptools::readShapePoly("../data/MEOW2/meow_ecos.shp")
er@data$id = rownames(er@data)
er_points = ggplot2::fortify(er, region = "id")
er_dat_class <- plyr::join(er_points, er@data, by = "id")
er_dat_class <- plyr::join(er_dat_class, by.prov.classes, by = "PROV_CODE")

p <- ggplot(by.prov.classes, aes(log_OBIS_records, mean.ext)) + geom_point() + stat_smooth(method = "loess")
ggsave("meanext-vs-obis-n-by-prov.pdf", width = 6, height = 5)

# go source in the code from merge-predictions-spatial...
# to make d.eco.filled
# ...
# starting again:

load("../data/composite.occ2.rda")
library(dplyr)
counts <- composite.occ2 %>% group_by(genus) %>%
  summarise(n = n())
modern <- left_join(modern, counts)

modern_long <- reshape2::melt(modern, id.vars = c("class", "group", "genus"),
  measure.vars = c("richness", "occupancy", "min.lat", "max.lat", "lat.range",
    "mean.lat", "great.circle", "gbm_pred_binned"))
modern_long <- inner_join(modern_long, modern[,c("genus", "n")])
modern_long <- filter(modern_long, !class %in% c("Mammalia", "Elasmobranchii"))

 p1 <- ggplot(filter(modern_long, variable %in% c("richness", "occupancy")), aes(log10(n), value, colour = class)) + facet_wrap(~variable) + geom_point(position = position_jitter(height = 0.1), alpha = 0.15)

p4 <- ggplot(filter(modern_long, variable %in% c("gbm_pred_binned")), aes(log10(n), value, colour = class)) + facet_wrap(~variable) + geom_point(position = position_jitter(height = 0.05), alpha = 0.2)

p2 <- ggplot(filter(modern_long, variable %in% c("min.lat", "max.lat", "lat.range", "mean.lat")), aes(log10(n), value, colour = class)) + facet_wrap(~variable) + geom_point(position = position_jitter(height = 3), alpha = 0.1)

p3 <- ggplot(filter(modern_long, variable %in% c("great.circle")), aes(log10(n), value, colour = class)) + facet_wrap(~variable) + geom_point(position = position_jitter(height = 600), alpha = 0.2)

pdf("obis-samping-vs-predictors.pdf", width = 10, height = 7)
gridExtra::grid.arrange(p4, p3, p1, p2)
dev.off()

p <- ggplot(modern_long, aes(value, log10(n), colour = class)) + facet_grid(class~variable, scales = "free_x") + geom_point(position = position_jitter(height = 0.1), alpha = 0.09)
ggsave("obis-sampling-vs-predictors-class-facets.pdf", width = 14, height = 7)

# `🐳` <- "whale!"

# single_obs <- filter(modern, n ==1, !class %in% c("Elasmobranchii", "Mammalia")) %>% select(genus)

# x1 <- filter(composite.occ2, genus == single_obs$genus[2])
# library(maps)
# map("world", fill=TRUE, col="grey80", border = "grey80")
# points(x1$longitude, x1$latitude)

# make maps with cutoffs of 5, 10, 10, 50 OBIS observations


s
