# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jul 02, 2012
# Last modified: Jul 03, 2012
# Purpose:       try ggplot plotting and fortify df
# ====================================================================

load("../data/data.finite.with.ecoregions.20120702.rda")
land <- readShapePoly("../data/110m-land/110m_land.shp")
land.fort <- fortify(land)

require(maptools)
require(ggplot2)
require(plyr)

er <- readShapePoly("../data/MEOW2/meow_ecos.shp")
er@data$id = rownames(er@data)
er.points = fortify(er, region = "id")
er.df <- join(er.points, er@data, by = "id")

#mean.ext.by.prov <- ddply(d.eco, "PROV_CODE", summarize, mean.prov.ext = mean(ext))
mean.ext.by.prov <- ddply(subset(d.eco, class != "Mammalia"), "PROV_CODE", summarize, mean.prov.ext = mean(ext))

er.df$mean.prov.ext <- NULL
er.df <- join(er.df, mean.ext.by.prov, by = "PROV_CODE")
ggplot(er.df) + aes(long, lat, group=group,fill=mean.prov.ext) + geom_polygon()

#####
d.eco.prov.jack <- ddply(subset(d.eco, !class %in% "Mammalia"), "PROV_CODE", function(x) {
  data.frame(no.Bivalvia = mean(subset(x, !class %in% "Bivalvia")$ext),
             no.Echinoidea = mean(subset(x, !class %in% "Echinoidea")$ext), 
             no.Gastopoda = mean(subset(x, !class %in% "Gastropoda")$ext),
             no.Anthozoa = mean(subset(x, !class %in% "Anthozoa")$ext),
             no.Polythalamea = mean(subset(x, !class %in% "Polythalamea")$ext),
             no.Elasmobranchii = mean(subset(x, !class %in% "Elasmobranchii")$ext),
             #no.Mammalia = mean(subset(x, !class %in% "Mammalia")$ext),
             no.Malacostraca = mean(subset(x, !class %in% "Malacostraca")$ext),
             no.Rhynchonellata = mean(subset(x, !class %in% "Rhynchonellata")$ext),
             no.Reptilia = mean(subset(x, !class %in% "Reptilia")$ext))
})
d.eco.prov.indiv <- ddply(subset(d.eco, !class %in% "Mammalia"), "PROV_CODE", function(x) {
  data.frame(only.Bivalvia = mean(subset(x, class %in% "Bivalvia")$ext),
             only.Echinoidea = mean(subset(x, class %in% "Echinoidea")$ext), 
             only.Gastopoda = mean(subset(x, class %in% "Gastropoda")$ext),
             only.Anthozoa = mean(subset(x, class %in% "Anthozoa")$ext),
             only.Polythalamea = mean(subset(x, class %in% "Polythalamea")$ext),
             only.Elasmobranchii = mean(subset(x, class %in% "Elasmobranchii")$ext),
             #only.Mammalia = mean(subset(x, class %in% "Mammalia")$ext),
             only.Malacostraca = mean(subset(x, class %in% "Malacostraca")$ext),
             only.Rhynchonellata = mean(subset(x, class %in% "Rhynchonellata")$ext),
             only.Reptilia = mean(subset(x, class %in% "Reptilia")$ext))
})

d.eco.prov.jack.long <- melt(d.eco.prov.jack, id = "PROV_CODE")
d.eco.prov.jack.long <- ddply(d.eco.prov.jack.long, "variable", transform, scaled.value = scale(value))
er.df.jac <- join(er.df, d.eco.prov.jack.long, by = "PROV_CODE")

d.eco.prov.indiv.long <- melt(d.eco.prov.indiv, id = "PROV_CODE")
d.eco.prov.indiv.long <- ddply(d.eco.prov.indiv.long, "variable", transform, scaled.value = scale(value))
er.df.ind <- join(er.df, d.eco.prov.indiv.long, by = "PROV_CODE")

# not scaled versions:
p <- ggplot() + geom_polygon(data = er.df.jac, aes(long, lat, group=group,fill=value)) + facet_wrap(~variable) + coord_equal() + theme_bw() + geom_polygon(data = land.fort, aes(long, lat, group = group), fill = "grey95") + scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) + scale_fill_continuous(name = "Ext rate") + opts(strip.background = theme_rect(colour =NA)) + xlab("Longitude") + ylab("Latitude")
ggsave("../fig/ecoregions/jacknife-by-province-with-no-mammals.pdf", width = 10, height = 5.8)

p <- ggplot() + geom_polygon(data = er.df.ind, aes(long, lat, group=group,fill=value)) + facet_wrap(~variable) + coord_equal() + theme_bw() + geom_polygon(data = land.fort, aes(long, lat, group = group), fill = "grey95") + scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) + scale_fill_continuous(name = "Ext rate") + opts(strip.background = theme_rect(colour =NA)) + xlab("Longitude") + ylab("Latitude")
ggsave("../fig/ecoregions/individual-taxa-by-province-with-no-mammals.pdf", width = 10, height = 5.8)

# scaled versions:
p <- ggplot() + geom_polygon(data = er.df.jac, aes(long, lat, group=group,fill=scaled.value)) + facet_wrap(~variable) + coord_equal() + theme_bw() + geom_polygon(data = land.fort, aes(long, lat, group = group), fill = "grey95") + scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) + scale_fill_continuous(name = "Ext rate") + opts(strip.background = theme_rect(colour =NA)) + xlab("Longitude") + ylab("Latitude")
ggsave("../fig/ecoregions/jacknife-scaled-by-province-with-no-mammals.pdf", width = 10, height = 5.8)

p <- ggplot() + geom_polygon(data = er.df.ind, aes(long, lat, group=group,fill=scaled.value)) + facet_wrap(~variable) + coord_equal() + theme_bw() + geom_polygon(data = land.fort, aes(long, lat, group = group), fill = "grey95") + scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) + scale_fill_continuous(name = "Ext rate") + opts(strip.background = theme_rect(colour =NA)) + xlab("Longitude") + ylab("Latitude")
ggsave("../fig/ecoregions/individual-taxa-scaled-by-province-with-no-mammals.pdf", width = 10, height = 5.8)




ggplot() + geom_polygon(data = subset(er.df.jac, variable == "no.Gastopoda"), aes(long, lat, group=group,fill=value)) + coord_equal() + geom_polygon(data = map_data("world"), aes(long, lat, group = group), fill = "white") + theme_bw()

ggplot() + geom_polygon(data = subset(er.df.jac, variable == "no.Gastopoda"), aes(long, lat, group=group,fill=value))  + geom_polygon(data = land.fort, aes(long, lat, group = group), fill = "grey93") + theme_bw() + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) + coord_map(project = "mollweide") + scale_x_continuous(limits = c(-180, 180), breaks = c(0)) + scale_fill_continuous(name = "Ext rate")
