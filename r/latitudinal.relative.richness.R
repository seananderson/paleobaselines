# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jun 08, 2012
# Last modified: Jun 08, 2012
# Purpose:       try Seth's "Latitudinal alpha richness gradients of
# groups for which paleo rate estimates are mapped.pdf" figure but
# with relative richness
# ====================================================================


load("../data/global_45x14_ext_rich_vel_hlp_20120607.rda")

d <- d.45x14
d <- d[!duplicated(d[,c("PID", "SID", "MatchTaxon")]),]
#d <- subset(d, depth_max >= -200 & class != "Mammalia")
d <- subset(d, depth_max >= -200)
d <- ddply(d, c("PID", "SID"), transform, cell_richness = sum(rich))
d <- subset(d, cell_richness > 100)

d <- transform(d, relative_cell_richness = rich/cell_richness)

d$MatchTaxon <- as.factor(d$MatchTaxon)
d <- transform(d, MatchTaxon_OrderExt = reorder(MatchTaxon, -ext))

p <- ggplot(d, aes(lat, relative_cell_richness)) + geom_point(aes(colour = ext), alpha = 0.50) + facet_wrap(~MatchTaxon_OrderExt) +  xlab("Latitude") + ylab("Proportion of cell richness") + scale_colour_continuous(name = "Ext. rate", trans = "log", breaks = seq(0, 1, 0.1)) + geom_vline(xintercept = 0, alpha = 0.3)
#+ stat_smooth(span = 0.6, method = "loess")
ggsave("../fig/Latitudinal relative cell richness gradients ordered by ext rate (with mammals), min. depth > 200 and cell richness > 100.pdf", width = 13, height = 10)

