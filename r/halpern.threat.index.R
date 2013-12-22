# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Jan 26, 2012
# Last modified: Jan 29, 2012
# Purpose:       Try halpern threat index by match taxon
# ====================================================================

source("grid.hal.data.R")
source("grid.ext.data.R")
source("smooth.pal.R")
halpern.data <- read.csv("~/Dropbox/NESCent-extinction/map/data/Halpern_impact_grid.csv")
#load("~/Dropbox/NESCent-extinction/map/data/map.data.jan26.2012.rda") # data6
load("~/Desktop/data.finite.rda")

library(plyr)
hal.out <- grid.hal.data(halpern.data)
library(ggplot2)
#plot(density(out$Z_hlp))
#ggplot(out, aes(x = Z_hlp)) + geom_histogram()
data6 <- data.finite

out <- ddply(data6, "MatchTaxon", function(x) {
  grid.ext.data(x, combine.type = "mean", return.events = TRUE)
})
#subset(out, Z > 0)

out.enough <- ddply(out, "MatchTaxon", function(x) if(nrow(x) >= 50) x)
source("eq.area.grid.R")
grid.round <- eq.area.grid()
grid.round <- transform(grid.round, round.X = round(X, 0), round.Y = round(Y, 0))
out.enough <- transform(out.enough, round.X = round(X.int, 0), round.Y = round(Y.int, 0))
grid.round$X <- NULL
grid.round$Y <- NULL
out.enough.g <- merge(out.enough, grid.round)
out.enough.g <- merge(hal.out, out.enough.g)

#out.enough <- merge(out.enough, hal.out)

out.enough.g.no.zero <- subset(out.enough.g, Z_hlp != 0)

### histograms:
#q <- ddply(out.enough.g.no.zero, "MatchTaxon", summarize, m_hlp = median(Z_hlp), l_hlp = quantile(Z_hlp, 0.25), u_hlp = quantile(Z_hlp, 0.75))
#q <- q[order(q$median), ]
#q$n <- 1:nrow(q)
#q <- transform(q, Taxon = reorder(MatchTaxon,n ))
#q$Taxon <- factor(q$Taxon)
#ggsave("halpern-threat-median.pdf", width = 4.5, height = 7.4)

#q <- q[order(-q$median), ]
#q$n <- 1:nrow(q)
#q <- transform(q, Taxon = reorder(MatchTaxon,n ))
#q$Taxon <- factor(q$Taxon)

#out.enough.g.no.zero.q <- merge(q, out.enough.g.no.zero)

#### dot and line plots:
##p <- ggplot(out.enough.g.no.zero, aes(x = Z_hlp)) + geom_histogram() + facet_wrap("MatchTaxon", scales = "free_y")
#p <- ggplot(out.enough.g.no.zero.q, aes(x = Z_hlp)) + geom_histogram() + facet_wrap("Taxon", scales = "free_y")
#ggsave("~/Dropbox/NESCent-extinction/map/halpern-threat/gg-hist-hal-threat-match-taxon3.pdf", p, width = 12, height = 9)

out.enough.g.no.zero <- transform(out.enough.g.no.zero, grid_cell = paste(PID, SID, sep = "_"))
## add in SD of ext by order:
sep.ord <- read.csv("~/Dropbox/NESCent-extinction/map/data/Sepkoski ordinal rates.csv", header = TRUE, stringsAsFactors = FALSE)
names(sep.ord) <- paste(tolower(names(sep.ord)), "ext", sep = "_")
names(sep.ord)[1] <- "MatchTaxon"
out.enough.g.no.zero <- merge(out.enough.g.no.zero, sep.ord[,c("MatchTaxon", "stdev_ext", "max_ext", "mean_ext", "min_ext")], all.x = TRUE)


d_tax <- ddply(out.enough.g.no.zero, "MatchTaxon", summarize, m_hlp = mean(Z_hlp), l_hlp = quantile(Z_hlp, 0.25), u_hlp = quantile(Z_hlp, 0.75), m_ext = mean(Z), l_ext = mean(Z) - mean(stdev_ext), u_ext = mean(Z) + mean(stdev_ext))
d_tax$MatchTaxon <- NULL

d_cel <- ddply(out.enough.g.no.zero, "grid_cell", summarize, m_hlp = mean(Z_hlp), l_hlp = quantile(Z_hlp, 0.25), u_hlp = quantile(Z_hlp, 0.75), m_ext = mean(Z), l_ext = mean(Z) - mean(stdev_ext), u_ext = mean(Z) + mean(stdev_ext))
d_cel$grid_cell <- NULL

d_cel$type <- "cell"
d_tax$type <- "taxon"
d <- rbind(d_cel, d_tax)




#hlp_temp <- d_cel[,c("m_hlp", "l_hlp", "u_hlp")]
#ext_temp <- d_cel[,c("m_ext", "l_ext", "u_ext")]

d <- subset(d, m_hlp < 15)
p <- ggplot(d, aes(m_ext, m_hlp)) + facet_wrap(~type, scales = "free") + geom_point(colour = "#00000030") + geom_segment(aes(x = m_ext, xend = m_ext, y = l_hlp, yend = u_hlp), colour = "#00000010") + geom_segment(aes(x = l_ext, xend = u_ext, y = m_hlp, yend = m_hlp), colour = "#00000010") #+ ylim(1.5, 14) + xlim(0, 0.9)
ggsave("hal-taxa-cell-cross-plots-gg.pdf", width = 6, height = 3.3)


### code to look at the distribution by matchtaxon:
#out.enough$Z <- 1
#pdf("~/Desktop/maps.pdf", width = 6, height = 4)
#d_ply(out.enough, "MatchTaxon", function(x) {
 #plot.ext.map(x, col.range = c(0.5, 0.7))
              #mtext(unique(x$MatchTaxon))
#})

#dev.off()

#pdf("~/Desktop/maps-multi.pdf", width = 20, height = 12)
#par(mfrow = c(6,6))
#par(cex = 0.5)
#par(mar = c(0,0,0,0))
#d_ply(out.enough, "MatchTaxon", function(x) {
 #plot.ext.map(x, col.range = c(0, 2))
              #mtext(unique(x$MatchTaxon))
#})
#dev.off()

