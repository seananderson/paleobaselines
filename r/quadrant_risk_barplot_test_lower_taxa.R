# ====================================================================
# Created by:    Sean Anderson, sean@seananderson.ca
# Created:       Month 00, 2012
# Last modified: Sep 16, 2012
# Purpose:       Try a barplot type plot of the four quadrats of
# extinction risk. (Figure 4?)
# ====================================================================

d<- read.csv("~/Downloads/Figure 4 kernel smoothing data.csv")

data<-read.csv("~/Dropbox/nescent_extinction_map/data/CombinedData2.csv", header = TRUE, na.strings = "")
head(data)
data <- subset(data,occupancy>1)

junk <- data[!duplicated(data[ ,c("class", "New_Group")]), c("class", "New_Group")]

# Note that not all classes have a "New_Group"!!!!!!!
d <- merge(d, junk, all.x = FALSE)
quad_halpern <- c(-999, quantile(d$mean.Hal, c(.5)), 999)
quad_ext <- c(-999, quantile(d$mean.ext, c(.51)), 999)

library(RColorBrewer)
pal <- brewer.pal(4, "Dark2")

d <- transform(d, halpern_half = findInterval(mean.Hal, quad_halpern))
d <- transform(d, ext_half = findInterval(mean.ext, quad_ext))

d$quad <- NA
d[d$halpern_half == 2 & d$ext_half == 2, "quad"] <- 4
d[d$halpern_half == 1 & d$ext_half == 1, "quad"] <- 1
d[d$halpern_half == 1 & d$ext_half == 2, "quad"] <- 2
d[d$halpern_half == 2 & d$ext_half == 1, "quad"] <- 3

library(reshape2)

dw <- as.data.frame(dcast(d, New_Group ~ quad, value.var = "quad", fun.aggregate = length))
dw.bar <- t(apply(dw[,-1], 1, function(x) x / sum(x)))

# original plot:
#par(mar = c(3,8,1,1));barplot(t(dw.bar), horiz = TRUE, names.arg = dw[,1], las = 1, col = pal)

dw.seth <- matrix(ncol = 5, nrow = nrow(dw))
# the middle:
dw.seth[, 3] <- 0

dw.seth[, 1] <- -1 * (dw.bar[,1] + dw.bar[,3])
dw.seth[, 2] <- -1 * (dw.bar[,3])

dw.seth[, 4] <- dw.bar[,2]
dw.seth[, 5] <- dw.bar[,2] + dw.bar[,4]

width <- 0.8

#pal <- c("grey20", "grey80", "grey80", "grey20")
pal <- c("#2E6FFF", "grey50", "grey50", "#FF0710")

bar.order <- sort.int(-1 * dw.seth[,1], index.return = TRUE)$ix
dw.seth <- dw.seth[bar.order, ]
dw <- dw[bar.order, ]
dw.bar <- dw.bar[bar.order, ]

pdf("../fig/seth_bars_smaller_group.pdf", width = 4.5, height = 5.0)
par(mar = c(2.4, 13.2, 2.8, .5), oma = c(0,0,0,0), cex = 0.8)
plot(1,1, xlim = c(-0.8, 0.8), ylim = c(0.5,nrow(dw.seth)+0.5), type = "n", yaxs = "i", axes = FALSE, xlab = "", ylab = "")
for(i in 1:nrow(dw.seth)) {
  for(j in 1:4) {
    rect(dw.seth[i,j], i-width/2, dw.seth[i,j+1], i + width/2, border = FALSE, col = pal[j])
  }
}
#abline(v = 0, lwd = 1.5, col = "grey10", lty = 2)
axis(2, at = 1:nrow(dw.seth), labels = gsub("_", " ", dw[,1]), las = 1, tck = 0, lty = 0, col.axis = "grey20", line = -0.7)
axis(1, col = "grey20", col.axis = "grey20", at = c(-0.8,  -0.4, 0, 0.4, 0.8), labels = c(80,40, 0, 40,80), tck = -0.03, line = 0.4, padj = -0.5)
mtext("Below median\npaleo risk", side = 3, cex = 0.85, at = -0.5, adj = 0.5, line = 0.5, col = "grey20")

mtext("Above median\npaleo risk", side = 3, cex = 0.85, at = 0.5, adj = .5,line = 0.5, col = "grey20")

par(xpd = NA)
segments(0, 0.3, 0, nrow(dw.seth)+2, lwd = 1, col = "grey10", lty = 1)

dev.off()

