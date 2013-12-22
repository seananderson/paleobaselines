#' This script takes the data from "Make Figure 1 partial dependence draft 2.0.R"
#' and creates a more customized plot using baseplot
#' Started by Sean on 2013-06-07
#' See "Run all analysis and figure plotting scripts.R" to re-create the
#' initial data.

library(plyr)
source("Make figure 1 partial dependence plot 2.0.R")
# this creates the dataframes: others and groups
g <- groups
o <- others

#pal <- rev(RColorBrewer::brewer.pal(4, "Spectral"))
#pal <- rev(RColorBrewer::brewer.pal(4, "PiYG"))
#pal <- rev(RColorBrewer::brewer.pal(4, "PuOr"))
#pal <- RColorBrewer::brewer.pal(4, "Set1")

#gg_color_hue <- function(n, start = 10, end = 330, l = 65) {
  #hues = seq(start, end, length=n+1)
  #hcl(h=hues, l=l, c=100)[1:n]
#}

# Set colours, point types, and line widths
#pal <- rev(gg_color_hue(4))
#pal <- rev(rainbow(4, s = 0.6, v = .9, end = .65))
#pal <- gplots::rich.colors(6, palette = "blues")[1:4]
#pal <- gplots::rich.colors(6, palette = "temperature")[2:5]
#pal <- c("blue", "green", "orange", "red")
#pal <- paste0(colorspace::rainbow_hcl(4, c = 90, l = 65), "99")
pal <- rev(colorspace::rainbow_hcl(4, c = 90, l = 65))
#pal <- RColorBrewer::brewer.pal(4, "Dark2")
pal <- c(pal, "black")
#pch <- c(4, 21, 2, 15, 19)
#pch <- c(NA, NA, NA, NA, NA)
pch <- c(rep(19, 4), 19)
#cex <- c(0.75, 0.6, 0.6, 0.75, 1) * 1.2
cex <- c(rep(0.55, 4), .8) * 1.2
#pch <- c(NA, NA, NA, NA, 19)
lwd <- c(rep(1.5, 4), 2.6)
col.axis <- "grey55"
cex.axis <- 0.85
cols.plot <- 3:7
col.main.labels <- "grey15"
col.axis.labels <- "grey45"
g.ylim.l <- 0
g.ylim.u <- 1
g.y.axis.at <- c(0, 0.5, 1)
rect.cols <- rep(c("grey93", NA), 99)

# Reshape data:
junk <- do.call("rbind", strsplit(as.character(g$value), "_"))
g$.class <- junk[, 1]
g$.order <- junk[, 2]
g$value <- NULL
g[g$.class %in% c("Mammalia", "Elasmobranchii", "Testudines"), ".class"] <- "Chordata"
g[g$.order %in% "az", ".order"] <- "Azoozanthelate"
g[g$.order %in% "other", ".order"] <- "Other"
g[g$.class %in% "Decapoda", ".class"] <- "Dec."

class.means <- plyr::ddply(subset(g, stage == "Neogene mean"), ".class", summarize, class.mean.resp = mean(response))
order.means <- plyr::ddply(subset(g, stage == "Neogene mean"), ".order", summarize, order.mean.resp = mean(response))

g <- reshape2::dcast(g,  .class + .order ~ stage, value.var = "response")
g <- plyr::join(g, class.means)
g <- plyr::join(g, order.means)
g <- g[order(-g$class.mean.resp, -g$order.mean.resp), ]

# for convenience:
n.class <- length(unique(g$.class))
n.tot <- nrow(g)

# assign class numbers for convenience:
c.num.df <- data.frame(c.num = 1:n.class, .class = unique(g$.class), stringsAsFactors = FALSE)
g <- plyr::join(g, c.num.df)

# figure out x positions:
g$x.pos <- 1:nrow(g)
diffs <- diff(g$c.num)
g$diffs <- c(1, diffs)
x.gap <- .4
x.gap.lab <- -.3
diffs <- diffs*x.gap
g$x.pos <- g$x.pos + cumsum(c(0, diffs))


o.names <- data.frame(predictor = c("occupancy", "occurrences", "richness", "great.circle", "max.lat", "min.lat", "mean.lat", "lat.range"), predictor.clean = c("Occupancy", "Occurrences", "Richness", "Great circle distance", "Maximum latitude", "Minimum latitude", "Mean latitude", "Latitude range"), axis.1 = c(rep("c(0, 0.4, 0.8)", 3), "c(5000, 15000)", rep("c(20, 40, 60)", 3), "c(20, 60, 100)"), units = c(rep("Scaled value", 3), "Km", rep("Absolute degrees", 3), "Degrees"), stringsAsFactors = FALSE)

o <- join(o, o.names)

l <- rbind(c(1,2,3,4),
           c(5,6,7,8),
           c(9,9,9,9))

pdf("fig1-iter2.pdf", width = 5.9, height = 5.0)
layout(l)

## the upper half:
par(oma = c(5, 2.7, 1.2, .4), cex = 0.8, tck = -0.03, mgp = c(2, 0.35, 0), mar = c(2.4,.2,0,0))
ii <<- 0
lwd.u <- c(rep(2.0, 4), 3.9)
plyr::d_ply(o, "predictor", function(x) {
  ii <<- ii + 1
x <- x[order(x$value), ]
plot(1, 1, xlim = range(x$value), ylim = c(g.ylim.l, g.ylim.u), ann = FALSE, axes = FALSE, xaxs = "i", yaxs = "i", type = "n")
for(i in 1:length(unique(x$stage))) {
  dat <- subset(x, stage == unique(x$stage)[i])
  with(dat, lines(value, response, col = pal[i], lwd = lwd.u[i]))
}
box(col = col.axis)
axis(1, las = 1, col = col.axis, col.axis = col.axis, cex.axis = cex.axis, padj = -0.5, at = eval(parse(text = unique(x$axis.1))))
if(ii %in% c(1, 5)) axis(2, at = g.y.axis.at, las = 1, col = col.axis, col.axis = col.axis, cex.axis = cex.axis)

if(ii == 3) {
  par(xpd = NA)
legend(-2.11, par("usr")[4] * 1.23, legend = names(g)[cols.plot], bty = "n", cex = 0.8, text.col = "grey40", fill = pal, border = pal, horiz = TRUE)
  par(xpd = FALSE)
}

u <- par("usr")
par(xpd = NA)
text(x = u[1] + (u[2]-u[1])*0.03, y = u[4] - (u[4]-u[3])*.135, substitute(paste(bold(let), " ", lab, phantom("g")), list(let = LETTERS[ii], lab = unique(dat$predictor.clean))), cex = 0.8, adj = c(0, 0), col = col.main.labels)
mtext(unique(x$units), side = 1, cex = 0.70, line = .8, col = col.axis.labels)
par(xpd = FALSE)
})


## lower half:
# make blank plot:
plot(1, 1, xlim = c(0.3, max(g$x.pos)+0.5), ylim = c(0, 1), yaxs = "i", axes = FALSE, ann = FALSE, type = "n", xaxs = "i")

# shading:
rects <- subset(g, diffs == 1)
for(i in 1:(nrow(rects)-1)) {
  rect(rects[i, "x.pos"]-x.gap + x.gap.lab, 0, rects[i+1, "x.pos"]-x.gap + x.gap.lab, 1, border = NA, col = rect.cols[i])
}

# vertical lines:
# class level:
#abline(v = g$x.pos[g$diffs == 1] - x.gap+ x.gap.lab, col = "lightgrey")
# all:
abline(v = g$x.pos, col = "grey88", lwd = .5)

# the points and lines:
# and add data:
add.seg <- TRUE
cols.plot <- 3:7
for(i in 1:5) {
  ci <- cols.plot[i] # [c]olumn [i]
  points(g$x.pos, g[,ci], col = pal[i], pch = pch[i], cex = cex[i])
  if(add.seg) { # for speed of development
  plyr::d_ply(g, ".class", function(x) {
    for(j in 1:(nrow(x)-1)){
      if(nrow(x) > 1)
        segments(x[j,"x.pos"], x[j, ci], x[j,"x.pos"]+1, x[j+1, ci], col = pal[i], lwd = lwd[i])
      #else
        #points(x[1,"x.pos"], x[1, ci], col = pal[i], cex[i])
    }
})
  }
}

# axes and labels:
box(col= col.axis)
axis(2, at = c(0, 0.5, 1), las = 1, col = col.axis, col.axis = col.axis, cex.axis = cex.axis)
par(xpd = NA)

labs <- ddply(g, ".class", summarize, x.mid = min(x.pos) +
  diff(range(x.pos))/2, l = min(x.pos), u = max(x.pos))

with(labs, text(x = x.mid, y = -1.18, label = .class, srt = 0, col = col.main.labels, cex = 0.9, adj = c(0.5, 0)))

u.s <- with(labs, u+x.gap-x.gap.lab-0.12)
l.s <- with(labs, l-x.gap+x.gap.lab+0.12)
h.s <- with(labs, rep(-0.95, length(l.s)))

with(labs, segments(l.s, h.s, l.s, h.s-0.1, col = col.axis))
with(labs, segments(u.s, h.s, u.s, h.s-0.1, col = col.axis))
with(labs, segments(l.s, h.s-0.1, u.s, h.s-0.1, col = col.axis))

with(g, text(x = x.pos, y = -0.05, label = .order, srt = 90, pos = 2, col = col.axis.labels, offset = 0, cex = 0.85))
par(xpd = FALSE)

u <- par("usr")
par(xpd = NA)

#text(x = u[2] - 5.5, y = u[4] - (u[4]-u[3])*0.09, substitute(paste(bold(let), " ", lab), list(let = "I", lab = "Taxonomy")), pos = 4, offset = 0.5, cex = 0.8)

#text(x = u[1] + (u[2]-u[1])*0.01, y = u[4] - (u[4]-u[3])*.135, substitute(paste(bold(let), " ", lab, phantom("g")), list(let = LETTERS[ii+1], lab = "Taxonomy")), cex = 0.8, adj = c(0, 0), col = col.main.labels)
text(x = 24.12, y = u[4] - (u[4]-u[3])*.135, substitute(paste(bold(let), " ", lab, phantom("g")), list(let = LETTERS[ii+1], lab = "Taxonomy")), cex = 0.8, adj = c(0, 0), col = col.main.labels)

mtext("Extinction rate", side = 2, outer = FALSE, col = col.axis.labels, line = 1.7, cex = 0.75)
mtext("Extinction rate", side = 2, outer = FALSE, col = col.axis.labels, line = 1.7, cex = 0.75, adj = 13)

dev.off()

