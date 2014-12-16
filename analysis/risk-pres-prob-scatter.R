#library(gridExtra)

### make data frame that includes both measures of completeness
dat <- readRDS("../data/modern_and_paleo_ranges.rds")
pseudo.ex <- read.csv("../data/4.6.2014.false.extinctions.csv", header = TRUE)
dat2 <- merge(dat, pseudo.ex)
dat2 <- dat2[(dat2$top > 0), ]
dat3 <- unique(dat2[c("class", "group", "genus", "prop_comp", "FALSE.EXT")])

### get mean values
mean.prop <- function(df) mean(df$prop_comp)
mean.pseudo.ex <-function(df) mean(df$FALSE.EXT)

preservation <- plyr::ddply(dat3, plyr::.(class, group), plyr::each(mean.prop, mean.pseudo.ex))

### extract mean completness metrics
predictions <- readRDS("../data/modern-predictions.rds")
predictions <- predictions[(predictions$Interval_Name == "Modern_merged"), ]

### get mean values
mean.risk <- function(df) mean(df$pred)
n.gen <- function(df) length(df$pred)
preds <- plyr::ddply(predictions, plyr::.(class, group), plyr::each(mean.risk, n.gen))

### merge preservation and prediction
plot.dat <- merge(preservation, preds)

plot.dat$group <- data.frame(t(do.call("cbind",
      strsplit(as.character(plot.dat$group), "_"))))[, 2]

p1 <- ggplot(plot.dat, aes(mean.prop, mean.risk, colour = class, label = group)) +
  geom_point(size = 3) + geom_text(size = 3, hjust = .5,  vjust = -.6) + theme_bw() +
  xlab("Mean preservation probability") + ylab("Mean predicted intrinsic risk") +
  coord_cartesian(xlim = c(.65, 1)) + labs(colour = "Group")
  #+ theme(legend.position = "none")

p2 <- ggplot(plot.dat, aes(mean.pseudo.ex, mean.risk, colour = class, label = group)) +
  geom_point(size = 3) + geom_text(size = 3, hjust = .5,  vjust = -.6) + theme_bw() +
  xlab("Proportion of false extinctions") +
  ylab("Mean predicted intrinsic risk") +
  coord_cartesian(xlim = c(-.08, .5)) + labs(colour = "Group")

pdf("../figs/risk-vs-preservation.pdf",  width = 12,  height = 5.1)
gridExtra::grid.arrange(p1, p2, ncol = 2)
dev.off()
