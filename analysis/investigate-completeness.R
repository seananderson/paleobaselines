# This file makes a bunch of plots to investigate the role of fossil
# record completeness in the extinction risk estimates
#
# Run after running 
# "make-partial-dependence-data-child.Rnw"
# Sean 20140926

# make an expanded version for plyr:
ne <- neog
ne$prop_comp_thresh <- 0
for(i in seq(0.25, 1, 0.25)) {
  x <- subset(neog, prop_comp >= i)
  x$prop_comp_thresh <- i
  ne <- rbind(ne, x)
}
ne_long <- melt(ne, id.vars = c("stage", "prop_comp_thresh", "genus", "Ex"), measure.vars = c("richness", "occupancy", "occurrences", "min.lat", "max.lat", "lat.range", "mean.lat", "great.circle"))

# find sample sizes:
ne_sum <- ddply(ne_long, c("prop_comp_thresh", "variable", "value"), 
  plyr::summarize,
  N = length(Ex), N_ex = sum(Ex))

# add predictions:
neog$pred <- predict(stage_models_culls[[1]][[1]], n.trees = NTREES, type = "response")
ne$pred <- predict(stage_models_culls[[1]][[1]], n.trees = NTREES, type = "response",newdata = ne)

ns <- ddply(neog, "group", plyr::summarise, mean_prop_comp = mean(prop_comp), median_prop_comp = median(prop_comp), l_prop_comp = quantile(prop_comp, probs = 0.25), u_prop_comp = quantile(prop_comp, probs = 0.75))





### make plots

p1 <- ggplot(partial_continuous_culled, aes(value, median_shifted, colour = as.factor(preservation_cutoff))) + geom_line(lwd = 1.8) + facet_wrap(~predictor, scales = "free", nrow = 2) + theme_bw() + scale_colour_manual(values = rev(c(RColorBrewer::brewer.pal(5, "YlGnBu"))[]), name = "Preservation threshold") + ylab("Relative partial dependence") + xlab("Value")
ggsave("../figs/fossil-cull-comparison-continuous.pdf", width = 12, height = 5)

p2 <- ggplot(partial_groups_culled, aes(median, value, fill = as.factor(preservation_cutoff))) + geom_point(cex = 3, pch = 21, col = "black") + theme_bw() + scale_fill_manual(values = rev(c(RColorBrewer::brewer.pal(5, "YlGnBu"))), name = "Preservation threshold") + xlab("Relative partial dependence") + ylab("")
ggsave("../figs/fossil-cull-comparison-group.pdf", width = 7, height = 5)

p3 <- ggplot(partial_groups_culled, aes(preservation_cutoff, median, group = value)) + geom_line() + theme_bw() + ylab("Relative influence on extinction probability") + xlab("Preservation threshold")
ggsave("../figs/fossil-cull-comparison-bump.pdf", width = 7, height = 5)

p1 <- ggplot(ne_sum, aes(value, N, colour = prop_comp_thresh, group = prop_comp_thresh)) + geom_line(lwd = 1.5) + facet_wrap(~variable, scales = "free", nrow = 2) + labs(colour = ">= proportion complete") + theme_bw()

p2 <- ggplot(ne_sum, aes(value, N_ex, colour = prop_comp_thresh, group = prop_comp_thresh)) + geom_line(lwd = 1.5) + facet_wrap(~variable, scales = "free", nrow = 2) + labs(colour = ">= proportion complete") + theme_bw()

p3 <- ggplot(ne_sum, aes(value, N_ex/N, colour = prop_comp_thresh, group = prop_comp_thresh)) + geom_line(lwd = 1.5) + facet_wrap(~variable, scales = "free", nrow = 2) + labs(colour = ">= proportion complete") + theme_bw()

pdf("../figs/fossil-cull-N.pdf", width = 10, height = 9)
gridExtra::grid.arrange(p1, p2, p3)
dev.off()

p1 <- ggplot(neog, aes(prop_comp, pred, fill = class, colour = class)) + geom_violin(aes(group = prop_comp), scale = "width") + facet_wrap(~class)+ theme_bw() + geom_point(alpha = 0.1, colour = "black", position = position_jitter(width = 0.05)) + xlab("Proportional completeness") + ylab("Predicted relative extinction risk")

p2 <- ggplot(neog, aes(prop_comp, pred, fill = class, colour = class)) + geom_boxplot(aes(group = prop_comp)) + facet_wrap(~class)+ theme_bw() + geom_point(alpha = 0.05, colour = "black", position = position_jitter(width = 0.05)) + xlab("Proportional completeness") + ylab("Predicted relative extinction risk")
pdf("../figs/fossil-cull-self-prediction-distributions.pdf", width = 9, height = 7)
gridExtra::grid.arrange(p1, p2)
dev.off()

p1 <- ggplot(ne, aes(prop_comp_thresh, pred, fill = class, colour = class)) + geom_violin(aes(group = prop_comp_thresh), scale = "width") + facet_wrap(~class)+ theme_bw() + xlab("Threshold prop. completeness") + ylab("Predicted relative extinction risk")

p2 <- ggplot(ne, aes(prop_comp_thresh, pred, fill = class, colour = class)) + geom_boxplot(aes(group = prop_comp_thresh)) + facet_wrap(~class)+ theme_bw() +  xlab("Threshold prop. completeness") + ylab("Predicted relative extinction risk")
pdf("../figs/fossil-cull-self-prediction-threshold-distributions.pdf", width = 9, height = 7)
gridExtra::grid.arrange(p1, p2)
dev.off()

