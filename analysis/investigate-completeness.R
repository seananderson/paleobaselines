# This file makes a number of plots to investigate the role of fossil
# record completeness in the extinction risk estimates.
#
# Run after running
# "make-partial-dependence-data-child.Rnw"
# or from within risksupp.Rnw

neog <- readRDS("../data/stand-predictors-cen-obis.rds")
neog <- droplevels(subset(neog, stage_top < 23 & stage_top != 0))

ne2 <- neog
ne2_no_singles <- subset(ne2, single_obs == 0)
ne2$single_obs_TF <- FALSE
ne2_no_singles$single_obs_TF <- TRUE
ne2 <- rbind(ne2, ne2_no_singles)

#p2 <- ggplot(ne2, aes(single_obs_TF, fill = class)) + geom_histogram() + xlab("Excude singletons?") + ylab("Number of genera")
#ggsave("../figs/hist-single-occ.pdf", width = 6, height = 3)

# make an expanded version for plyr:
ne <- neog
cull_cuts <- c(0, sort(unique(neog$prop_comp))) # all possible
ne$prop_comp_thresh <- 0
for(i in cull_cuts) {
  x <- subset(neog, prop_comp >= i)
  x$prop_comp_thresh <- i
  ne <- rbind(ne, x)
}
ne_long <- reshape2::melt(ne, id.vars = c("stage", "prop_comp_thresh", "genus", "Ex"), measure.vars = c("richness", "occupancy", "occurrences", "min.lat", "max.lat", "lat.range", "mean.lat", "great.circle"))

ne_long_sing <- reshape2::melt(ne2, id.vars = c("stage", "single_obs_TF", "genus", "Ex"), measure.vars = c("richness", "occupancy", "occurrences", "min.lat", "max.lat", "lat.range", "mean.lat", "great.circle"))

# find sample sizes:
ne_sum <- plyr::ddply(ne_long, c("prop_comp_thresh", "variable", "value"),
  plyr::summarize,
  N = length(Ex), N_ex = sum(Ex))

ne_sum_sing <- plyr::ddply(ne_long_sing, c("single_obs_TF", "variable", "value"),
  plyr::summarize,
  N = length(Ex), N_ex = sum(Ex))

# and find sample sizes by stage (raw version of main Fig 1):

# colours to match fig 1 in main paper:
pal <- rev(colorspace::rainbow_hcl(4, c = 90, l = 65))
pal <- c(pal, "#000000")

ne_long_w_neog <- ne_long
ne_long_w_neog$stage <- "Neogene"
ne_long_w_neog <- rbind(ne_long, ne_long_w_neog)
ne_stage_sum <- ne_long_w_neog %>%
  filter(prop_comp_thresh == 1.0) %>%
  group_by(stage, prop_comp_thresh, variable, value) %>%
  dplyr::summarise(N = length(Ex), N_ex = sum(Ex))
p99 <- ggplot(ne_stage_sum, aes(value, N_ex/N, colour = stage, group = stage, size = stage)) + geom_line() + facet_wrap(~variable, scales = "free_x", nrow = 2) + labs(colour = "Stage") + theme_bw() + scale_colour_manual(values = pal) + scale_size_manual(values = c(1, 1, 1, 1, 2)) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) + xlab("Predictor value") + ylab("Proportion extinct") + guides(size = FALSE)
ggsave("../figs/raw-predictors-proportion-ext.pdf", width = 11, height = 4.5)


# add predictions:
neog$pred <- predict(stage_models_culls[[1]][[1]], n.trees = NTREES, type = "response")
ne$pred <- predict(stage_models_culls[[1]][[1]], n.trees = NTREES, type = "response",newdata = ne)

x <- plyr::ddply(neog, c("class", "group"), plyr::summarise, mean_prop_comp = mean(prop_comp), median_prop_comp = median(prop_comp), l_prop_comp = quantile(prop_comp, probs = 0.25), u_prop_comp = quantile(prop_comp, probs = 0.75))

partial_groups_culled$group <- partial_groups_culled$value
partial_groups_culled <- plyr::join(partial_groups_culled, x)

### make plots

p <- ggplot(partial_groups_culled, aes(mean_prop_comp, median, colour = class)) + geom_point() + xlab("Mean preservation probability") + ylab("Partial dependence extinction risk") + theme_bw()
ggsave("../figs/fossil-cull-partial-groups-vs-completeness.pdf",  width = 7, height = 5)

p1 <- ggplot(partial_continuous_culled, aes(value, median_shifted, colour = preservation_cutoff, group = preservation_cutoff)) + geom_line(lwd = 1.8) + facet_wrap(~predictor, scales = "free_x", nrow = 2) + theme_bw() + ylab("Relative partial dependence") + xlab("Value") + ylim(-0.5, 0.5)
ggsave("../figs/fossil-cull-comparison-continuous.pdf", width = 12, height = 5)

p2 <- ggplot(partial_groups_culled, aes(median, value, fill = preservation_cutoff, group = preservation_cutoff)) + geom_point(cex = 3, pch = 21, col = "black") + theme_bw() + scale_fill_continuous(name = "Preservation threshold") + xlab("Relative partial dependence") + ylab("")
ggsave("../figs/fossil-cull-comparison-group.pdf", width = 7, height = 5)

p3 <- ggplot(partial_groups_culled, aes(preservation_cutoff, median, group = value)) + geom_line() + theme_bw() + ylab("Relative influence on extinction probability") + xlab("Preservation threshold")
ggsave("../figs/fossil-cull-comparison-bump.pdf", width = 7, height = 5)

p1 <- ggplot(ne_sum, aes(value, N, colour = prop_comp_thresh, group = prop_comp_thresh)) + geom_line(lwd = 1.5) + facet_wrap(~variable, scales = "free_x", nrow = 2) + labs(colour = ">= preservation probability") + theme_bw()
p2 <- ggplot(ne_sum, aes(value, N_ex, colour = prop_comp_thresh, group = prop_comp_thresh)) + geom_line(lwd = 1.5) + facet_wrap(~variable, scales = "free_x", nrow = 2) + labs(colour = ">= preservation probability") + theme_bw()
p3 <- ggplot(ne_sum, aes(value, N_ex/N, colour = prop_comp_thresh, group = prop_comp_thresh)) + geom_line(lwd = 1.5) + facet_wrap(~variable, scales = "free_x", nrow = 2) + labs(colour = ">= preservation probability") + theme_bw()
pdf("../figs/fossil-cull-N.pdf", width = 10, height = 9)
gridExtra::grid.arrange(p1, p2, p3)
dev.off()

## same with singular obs. cull:
#ne_sum_sing <- plyr::join(ne_sum_sing, data.frame(single_obs = c(0, 1), single_obs_TF = c(FALSE, TRUE)))
p1 <- ggplot(ne_sum_sing, aes(value, N, colour = single_obs_TF, group = single_obs_TF)) + geom_line(lwd = 1.5) + facet_wrap(~variable, scales = "free", nrow = 2) + labs(colour = "Cull single\noccurrences") + theme_bw()
p2 <- ggplot(ne_sum_sing, aes(value, N_ex, colour = single_obs_TF, group = single_obs_TF)) + geom_line(lwd = 1.5) + facet_wrap(~variable, scales = "free", nrow = 2) + labs(colour = "Cull single\noccurrences") + theme_bw()
p3 <- ggplot(ne_sum_sing, aes(value, N_ex/N, colour = single_obs_TF, group = single_obs_TF)) + geom_line(lwd = 1.5) + facet_wrap(~variable, scales = "free", nrow = 2) + labs(colour = "Cull single\noccurrences") + theme_bw()
pdf("../figs/fossil-cull-N-sing.pdf", width = 10, height = 9)
gridExtra::grid.arrange(p1, p2, p3)
dev.off()

p1 <- ggplot(neog, aes(prop_comp, pred, fill = class, colour = class)) + geom_violin(aes(group = prop_comp), scale = "width") + facet_wrap(~class)+ theme_bw() + geom_point(alpha = 0.1, colour = "black", position = position_jitter(width = 0.05)) + xlab("Proportional completeness") + ylab("Predicted relative extinction risk")

p2 <- ggplot(neog, aes(prop_comp, pred, fill = class, colour = class)) + geom_boxplot(aes(group = prop_comp)) + facet_wrap(~class)+ theme_bw() + geom_point(alpha = 0.05, colour = "black", position = position_jitter(width = 0.05)) + xlab("Preservation probability") + ylab("Predicted relative extinction risk")
pdf("../figs/fossil-cull-self-prediction-distributions.pdf", width = 9, height = 7)
gridExtra::grid.arrange(p1, p2)
dev.off()

class_medians_culls <- ne %>% group_by(class, prop_comp_thresh) %>%
  dplyr::summarise(m = median(pred),
    l = quantile(pred, probs = 0.4),
    u = quantile(pred, probs = 0.6)) %>%
  group_by(prop_comp_thresh) %>%
  dplyr::arrange(m) %>%
  dplyr::mutate(m_order = seq_along(m))

# ggplot(class_medians_culls, aes(prop_comp_thresh, m_order, group = class)) + geom_line()
p1 <- ggplot(class_medians_culls, aes(prop_comp_thresh, m, group = class, colour = class)) +
  geom_ribbon(aes(ymax = u, ymin = l, fill = class, linetype = NA), alpha = 0.15) +
  geom_line(lwd = 1.5) +
  scale_y_log10(breaks = c(0.05, 0.1, 0.2, 0.5, 1)) +
  ylab("Partial dependence component aggregated by class") +
  xlab("Preservation probability threshold") + theme_bw() + labs(colour = "Group")
ggsave("../figs/partial-class-level-median-estimates.pdf", width = 8, height = 5)

p1 <- ggplot(ne, aes(prop_comp_thresh, pred, fill = class, colour = class)) + geom_violin(aes(group = prop_comp_thresh), scale = "width") + facet_wrap(~class)+ theme_bw() + xlab("Preservation probability threshold") + ylab("Predicted relative extinction risk")

p2 <- ggplot(ne, aes(prop_comp_thresh, pred, fill = class)) + geom_boxplot(aes(group = prop_comp_thresh)) + facet_wrap(~class)+ theme_bw() +  xlab("Preservation probability threshold") + ylab("Predicted relative extinction risk") + theme(legend.position = "none")
#   geom_point(data = class_medians_culls, aes(x = prop_comp_thresh, y = m), colour = "black", pch = 3)

# pdf("../figs/fossil-cull-self-prediction-threshold-distributions.pdf", width = 9, height = 7)
pdf("../figs/fossil-cull-self-prediction-threshold-distributions.pdf", width = 9, height = 4)
# gridExtra::grid.arrange(p1, p2)
print(p2)
dev.off()

#ggplot(class_medians_culls, aes())
