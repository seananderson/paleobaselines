class2 <- data.frame(c("Anthozoa", "Bivalvia", "Echinoidea",
  "Elasmobranchii", "Gastropoda", "Mammalia"), c("Scleractinia",
    "Bivalvia", "Echinoidea", "Sharks", "Gastropoda", "Mammalia"))
colnames(class2) <- c("class", "newclass")
modern <- merge(modern, class2, by = "class")
group <- levels(as.factor(modern$group))
group2 <- data.frame(c("Anthozoa_Acroporidae", "Anthozoa_az",
  "Anthozoa_Faviidae", "Anthozoa_other", "Bivalvia_Anomalodesmata",
  "Bivalvia_Arcoida", "Bivalvia_Carditoida", "Bivalvia_Limoida",
  "Bivalvia_Myoida", "Bivalvia_Pholadomyoida", "Bivalvia_Protobranchia",
  "Bivalvia_Pterioida", "Bivalvia_Veneroida", "Echinoidea_Carinacea",
  "Echinoidea_Cidaroida", "Echinoidea_Irregularia", "Elasmobranchii_Elasmobranchii",
  "Gastropoda_Architaenioglossa", "Gastropoda_Heterobranchia",
  "Gastropoda_Neogastropoda", "Gastropoda_Neotaenioglossa",
  "Gastropoda_Neritopsina", "Gastropoda_Patellogastropoda",
  "Gastropoda_Sorbeoconcha", "Gastropoda_Vetigastropoda",
  "Mammalia_Carnivora", "Mammalia_Mysticeti", "Mammalia_Odontoceti"),
  c("Scleractinia_Acroporidae", "Scleractinia_azooxanthellate",
    "Scleractinia_Faviidae", "Scleractinia_other", "Bivalvia_Anomalodesmata",
    "Bivalvia_Arcoida", "Bivalvia_Carditoida", "Bivalvia_Limoida",
    "Bivalvia_Myoida", "Bivalvia_Pholadomyoida", "Bivalvia_Protobranchia",
    "Bivalvia_Pterioida", "Bivalvia_Veneroida", "Echinoidea_Carinacea",
    "Echinoidea_Cidaroida", "Echinoidea_Irregularia",
    "Sharks", "Gastropoda_Architaenioglossa", "Gastropoda_Heterobranchia",
    "Gastropoda_Neogastropoda", "Gastropoda_Neotaenioglossa",
    "Gastropoda_Neritopsina", "Gastropoda_Patellogastropoda",
    "Gastropoda_Sorbeoconcha", "Gastropoda_Vetigastropoda",
    "Mammalia_Carnivora", "Mammalia_Mysticeti", "Mammalia_Odontoceti"))
colnames(group2) <- c("group", "group2")
modern <- merge(modern, group2, by = "group")

d2 <- plyr::ddply(modern, "newclass", transform,
  group_id = as.numeric(as.factor(as.character(as.numeric(group2)))))

p <- ggplot(d2, aes(y = jitter(gbm_pred_binned, amount = 0.02),
  x = jitter(great.circle, amount = 800), colour = as.factor(group2))) +
  geom_hline(yintercept = mean(d2$gbm_pred_binned), colour = "red") +
  geom_point(alpha = 0.6, pch = 1, size = 3) + facet_wrap(~newclass) +
  theme_bw() + xlab("Great circle distance (km)") + ylab("Intrinsic risk") +
  theme(strip.text.x = element_text(size = 8), axis.text = element_text(size = 8)) +
  theme(legend.text = element_text(size = 9)) + scale_colour_manual(values = c("red",
    "blue", "green3", "deeppink1", "orange", "cyan", "yellow3",
    "cornflowerblue", "darkorchid1", "red", "blue", "green3",
    "red", "blue", "green3", "deeppink1", "orange", "cyan",
    "yellow3", "cornflowerblue", "red", "blue", "green3",
    "red", "blue", "green3", "deeppink1", "red")) + theme(legend.title = element_blank()) +
  theme(strip.text.x = element_text(size = 14)) + stat_density2d(colour = "black",
    size = 0.25, bins = 16) + coord_cartesian(xlim = c(0,
      20800), ylim = c(0, 1))
pdf("../figs/plot-genus-predictions-against-gcd.pdf", width = 10, height = 7.5)
print(p)
dev.off()
