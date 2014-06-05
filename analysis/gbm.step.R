
library(dismo)
g1 <- gbm.step(data = neog, gbm.x = c(4, 5, 7:14), gbm.y = 16, family = "bernoulli", tree.complexity = 5, learning.rate = 0.01, bag.fraction = 0.5, site.weights = weights, max.trees = 4000)

#1.728893

g2 <- gbm.step(data = neog, gbm.x = c(4, 5, 7:14), gbm.y = 16, family = "bernoulli", tree.complexity = 3, learning.rate = 0.01, site.weights = weights, max.trees = 4000)

g3 <- gbm.step(data = neog, gbm.x = c(4, 5, 7:14), gbm.y = 16, family = "bernoulli", tree.complexity = 1, learning.rate = 0.01, site.weights = weights, max.trees = 4000)

g3 <- gbm.step(data = neog, gbm.x = c(4, 5, 7:14), gbm.y = 16, family = "bernoulli", tree.complexity = 5, learning.rate = 0.01, site.weights = weights, max.trees = 4000)

g5 <- gbm.step(data = neog, gbm.x = c(4, 5, 7:14), gbm.y = 16, family = "bernoulli", tree.complexity = 5, learning.rate = 0.005, site.weights = weights, max.trees = 3000, n.trees = 800)

ggplot(neog, aes(jitter(Ex), fitted, colour = class)) + geom_point(alpha = 0.3) + facet_wrap(~class)
