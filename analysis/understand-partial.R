x <- 1:100
y <- 1:100 * 2.4
library(gbm)
m <- gbm(y ~ x, n.trees = 300, interaction.depth = 1, shrinkage = 0.001)
plot.gbm(m)
m <- gbm(y ~ x, n.trees = 300, interaction.depth = 1, shrinkage = 0.05)
plot.gbm(m)
m <- gbm(y ~ x, n.trees = 3000, interaction.depth = 1, shrinkage = 0.001)
plot.gbm(m)
m <- gbm(y ~ x, n.trees = 30000, interaction.depth = 1, shrinkage = 0.001)
plot.gbm(m)

y <- rbinom(3000, 1, seq(0, 1, length.out = 3000))
y <- rbinom(3000, 1, 0.2)
x <- 1:3000
m <- gbm(y ~ x, n.trees = 1000, interaction.depth = 1, shrinkage = 0.01)
plot.gbm(m, type = "response")

x <- 1:100
b1 <- 2.3
y <-

x1 <- rnorm(1000)
x2 <- rnorm(1000)
z <- -4 + 2*x1 + 3*x2
pr <- 1/(1+exp(-z))
y <- rbinom(1000,1,pr)
df <- data.frame(y=y,x1=x1,x2=x2)
m <- gbm(y ~ x1 + x2, n.trees = 1000, shrinkage = 0.05, interaction.dept = 1, data = df, distribution = "bernoulli")
p1 <- plot.gbm(m, return.grid = TRUE,  i.var = 1, type = "response")
p2 <- plot.gbm(m, return.grid = TRUE, i.var = 2, type = "response")
plot(p1$x1, 0.0 + p1$y - mean(p1$y), ylim = c(-1, 1), type = "l", yaxs = "i", col = "red")
lines(p2$x2, 0.0 + p2$y - mean(p2$y), lty = 1, col = "blue")



plot.gbm(m, i.var = 1)
plot.gbm(m, i.var = 2)

m.glm <- glm( y~x1+x2,data=df,family="binomial")
