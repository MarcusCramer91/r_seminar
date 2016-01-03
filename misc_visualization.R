if (!"mvtnorm" %in% rownames(installed.packages())) install.packages("mvtnorm")
require(mvtnorm)
#################################################################

#show the concept of Evolution strategies
#demonstration function
objective = function(x) {
  y = x * sin(3*x)+1
  if (x > 4 || x < -4) y = -99999L
  return(y)
}
png(file = "naive example1.png", height = 1200, width = 1200)
par(mar=c(9,9,5,2))
par(mgp = c(6,2,0))
curve(objective, from = -4, to = 4, ylab = "f(x)", cex.axis = 4, cex.lab = 4, lwd = 4)
dev.off()

#show how naive optimization fails
png(file = "naive example2.png", height = 1200, width = 1200)
par(mar=c(9,9,5,2))
par(mgp = c(6,2,0))
x = 1
curve(objective, from = -4, to = 4, ylab = "f(x)", cex.axis = 4, cex.lab = 4, lwd = 4)
points(x, objective(x), col = "red", cex = 5, pch = 16)
dev.off()
#naive optimization
finished = FALSE
while (!finished) {
  value = objective(x)
  if (objective(x+0.01) < value) x = x+0.01
  else if (objective(x-0.01) < value) x = x-0.01
  else finished = TRUE
}
png(file = "naive example3.png", height = 1200, width = 1200)
par(mar=c(9,9,5,2))
par(mgp = c(6,2,0))
curve(objective, from = -4, to = 4, ylab = "f(x)", cex.axis = 4, cex.lab = 4, lwd = 4)
points(1, objective(1), col = "red", cex = 5, pch = 16)
points(x, objective(x), col = "red", cex = 5, pch = 16)
dev.off()


#simple ES
lambda = 10
mu = 5
x = runif(lambda, -4, 4)


maxit = 10
for (i in 1:maxit) {
  values = objective(x)
  
  png(file = paste("ES demo", i, ".png"), height = 1200, width = 1200)
  par(mar=c(9,9,5,2))
  par(mgp = c(6,2,0))
  curve(objective, from = -4, to = 4, ylab = "f(x)", cex.axis = 4, cex.lab = 4, lwd = 4)
  points(x, values, col = "red", cex = 5, pch = 16)
  
  orderedIndex = sort(values, index.return = TRUE)$ix
  x = x[orderedIndex]
  values = values[orderedIndex]
  xMu = x[1:mu]
  vMu = values[1:mu]
  points(xMu, vMu, col = "red", pch = 1, cex = 4)
  
  #weight individuals decreasing
  weights = mu:1
  sumWeights = sum(weights)
  weights = weights / sumWeights
  
  #calculate new center
  center = sum(xMu * weights)
  x = rnorm(10, mean = center, sd = 1)
  x = ifelse(x < -4, -4, ifelse(x > 4, 4, x))
  points(center, objective(center), pch = 17, cex = 5, col = "green")
  dev.off()
}

############################################
#some more visuals for the presentation
#plot normal distribution density for different parameter spaces
if (!"MASS" %in% rownames(installed.packages())) install.packages("MASS")
if (!"lattice" %in% rownames(installed.packages())) install.packages("lattice")
require(MASS)
require(lattice)

png("normal1.png", height = 1200, width = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
curve(dnorm, from = -10, to = 10, ylab = "density", axes = FALSE, cex.lab = 5, lwd = 3, ylim = c(0, 0.5))
axis(1, cex.axis = 4)
axis(2, cex.axis = 4)
box()
lines(c(-10, -10), c(-1, 10), col = "red", lwd = 3)
lines(c(10, 10), c(10, -1), col = "red", lwd = 3)
dev.off()


png("normal2.png", height = 1200, width = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
curve(dnorm, from = -1, to = 1, ylab = "density", axes = FALSE, cex.lab = 5, lwd = 3, ylim = c(0, 0.5))
axis(1, cex.axis = 4)
axis(2, cex.axis = 4)
box()
lines(c(-1, -1), c(-10, 10), col = "red", lwd = 3)
lines(c(1, 1), c(-10, 10), col = "red", lwd = 3)
dev.off()

png("normal3.png", height = 1200, width = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
curve(dnorm, from = -100, to = 100, ylab = "density", axes = FALSE, cex.lab = 5, lwd = 3, ylim = c(0, 0.5))
axis(1, cex.axis = 4)
axis(2, cex.axis = 4)
box()
lines(c(-100, -100), c(-10, 10), col = "red", lwd = 3)
lines(c(100, 100), c(-10, 10), col = "red", lwd = 3)
dev.off()

#plot multivariate normal distribution (dim = 2)
#plot samples from 2 normal distributions first
rnorm1_1 = rnorm(100, 0, 1)
rnorm1_2 = rnorm(100, 0, 1)
rnorm2_1 = rnorm(100, 0, 1)
rnorm2_2 = rnorm(100, 0, 5)
rnorm3_1 = rnorm(100, 0, 5)
rnorm3_2 = rnorm(100, 0, 1)
png("multiple_norm.png", height = 1200, width = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(rnorm1_1, rnorm1_2, xlim = c(-5, 8), ylim = c(-5, 8), col = "black", pch = 16, cex = 2, axes = FALSE, 
     xlab = "x", ylab = "y", cex.lab = 4)
axis(1, cex.axis = 4)
axis(2, cex.axis = 4)
box()
points(rnorm2_1, rnorm2_2, col = "red", pch = 16, cex = 2)
points(rnorm3_1, rnorm3_2, col = "green", pch = 16, cex = 2)
legend("topright", legend = c("rnorm(0,1),rnorm(0,1)", "rnorm(0,1),rnorm(0,5)", "rnorm(0,5),rnorm(0,1)"), 
       col = c("black", "red", "green"), cex = 3, pch = c(16, 16, 16))
dev.off()

#plot samples from multivariate distribution
C1 = diag(2)
C2 = matrix(c(1, -0.75, -0.75, 1), ncol = 2)
C3 = matrix(c(1, 0.75, 0.75, 1), ncol = 2)

rmvnorm1 = rmvnorm(n = 100, mean = c(0,0), sigma = C1)
rmvnorm2 = rmvnorm(n = 100, mean = c(0,0), sigma = C2)
rmvnorm3 = rmvnorm(n = 100, mean = c(0,0), sigma = C3)

png("mvnorm.png", height = 800, width = 800)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(rmvnorm1, xlim = c(-5, 8), ylim = c(-5, 8), col = "black", pch = 16, cex = 2, axes = FALSE, 
     xlab = "x", ylab = "y", cex.lab = 4)
axis(1, cex.axis = 4)
axis(2, cex.axis = 4)
box()
points(rmvnorm2, col = "red", pch = 16, cex = 2)
points(rmvnorm3, col = "green", pch = 16, cex = 2)
legend("topright", legend = c("no correlation", "positive correlation", "negative correlation"), 
       col = c("black", "red", "green"), cex = 3, pch = c(16, 16, 16))
dev.off()

#visualize OCD example
png("OCD_example.png", width = 1200, height = 1200)
solutions = c(13, 11, 11, 11, 10, 7, 5, 5, 5, 5, 5)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(solutions, col = "black", pch = 16, cex = 5,
     xlab = "x", ylab = "best value", cex.lab = 4, cex.axis = 4)
lines(solutions, lwd = 4)
legend("topright", legend = c("fitness"), col = "black", lwd = 4, cex = 4)
dev.off()

#plot with abline
png("OCD_example2.png", width = 1200, height = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(solutions, col = "black", pch = 16, cex = 5,
    xlab = "x", ylab = "best value", cex.lab = 4, cex.axis = 4)
lines(solutions, lwd = 4)
abline(13, -8/13, col = "red", lwd = 4)
legend("topright", legend = c("fitness", "regression line"), col = c("black", "red"), lwd = 4, cex = 4)
dev.off()

#plot deviation
png("OCD_example3.png", width = 1200, height = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(solutions, col = "black", pch = 16, cex = 5,
     xlab = "x", ylab = "best value", cex.lab = 4, cex.axis = 4)
segments(2, 11, 2, 13, lwd = 4, col = "red")
segments(3, 11, 3, 13, lwd = 4, col = "red")
segments(4, 11, 4, 13, lwd = 4, col = "red")
segments(5, 10, 5, 13, lwd = 4, col = "red")
segments(6, 7, 6, 13, lwd = 4, col = "red")
segments(7, 5, 7, 13, lwd = 4, col = "red")
segments(8, 5, 8, 13, lwd = 4, col = "red")
segments(9, 5, 9, 13, lwd = 4, col = "red")
segments(10, 5, 10, 13, lwd = 4, col = "red")
segments(11, 5, 11, 13, lwd = 4, col = "red")
legend("topright", legend = c("fitness", "Improvement"), col = c("black", "red"), lwd = 4, cex = 4)
dev.off()