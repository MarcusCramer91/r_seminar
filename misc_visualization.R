if (!"smoof" %in% rownames(installed.packages())) install.packages("smoof")
if (!"devtools" %in% rownames(installed.packages())) install.packages("devtools")
require(devtools)
install_github(repo = "MarcusCramer91/cmaesr")
require(smoof)
require(cmaesr)


#show the population development of a CMAES run
fun = makeBBOBFunction(dimension = 2, fid = 1, iid = 1)
stopcond = stopOnMaxIters(100)
pdf(file = "visualization.pdf")
dump = cmaes(fun, monitor = makeVisualizingMonitor(), control = list(stop.ons = list(stopcond)))
dev.off()

#################################################################

#show the concept of Evolution strategies
#demonstration function
objective = function(x) {
  y = x * sin(3*x)+1
  if (x > 4 || x < -4) y = -99999L
  return(y)
}
png(file = "naive example1.png")
curve(objective, from = -4, to = 4, ylab = "f(x)", cex.axis = 1.5, cex.lab = 1.5)
dev.off()

#show how naive optimization fails
png(file = "naive example2.png")
x = 1
curve(objective, from = -4, to = 4, ylab = "f(x)", cex.axis = 1.5, cex.lab = 1.5)
points(x, objective(x), col = "red", cex = 2, pch = 16)
dev.off()
#naive optimization
finished = FALSE
while (!finished) {
  value = objective(x)
  if (objective(x+0.01) < value) x = x+0.01
  else if (objective(x-0.01) < value) x = x-0.01
  else finished = TRUE
}
png(file = "naive example3.png")
curve(objective, from = -4, to = 4, ylab = "f(x)", cex.axis = 1.5, cex.lab = 1.5)
points(1, objective(1), col = "red", cex = 2, pch = 16)
points(x, objective(x), col = "red", cex = 2, pch = 16)
dev.off()


#simple ES
lambda = 10
mu = 5
x = runif(lambda, -4, 4)


maxit = 10
for (i in 1:maxit) {
  values = objective(x)
  
  png(file = paste("ES demo", i, ".png"))
  curve(objective, from = -4, to = 4, ylab = "f(x)", cex.axis = 1.5, cex.lab = 1.5)
  points(x, values, col = "red", cex = 2, pch = 16)
  
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
  points(center, objective(center), pch = 2, cex = 2, col = "green")
  dev.off()
}

############################################
#some more visuals for the presentation
if (!"MASS" %in% rownames(installed.packages())) install.packages("MASS")
if (!"lattice" %in% rownames(installed.packages())) install.packages("lattice")
require(MASS)
require(lattice)

png("normal1.png", height = 800, width = 800)
par(mar=c(5,5,5,5))
curve(dnorm, from = -10, to = 10, ylab = "density", cex.lab = 4, cex.axis = 4, lwd = 3)
lines(c(-10, -10), c(-1, 10), col = "red", lwd = 3)
lines(c(10, 10), c(10, -1), col = "red", lwd = 3)
dev.off()


png("normal2.png", height = 800, width = 800)
par(mar=c(5,5,5,5))
curve(dnorm, from = -1, to = 1, ylab = "density", cex.lab = 4, cex.axis = 4, lwd = 3)
lines(c(-1, -1), c(-10, 10), col = "red", lwd = 3)
lines(c(1, 1), c(-10, 10), col = "red", lwd = 3)
dev.off()

require(lattice)
x = seq(from = -10, to = 10, by = 0.1)
y = seq(from = -10, to = 10, by = 0.1)
z = m