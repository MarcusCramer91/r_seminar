###################################################################################
#Output analysis of CMAES with only default stopping criteria

#get all results
CMAES_only_default_results = 
  loadAllResults(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./CMAES_only_default")

#aggregate results
CMAES_only_default_aggResult = aggregateResults(CMAES_only_default_results)

#get data for cumulative distribution
CMAES_only_default_cumulativeDistribution1 = extractECDFofFunctions(
  CMAES_only_default_aggResult, fitnessGap = 1000)
CMAES_only_default_cumulativeDistribution2 = extractECDFofFunctions(
  CMAES_only_default_aggResult, fitnessGap = 100)
CMAES_only_default_cumulativeDistribution3 = extractECDFofFunctions(
  CMAES_only_default_aggResult, fitnessGap = 10)
CMAES_only_default_cumulativeDistribution4 = extractECDFofFunctions(
  CMAES_only_default_aggResult, fitnessGap = 1)
CMAES_only_default_cumulativeDistribution5 = extractECDFofFunctions(
  CMAES_only_default_aggResult, fitnessGap = 1e-02)
CMAES_only_default_cumulativeDistribution6 = extractECDFofFunctions(
  CMAES_only_default_aggResult, fitnessGap = 1e-04)
CMAES_only_default_cumulativeDistribution7 = extractECDFofFunctions(
  CMAES_only_default_aggResult, fitnessGap = 1e-06)
CMAES_only_default_cumulativeDistribution8 = extractECDFofFunctions(
  CMAES_only_default_aggResult, fitnessGap = 1e-08)
plot(CMAES_only_default_cumulativeDistribution1, col = "red", type = "l")
lines(CMAES_only_default_cumulativeDistribution2, col = "blue", type = "l")
lines(CMAES_only_default_cumulativeDistribution3, col = "green", type = "l")
lines(CMAES_only_default_cumulativeDistribution4, col = "cyan", type = "l")
lines(CMAES_only_default_cumulativeDistribution5, col = "yellow", type = "l")
lines(CMAES_only_default_cumulativeDistribution6, col = "darkgoldenrod", type = "l")
lines(CMAES_only_default_cumulativeDistribution7, col = "grey", type = "l")
lines(CMAES_only_default_cumulativeDistribution8, col = "deeppink", type = "l")

#plot runtime
boxplot(CMAES_only_default_aggResult$aggregatedAllRuns)

#plot stagnation time
boxplot(CMAES_only_default_aggResult$aggregatedAllStagnation)
plot(ecdf(CMAES_only_default_aggResult$aggregatedAllStagnation))
hist(log(CMAES_only_default_aggResult$aggregatedAllStagnation))

#plot average best solutions
nFunctions = 24
nDimensions = 4
nInstances = 15
avgBest = double(0)
for (i in 1:(nFunctions*nDimensions)) {
  avgBest = c(avgBest, mean(CMAES_only_default_aggResult$aggregatedAllBest[((i-1)*nInstances+1):(i*nInstances)]))
}

plot(log(avgBest+1))
outlierVector = which(log(avgBest+1) > 8)
points(outlierVector,log(avgBest+1)[outlierVector], col = "red")
#this shows 3 outliers that need to be removed
avgBestCleaned = avgBest[-outlierVector]

#get average best per function
averages = double(0)
#consider cleaned elements
cleanedCount = 0
for (i in 1:nFunctions) {
  startIndex = (i-1)*nDimensions+1
  #consider cleaned elements
  cleanedCount = cleanedCount + sum(outlierVector %in% (((i-1)*nDimensions+1):(i*nDimensions)))
  endIndex = (i*nDimensions-cleanedCount)
  averages = c(averages, mean(avgBestCleaned[startIndex:endIndex]))
}
#add one in order to avoid large negative values for log -> better readibility of the plot
plot(log(averages+1), axes = FALSE)
axis(1, at = seq(2, 24, by = 2))
axis(2)
box()

#get average best per dimension
averagesDim = double(0)
for (i in 1:nDimensions) {
  relevantIndexes = seq(from = i, to = (nDimensions*nFunctions)-(nDimensions-i), by = nDimensions)
  #consider cleaned elements
  relevantIndexes = relevantIndexes[!relevantIndexes %in% outlierVector]
  averagesDim = c(averagesDim, mean(avgBest[relevantIndexes]))
}
#add one in order to avoid large negative values for log -> better readibility of the plot
plot(c(2, 5, 10, 20), log(averagesDim+1), axes = FALSE)
#testing lists
#they are a bitch
a = list(a1 = c(1,2), a2 = c(3,4))
b = list(b1 = c(5,6), b2 = c(7,8))
c = list(c1 = c(9,10), c2 = c(11,12))
d = list(d1 = c(13,14), d2 = c(15,16))
ab = list(a,b)
abc = c(ab, list(c))
abcd = c(abc, list(d))
abcd[[1]]$a1
abcd[[1]]$a2
abcd[[2]]$b1
abcd[[2]]$b2
abcd[[3]]$c1
abcd[[3]]$c2
abcd[[4]]$d1
abcd[[4]]$d2

CMAES_only_default_aggResult$aggregatedAllBest
