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

##############################
#analyze convergence behavior on different functions
aggregatedConvergenceFunctions = getAggregatedConvergenceFunctions(CMAES_only_default_aggResult, 
                                                                   nFunctions = 24, nDimensions = 4)

#add information on FEs
plot(log(aggregatedConvergenceFunctions[
  1:CMAES_only_default_aggResult$aggregatedAllRuns,1]+1), type = "l")
plot(log(aggregatedConvergenceFunctions[,2]+1), type = "l")
plot(log(aggregatedConvergenceFunctions[,3]+1), type = "l")
plot(log(aggregatedConvergenceFunctions[,4]+1), type = "l")
plot(log(aggregatedConvergenceFunctions[,5]+1), type = "l")
plot(log(aggregatedConvergenceFunctions[,6]+1), type = "l")
plot(log(aggregatedConvergenceFunctions[,7]+1), type = "l") #stagnates around 1
plot(log(aggregatedConvergenceFunctions[,8]+1), type = "l") #stagnates clearly above 0
plot(log(aggregatedConvergenceFunctions[,9]+1), type = "l") 
plot(log(aggregatedConvergenceFunctions[,10]+1), type = "l") #stagnaties around 11
plot(log(aggregatedConvergenceFunctions[,11]+1), type = "l") #stagnaties around 2
plot(log(aggregatedConvergenceFunctions[,12]+1), type = "l") #stagnaties around 7
plot(log(aggregatedConvergenceFunctions[,13]+1), type = "l") #stagnaties above 3
plot(log(aggregatedConvergenceFunctions[,14]+1), type = "l")
plot(log(aggregatedConvergenceFunctions[,15]+1), type = "l") #stagnaties around 3
plot(log(aggregatedConvergenceFunctions[,16]+1), type = "l") #stagnaties around 0.5
plot(log(aggregatedConvergenceFunctions[,17]+1), type = "l") #stagnaties around 0.5
plot(log(aggregatedConvergenceFunctions[,18]+1), type = "l") #stagnaties around 2.5
plot(log(aggregatedConvergenceFunctions[,19]+1), type = "l") #stagnaties around 0.5
plot(log(aggregatedConvergenceFunctions[,20]+1), type = "l") #stagnaties around 1
plot(log(aggregatedConvergenceFunctions[,21]+1), type = "l") #stagnaties around 2
plot(log(aggregatedConvergenceFunctions[,22]+1), type = "l") #stagnaties around 2.8
plot(log(aggregatedConvergenceFunctions[,23]+1), type = "l") #stagnaties around 0.75
plot(log(aggregatedConvergenceFunctions[,24]+1), type = "l") #stagnaties around 3

#show stagnation for the example of one function 10 dimension 20 run
file = "./CMAES_only_default/CMAES_output_10_20.txt"
fun10_dim20_singleResult = readOutput(file)

#plot runtime
boxplot(x = CMAES_only_default_aggResult$aggregatedAllRunsEval)

#plot stagnation time
#multiply by eval/iter ratio
boxplot(CMAES_only_default_aggResult$aggregatedAllStagnation 
        * (CMAES_only_default_aggResult$aggregatedAllRunsEval / CMAES_only_default_aggResult$aggregatedAllRuns))
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
