###################################################################################
#Output analysis of CMAES with only default stopping criteria

#get all results
CMAES_only_default_results = 
  loadAllResults(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./CMAES_only_default")
RS_results = 
  loadAllResults(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./Random_Search_234204")

#aggregate results
CMAES_only_default_aggResult = aggregateResults(CMAES_only_default_results)
RS_results_aggregated = aggregateResults(RS_results)

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

#identify worst stagnation example
#########################################################################
which(CMAES_only_default_aggResult$aggregatedAllStagnation == 
        max(CMAES_only_default_aggResult$aggregatedAllStagnation))
#420
420/15
#file number 28
file = "./Stagnation_analysis/CMAES_output_7_20.txt"
worstStagnation = readOutput(file)
worstStagnationIndex = which(worstStagnation$allStagnations == max(worstStagnation$allStagnations))
feMultiplier = worstStagnation$allRunsEval[worstStagnationIndex] / 
  worstStagnation$allRuns[worstStagnationIndex]
worstStagnationData = worstStagnation$allConvergence[,worstStagnationIndex]
plot((1:worstStagnation$allRuns[worstStagnationIndex]*feMultiplier), 
           log(worstStagnationData[1:worstStagnation$allRuns[worstStagnationIndex]]+1),type = "l")

#draw red lines where stagnation takes place
lines(((worstStagnation$allRuns[worstStagnationIndex]-max(worstStagnation$allStagnations)):
         (worstStagnation$allRuns[worstStagnationIndex])*feMultiplier),
      log(worstStagnationData[(length(worstStagnationData)-max(worstStagnation$allStagnations)):
                            (length(worstStagnationData))]+1), 
      col ="red", cex = 2)

activeFunctions = getActiveFunctions(CMAES_only_default_aggResult)
feMultiplier = CMAES_only_default_aggResult$aggregatedAvgRunEval/CMAES_only_default_aggResult$aggregatedAvgRun
plot((1:length(activeFunctions))*feMultiplier, activeFunctions, type = "l")


s#plot runtime
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

#plots to show that multiple restarts make for better solutions

file1 = "./restart_test_runs/CMAES_output_10_20_1.txt"
file2 = "./restart_test_runs/CMAES_output_10_20_2.txt"
file3 = "./restart_test_runs/CMAES_output_10_20_3.txt"
file4 = "./restart_test_runs/CMAES_output_10_20_4.txt"
file5 = "./restart_test_runs/CMAES_output_10_20_5.txt"
restart_test1 = readOutput(file1)
restart_test2 = readOutput(file2)
restart_test3 = readOutput(file3)
restart_test4 = readOutput(file4)
restart_test5 = readOutput(file5)

#order descending
restart_test1$allBest #3
restart_test2$allBest #5
restart_test3$allBest #1
restart_test4$allBest #3
restart_test5$allBest #1

feMultiplier = restart_test1$longestRunEval / restart_test1$longestRun
#assume all restarts are equally long
plot((((5-5) * restart_test3$longestRun + 1):(restart_test3$longestRun * (5-4)))* feMultiplier, 
     log(restart_test3$allConvergence[,1]+1), type = "l", 
     xlim = c(0, (restart_test3$longestRun * feMultiplier * 5)), ylim = c(12, 18))

lines((((5-4) * restart_test5$longestRun + 1):(restart_test5$longestRun * (5-3)))* feMultiplier, 
     log(restart_test5$allConvergence[,1]+1))

lines((((5-3) * restart_test2$longestRun + 1):(restart_test2$longestRun * (5-2)))* feMultiplier, 
      log(restart_test2$allConvergence[,1]+1))

lines((((5-2) * restart_test4$longestRun + 1):(restart_test4$longestRun * (5-1)))* feMultiplier, 
      log(restart_test4$allConvergence[,1]+1))

lines((((5-1) * restart_test2$longestRun + 1):(restart_test2$longestRun * (5-0)))* feMultiplier, 
      log(restart_test2$allConvergence[,1]+1))

#in comparison the normal run
lines((1:nrow(CMAES_only_default_aggResult$aggregatedAllConvergence)) * feMultiplier,
  log(CMAES_only_default_aggResult$aggregatedAllConvergence[,40])+1, type = "l", col = "red")


