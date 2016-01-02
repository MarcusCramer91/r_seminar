#This file contains visualization of output in a non-sorted manner
#output was created as it was required for analysis and the presentation / documentation
#therefore, expect a bit of chaos
###################################################################################
#Output analysis of CMAES with only default stopping criteria

if (!"ggplot2" %in% rownames(installed.packages())) install.packages("ggplot2")
require(ggplot2)
#source necessary functions
source("output_interpreter.R")
################################################################################
#get results for the first default run
CMAES_only_default_results = 
  loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./CMAES_only_default", 
                         algorithmName = "CMAES")

#aggregate results
CMAES_only_default_aggResult = aggregateResults(CMAES_only_default_results)

#get data for cumulative distribution
CMAES_only_default_cumulativeDistribution1 = extractECDFofFunctions(
  CMAES_only_default_aggResult, fitnessGap = 100)
CMAES_only_default_cumulativeDistribution2 = extractECDFofFunctions(
  CMAES_only_default_aggResult, fitnessGap = 1)
CMAES_only_default_cumulativeDistribution3 = extractECDFofFunctions(
  CMAES_only_default_aggResult, fitnessGap = 1e-04)
CMAES_only_default_cumulativeDistribution4 = extractECDFofFunctions(
  CMAES_only_default_aggResult, fitnessGap = 1e-08)

png("default_only_expectedFE.png", width = 1200, height = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(CMAES_only_default_cumulativeDistribution1, col = "red", type = "l", xlab = "Function evaluations", 
     ylab = "% of functions solved", cex.axis = 4, cex.lab = 4, lwd = 4, cex = 5, axes = FALSE, ylim = c(0, 1.5))
axis(1, cex.axis = 4)
axis(2, at = c(0.0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 4)
box()
lines(CMAES_only_default_cumulativeDistribution2, col = "blue", type = "l", lwd = 4)
lines(CMAES_only_default_cumulativeDistribution3, col = "green", type = "l", lwd = 4)
lines(CMAES_only_default_cumulativeDistribution4, col = "black", type = "l", lwd = 4)
legend("topright", legend = c("100", "1", "1e-04", "1e-08"), col = c("red", "blue", "green", "black"), lwd = 4, cex = 4)
dev.off()

#analyze average convergence behavior
png("convergence_default_only.png", width = 1200, height = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(CMAES_only_default_aggResult$aggregatedAvgConvergence[,1], log(CMAES_only_default_aggResult$aggregatedAvgConvergence[,2]+1), 
     col = "black", type = "l", xlab = "Function evaluations", 
     ylab = "log(average best value + 1)", cex.axis = 4, cex.lab = 4, lwd = 4, cex = 5)
dev.off()


#
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
file = "./CMAES_only_default/CMAES_output_7_20.txt"
worstStagnation = readOutput(file)
worstStagnationIndex = which(worstStagnation$allStagnations == max(worstStagnation$allStagnations)) + 1
feMultiplier = worstStagnation$allRunsEval[worstStagnationIndex] / 
  worstStagnation$allRuns[worstStagnationIndex]
worstStagnationData = worstStagnation$allConvergence[,c(1,worstStagnationIndex)]


png("default_worst_stagnation.png", height = 1200, width = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(worstStagnationData[,1], 
     log(worstStagnationData[,2]+1),type = "l", lwd = 4, cex = 5, cex.lab = 4, cex.axis = 4,
     xlab = "Function evaluations", ylab = "log(average best value + 1)")

#draw red lines where stagnation takes place
lines(worstStagnationData[worstStagnationData[,2] == min(worstStagnationData[,2]),1], 
      log(worstStagnationData[worstStagnationData[,2] == min(worstStagnationData[,2]),2]+1), col = "red", lwd = 4)

dev.off()

#plot runtime
boxplot(x = CMAES_only_default_aggResult$aggregatedAllRunsEval)

#plot stagnation time
#multiply by eval/iter ratio
png("default_stagnation.png", height = 1200, width = 1200)
par(mar=c(9,9,5,2))
par(mgp = c(6,2,0))
boxplot(CMAES_only_default_aggResult$aggregatedAllStagnation 
        * (CMAES_only_default_aggResult$aggregatedAllRunsEval / CMAES_only_default_aggResult$aggregatedAllRuns),
        ylab = "Stagnation length", cex.lab = 4, cex = 5, cex.axis = 4)
dev.off()


plot(ecdf(CMAES_only_default_aggResult$aggregatedAllStagnation))
hist(log(CMAES_only_default_aggResult$aggregatedAllStagnation))

#plot average best solutions
#deprecated
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
#deprecated end

#plots to show that multiple restarts might result in better solutions

file1 = "./CMAES_restart_test/cmaes1_output_12_20.txt"
file2 = "./CMAES_restart_test/cmaes2_output_12_20.txt"
file3 = "./CMAES_restart_test/cmaes3_output_12_20.txt"
file4 = "./CMAES_restart_test/cmaes4_output_12_20.txt"
file5 = "./CMAES_restart_test/cmaes5_output_12_20.txt"
file6 = "./CMAES_restart_test/cmaes6_output_12_20.txt"
restart_test1 = readOutput(file1)
restart_test2 = readOutput(file2)
restart_test3 = readOutput(file3)
restart_test4 = readOutput(file4)
restart_test5 = readOutput(file5)
restart_test6 = readOutput(file6)

#order descending
restart_test1$allBest #3
restart_test2$allBest #1
restart_test3$allBest #5
restart_test4$allBest #2
restart_test5$allBest #4

#concatenate all results to show the current best results over all restarts
cumulated = c(restart_test2$avgConvergence[,2], 
              restart_test4$avgConvergence[,2],
              restart_test1$avgConvergence[,2],
              restart_test5$avgConvergence[,2],
              restart_test3$avgConvergence[,2])
cumulatedTicks = c(restart_test2$avgConvergence[,1], 
                   restart_test4$avgConvergence[,1]+50000,
                   restart_test1$avgConvergence[,1]+100000,
                   restart_test5$avgConvergence[,1]+150000,
                   restart_test3$avgConvergence[,1]+200000)
#get the current best result (red line in the plot)
bestLine = numeric()
for (i in 1:length(cumulated)) {
  bestLine = c(bestLine, min(cumulated[1:i]))
}

#assume all restarts are equally long
png("restart_demo.png", height = 1200, width = 2000)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(restart_test2$avgConvergence[,1], 
     log(restart_test2$avgConvergence[,2]+1), type = "l", 
     xlim = c(0, (restart_test4$longestRunEval * 5)), ylim = c(0,30), xlab = "Function evaluations",
     ylab = "log(average best value + 1)", cex = 5, lwd = 4, cex.lab = 4, cex.axis = 4)
lines(restart_test4$avgConvergence[,1] + 50000, 
      log(restart_test4$avgConvergence[,2]+1), lwd = 4)
lines(restart_test1$avgConvergence[,1] + 100000, 
      log(restart_test1$avgConvergence[,2]+1), lwd = 4)
lines(restart_test5$avgConvergence[,1] + 150000, 
      log(restart_test5$avgConvergence[,2]+1), lwd = 4)
lines(restart_test3$avgConvergence[,1] + 200000, 
      log(restart_test3$avgConvergence[,2]+1), lwd = 4)
#nudge by 0.1 so the original line is still visible
lines(cumulatedTicks, log(bestLine+1)+0.1, col = "red", lwd = 4)
legend("topright", legend = c("Independent restarts", "Current best"), col = c("black", "red"), lwd = 4, cex = 4)
dev.off()

#in comparison the normal run
png("restart_demo2.png", height = 1200, width = 2000)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(restart_test2$avgConvergence[,1], 
     log(restart_test2$avgConvergence[,2]+1), type = "l", 
     xlim = c(0, (restart_test4$longestRunEval * 5)), ylim = c(0,30), xlab = "Function evaluations",
     ylab = "log(average best value + 1)", cex = 5, lwd = 4, cex.lab = 4, cex.axis = 4)
lines(restart_test4$avgConvergence[,1] + 50000, 
      log(restart_test4$avgConvergence[,2]+1), lwd = 4)
lines(restart_test1$avgConvergence[,1] + 100000, 
      log(restart_test1$avgConvergence[,2]+1), lwd = 4)
lines(restart_test5$avgConvergence[,1] + 150000, 
      log(restart_test5$avgConvergence[,2]+1), lwd = 4)
lines(restart_test3$avgConvergence[,1] + 200000, 
      log(restart_test3$avgConvergence[,2]+1), lwd = 4)
lines(restart_test6$avgConvergence[,1],
      log(restart_test6$avgConvergence[,2]+1), type = "l", col = "green", lwd = 4)
#nudge by 0.1 so the original line is still visible
lines(cumulatedTicks, log(bestLine+1)+0.1, col = "red", lwd = 4)
legend("topright", legend = c("Independent restarts", "Current best", "Default run without restarts"), 
       col = c("black", "red", "green"), lwd = 4, cex = 4)
dev.off()


#######################################################################
#load data for 100k function evaluations from random search, CMAES and GA to compare
#this will probably take up to 10 minutes
CMAES_default_restart_results = 
  loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./CMAES_default_with_restart",
                         algorithmName = "CMAES")
RS_results = 
  loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./Random_Search_100000",
                         algorithmName = "random search")
GA_results = 
  loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./GA_default",
                         algorithmName = "GA")

#aggregate results
CMAES_default_restart_results_agg = aggregateResults(CMAES_default_restart_results)
RS_results_agg = aggregateResults(RS_results)
GA_results_agg = aggregateResults(GA_results)

#plot averaged convergence ggplot style
png("test.png", height = 800, width = 800)
ggplot(as.data.frame(CMAES_default_restart_results_agg$aggregatedAvgConvergence), 
       aes(x = allConvergenceTicks, y = log(aggregatedAvgConvergence + 1))) + 
  geom_line() +
  xlab("Function evaluations") +
  ylab("log(averaged convergence + 1)")
dev.off()
#we cannot see any advantage of using ggplot for now, so stick to normal plot
#also ggplot looks horrible in RStudio when using a 4k-monitor

#plot normal style
png("allConvergence_noOCD.png", height = 1200, width = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(CMAES_default_restart_results_agg$aggregatedAvgConvergence[CMAES_default_restart_results_agg$aggregatedAvgConvergence[,1] < 100000,1],
     log(CMAES_default_restart_results_agg$aggregatedAvgConvergence[CMAES_default_restart_results_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), type = "l", ylim = c(5, 20),
     xlab = "Function evaluations", ylab = "log(average best value + 1)", cex.axis = 4, cex.lab = 4, lwd = 4)

points(GA_results_agg$aggregatedAvgConvergence[GA_results_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(GA_results_agg$aggregatedAvgConvergence[GA_results_agg$aggregatedAvgConvergence[,1] < 100000,2])+1, type = "l", col = "red", lwd = 4)

points(RS_results_agg$aggregatedAvgConvergence[RS_results_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(RS_results_agg$aggregatedAvgConvergence[RS_results_agg$aggregatedAvgConvergence[,1] < 100000,2])+1, type = "l", col = "green", lwd = 4)
legend("topright", legend = c("CMA-ES", "GA", "RS"), col = c("black", "red", "green"), lty = 1, lwd = 4, cex = 4)
dev.off()

#plot number of function solved for gap = 1
CMAES_default_restart_cumulativeDistribution = extractECDFofFunctions(
  CMAES_default_restart_results_agg, fitnessGap = 1)
GA_cumulativeDistribution = extractECDFofFunctions(
  GA_results_agg, fitnessGap = 1)
RS_cumulativeDistribution = extractECDFofFunctions(
  RS_results_agg, fitnessGap = 1)

#limit to only entries below 100000 FEs
cumulativeDistCMAES = CMAES_default_restart_cumulativeDistribution[CMAES_default_restart_cumulativeDistribution[,1] < 100000,]
cumulativeDistGA = GA_cumulativeDistribution[GA_cumulativeDistribution[,1] < 100000,]
cumulativeDistRS = RS_cumulativeDistribution[RS_cumulativeDistribution[,1] < 100000,]
#add a point at at 100000 FEs so that graph extends to that point
cumulativeDistCMAES = rbind(cumulativeDistCMAES, c(100000, cumulativeDistCMAES[nrow(cumulativeDistCMAES),2]))
cumulativeDistGA = rbind(cumulativeDistGA, c(100000, cumulativeDistGA[nrow(cumulativeDistGA),2]))
cumulativeDistRS = rbind(cumulativeDistRS, c(100000, cumulativeDistRS[nrow(cumulativeDistRS),2]))
png("default_gap_1.png", height = 1200, width = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(cumulativeDistCMAES,
     col = "black", type = "l",
     xlim = c(0, 100000), 
     ylim = c(0, 1), xlab = "Function evaluations", ylab = "% of functions", 
     cex.axis = 4, cex.lab = 4, lwd = 4)
lines(cumulativeDistGA, col = "red", type = "l", lwd = 4)
lines(cumulativeDistRS, col = "green", type = "l", lwd = 4)
legend("topright", legend = c("CMA-ES", "GA", "RS"), col = c("black", "red", "green"), lty = 1, lwd = 4, cex = 4)
dev.off()

#apparently CMAES just has horrible results for certain functions
#find out these functions
avgBestPerFunction_CMAES = getAvgBestPerFunction(results = CMAES_default_restart_results_agg, nFunctions = 24, 
                                                 nDimensions = 4)
avgBestPerFunction_GA = getAvgBestPerFunction(results = GA_results_agg, nFunctions = 24, 
                                              nDimensions = 4)
avgBestPerFunction_RS = getAvgBestPerFunction(results = RS_results_agg, nFunctions = 24, 
                                              nDimensions = 4)

png("default_best_per_function.png", height = 1200, width = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(log(avgBestPerFunction_CMAES+1), pch = 16, ylim = c(0, 20), axes = FALSE, 
     ylab = "log(average best value + 1)", xlab = "Function",
     cex.axis = 4, cex.lab = 4, lwd = 4, cex = 5)
axis(1, at = 1:24, cex.axis = 4)
axis(2, cex.axis = 4)
box()
points(log(avgBestPerFunction_GA+1), col = "red", pch = 16, cex = 5)
points(log(avgBestPerFunction_RS+1), col = "green", pch = 16, cex = 5)
legend("topright", legend = c("CMA-ES", "GA", "RS"), col = c("black", "red", "green"), pch = 16, cex = 4)
dev.off()
#without the logarithm, the result is even more drastic
png("default_best_per_function_non_log.png", height = 1200, width = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(avgBestPerFunction_CMAES, pch = 16, axes = FALSE, 
     ylab = "Average best value", xlab = "Function",
     cex.axis = 4, cex.lab = 4, lwd = 4, cex = 5)
axis(1, at = 1:24, cex.axis = 4)
axis(2, cex.axis = 4)
box()
points(avgBestPerFunction_GA, col = "red", pch = 16, cex = 5)
points(avgBestPerFunction_RS, col = "green", pch = 16, cex = 5)
legend("topright", legend = c("CMA-ES", "GA", "RS"), col = c("black", "red", "green"), pch = 16, cex = 4)
dev.off()

#apparently function 12 is a major outlier
#plot convergence without function 12
avgConvergence_CMAES = averageConvergence(allConvergence = CMAES_default_restart_results_agg$aggregatedAllConvergence,
                                          includedFunctions = c(1:7, 9:24), includedDimensions = (1:4), 
                                          nDimensions = 4)
avgConvergence_GA = averageConvergence(allConvergence = GA_results_agg$aggregatedAllConvergence,
                                       includedFunctions = c(1:7, 9:24), includedDimensions = (1:4), 
                                       nDimensions = 4)
avgConvergence_RS = averageConvergence(allConvergence = RS_results_agg$aggregatedAllConvergence,
                                       includedFunctions = c(1:7, 9:24), includedDimensions = (1:4), 
                                       nDimensions = 4)

png("allConvergence_noOCD_noFunction8.png", height = 1200, width = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
#limit to entries below 100.000
#due to massively increasing populations, FEs might exceed 100.000 (it is only checked every iteration)
plot(avgConvergence_CMAES[avgConvergence_CMAES[,1] < 100000,1],
     log(avgConvergence_CMAES[avgConvergence_CMAES[,1] < 100000,2]+1), type = "l", ylim = c(0, 20), xlab = "Function evaluations", ylab = "log(best value + 1)",
     cex.axis = 4, cex.lab = 4, lwd = 4, cex = 5)
points(avgConvergence_GA[avgConvergence_GA[,1] < 100000,1],
       log(avgConvergence_GA[avgConvergence_GA[,1] < 100000,2]+1), type = "l", col = "red",  lwd = 4)
points(avgConvergence_RS[avgConvergence_RS[,1] < 100000,1],
       log(avgConvergence_RS[avgConvergence_RS[,1] < 100000,2]+1), type = "l", col = "green", lwd = 4)
legend("topright", legend = c("CMA-ES", "GA", "RS"), col = c("black", "red", "green"), lty = 1, lwd = 4, cex = 4)
dev.off()

#now, CMAES already is much better than random search

#do the same analysis per dimensions
avgBestPerDimension_CMAES = getAvgBestPerDimension(results = CMAES_default_restart_results_agg, nFunctions = 24, 
                                                   nDimensions = 4)
avgBestPerDimension_GA = getAvgBestPerDimension(results = GA_results_agg, nFunctions = 24, 
                                                nDimensions = 4)
avgBestPerDimension_RS = getAvgBestPerDimension(results = RS_results_agg, nFunctions = 24, 
                                                nDimensions = 4)

png("default_best_per_dimension.png", height = 1200, width = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(log(avgBestPerDimension_CMAES+1), pch = 16, ylim = c(0, 20), axes = FALSE, xlab = "Dimension", 
     ylab = "log(average best value + 1)", cex = 5, cex.lab = 5)
axis(1, at = 1:4, labels = c("2", "5", "10", "20"), cex.axis = 4)
axis(2, cex.axis = 4)
box()
points(log(avgBestPerDimension_GA+1), col = "red", pch = 16, cex = 5)
points(log(avgBestPerDimension_RS+1), col = "green", pch = 16, cex = 5)
legend("topright", legend = c("CMA-ES", "GA", "RS"), col = c("black", "red", "green"), lty = 1, lwd = 4, cex = 4)
dev.off()
#without the logarithm, the result is even more drastic
png("default_best_per_dimension_non_log.png", height = 1200, width = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(avgBestPerDimension_CMAES, pch = 16, axes = FALSE, xlab = "Dimension", 
     ylab = "Average best value", cex = 5, cex.lab = 5, ylim = c(0, max(avgBestPerDimension_CMAES) + 
                                                                   max(avgBestPerDimension_CMAES)/2)) 
axis(1, at = 1:4, labels = c("2", "5", "10", "20"), cex.axis = 4)
axis(2, cex.axis = 4)
box()
points(avgBestPerDimension_GA, col = "red", pch = 16, cex = 5)
points(avgBestPerDimension_RS, col = "green", pch = 16, cex = 5)
legend("topright", legend = c("CMA-ES", "GA", "RS"), col = c("black", "red", "green"), lty = 1, lwd = 4, cex = 4)
dev.off()

#plot the convergence without dimension 20 and without function 12
avgConvergence_CMAES = averageConvergence(allConvergence = CMAES_default_restart_results_agg$aggregatedAllConvergence,
                                          includedFunctions = c(1:7, 9:24), includedDimensions = (1:3), 
                                          nDimensions = 4)
avgConvergence_GA = averageConvergence(allConvergence = GA_results_agg$aggregatedAllConvergence,
                                       includedFunctions = c(1:7, 9:24), includedDimensions = (1:3), 
                                       nDimensions = 4)
avgConvergence_RS = averageConvergence(allConvergence = RS_results_agg$aggregatedAllConvergence,
                                       includedFunctions = c(1:7, 9:24), includedDimensions = (1:3), 
                                       nDimensions = 4)

png("allConvergence_noOCD_noFunction8_noDim20.png", height = 1200, width = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(avgConvergence_CMAES[avgConvergence_CMAES[,1] < 100000,1],
     log(avgConvergence_CMAES[avgConvergence_CMAES[,1] < 100000,2]+1), type = "l", ylim = c(0, 20), xlab = "Function evaluations", ylab = "log(best value + 1)",
     cex.axis = 4, cex.lab = 4, lwd = 4, cex = 5)

points(avgConvergence_GA[avgConvergence_GA[,1] < 100000,1],
       log(avgConvergence_GA[avgConvergence_GA[,1] < 100000,2]+1), type = "l", col = "red", lwd = 4)

points(avgConvergence_RS[avgConvergence_RS[,1] < 100000,1],
       log(avgConvergence_RS[avgConvergence_RS[,1] < 100000,2]+1), type = "l", col = "green", lwd = 4)
legend("topright", legend = c("CMA-ES", "GA", "RS"), col = c("black", "red", "green"), lty = 1, lwd = 4, cex = 4)
dev.off()


##########################################
#compare different settings for ocd
#load all configuration results
CMAES_OCD1 = 
  loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./OCD_parametrization/OCD_RUN_0.01_10",
                         algorithmName = "CMAES_OCD")
CMAES_OCD1 = 
  loadAllResults(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./OCD_parametrization/OCD_RUN_0.01_10",
                 algorithmName = "CMAES_OCD")
CMAES_OCD2 = 
  loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./OCD_parametrization/OCD_RUN_0.01_100",
                         algorithmName = "CMAES_OCD")
CMAES_OCD3 = 
  loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./OCD_parametrization/OCD_RUN_0.01_1000",
                         algorithmName = "CMAES_OCD")
CMAES_OCD4 = 
  loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./OCD_parametrization/OCD_RUN_0.001_10",
                         algorithmName = "CMAES_OCD")
CMAES_OCD5 = 
  loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./OCD_parametrization/OCD_RUN_0.001_100",
                         algorithmName = "CMAES_OCD")
CMAES_OCD6 = 
  loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./OCD_parametrization/OCD_RUN_0.001_1000",
                         algorithmName = "CMAES_OCD")
CMAES_OCD7 = 
  loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./OCD_parametrization/OCD_RUN_0.0001_10",
                         algorithmName = "CMAES_OCD")
CMAES_OCD8 = 
  loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./OCD_parametrization/OCD_RUN_0.0001_100",
                         algorithmName = "CMAES_OCD")
CMAES_OCD9 = 
  loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./OCD_parametrization/OCD_RUN_0.0001_1000",
                         algorithmName = "CMAES_OCD")

#aggregate results
CMAES_OCD1_agg = aggregateResults(CMAES_OCD1)
CMAES_OCD2_agg = aggregateResults(CMAES_OCD2)
CMAES_OCD3_agg = aggregateResults(CMAES_OCD3)
CMAES_OCD4_agg = aggregateResults(CMAES_OCD4)
CMAES_OCD5_agg = aggregateResults(CMAES_OCD5)
CMAES_OCD6_agg = aggregateResults(CMAES_OCD6)
CMAES_OCD7_agg = aggregateResults(CMAES_OCD7)
CMAES_OCD8_agg = aggregateResults(CMAES_OCD8)
CMAES_OCD9_agg = aggregateResults(CMAES_OCD9)

#plot the average best results WITH default configuration
allAverageBest = c(CMAES_OCD1_agg$aggregatedAvgBest, 
                   CMAES_OCD2_agg$aggregatedAvgBest, 
                   CMAES_OCD3_agg$aggregatedAvgBest, 
                   CMAES_OCD4_agg$aggregatedAvgBest, 
                   CMAES_OCD5_agg$aggregatedAvgBest, 
                   CMAES_OCD6_agg$aggregatedAvgBest, 
                   CMAES_OCD7_agg$aggregatedAvgBest, 
                   CMAES_OCD8_agg$aggregatedAvgBest, 
                   CMAES_OCD9_agg$aggregatedAvgBest)

allAverageRestarts = c(mean(CMAES_OCD1_agg$aggregatedAllRestarts),
                       mean(CMAES_OCD2_agg$aggregatedAllRestarts),
                       mean(CMAES_OCD3_agg$aggregatedAllRestarts),
                       mean(CMAES_OCD4_agg$aggregatedAllRestarts),
                       mean(CMAES_OCD5_agg$aggregatedAllRestarts),
                       mean(CMAES_OCD6_agg$aggregatedAllRestarts),
                       mean(CMAES_OCD7_agg$aggregatedAllRestarts),
                       mean(CMAES_OCD8_agg$aggregatedAllRestarts),
                       mean(CMAES_OCD9_agg$aggregatedAllRestarts))

png("OCD_Parametrization.png", width = 1200, height = 1200)
par(mar=c(25,9,5,2))
par(mgp = c(6,2,0))
par(las = 3)
plot(allAverageBest, axes = FALSE, pch = 16, ylim = c(0, CMAES_default_restart_results_agg$aggregatedAvgBest), 
     xlab = "", ylab = "Average best value", cex = 5, cex.lab = 4, 
     type = "h", lwd = 4)
axis(1, at = 1:9, labels = c("0.01; 10", 
                             "0.01; 100",
                             "0.01; 1000",
                             "0.001; 10",
                             "0.001; 100",
                             "0.001; 1000",
                             "0.0001; 10",
                             "0.0001; 100",
                             "0.0001; 1000"), cex.axis = 4)
axis(2, at = c(200000, 700000, 1200000), labels = c("2e05", "7e05", "1.2e06"), cex.axis = 4)
box()
#add the average best of default run
lines(c(0,17), c(CMAES_default_restart_results_agg$aggregatedAvgBest, CMAES_default_restart_results_agg$aggregatedAvgBest), 
      col = "red", lwd = 4)
dev.off()

#apparently varLimit = 0.0001 and nPreGen = 100 yields the best results
#this is number 8
#determine boundaries for the plot
lower = min(c(log(CMAES_OCD1_agg$aggregatedAvgConvergence[CMAES_OCD1_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
              log(CMAES_OCD2_agg$aggregatedAvgConvergence[CMAES_OCD2_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
              log(CMAES_OCD3_agg$aggregatedAvgConvergence[CMAES_OCD3_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
              log(CMAES_OCD4_agg$aggregatedAvgConvergence[CMAES_OCD4_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
              log(CMAES_OCD5_agg$aggregatedAvgConvergence[CMAES_OCD5_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
              log(CMAES_OCD6_agg$aggregatedAvgConvergence[CMAES_OCD6_agg$aggregatedAvgConvergence[,1] < 100000,2]+1),
              log(CMAES_OCD7_agg$aggregatedAvgConvergence[CMAES_OCD7_agg$aggregatedAvgConvergence[,1] < 100000,2]+1),
              log(CMAES_OCD8_agg$aggregatedAvgConvergence[CMAES_OCD8_agg$aggregatedAvgConvergence[,1] < 100000,2]+1),
              log(CMAES_OCD9_agg$aggregatedAvgConvergence[CMAES_OCD9_agg$aggregatedAvgConvergence[,1] < 100000,2]+1)))

upper = range(c(log(CMAES_OCD1_agg$aggregatedAvgConvergence[CMAES_OCD1_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
                log(CMAES_OCD2_agg$aggregatedAvgConvergence[CMAES_OCD2_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
                log(CMAES_OCD3_agg$aggregatedAvgConvergence[CMAES_OCD3_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
                log(CMAES_OCD4_agg$aggregatedAvgConvergence[CMAES_OCD4_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
                log(CMAES_OCD5_agg$aggregatedAvgConvergence[CMAES_OCD5_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
                log(CMAES_OCD6_agg$aggregatedAvgConvergence[CMAES_OCD6_agg$aggregatedAvgConvergence[,1] < 100000,2]+1),
                log(CMAES_OCD7_agg$aggregatedAvgConvergence[CMAES_OCD7_agg$aggregatedAvgConvergence[,1] < 100000,2]+1),
                log(CMAES_OCD8_agg$aggregatedAvgConvergence[CMAES_OCD8_agg$aggregatedAvgConvergence[,1] < 100000,2]+1),
                log(CMAES_OCD9_agg$aggregatedAvgConvergence[CMAES_OCD9_agg$aggregatedAvgConvergence[,1] < 100000,2]+1)),
              finite = TRUE)[2]
#add margin for the legend
upper = upper + upper/3

#plot the convergence
png("convergence_OCD_parametrization.png", height = 1200, width = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(CMAES_default_restart_results_agg$aggregatedAvgConvergence[,1],
     log(CMAES_default_restart_results_agg$aggregatedAvgConvergence[,2]+1), type = "l", ylim = c(lower, upper),
     xlab = "Function evaluations", ylab = "log(average best value + 1)", cex.axis = 4, cex.lab = 4, lwd = 4, col = "red")

#due to large population sizes, 100k FEs can be surpassed
#limit plot to 100k FEs
points(CMAES_OCD1_agg$aggregatedAvgConvergence[CMAES_OCD1_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD1_agg$aggregatedAvgConvergence[CMAES_OCD1_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "black", lwd = 4)

points(CMAES_OCD2_agg$aggregatedAvgConvergence[CMAES_OCD2_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD2_agg$aggregatedAvgConvergence[CMAES_OCD2_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "black", lwd = 4)

points(CMAES_OCD3_agg$aggregatedAvgConvergence[CMAES_OCD3_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD3_agg$aggregatedAvgConvergence[CMAES_OCD3_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "black", lwd = 4)

points(CMAES_OCD4_agg$aggregatedAvgConvergence[CMAES_OCD4_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD4_agg$aggregatedAvgConvergence[CMAES_OCD4_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "black", lwd = 4)

points(CMAES_OCD5_agg$aggregatedAvgConvergence[CMAES_OCD5_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD5_agg$aggregatedAvgConvergence[CMAES_OCD5_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "black", lwd = 4)

points(CMAES_OCD6_agg$aggregatedAvgConvergence[CMAES_OCD6_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD6_agg$aggregatedAvgConvergence[CMAES_OCD6_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "black", lwd = 4)

points(CMAES_OCD7_agg$aggregatedAvgConvergence[CMAES_OCD7_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD7_agg$aggregatedAvgConvergence[CMAES_OCD7_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "black", lwd = 4)

points(CMAES_OCD8_agg$aggregatedAvgConvergence[CMAES_OCD8_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD8_agg$aggregatedAvgConvergence[CMAES_OCD8_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "cyan", lwd = 4)

points(CMAES_OCD9_agg$aggregatedAvgConvergence[CMAES_OCD9_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD9_agg$aggregatedAvgConvergence[CMAES_OCD9_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "black", lwd = 4)

legend("topright", legend = c("Other OCD", "Default CMA-ES", "0.0001; 100"), 
       col = c("black", "red", "cyan"), lty = 1, lwd = 4, cex = 4)
dev.off()

#plot above plots WITHOUT default
png("OCD_Parametrization_withoutDefault.png", width = 1200, height = 1200)
par(mar=c(25,9,5,9))
par(mgp = c(6,2,0))
par(las = 3)
plot(allAverageBest, axes = FALSE, pch = 16, ylim = c(0, CMAES_default_restart_results_agg$aggregatedAvgBest), 
     xlab = "", ylab = "Average best value", cex = 5, cex.lab = 4, 
     type = "h", lwd = 4)
axis(1, at = 1:9, labels = c("0.01; 10", 
                             "0.01; 100",
                             "0.01; 1000",
                             "0.001; 10",
                             "0.001; 100",
                             "0.001; 1000",
                             "0.0001; 10",
                             "0.0001; 100",
                             "0.0001; 1000"), cex.axis = 4)
axis(2, at = c(200000, 700000, 1200000), labels = c("2e05", "7e05", "1.2e06"), cex.axis = 4)
box()

par(new = TRUE)
plot(allAverageRestarts, pch = 16, axes = FALSE, xlab = NA, ylab = NA, cex = 5)
axis(side = 4, cex.axis = 4)
mtext(side = 4, line = 5, "Average restarts", cex = 4)
dev.off()

#apparently varLimit = 0.0001 and nPreGen = 100 yields the best results
#this is number 8

#plot the convergence
png("convergence_OCD_parametrization_withoutDefault.png", height = 1200, width = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(CMAES_OCD1_agg$aggregatedAvgConvergence[CMAES_OCD1_agg$aggregatedAvgConvergence[,1] < 100000,1],
     log(CMAES_OCD1_agg$aggregatedAvgConvergence[CMAES_OCD1_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
     type = "l", ylim = c(lower, upper),
     xlab = "Function evaluations", ylab = "log(average best value + 1)", cex.axis = 4, cex.lab = 4, lwd = 4, col = "black")

#due to large population sizes, 100k FEs can be surpassed
#limit plot to 100k FEs
points(CMAES_OCD2_agg$aggregatedAvgConvergence[CMAES_OCD2_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD2_agg$aggregatedAvgConvergence[CMAES_OCD2_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "black", lwd = 4)

points(CMAES_OCD3_agg$aggregatedAvgConvergence[CMAES_OCD3_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD3_agg$aggregatedAvgConvergence[CMAES_OCD3_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "black", lwd = 4)

points(CMAES_OCD4_agg$aggregatedAvgConvergence[CMAES_OCD4_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD4_agg$aggregatedAvgConvergence[CMAES_OCD4_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "black", lwd = 4)

points(CMAES_OCD5_agg$aggregatedAvgConvergence[CMAES_OCD5_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD5_agg$aggregatedAvgConvergence[CMAES_OCD5_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "black", lwd = 4)

points(CMAES_OCD6_agg$aggregatedAvgConvergence[CMAES_OCD6_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD6_agg$aggregatedAvgConvergence[CMAES_OCD6_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "black", lwd = 4)

points(CMAES_OCD7_agg$aggregatedAvgConvergence[CMAES_OCD7_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD7_agg$aggregatedAvgConvergence[CMAES_OCD7_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "black", lwd = 4)

points(CMAES_OCD8_agg$aggregatedAvgConvergence[CMAES_OCD8_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD8_agg$aggregatedAvgConvergence[CMAES_OCD8_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "cyan", lwd = 4)

points(CMAES_OCD9_agg$aggregatedAvgConvergence[CMAES_OCD9_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD9_agg$aggregatedAvgConvergence[CMAES_OCD9_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "red", lwd = 4)

legend("topright", legend = c("Other OCD", "0.0001; 1000", "0.0001; 100"), 
       col = c("black", "red", "cyan"), lty = 1, lwd = 4, cex = 4)
dev.off()

##############
#compare different performance indicators of OCD
CMAES_OCD_disp = 
  loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./OCD_disp",
                         algorithmName = "CMAES_OCD")
CMAES_OCD_disp_fit = 
  loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./OCD_disp_fit",
                         algorithmName = "CMAES_OCD")
CMAES_OCD_evo = 
  loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./OCD_evo",
                         algorithmName = "CMAES_OCD")
CMAES_OCD_evo_disp = 
  loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./OCD_evo_disp",
                         algorithmName = "CMAES_OCD")
CMAES_OCD_evo_disp_fit = 
  loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./OCD_evo_disp_fit",
                         algorithmName = "CMAES_OCD")
CMAES_OCD_fit = 
  loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./OCD_fit",
                         algorithmName = "CMAES_OCD")
CMAES_OCD_evo_fit = 
  loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./OCD_evo_fit",
                         algorithmName = "CMAES_OCD")

CMAES_OCD_disp_agg = aggregateResults(CMAES_OCD_disp)
CMAES_OCD_disp_fit_agg = aggregateResults(CMAES_OCD_disp_fit)
CMAES_OCD_evo_agg = aggregateResults(CMAES_OCD_evo)
CMAES_OCD_evo_disp_agg = aggregateResults(CMAES_OCD_evo_disp)
CMAES_OCD_evo_disp_fit_agg = aggregateResults(CMAES_OCD_evo_disp_fit)
CMAES_OCD_fit_agg = aggregateResults(CMAES_OCD_fit)
CMAES_OCD_evo_fit_agg = aggregateResults(CMAES_OCD_evo_fit)
#plot the average best results
allAverageBestPI = c(CMAES_OCD_fit_agg$aggregatedAvgBest, 
                     CMAES_OCD_disp_agg$aggregatedAvgBest, 
                     CMAES_OCD_evo_agg$aggregatedAvgBest, 
                     CMAES_OCD_disp_fit_agg$aggregatedAvgBest, 
                     CMAES_OCD_evo_disp_agg$aggregatedAvgBest, 
                     CMAES_OCD_evo_fit_agg$aggregatedAvgBest, 
                     CMAES_OCD_evo_disp_fit_agg$aggregatedAvgBest)

allAverageRestartsPI = c(mean(CMAES_OCD_fit_agg$aggregatedAllRestarts),
                         mean(CMAES_OCD_disp_agg$aggregatedAllRestarts),
                         mean(CMAES_OCD_evo_agg$aggregatedAllRestarts),
                         mean(CMAES_OCD_disp_fit_agg$aggregatedAllRestarts),
                         mean(CMAES_OCD_evo_disp_agg$aggregatedAllRestarts),
                         mean(CMAES_OCD_evo_fit_agg$aggregatedAllRestarts),
                         mean(CMAES_OCD_evo_disp_fit_agg$aggregatedAllRestarts))

#plot average best values
png("OCD_performance_indicators.png", width = 1200, height = 1200)
par(mar=c(25,9,5,9))
par(mgp = c(6,2,0))
par(las = 3)
plot(allAverageBestPI, axes = FALSE, pch = 16, 
     xlab = "", ylab = "Average best value", cex = 5, cex.lab = 4, 
     type = "h", lwd = 4)
axis(1, at = 1:7, labels = c("F", 
                             "D",
                             "E",
                             "F,D",
                             "E,D",
                             "F,E",
                             "F,D,E"), cex.axis = 4)
axis(2, at = c(200000, 1200000, 2200000, 3200000), labels = c("2e05", "1.2e06", "2.2e06", "3.2e06"), cex.axis = 4)
box()

par(new = TRUE)
plot(allAverageRestartsPI, pch = 16, axes = FALSE, xlab = NA, ylab = NA, cex = 5)
axis(side = 4, cex.axis = 4)
mtext(side = 4, line = 5, "Average restarts", cex = 4)
dev.off()
par(las = 1)

#plot the convergence
#determine boundaries for the plot
lower = min(c(log(CMAES_OCD_fit_agg$aggregatedAvgConvergence[CMAES_OCD_fit_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
              log(CMAES_OCD_disp_agg$aggregatedAvgConvergence[CMAES_OCD_disp_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
              log(CMAES_OCD_evo_agg$aggregatedAvgConvergence[CMAES_OCD_evo_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
              log(CMAES_OCD_disp_fit_agg$aggregatedAvgConvergence[CMAES_OCD_disp_fit_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
              log(CMAES_OCD_evo_disp_agg$aggregatedAvgConvergence[CMAES_OCD_evo_disp_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
              log(CMAES_OCD_evo_fit_agg$aggregatedAvgConvergence[CMAES_OCD_evo_fit_agg$aggregatedAvgConvergence[,1] < 100000,2]+1),
              log(CMAES_OCD_evo_disp_fit_agg$aggregatedAvgConvergence[CMAES_OCD_evo_disp_fit_agg$aggregatedAvgConvergence[,1] < 100000,2]+1)))

upper = range(c(log(CMAES_OCD_fit_agg$aggregatedAvgConvergence[CMAES_OCD_fit_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
                log(CMAES_OCD_disp_agg$aggregatedAvgConvergence[CMAES_OCD_disp_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
                log(CMAES_OCD_evo_agg$aggregatedAvgConvergence[CMAES_OCD_evo_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
                log(CMAES_OCD_disp_fit_agg$aggregatedAvgConvergence[CMAES_OCD_disp_fit_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
                log(CMAES_OCD_evo_disp_agg$aggregatedAvgConvergence[CMAES_OCD_evo_disp_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
                log(CMAES_OCD_evo_fit_agg$aggregatedAvgConvergence[CMAES_OCD_evo_fit_agg$aggregatedAvgConvergence[,1] < 100000,2]+1),
                log(CMAES_OCD_evo_disp_fit_agg$aggregatedAvgConvergence[CMAES_OCD_evo_disp_fit_agg$aggregatedAvgConvergence[,1] < 100000,2]+1)), 
              finite = TRUE)[2]
#add margin for the legend
upper = upper + upper/4

#plot the convergence
png("convergence_OCD_performance_indicators.png", height = 1200, width = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(CMAES_OCD_fit_agg$aggregatedAvgConvergence[CMAES_OCD_fit_agg$aggregatedAvgConvergence[,1] < 100000,1],
     log(CMAES_OCD_fit_agg$aggregatedAvgConvergence[CMAES_OCD_fit_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
     type = "l", ylim = c(lower, upper),
     xlab = "Function evaluations", ylab = "log(average best value + 1)", cex.axis = 4, cex.lab = 4, lwd = 4, col = "black")

#due to large population sizes, 100k FEs can be surpassed
#limit plot to 100k FEs
points(CMAES_OCD_disp_agg$aggregatedAvgConvergence[CMAES_OCD_disp_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD_disp_agg$aggregatedAvgConvergence[CMAES_OCD_disp_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "grey", lwd = 4)

points(CMAES_OCD_evo_agg$aggregatedAvgConvergence[CMAES_OCD_evo_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD_evo_agg$aggregatedAvgConvergence[CMAES_OCD_evo_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "green", lwd = 4)

points(CMAES_OCD_disp_fit_agg$aggregatedAvgConvergence[CMAES_OCD_disp_fit_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD_disp_fit_agg$aggregatedAvgConvergence[CMAES_OCD_disp_fit_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "red", lwd = 4)

points(CMAES_OCD_evo_disp_agg$aggregatedAvgConvergence[CMAES_OCD_evo_disp_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD_evo_disp_agg$aggregatedAvgConvergence[CMAES_OCD_evo_disp_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "blue", lwd = 4)

points(CMAES_OCD_evo_fit_agg$aggregatedAvgConvergence[CMAES_OCD_evo_fit_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD_evo_fit_agg$aggregatedAvgConvergence[CMAES_OCD_evo_fit_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "purple", lwd = 4)

points(CMAES_OCD_evo_disp_fit_agg$aggregatedAvgConvergence[CMAES_OCD_evo_disp_fit_agg$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_OCD_evo_disp_fit_agg$aggregatedAvgConvergence[CMAES_OCD_evo_disp_fit_agg$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "cyan", lwd = 4)


legend("topright", legend = c("F", "D", "E", "F,D", "E,D", "F,E", "F,D,E"), 
       col = c("black", "grey", "green", "red", "blue", "purple", "cyan"), lty = 1, lwd = 4, cex = 4)
dev.off()

#############################
#compare ocd and non-ocd results
#check if results are still loaded
#if not load again
if(!exists("CMAES_OCD_evo_disp_agg")) {
  CMAES_OCD_evo_disp = loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./OCD_evo_disp",
                                              algorithmName = "CMAES_OCD")
  CMAES_OCD_evo_disp_agg = aggregateResults(CMAES_OCD_evo_disp)
  
}
CMAES_OCD = CMAES_OCD_evo_disp_agg

if(!exists("CMAES_default_restart_results_agg")) {
  CMAES_default_restart_results = 
    loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./CMAES_default_with_restart",
                           algorithmName = "CMAES")
  CMAES_default_restart_results_agg = aggregateResults(CMAES_default_restart_results)
}
CMAES_Default = CMAES_default_restart_results_agg
if(!exists("RS_results_agg")) {
  RS_results = loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./Random_Search_100000",
                                      algorithmName = "random search")
  RS_results_agg = aggregateResults(RS_results)
}
RS = RS_results_agg
GA_OCD = loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./GA_OCD",
                                algorithmName = "GA")
GA_OCD = aggregateResults(GA_OCD)
if(!exists("GA_results_agg")) {
  GA_results_agg = loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./GA_default",
                                          algorithmName = "GA")
  GA_results_agg = aggregateResults(GA_Default)
}
GA_Default = GA_results_agg

#plot convergence
#plot the convergence
#determine boundaries for the plot
lower = min(c(log(CMAES_OCD$aggregatedAvgConvergence[CMAES_OCD$aggregatedAvgConvergence[,1] < 100000,2]+1), 
              log(CMAES_Default$aggregatedAvgConvergence[CMAES_Default$aggregatedAvgConvergence[,1] < 100000,2]+1), 
              log(RS$aggregatedAvgConvergence[RS$aggregatedAvgConvergence[,1] < 100000,2]+1), 
              log(GA_OCD$aggregatedAvgConvergence[GA_OCD$aggregatedAvgConvergence[,1] < 100000,2]+1), 
              log(GA_Default$aggregatedAvgConvergence[GA_Default$aggregatedAvgConvergence[,1] < 100000,2]+1)))

upper = range(c(log(CMAES_OCD$aggregatedAvgConvergence[CMAES_OCD$aggregatedAvgConvergence[,1] < 100000,2]+1), 
                log(CMAES_Default$aggregatedAvgConvergence[CMAES_Default$aggregatedAvgConvergence[,1] < 100000,2]+1), 
                log(RS$aggregatedAvgConvergence[RS$aggregatedAvgConvergence[,1] < 100000,2]+1), 
                log(GA_OCD$aggregatedAvgConvergence[GA_OCD$aggregatedAvgConvergence[,1] < 100000,2]+1), 
                log(GA_Default$aggregatedAvgConvergence[GA_Default$aggregatedAvgConvergence[,1] < 100000,2]+1)), 
              finite = TRUE)[2]

#add margin for the legend
upper = upper + upper/4

#plot the convergence
png("convergence_OCD_vs_Default.png", height = 1200, width = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(CMAES_OCD$aggregatedAvgConvergence[CMAES_OCD$aggregatedAvgConvergence[,1] < 100000,1],
     log(CMAES_OCD$aggregatedAvgConvergence[CMAES_OCD$aggregatedAvgConvergence[,1] < 100000,2]+1), 
     type = "l", ylim = c(lower, upper),
     xlab = "Function evaluations", ylab = "log(average best value + 1)", cex.axis = 4, cex.lab = 4, lwd = 4, col = "cyan")

#due to large population sizes, 100k FEs can be surpassed
#limit plot to 100k FEs
points(CMAES_Default$aggregatedAvgConvergence[CMAES_Default$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_Default$aggregatedAvgConvergence[CMAES_Default$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "black", lwd = 4)

points(RS$aggregatedAvgConvergence[RS$aggregatedAvgConvergence[,1] < 100000,1],
       log(RS$aggregatedAvgConvergence[RS$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "green", lwd = 4)

points(GA_OCD$aggregatedAvgConvergence[GA_OCD$aggregatedAvgConvergence[,1] < 100000,1],
       log(GA_OCD$aggregatedAvgConvergence[GA_OCD$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "blue", lwd = 4)

points(GA_Default$aggregatedAvgConvergence[GA_Default$aggregatedAvgConvergence[,1] < 100000,1],
       log(GA_Default$aggregatedAvgConvergence[GA_Default$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "red", lwd = 4)


legend("topright", legend = c("CMA-ES OCD", "CMA-ES default", "RandomSearch", "GA OCD", "GA default"), 
       col = c("cyan", "black", "green", "blue", "red"), lty = 1, lwd = 4, cex = 4)
dev.off()

#get average number of restarts
mean(CMAES_OCD$aggregatedAllRestarts) #4.67
mean(CMAES_Default$aggregatedAllRestarts) #3.71
mean(GA_OCD$aggregatedAllRestarts) #3.49


#compare OCD without restarts to default without restarts
if(!exists("CMAES_only_default_results")) {
  CMAES_only_default_results = loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), 
                                                      path = "./CMAES_only_default", algorithmName = "CMAES")
  CMAES_only_default_results = aggregateResults(CMAES_only_default_results)
}
CMAES_default_no_restarts = CMAES_only_default_results
CMAES_OCD_no_restarts = loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), 
                                               path = "./CMAES_OCD_no_restarts", algorithmName = "CMAES_OCD")
CMAES_OCD_no_restarts = aggregateResults(CMAES_OCD_no_restarts)

#define boundaries
lower = min(c(log(CMAES_default_no_restarts$aggregatedAvgConvergence[,2]+1), 
              log(CMAES_OCD_no_restarts$aggregatedAvgConvergence[,2]+1)))

upper = range(c(log(CMAES_default_no_restarts$aggregatedAvgConvergence[,2]+1), 
                log(CMAES_OCD_no_restarts$aggregatedAvgConvergence[,2]+1)), 
              finite = TRUE)[2]

#add margin for the legend
upper = upper + upper/4
#plot the convergence
png("convergence_OCD_vs_Default_no_restarts.png", height = 1200, width = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(CMAES_default_no_restarts$aggregatedAvgConvergence[,1],
     log(CMAES_default_no_restarts$aggregatedAvgConvergence[,2]+1), 
     type = "l", 
     xlab = "Function evaluations", ylab = "log(average best value + 1)", cex.axis = 4, cex.lab = 4, lwd = 4, col = "black")

points(CMAES_OCD_no_restarts$aggregatedAvgConvergence[,1],
       log(CMAES_OCD_no_restarts$aggregatedAvgConvergence[,2]+1), 
       type = "l", col = "cyan", lwd = 4)

legend("topright", legend = c("CMA-ES default", "CMA-ES OCD"), 
       col = c("black", "cyan"), lty = 1, lwd = 4, cex = 4)
dev.off()

#plot active functions
activeFunctionsDefault = getActiveFunctions(results = CMAES_default_no_restarts)
activeFunctionsOCD = getActiveFunctions(results = CMAES_OCD_no_restarts)
png("active_functions_no_restarts.png", height = 1200, width = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot((1:length(activeFunctionsDefault)*100), activeFunctionsDefault, 
     type = "l", 
     xlab = "Function evaluations", ylab = "log(average best value + 1)", cex.axis = 4, cex.lab = 4, lwd = 4, col = "black")

points((1:length(activeFunctionsOCD)*100), activeFunctionsOCD, 
       type = "l", col = "cyan", lwd = 4)

legend("topright", legend = c("CMA-ES default", "CMA-ES OCD"), 
       col = c("black", "cyan"), lty = 1, lwd = 4, cex = 4)
dev.off()

#plot expected FE to reach a certain optimality gap
#get data for cumulative distribution
CMAES_OCD_cumulative_distribution = extractECDFofFunctions(
  CMAES_OCD, fitnessGap = 1)
CMAES_Default_cumulative_distribution = extractECDFofFunctions(
  CMAES_Default, fitnessGap = 1)
RS_cumulative_distribution = extractECDFofFunctions(
  RS, fitnessGap = 1)
GA_OCD_cumulative_distribution = extractECDFofFunctions(
  GA_OCD, fitnessGap = 1)
GA_Default_cumulative_distribution = extractECDFofFunctions(
  GA_Default, fitnessGap = 1)

#limit to only entries below 100000 FEs
CMAES_OCD_cumulative_distribution = 
  CMAES_OCD_cumulative_distribution[CMAES_OCD_cumulative_distribution[,1] < 100000,]
CMAES_Default_cumulative_distribution = 
  CMAES_Default_cumulative_distribution[CMAES_Default_cumulative_distribution[,1] < 100000,]
RS_cumulative_distribution = 
  RS_cumulative_distribution[RS_cumulative_distribution[,1] < 100000,]
GA_OCD_cumulative_distribution = 
  GA_OCD_cumulative_distribution[GA_OCD_cumulative_distribution[,1] < 100000,]
GA_Default_cumulative_distribution = 
  GA_Default_cumulative_distribution[GA_Default_cumulative_distribution[,1] < 100000,]
#add a point at at 100000 FEs so that graph extends to that point
CMAES_OCD_cumulative_distribution = 
  rbind(CMAES_OCD_cumulative_distribution, 
        c(100000, CMAES_OCD_cumulative_distribution[nrow(CMAES_OCD_cumulative_distribution),2]))
CMAES_Default_cumulative_distribution = 
  rbind(CMAES_Default_cumulative_distribution, 
        c(100000, CMAES_Default_cumulative_distribution[nrow(CMAES_Default_cumulative_distribution),2]))
RS_cumulative_distribution = 
  rbind(RS_cumulative_distribution, 
        c(100000, RS_cumulative_distribution[nrow(RS_cumulative_distribution),2]))
GA_OCD_cumulative_distribution = 
  rbind(GA_OCD_cumulative_distribution, 
        c(100000, GA_OCD_cumulative_distribution[nrow(GA_OCD_cumulative_distribution),2]))
GA_Default_cumulative_distribution = 
  rbind(GA_Default_cumulative_distribution, 
        c(100000, GA_Default_cumulative_distribution[nrow(GA_Default_cumulative_distribution),2]))
png("gap1_comparison.png", width = 1200, height = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(CMAES_OCD_cumulative_distribution, col = "cyan", type = "l", xlab = "Function evaluations", 
     ylab = "% of functions solved", cex.axis = 4, cex.lab = 4, lwd = 4, cex = 5, axes = FALSE, ylim = c(0, 1.5))
axis(1, cex.axis = 4)
axis(2, at = c(0.0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 4)
box()
lines(CMAES_Default_cumulative_distribution, col = "black", type = "l", lwd = 4)
lines(RS_cumulative_distribution, col = "green", type = "l", lwd = 4)
lines(GA_OCD_cumulative_distribution, col = "blue", type = "l", lwd = 4)
lines(GA_Default_cumulative_distribution, col = "red", type = "l", lwd = 4)

legend("topright", legend = c("CMA-ES OCD", "CMA-ES default", "RandomSearch", "GA OCD", "GA default"), 
       col = c("cyan", "black", "green", "blue", "red"), lty = 1, lwd = 4, cex = 4)
dev.off()

#repeat for a gap of 0.01
#plot expected FE to reach a certain optimality gap
#get data for cumulative distribution
CMAES_OCD_cumulative_distribution = extractECDFofFunctions(
  CMAES_OCD, fitnessGap = 0.01)
CMAES_Default_cumulative_distribution = extractECDFofFunctions(
  CMAES_Default, fitnessGap = 0.01)
RS_cumulative_distribution = extractECDFofFunctions(
  RS, fitnessGap = 0.01)
GA_OCD_cumulative_distribution = extractECDFofFunctions(
  GA_OCD, fitnessGap = 0.01)
GA_Default_cumulative_distribution = extractECDFofFunctions(
  GA_Default, fitnessGap = 0.01)

#limit to only entries below 100000 FEs
CMAES_OCD_cumulative_distribution = 
  CMAES_OCD_cumulative_distribution[CMAES_OCD_cumulative_distribution[,1] < 100000,]
CMAES_Default_cumulative_distribution = 
  CMAES_Default_cumulative_distribution[CMAES_Default_cumulative_distribution[,1] < 100000,]
RS_cumulative_distribution = 
  RS_cumulative_distribution[RS_cumulative_distribution[,1] < 100000,]
GA_OCD_cumulative_distribution = 
  GA_OCD_cumulative_distribution[GA_OCD_cumulative_distribution[,1] < 100000,]
GA_Default_cumulative_distribution = 
  GA_Default_cumulative_distribution[GA_Default_cumulative_distribution[,1] < 100000,]
#add a point at at 100000 FEs so that graph extends to that point
CMAES_OCD_cumulative_distribution = 
  rbind(CMAES_OCD_cumulative_distribution, 
        c(100000, CMAES_OCD_cumulative_distribution[nrow(CMAES_OCD_cumulative_distribution),2]))
CMAES_Default_cumulative_distribution = 
  rbind(CMAES_Default_cumulative_distribution, 
        c(100000, CMAES_Default_cumulative_distribution[nrow(CMAES_Default_cumulative_distribution),2]))
RS_cumulative_distribution = 
  rbind(RS_cumulative_distribution, 
        c(100000, RS_cumulative_distribution[nrow(RS_cumulative_distribution),2]))
GA_OCD_cumulative_distribution = 
  rbind(GA_OCD_cumulative_distribution, 
        c(100000, GA_OCD_cumulative_distribution[nrow(GA_OCD_cumulative_distribution),2]))
GA_Default_cumulative_distribution = 
  rbind(GA_Default_cumulative_distribution, 
        c(100000, GA_Default_cumulative_distribution[nrow(GA_Default_cumulative_distribution),2]))
png("gap001_comparison.png", width = 1200, height = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(CMAES_OCD_cumulative_distribution, col = "cyan", type = "l", xlab = "Function evaluations", 
     ylab = "% of functions solved", cex.axis = 4, cex.lab = 4, lwd = 4, cex = 5, axes = FALSE, ylim = c(0, 1.5))
axis(1, cex.axis = 4)
axis(2, at = c(0.0, 0.2, 0.4, 0.6, 0.8, 1), cex.axis = 4)
box()
lines(CMAES_Default_cumulative_distribution, col = "black", type = "l", lwd = 4)
lines(RS_cumulative_distribution, col = "green", type = "l", lwd = 4)
lines(GA_OCD_cumulative_distribution, col = "blue", type = "l", lwd = 4)
lines(GA_Default_cumulative_distribution, col = "red", type = "l", lwd = 4)

legend("topright", legend = c("CMA-ES OCD", "CMA-ES default", "RandomSearch", "GA OCD", "GA default"), 
       col = c("cyan", "black", "green", "blue", "red"), lty = 1, lwd = 4, cex = 4)
dev.off()

#plot convergence for another second run
#load results
CMAES_OCD_evo_disp2 = loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./OCD_evo_disp2",
                                            algorithmName = "CMAES_OCD")
CMAES_OCD_evo_disp_agg2 = aggregateResults(CMAES_OCD_evo_disp2)
CMAES_OCD2 = CMAES_OCD_evo_disp_agg2

CMAES_default_restart_results2 = 
  loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./CMAES_default_with_restart2",
                         algorithmName = "CMAES")
CMAES_default_restart_results_agg2 = aggregateResults(CMAES_default_restart_results2)
CMAES_Default2 = CMAES_default_restart_results_agg2
if(!exists("RS_results_agg")) {
  RS_results = loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./Random_Search_100000",
                                      algorithmName = "random search")
  RS_results_agg = aggregateResults(RS_results)
}
RS = RS_results_agg

GA_OCD2 = loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./GA_OCD2",
                                algorithmName = "GA")
GA_OCD2 = aggregateResults(GA_OCD2)

GA_results_agg2 = loadAllResultsParallel(usedFunctions = 1:24, usedDimensions = c(2,5,10,20), path = "./GA_default2",
                                        algorithmName = "GA")
GA_results_agg2 = aggregateResults(GA_Default2)
GA_Default2 = GA_results_agg2

#plot convergence
#plot the convergence
#determine boundaries for the plot
lower = min(c(log(CMAES_OCD2$aggregatedAvgConvergence[CMAES_OCD2$aggregatedAvgConvergence[,1] < 100000,2]+1), 
              log(CMAES_Default2$aggregatedAvgConvergence[CMAES_Default2$aggregatedAvgConvergence[,1] < 100000,2]+1), 
              log(RS$aggregatedAvgConvergence[RS$aggregatedAvgConvergence[,1] < 100000,2]+1), 
              log(GA_OCD2$aggregatedAvgConvergence[GA_OCD2$aggregatedAvgConvergence[,1] < 100000,2]+1), 
              log(GA_Default2$aggregatedAvgConvergence[GA_Default2$aggregatedAvgConvergence[,1] < 100000,2]+1)))

upper = range(c(log(CMAES_OCD2$aggregatedAvgConvergence[CMAES_OCD2$aggregatedAvgConvergence[,1] < 100000,2]+1), 
                log(CMAES_Default2$aggregatedAvgConvergence[CMAES_Default2$aggregatedAvgConvergence[,1] < 100000,2]+1), 
                log(RS$aggregatedAvgConvergence[RS$aggregatedAvgConvergence[,1] < 100000,2]+1), 
                log(GA_OCD2$aggregatedAvgConvergence[GA_OCD2$aggregatedAvgConvergence[,1] < 100000,2]+1), 
                log(GA_Default2$aggregatedAvgConvergence[GA_Default2$aggregatedAvgConvergence[,1] < 100000,2]+1)), 
              finite = TRUE)[2]

#add margin for the legend
upper = upper + upper/4

#plot the convergence
png("convergence_OCD_vs_Default2.png", height = 1200, width = 1200)
par(mar=c(9,9,2,2))
par(mgp = c(6,2,0))
plot(CMAES_OCD2$aggregatedAvgConvergence[CMAES_OCD2$aggregatedAvgConvergence[,1] < 100000,1],
     log(CMAES_OCD2$aggregatedAvgConvergence[CMAES_OCD2$aggregatedAvgConvergence[,1] < 100000,2]+1), 
     type = "l", ylim = c(lower, upper),
     xlab = "Function evaluations", ylab = "log(average best value + 1)", cex.axis = 4, cex.lab = 4, lwd = 4, col = "cyan")

#due to large population sizes, 100k FEs can be surpassed
#limit plot to 100k FEs
points(CMAES_Default2$aggregatedAvgConvergence[CMAES_Default2$aggregatedAvgConvergence[,1] < 100000,1],
       log(CMAES_Default2$aggregatedAvgConvergence[CMAES_Default2$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "black", lwd = 4)

points(RS$aggregatedAvgConvergence[RS$aggregatedAvgConvergence[,1] < 100000,1],
       log(RS$aggregatedAvgConvergence[RS$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "green", lwd = 4)

points(GA_OCD2$aggregatedAvgConvergence[GA_OCD2$aggregatedAvgConvergence[,1] < 100000,1],
       log(GA_OCD2$aggregatedAvgConvergence[GA_OCD2$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "blue", lwd = 4)

points(GA_Default2$aggregatedAvgConvergence[GA_Default2$aggregatedAvgConvergence[,1] < 100000,1],
       log(GA_Default2$aggregatedAvgConvergence[GA_Default2$aggregatedAvgConvergence[,1] < 100000,2]+1), 
       type = "l", col = "red", lwd = 4)


legend("topright", legend = c("CMA-ES OCD", "CMA-ES default", "RandomSearch", "GA OCD", "GA default"), 
       col = c("cyan", "black", "green", "blue", "redd"), lty = 1, lwd = 4, cex = 4)
dev.off()