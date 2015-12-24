#file = "C:/Users/M/Documents/r_seminar/2015-12-21_standard_BBOB_run/CMAES_output_1_1.txt"

################### Andi #######################
file = "/Users/Andreas/Documents/Uni/R/2015-12-23_CMAES_default_run/CMAES_output_1_2.txt"
file1 = "/Users/Andreas/Documents/Uni/R/2015-12-23_CMAES_default_run/CMAES_output_1_5.txt"
################### Andi #######################

#read file
#data = read.table(file, skip = 1, fill = TRUE)

#name columns
#when x1 and x2 are not given
#colnames only for the original BBOB output which we do not use anymore (we use our own framework)
#colnames(data) = c("FE", "fitness_minus_Fopt", "best_fitness_minus_Fopt", 
#              "measured_fitness", "best_measured_fitness")
#when x1 and x2 are given

#colnames(data) = c("FE", "fitness_minus_Fopt", "best_fitness_minus_Fopt", 
#                   "measured_fitness", "best_measured_fitness", "x1", "x2")

#convert everything to integer
#values that are no integers are indicative of a new function run
#dataframe should be separated at these points

if (!"BBmisc" %in% rownames(installed.packages())) install.packages("BBmisc")
require(BBmisc)


######################################################################
#for own output (CMAESr)
readOutput = function(file) {
  data = read.table(file, skip = 1, fill = TRUE)
  
  data = suppressWarnings(apply(data, 2, as.double))
  
  #get separate runs
  #get split points (NAs) and increment run counter accordingly
  data = as.data.frame(cbind(data, run_id = integer(nrow(data))))
  run = 1
  #save all run ids for later use
  allRunIDs = 1
  inBreakPoint = FALSE
  for (i in 1:nrow(data)) {
    data$run_id[i] = run
    if (is.na(data[i,1]) && inBreakPoint == FALSE) {
      run = run + 1
      allRunIDs = c(allRunIDs, run)
      inBreakPoint = TRUE
    }
    if (!is.na(data[i,1]) && inBreakPoint == TRUE) inBreakPoint = FALSE
  }
  #remove all run ids overlap (if there are 20 runs, the above code actually detects 21)
  allRunIDs = allRunIDs[1:(length(allRunIDs)-1)]
  
  #remove NA rows
  data = data[!is.na(data[,1]),]
  
  #clean fitness values
  #due to rounding errors in the cmaesr there might be fitness values of less than zero 
  #(smaller than global optimum), which of course, does not make sense
  data[,3] = ifelse(data[,3] < 0, 0, data[,3])

  #get data for fitness
  
  #get overall best fitness
  overallBest = min(data[,3])
  
  #get average best
  allBest = double()
  for (i in allRunIDs) {
    allBest = c(allBest, min(data[which(data$run_id %in% i), 3]))
  }
  avgBest = mean(allBest)
  
  #get sd
  sdBest = sd(allBest)
  
  #get worst best fitness
  overallWorst = max(allBest)
  
  #get data for iterations
  longestRun = max(data[,1])
  allRuns = integer()
  for (i in allRunIDs) {
    allRuns = c(allRuns, max(data[which(data$run_id %in% i), 1]))
  }
  shortestRun = min(allRuns)
  avgRun = mean(allRuns)
  sdRuns = sd(allRuns)
  
  #get data for function evaluations
  longestRunEval = max(data[,2])
  allRunsEval = double()
  for (i in allRunIDs) {
    allRunsEval = c(allRunsEval, max(data[which(data$run_id %in% i), 2]))
  }
  shortestRunEval = min(allRunsEval)
  avgRunEval = mean(allRunsEval)
  sdRunsEval = sd(allRunsEval)
  
  #get amount of final iterations without improvement
  allStagnations = integer(0)
  for (i in allRunIDs) {
    allStagnations = c(allStagnations, sum(data[which(data$run_id %in% allRunIDs[i]),3] == allBest[i]))
  }
  longestStagnation = max(allStagnations)
  shortestStagnation = min(allStagnations)
  avgStagnation = mean(allStagnations)
  sdStagnations = sd(allStagnations)
  
  #analyze convergence behavior 
  #get average convergence
  #for this purpose pad all runs that are shorter than the longest run with their last best found value
  #this is desired in order to average the convergence over all instances
  allConvergence = NULL
  for (i in allRunIDs) {
    tempRun = data[which(data$run_id %in% allRunIDs[i]),3]
    tempRun = c(tempRun, rep(tempRun[length(tempRun)], times = (longestRun - allRuns[i])))
    allConvergence = as.data.frame(cbind(allConvergence, tempRun))
    colnames(allConvergence)[i] = paste("Run", i, sep = "_")
  }
  avgConvergence = apply(allConvergence, 1, mean)
  
  #format return value
  result = list(allBest = allBest, avgBest = avgBest, overallBest = overallBest, overallWorst = overallWorst,
                sdBest = sdBest, 
                allRuns = allRuns, longestRun = longestRun, shortestRun = shortestRun, avgRun = avgRun, 
                sdRuns = sdRuns, allRunsEval = allRunsEval, longestRunEval = longestRunEval, 
                shortestRunEval = shortestRunEval, avgRunEval = avgRunEval, sdRunsEval = sdRunsEval, 
                allStagnations = allStagnations, longestStagnation = longestStagnation,
                shortestStagnation = shortestStagnation, avgStagnation = avgStagnation, 
                sdStagnations = sdStagnations, allConvergence = allConvergence, avgConvergence = avgConvergence)
  class(result) = "single_bbob_result"
  return(result)
}

aggregateResults = function(allResults) {
  #do some input checks
  if (!is.list(allResults)) stop("input must be of type list")
  for (i in 1:length(allResults)) {
    if(class(allResults[[i]]) != "single_bbob_result") stop("all elements of result list must be of type single_bbob_result")
  }
  
  #aggregate results from input single_bbob_results
  #aggregate best fitness values
  aggregatedAllBest = numeric(0)
  for (i in 1:length(allResults)) {
    aggregatedAllBest = c(aggregatedAllBest, allResults[[i]]$allBest)
  }
  aggregatedAvgBest = mean(aggregatedAllBest)
  aggregatedOverallBest = min(aggregatedAllBest)
  aggregatedOverallWorst = max(aggregatedAllBest)
  aggregatedSDBests = sd(aggregatedAllBest)
  
  #aggregate runtimes
  aggregatedAllRuns = integer(0)
  for (i in 1:length(allResults)) {
    aggregatedAllRuns = c(aggregatedAllRuns, allResults[[i]]$longestRun)
  }
  aggregatedLongestRun = max(aggregatedAllRuns)
  aggregatedShortestRun = min(aggregatedAllRuns)
  aggregatedAvgRun = mean(aggregatedAllRuns)
  aggregatedSDRuns = sd(aggregatedAllRuns)
  
  #aggregate runtimes by function evaluations
  aggregatedAllRunsEval = integer(0)
  for (i in 1:length(allResults)) {
    aggregatedAllRunsEval = c(aggregatedAllRunsEval, allResults[[i]]$longestRunEval)
  }
  aggregatedLongestRunEval = max(aggregatedAllRunsEval)
  aggregatedShortestRunEval = min(aggregatedAllRunsEval)
  aggregatedAvgRunEval = mean(aggregatedAllRunsEval)
  aggregatedSDRunsEval = sd(aggregatedAllRunsEval)
  
  #aggregate stagnation
  aggregatedAllStagnation = integer(0)
  for (i in 1:length(allResults)) {
    aggregatedAllStagnation = c(aggregatedAllStagnation, allResults[[i]]$allStagnations)
  }
  aggregatedLongestStagnation = max(aggregatedAllStagnation)
  aggregatedShortestStagnation = min(aggregatedAllStagnation)
  aggregatedAvgStagnation = mean(aggregatedAllStagnation)
  aggregatedSDStagnation = sd(aggregatedAllStagnation)
  
  #aggregate convergence
  aggregatedAllConvergence = matrix(nrow = aggregatedLongestRun, ncol = length(allResults))

  for (i in 1:length(allResults)) {
    #pad with last value to get the same number of rows for all elements
    paddingVector = rep(allResults[[i]]$avgConvergence[length(allResults[[i]]$avgConvergence)], 
        times = (aggregatedLongestRun-aggregatedAllRuns[i]))
    aggregatedAllConvergence[,i] = c(allResults[[i]]$avgConvergence, paddingVector)
  }
  aggregatedAvgConvergence = apply(aggregatedAllConvergence, 1, mean)
  
  #format return value
  result = list(aggregatedAllBest = aggregatedAllBest, aggregatedAvgBest = aggregatedAvgBest,
                aggregatedOverallBest = aggregatedOverallBest, aggregatedOverallWorst = aggregatedOverallWorst,
                aggregatedSDBests = aggregatedSDBests, aggregatedAllRuns = aggregatedAllRuns, 
                aggregatedLongestRun = aggregatedLongestRun, aggregatedShortestRun = aggregatedShortestRun,
                aggregatedAvgRun = aggregatedAvgRun, aggregatedSDRuns = aggregatedSDRuns, 
                aggregatedAllRunsEval = aggregatedAllRunsEval, aggregatedLongestRunEval = aggregatedLongestRunEval,
                aggregatedShortestRunEval = aggregatedShortestRunEval, aggregatedAvgRunEval = aggregatedAvgRunEval,
                aggregatedSDRunsEval = aggregatedSDRunsEval, aggregatedAllStagnation = aggregatedAllStagnation,
                aggregatedLongestStagnation = aggregatedLongestStagnation, 
                aggregatedShortestStagnation = aggregatedShortestStagnation,
                aggregatedAvgStagnation = aggregatedAvgStagnation,
                aggregatedSDStagnation = aggregatedSDStagnation, 
                aggregatedAllConvergence = aggregatedAllConvergence,
                aggregatedAvgConvergence = aggregatedAvgConvergence)
  return(result)
}



head(aggResult$aggregatedAllConvergence)

allConvergence = aggResult$aggregatedAllConvergence


#returns a cumulative distribution function of the functions that were solved within the desired fitnessGap
#by the amount of function evaluations this took
extractECDFofFunctions = function(results, fitnessGap = 1e-08) {
  allConvergence = results$aggregatedAllConvergence
  #get function evaluations multiplier (since only iterations are logged, these have to be multiplied by FEs)
  feMultiplier = results$aggregatedAvgRunEval/results$aggregatedAvgRun
  thresholds = integer(0)
  for (i in 1:ncol(allConvergence)) {
    if (!length(which(allConvergence[,i]<fitnessGap)) == 0) {
      thresholds = c(thresholds, min(which(allConvergence[,i]<fitnessGap)) * feMultiplier)
    }
  }
  #sort thresholds ascending
  thresholds = sort(thresholds)
  #make % values for cumulative distribution function
  breaks = seq(from = 1/ncol(allConvergence), to = 1, length.out = ncol(allConvergence))
  #remove % values that are not reached (these functions did not reach the desired value)
  breaks = breaks[1:length(thresholds)]
  #add a point (all iterations,max(breaks)) in order to show the stagnation in the plot
  breaks = c(breaks, max(breaks))
  thresholds = c(thresholds, nrow(allConvergence) * feMultiplier)
  #add (0,0) for better plots and return
  return(rbind(c(0,0), cbind(thresholds, breaks)))
}

#loads all results that correspond to the naming conventions used by bbob_custom
loadAllResults = function(usedFunctions, usedDimensions, path) {
  allResults = NULL
  pbar = makeProgressBar(min = 0, max = length(usedFunctions)*length(usedDimensions))
  pbar$set(0)
  for (i in 1:length(usedFunctions)) {
    for (j in 1:length(usedDimensions)) {
      file = paste(path, "/", "CMAES_output_", usedFunctions[i], "_", usedDimensions[j], ".txt", sep = "")
      result = readOutput(file)
      if (is.null(allResults)) allResults = list(result)
      else allResults = c(allResults, list(result))
      pbar$set((i-1)*length(usedDimensions)+j)
    }
  }
  return(allResults)
}


####some testing
ecdfres = extractECDFofFunctions(allConvergence)
plot(ecdfres)
lines(ecdfres)

file1 = "./CMAES_only_default/CMAES_output_1_2.txt"
file2 = "./CMAES_only_default/CMAES_output_24_20.txt"
res1 = readOutput(file1)
res2 = readOutput(file2)
allResults = list(res1, res2)

allConvergence = CMAES_only_default_aggResult$aggregatedAllConvergence
i = 90
fitnessGap = 1e-08
for (i in 1:ncol(allConvergence)) {
  if (!length(which(allConvergence[,i]<fitnessGap)) == 0) {
    thresholds = c(thresholds, min(which(allConvergence[,i]<fitnessGap)) * feMultiplier)
  }
}

CMAES_only_default_results[[96]]$allConvergence[nrow(CMAES_only_default_results[[96]]$allConvergence),]
CMAES_only_default_results[[1]]$allConvergence[nrow(CMAES_only_default_results[[1]]$allConvergence),]
CMAES_only_default_aggResult$aggregatedAllConvergence[nrow(CMAES_only_default_aggResult$aggregatedAllConvergence),]


###### testing Andi
fitnessGap = 1e-08
res <- readOutput(file)
res2 <- readOutput(file1)
allresults <- list(res, res2)
CMAES_only_default_aggResult <- aggregateResults(allresults)
allConvergence <- CMAES_only_default_aggResult$aggregatedAllConvergence
ecdfres <- extractECDFofFunctions(CMAES_only_default_aggResult, fitnessGap)
plot(ecdfres)
res$allBest
res$avgBest
sum(res$allBest)/length(res$allRuns)
res$allRuns
res$allRunsEval
res$allBest
