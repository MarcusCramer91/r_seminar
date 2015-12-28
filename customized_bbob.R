#if (!"devtools" %in% rownames(installed.packages())) install.packages("devtools")
if (!"snow" %in% rownames(installed.packages())) install.packages("snow")
if (!"parallel" %in% rownames(installed.packages())) install.packages("parallel")
if (!"smoof" %in% rownames(installed.packages())) install.packages("smoof")
if (!"BBmisc" %in% rownames(installed.packages())) install.packages("BBmisc")
#require(devtools)
#install_github(repo = "MarcusCramer91/cmaesr")
#require(cmaesr)
require(BBMisc)
require(snow)
require(parallel)
require(smoof)
source("./cmaes/cmaes.R")

#only non-noisy functions
bbob_custom = function(optimizer, algorithm_id, data_directory, dimensions = c(2, 3, 5, 10, 20, 40), 
                       instances = c(1:5, 41:50), function_ids = NULL, maxit = NULL, stopFitness = NULL, 
                       maxFE = NULL, OCD = FALSE, debug.logging = FALSE, max_restarts = 0, 
                       restart_multiplier = 1, restart_triggers = character(0)) {
  write(paste("Functions:", function_ids), file = "bbob_calls.txt", append = TRUE)
  write(paste("Dimensions:", dimensions), file = "bbob_calls.txt", append = TRUE)
  write(paste("Instances:", instances), file = "bbob_calls.txt", append = TRUE)
  write("================================", file = "bbob_calls.txt", append = TRUE)
  data_directory = paste(Sys.Date(), data_directory, sep = "_")
  dir.create(data_directory, showWarnings = FALSE)
  dimensions = sort(dimensions, decreasing = FALSE)
  if (is.null(function_ids)) {
    function_ids = 1:24
  }
  if (is.null(c(maxit, maxFE)) && !is.null(stopFitness)) stop("To ensure termination, stopFitness must be combined with either maxit or maxFE")
  nruns = length(function_ids)*length(dimensions)*length(instances)
  currentRun = 1
  pbar = makeProgressBar(min = 1, max = nruns)
  for (i in 1:length(function_ids)) {
    for (j in 1:length(dimensions)) {
      for (k in 1:length(instances)) {
        print(paste("Function:", function_ids[i], ",instance:", instances[k], ",dimensions:", dimensions[j]))
        result = optimizer(dimension = dimensions[j], instance = instances[k], function_id = function_ids[i], 
                           maxit = maxit, maxFE = maxFE, stopFitness = stopFitness, path = data_directory, 
                           OCD = OCD, debug.logging = debug.logging, max_restarts, restart_multiplier,
                           restart_triggers)
        pbar$set(currentRun)
        currentRun = currentRun + 1
        outputFile = file.path(data_directory, paste(algorithm_id, "_output_", function_ids[i], "_", dimensions[j], 
                                                     ".txt", sep = ""))
        write(result, file = outputFile, append = TRUE)
      }
    }
  }
}

optimizerCMAES = function(dimension, instance, function_id, maxit, maxFE, stopFitness, path, OCD = FALSE,
                          debug.logging = FALSE, max_restarts, restart_multiplier, restart_triggers) {
  if(!grepl(":/", path, fixed = TRUE)) path = file.path(getwd(), path)
  fun = makeBBOBFunction(dimension = dimension, fid = function_id, iid = instance)
  #create .txt creating monitor
  Fopt = getGlobalOptimum(fun)$value
  monitor = makeTXTMonitor(max.params = 4L, path, Fopt, function_id, dimension, instance)
  #use maxFE before maxit/stopfitness if both are not null
  condition1 = NULL
  condition2 = NULL
  #use maxit before stopfitness
  if (!is.null(maxFE)) condition1 = stopOnMaxEvals(maxFE)
  else if (!is.null(maxit)) condition1 = stopOnMaxIters(maxit)
  #stopFitness can only be used in combination with either maxFE or maxit (caught error)
  result = NULL
  if (!is.null(stopFitness)) {
    optValue = getGlobalOptimum(fun)$value
    condition2 = stopOnOptValue(optValue, stopFitness)
    result = cmaes_custom(fun, monitor = monitor, debug.logging = debug.logging,
                   control = list (stop.ons = c(list(condition1, condition2),
                                                           getDefaultStoppingConditions()), 
                                                           max.restarts = max_restarts,
                                                           restart.triggers = restart_triggers,
                                                           restart.multiplier = restart_multiplier))
  }
  else if (!is.null(condition1)) {
    result = cmaes_custom(fun, monitor = monitor, debug.logging = debug.logging, 
                   control = list (stop.ons = c(list(condition1), 
                                                           getDefaultStoppingConditions()),
                                                           max.restarts = max_restarts,
                                                           restart.triggers = restart_triggers,
                                                           restart.multiplier = restart_multiplier))
    result = cmaes_custom(fun, monitor = monitor, control = list (stop.ons = list(condition1, condition2)), 
                   debug.logging = debug.logging)
  }
  #use default if no stopping criterion is defined
  else result = cmaes(fun, monitor = monitor, debug.logging = debug.logging)
  return(result)
}

optimizerRS = function(dimension, instance, function_id, maxit, maxFE, stopFitness, path, OCD = FALSE,
                       debug.logging = FALSE, max_restarts = 0, 
                       restart_multiplier = 1, restart_triggers = character(0)) {
  fun = makeBBOBFunction(dimension = dimension, fid = function_id, iid = instance)
  result = random_search(fun, maxFE)
  return(result)
}

optimizerGA = function(dimension, instance, function_id, maxit, maxFE, stopFitness, path, OCD = FALSE,
                       debug.logging = FALSE, max_restarts = 0, 
                       restart_multiplier = 1, restart_triggers = character(0)) {
  #set population size to recommended (see R docu) size of 10 * dimension
  npop = dimension * 10
  #if maxFE is specified, convert to maxit
  if (!is.null(maxFE) && is.null(maxit)) maxit = maxFE / npop
  # set value to be reached to global optimum + stopFitness
  fun = makeBBOBFunction(dimension = dimension, fid = function_id, iid = instance)
  result = rbga(popSize = dimension * 10, iters = maxit, evalFunc = fun, stringMin = rep(-5, dimension), 
                stringMax = rep(5, dimension))
  return(result)
}

#wrapper function for bbob_custom that parallelizes it
#disables the progressbar so check the output files to see how far the algorithm has gottenhh
bbob_custom_parallel = function(optimizer, algorithm_id, data_directory, dimensions = c(2, 3, 5, 10, 20, 40), 
                                instances = c(1:5, 41:50), function_ids = NULL, maxit = NULL, stopFitness = NULL, 
                                maxFE = NULL, OCD = FALSE, debug.logging = FALSE, max_restarts = 0, 
                                restart_multiplier = 1, restart_triggers = character(0)) {
  nCores = detectCores()
  cluster = snow:::makeCluster(nCores, type = "SOCK")
  #export relevant libraries + functions to the clusters
  snow:::clusterCall(cluster, function() require(cmaesr))
  snow:::clusterCall(cluster, function() require(bbob))
  #export all environment functions
  ex = Filter(function(x) is.function(get(x, .GlobalEnv)), ls(.GlobalEnv))
  snow:::clusterExport(cluster, ex)
  snow:::clusterApply(cl = cluster, x = function_ids, fun = function(x) bbob_custom(optimizer = optimizer, 
                                                                  algorithm_id = algorithm_id, 
                                                                  data_directory = data_directory, 
                                                                  dimensions = dimensions,
                                                                  instances = instances,
                                                                  function_ids = x,
                                                                  maxit = maxit,
                                                                  stopFitness = stopFitness,
                                                                  maxFE = maxFE,
                                                                  OCD = OCD, 
                                                                  debug.logging = debug.logging,
                                                                  max_restarts = max_restarts, 
                                                                  restart_multiplier = restart_multiplier, 
                                                                  restart_triggers = restart_triggers))
  stopCluster(cluster)
}
