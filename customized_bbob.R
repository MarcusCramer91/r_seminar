if (!"devtools" %in% rownames(installed.packages())) install.packages("devtools")
install_github(repo = "MarcusCramer91/cmaesr")
require(cmaesr)
require(devtools)


#only non-noisy functions
bbob_custom = function(optimizer, algorithm_id, data_directory, dimensions = c(2, 3, 5, 10, 20, 40), 
                       instances = c(1:5, 41:50), function_ids = NULL, maxit = NULL, stopFitness = NULL, maxFE = NULL) {
  dir.create(data_directory, showWarnings = FALSE)
  dimensions = sort(dimensions, decreasing = FALSE)
  if (is.null(function_ids)) {
    function_ids = 1:24
  }
  if (is.null(c(maxit, maxFE)) && !is.null(stopFitness)) error("To ensure termination, stopFitness must be combined with
                                                               either maxit or maxFE")
  nruns = length(function_ids)*length(dimensions)*length(instances)
  currentRun = 1
  pbar = makeProgressBar(min = 1, max = nruns)
  for (i in 1:length(function_ids)) {
    for (j in 1:length(dimensions)) {
      for (k in 1:length(instances)) {
        print(data_directory)
        result = optimizer(dimensions, instances, function_ids, maxit, maxFE, stopFitness, path = data_directory)
        pbar$set(currentRun)
        currentRun = currentRun + 1
        outputFile = file.path(data_directory, paste("CMAES_output_", i, "_", j, ".txt", sep = ""))
        write(result, file = outputFile, append = TRUE)
      }
    }
  }
}

optimizer = function(dimension, instance, function_id, maxit, maxFE, stopFitness, path) {
  if(!grepl(":/", path, fixed = TRUE)) path = file.path(getwd(), path)
  fun = makeBBOBFunction(dimension, function_id, instance)
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
    optValue = getGlobalOptimum(fun)
    condition2 = stopOnOptValue(optValue, stopFitness)
    result = cmaes(fun, monitor = monitor, control = list (stop.ons = list(condition1, condition2)))
  }
  else if (!is.null(condition1)) {
    result = cmaes(fun, monitor = monitor, control = list (stop.ons = list(condition1)))
  }
  #use default if no stopping criterion is defined
  else result = cmaes(fun, monitor = monitor)
  return(result)
}


bbob_custom(optimizer, "cmaes", "C:/Users/Marcus/Desktop/monitortest", dimensions = 2, instances = 1, function_ids = 1, 
            maxit = NULL, stopFitness = NULL, maxFE = NULL)


