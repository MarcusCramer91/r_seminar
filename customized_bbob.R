#only non-noisy functions
bbob_custom = function(optimizer, algorithm_id, data_directory, dimensions = c(2, 3, 5, 10, 20, 40), 
                       instances = c(1:5, 41:50), function_ids = NULL, maxit = NULL, stopFitness = NULL, maxFE = NULL) {
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
        pbar$set(currentRun)
        currentRun = currentRun + 1
        optimizer(dimensions, instances, function_ids, maxit, maxFE, stopFitness)
      }
    }
  }
}

optimizer = function(dimension, instance, function_id, maxit, maxFE, stopFitness) {
  fun = makeBBOBFunction(dimension, function_id, instance)
  #use maxFE before maxit/stopfitness if both are not null
  condition1 = NULL
  condition2 = NULL
  #use maxit before stopfitness
  if (!is.null(maxFE)) condition1 = stopOnMaxEvals(maxFE)
  else if (!is.null(maxit)) condition1 = stopOnMaxIters(maxit)
  #stopFitness can only be used in combination with either maxFE or maxit (caught error)
  if (!is.null(stopFitness)) {
    optValue = getGlobalOptimum(fun)
    condition2 = stopOnOptValue(optValue, stopFitness)
    cmaes(fun, control = list (stop.ons = c(condition1, condition2)))
  }
  else if (!is.null(condition1)) cmaes(fun, control = list (stop.ons = condition1))
  #use default if no stopping criterion is defined
  else cmaes(fun)
}

bbob_custom(optimizer, "cmaes", "test", dimensions = 2, instances = 1, function_ids = 24, 
            maxit = NULL, stopFitness = NULL, maxFE = NULL)