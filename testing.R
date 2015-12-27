if (!"devtools" %in% rownames(installed.packages())) install.packages("devtools")
if (!"smoof" %in% rownames(installed.packages())) install.packages("smoof")

require(smoof)
require(devtools)
require(cmaesr)
install_github(repo = "MarcusCramer91/cmaesr")


fn = makeRosenbrockFunction(dimensions = 2L)
res = cmaes(
  fn,
  monitor = makeSimpleMonitor(),
  control = list(
    sigma = 1.5, lambda = 10,
    stop.ons = list(stopOnOCD(10, 18, 1000))
  )
)
getGlobalOptimum(fn)

fn = makeBBOBFunction(5, 1, 4)


res = suppressWarnings(cmaes(
  fn,
  monitor = makeSimpleMonitor(),
  control = list(
    sigma = 1.5, lambda = 30,
    stop.ons = c(list(stopOnMaxIters(10000),stopOnOptValue(getGlobalOptimum(fn)$value, 1e-04), stopOnOCD(0.00001,1000)))
)))

res = suppressWarnings(cmaes(
  fn,
  monitor = makeSimpleMonitor(),
  control = list(
    sigma = 1.5, lambda = 30,
    max.restarts = 10000,
    restart.multiplier = 1, 
    restart.triggers = c("tolX", "noEffectAxis", "noEffectCoord", "conditionCov", "indefCovMat"),
    stop.ons = c(list(stopOnMaxIters(1000),stopOnOptValue(getGlobalOptimum(fn)$value, 1e-04)), getDefaultStoppingConditions())
  )))


opt = getGlobalOptimum(fn)$value
opt

res = suppressWarnings(cmaes(
  fn,
  monitor = makeSimpleMonitor(),
  control = list(
    sigma = 1.5, lambda = 30,
    restart.triggers = c("OCD"),
    max.restarts = 1000,
    restart.multiplier = 1,
    stop.ons = c(list(stopOnOptValue(getGlobalOptimum(fn)$value, 1e-08), stopOnOCD(0.0001, 100)))
  )))


res = cmaes(
  fn,
  monitor = makeSimpleMonitor(),
  control = list(
    sigma = 1.5, lambda = 30,
    stop.ons = c(list(stopOnMaxIters(100), stopOnMaxIters(400)))
  ))



getGlobalOptimum(fn)


fn = makeRosenbrockFunction(dimensions = 2L)
res = cmaes(
  fn,
  monitor = makeSimpleMonitor(),
  control = list(
    sigma = 1.5, lambda = 10,
    stop.ons = getDefaultStoppingConditions()
  )
)

cmaes






points <- cbind(res$allBest, res$allRunsEval)
points
plot(points, bg="red", pch=21)
lines(points)


optimizerCMAES = function(dimension, instance, function_id, maxit, maxFE, stopFitness, path, OCD = FALSE,
                          debug.logging = FALSE) {
  if(!grepl(":/", path, fixed = TRUE)) path = file.path(getwd(), path)
  fun = makeBBOBFunction(dimension = dimension, fid = function_id, iid = instance)
  #create .txt creating monitor
  Fopt = getGlobalOptimum(fun)$value
  monitor = makeTXTMonitor(max.params = 4L, path, Fopt, function_id, dimension, instance)
  #use maxFE before maxit/stopfitness if both are not null
  condition1 = NULL
  condition2 = NULL
  if (OCD == TRUE) {
    condition1 = stopOnOCD(0.00001, 18, 1000)
    result = cmaes(fun, monitor = monitor, control = list (stop.ons = list(condition1)), 
                   debug.logging = debug.logging)
    return (result)
  }
  #use maxit before stopfitness
  if (!is.null(maxFE)) condition1 = stopOnMaxEvals(maxFE)
  else if (!is.null(maxit)) condition1 = stopOnMaxIters(maxit)
  #stopFitness can only be used in combination with either maxFE or maxit (caught error)
  result = NULL
  if (!is.null(stopFitness)) {
    optValue = getGlobalOptimum(fun)$value
    condition2 = stopOnOptValue(optValue, stopFitness)
    result = cmaes(fun, monitor = monitor, control = list (stop.ons = list(condition1, condition2)), 
                   debug.logging = debug.logging)
  }
  else if (!is.null(condition1)) {
    result = cmaes(fun, monitor = monitor, control = list (stop.ons = list(condition1)), 
                   debug.logging = debug.logging)
  }
  #use default if no stopping criterion is defined
  else result = cmaes(fun, monitor = monitor, debug.logging = debug.logging)
  return(result)
}




bbob_custom = function(optimizer, algorithm_id, data_directory, dimensions = c(2, 3, 5, 10, 20, 40), 
                       instances = c(1:5, 41:50), function_ids = NULL, maxit = NULL, stopFitness = NULL, 
                       maxFE = NULL, OCD = FALSE, debug.logging = FALSE) {
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
                           OCD = OCD, debug.logging = debug.logging)
        pbar$set(currentRun)
        currentRun = currentRun + 1
        outputFile = file.path(data_directory, paste("CMAES_output_", function_ids[i], "_", dimensions[j], 
                                                     ".txt", sep = ""))
        write(result, file = outputFile, append = TRUE)
      }
    }
  }
}

