#benchmarks the cpu times of cmaes, rcma and cmaesr packages
#findings: 
#1) Using a customized version of the bbob testbed (purely R code) is much faster than using the BBOB package 
#(factor 340) for CMAES (less significant under MAC)
#this is because BBOB uses some more detailled event logging (probably) and some additonal stuff in C-code
#2) rCMA is much slower than CMAES (factor 40) and CMAESr, CMAES is faster than CMAESr (but only factor 2)
if (!"cmaes" %in% rownames(installed.packages())) install.packages("cmaes")
if (!"bbob" %in% rownames(installed.packages())) install.packages("bbob")
if (!"rCMA" %in% rownames(installed.packages())) install.packages("rCMA")
if (!"rJava" %in% rownames(installed.packages())) install.packages("rJava")
if (!"microbenchmark" %in% rownames(installed.packages())) install.packages("microbenchmark")
if (!"devtools" %in% rownames(installed.packages())) install.packages("devtools")
if (!"smoof" %in% rownames(installed.packages())) install.packages("smoof")

require(cmaes)
require(bbob)
require(rCMA)
require(rJava)
require(microbenchmark)
require(smoof)
require(devtools)
require(cmaesr)
install_github(repo = "MarcusCramer91/cmaesr")

#customized bbob version that does not require any c-code and should be much faster 
#than the framework provided by the bbob package (this version is without result printing)
bbob_custom2 = function(optimizer, algorithm_id, data_directory, dimensions = c(2, 3, 5, 10, 20, 40), 
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
        result = optimizer(dimensions, instances, function_ids, maxit, maxFE, stopFitness, path = data_directory)
        pbar$set(currentRun)
        currentRun = currentRun + 1
      }
    }
  }
}

#internal stopping criterion only works if budget <= 100 * Dimension^2 * population
optimizerCMAES = function(dimensions, instances, function_ids, maxit, maxFE, stopFitness, path) {
  fn = makeBBOBFunction(dimensions, function_ids, instances)
  cma_es(par = runif(dimensions, -5, 5), fn = fun, lower = -5, upper = 5, 
         control = list(maxit = maxit))
}

benchmarkResultCMAES = microbenchmark(bbob_custom2(optimizerCMAES, "cmaes", "cmaes_test", dimensions = 2, 
                                                  instances = 1, function_ids = 24, maxit = 10), times = 500L)

#do the same testing with the original bbob framework
bbo_benchmark_original = function (optimizer, algorithm_id, data_directory, dimensions = c(2, 3, 5, 10, 20, 40), 
                                   instances = c(1:5, 41:50), replications = 1L, budget = 1e+07, noisy = FALSE, function_ids) 
{
  nruns <- length(dimensions) * length(function_ids)
  current_run <- 1
  pbar <- makeProgressBar(min = 1, max = nruns)
  for (dimension in sort(dimensions, decreasing = FALSE)) {
    for (function_id in function_ids) {
      pbar$set(current_run)
      current_run <- current_run + 1
      for (instance_id in instances) {
        for (run in 1:replications) {
          bbob_setup_experiment(algorithm_id, data_directory, 
                                function_id, instance_id, dimension)
          while (bbob_n_evaluations() < budget && bbob_optimality_gap() > 
                 1e-08) {
            if (bbob_n_evaluations() > 0) 
              bbob_log_restart("independent restart")
            par <- runif(dimension, -4, 4)
            optimizer(par, bbob_f_eval, lower = rep(-5, dimension), upper = rep(5, dimension), 
                      budget)
          }
          bbob_end_experiment()
        }
      }
    }
  }
}

optimizerCMAES = function(par, fun, lower, upper, max_eval) {
  cma_es(par = par, fn = fun, lower = lower, upper = upper, control = list(maxit = max_eval))
}

benchmarkResultCMAESOriginalBBOB = microbenchmark(bbo_benchmark_original(optimizerCMAES, "cmaes", "cmaes_test", 
                                                           budget = 10, function_ids = 24, dimensions = 2, 
                                                           instances = 1), times = 500L)


#############################################
#do the same for another implementation
#for the two dimensional case
optimizerRCMA = function(dimension, instances, function_ids, maxit, maxFE, stopFitness, path) {
  fun = makeBBOBFunction(dimension, function_ids, instances)
  cma = cmaNew()
  cmaSetDimension(cma, 2)
  #Define according to cmaes (100*D^2)
  cmaSetStopMaxFunEvals(cma, maxit * 10)
  #Set population size to 10 in order to know the runtime
  cmaSetPopulationSize(cma, 10)
  cmaInit(cma)
  res1 = cmaOptimDP(cma, fitFunc = fun)
}

benchmarkResultRCMA = microbenchmark(bbob_custom2(optimizerRCMA, "cmaes", "cmaes_test", dimensions = 2, 
                                                  instances = 1, function_ids = 24, maxit = 10), times = 500L)

############################################
#do the same for Bossek version


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

benchmarkResultCMAESr = microbenchmark(bbob_custom2(optimizer, "cmaes", "C:/Users/Marcus/Desktop/monitortest", 
                                                   dimensions = 2, instances = 1, function_ids = 24, 
                                                   maxit = 10), times = 500L)

mean(benchmarkResultCMAES$time)
#6789752
mean(benchmarkResultCMAESOriginalBBOB$time)
#2311153348
mean(benchmarkResultRCMA$time)
#282503096
mean(benchmarkResultCMAESr$time)
#11560471
