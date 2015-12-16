if (!"cmaes" %in% rownames(installed.packages())) install.packages("cmaes")
if (!"bbob" %in% rownames(installed.packages())) install.packages("bbob")
if (!"rCMA" %in% rownames(installed.packages())) install.packages("rCMA")
if (!"rJava" %in% rownames(installed.packages())) install.packages("rJava")
if (!"microbenchmark" %in% rownames(installed.packages())) install.packages("microbenchmark")

require(cmaes)
require(bbob)
require(rCMA)
require(rJava)
require(microbenchmark)


#internal stopping criterion only works if budget <= 100 * Dimension^2 * population
optimizerCMAES = function(par, fun, lower, upper, max_eval) {
  cma_es(par = par, fn = fun, lower = lower, upper = upper, control = list(mu = 15, lambda = 15))
}

benchmarkResultCMAES = microbenchmark(bbo_benchmark_custom(optimizerCMAES, "cmaes", "cmaes_test", budget = 10, function_ids = 24, 
                                                      dimensions = 2, instances = 1), times = 100L)

mean(benchmarkResultCMAES$time)

#############################################
#do the same for another implementation
#for the two dimensional case
optimizerRCMA = function(par, fun, lower, upper, max_eval) {
  cma = cmaNew()
  cmaSetDimension(cma, 2)
  #Define according to cmaes (100*D^2)
  cmaSetStopMaxFunEvals(cma, 100*2^2)
  #Set population size like in cmaes (default = 15)
  cmaSetPopulationSize(cma, 15)
  cmaInit(cma)
  res1 = cmaOptimDP(cma, fitFunc = fun)
}

benchmarkResultRCMA = microbenchmark(bbo_benchmark_custom(optimizerRCMA, "cmaes", "cmaes_test", budget = 10, function_ids = 24, 
                                                      dimensions = 2, instances = 1), times = 100L)

mean(benchmarkResultRCMA$time)
