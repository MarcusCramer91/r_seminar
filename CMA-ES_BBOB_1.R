optimizer = function(par, fun, lower, upper, max_eval, stopFitness) {
  #N = number of dimensions
  #lambda = 4 + floor(3*log(N)): for two dimensions: 6, for 20 dimensions: 12
  #max.iter = 100 * N^2
  #mu = floor(lambda / 2): for two dimensions: 3, for 20 dimensions: 6
  mu = 10
  lambda = 10
  maxit = max_eval / mu
  if (is.null(stopFitness)) cma_es(par = par, fn = fun, lower = lower, upper = upper, 
                                   control = list(mu = mu, lambda = lambda, maxit = maxit))
  else cma_es(par = par, fn = fun, lower = lower, upper = upper, 
              control = list(mu = mu, lambda = lambda, stopfitness = stopFitness))
}

optimizer_default = function(par, fun, lower, upper, max_eval, stopFitness) {
  cma_es(par = par, fn = fun, lower = lower, upper = upper)
}

budget = 1000
bbo_benchmark(optimizer, "cmaes", "cmaes_test01", budget = 100, dimensions = 2, instances = 1)
bbo_benchmark_custom(optimizer, "cmaes", "cmaes_test01", budget = 100, dimensions = 2, instances = 1, 
                     function_ids = 1, stopFitness = 0.005)
bbo_benchmark_custom(optimizer_default, "cmaes", "cmaes_test_default_settings", budget = 100)

#some explanations regarding the function
#budget limits per iteration, meaning that if we have a population of 10 and a budget of 10, at max 100 function evaluations are conducted
#function IDs corresponds to the functions to be used to assess the algorithm, 1-24 being non-noisy and 101-124 being noisy
#replications probably means complete reruns of the whole function
#instances - no idea
bbo_benchmark_custom = function (optimizer, algorithm_id, data_directory, dimensions = c(2, 3, 5, 10, 20, 40), 
                                 instances = c(1:5, 41:50), replications = 1L, 
                                 budget = NULL, noisy = FALSE, function_ids = NULL, stopFitness = NULL) {
  if (is.null(function_ids)) {
    function_ids <- if (noisy) 
      101:124
    else 1:24
  }
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
          #while loop not wanted
          #while (bbob_n_evaluations() < budget && bbob_optimality_gap() > 1e-08) { 
            if (bbob_n_evaluations() > 0) 
              bbob_log_restart("independent restart")
            par <- runif(dimension, -4, 4)
            
            firstEval = bbob_f_eval(par) #first evaluation, get optim value
            optimGap = bbob_optimality_gap()
            optimValue = firstEval - optimGap
            print(firstEval)
            print(optimGap)
            print(optimValue)
            print(optimValue + stopFitness)
            
            optimizer(par, bbob_f_eval, lower = rep(-5, dimension), upper = rep(5, dimension), 
                      budget - bbob_n_evaluations(), optimValue + stopFitness)
          }
          bbob_end_experiment()
        #}
      }
    }
  }
}


bbo_benchmark_custom(optimizer, "cmaes", "cmaes_test", budget = 10, function_ids = 24, dimensions = 1)

#internal stopping criterion only works if budget <= 100 * Dimension^2 * population
optimizer2 = function(par, fun, lower, upper, max_eval) {
  cma_es(par = par, fn = fun, lower = lower, upper = upper, control = list(mu = 15, lambda = 15))
}


bbo_benchmark_custom(optimizer2, "cmaes", "cmaes_test", budget = 10, function_ids = 1)

optimizer3 = function(dimension, function_id, instance_id, par, fun, lower, upper, max_eval) {
  #requires manual initialization of the functions
  #Sphere function
  fun = makeBBOBFunction(dimension, function_id, instance_id)
  cmaes(objective.fun = fun, start.point = par)
}


bbo_benchmark_custom2(optimizer3, "cmaes", "cmaes_test", budget = 10, function_ids = 24, dimension = 2, instances = 1)
