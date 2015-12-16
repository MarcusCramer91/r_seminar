library(cmaes)
library(bbob)
optimizer = function(par, fun, lower, upper, max_eval) {
    cma_es(par = par, fn = fun, lower = lower, upper = upper, control = list(maxit = max_eval))
}

budget = 1000
bbo_benchmark(optimizer, "cmaes", "cmaes_results", budget = budget)

#some explanations regarding the function
#budget limits per iteration, meaning that if we have a population of 10 and a budget of 10, at max 100 function evaluations are conducted
#function IDs corresponds to the functions to be used to assess the algorithm, 1-24 being non-noisy and 101-124 being noisy
#replications probably means complete reruns of the whole function
#instances - no idea
bbo_benchmark_custom = function (optimizer, algorithm_id, data_directory, dimensions = c(2, 3, 5, 10, 20, 40), instances = c(1:5, 41:50)
                                 , replications = 1L, budget = 1e+07, noisy = FALSE, function_ids = NULL) {
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
          while (bbob_n_evaluations() < budget && bbob_optimality_gap() > 1e-08) {
            if (bbob_n_evaluations() > 0) 
              bbob_log_restart("independent restart")
            par <- runif(dimension, -4, 4)
            optimizer(par, bbob_f_eval, lower = rep(-5, dimension), upper = rep(5, dimension), 
                      budget - bbob_n_evaluations())
          }
          bbob_end_experiment()
        }
      }
    }
  }
}


bbo_benchmark_custom(optimizer, "cmaes", "cmaes_test", budget = 10, function_ids = 24)

#internal stopping criterion only works if budget <= 100 * Dimension^2 * population
optimizer2 = function(par, fun, lower, upper, max_eval) {
  cma_es(par = par, fn = fun, lower = lower, upper = upper, control = list(mu = 10, lambda = 10))
}


bbo_benchmark_custom(optimizer2, "cmaes", "cmaes_test", budget = 1000, function_ids = 24)
