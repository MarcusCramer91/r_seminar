#default run of CMAES with only default stopping criteria
suppressWarnings(bbob_custom(optimizerCMAES, "cmaes", "CMAES_default_run", 
                             maxit = NULL, stopFitness = NULL, maxFE = NULL, 
                             function_ids = 1, instances = 1:15, dimensions = c(5)))

#run of random search with 100000 function evaluations
#this result is used for comparison with simple random search
#since results are purely random anyway, only one instance is run
bbob_custom(optimizerRS, "random search", "Random_Search_100000", function_ids = 1:24, instances = 1,
            dimensions = c(2,5,10,20), maxFE = 100000)

#use this run to show the advantages of multiple restarts with the same settings on the same function
#use a difficult function like 10 for this
suppressWarnings(bbob_custom(optimizerCMAES, "cmaes", "restart_test_run1", 
                             maxit = NULL, stopFitness = NULL, maxFE = 50000, 
                             function_ids = 10, instances = 1, dimensions = 20))
suppressWarnings(bbob_custom(optimizerCMAES, "cmaes", "restart_test_run2", 
                             maxit = NULL, stopFitness = NULL, maxFE = 50000, 
                             function_ids = 10, instances = 1, dimensions = 20))
suppressWarnings(bbob_custom(optimizerCMAES, "cmaes", "restart_test_run3", 
                             maxit = NULL, stopFitness = NULL, maxFE = 50000, 
                             function_ids = 10, instances = 1, dimensions = 20))
suppressWarnings(bbob_custom(optimizerCMAES, "cmaes", "restart_test_run4", 
                             maxit = NULL, stopFitness = NULL, maxFE = 50000, 
                             function_ids = 10, instances = 1, dimensions = 20))
suppressWarnings(bbob_custom(optimizerCMAES, "cmaes", "restart_test_run5", 
                             maxit = NULL, stopFitness = NULL, maxFE = 50000, 
                             function_ids = 10, instances = 1, dimensions = 20))

#run CMAES with maximum of 100k function evaluations and default conditions as restart conditions
#set max restarts = maxFE so it does not limit the algorithm
#throws an error sometimes, but i dont know why and when
#also limit the stopfitness to 1e-08 so that no computation time is wasted
suppressWarnings(bbob_custom_parallel(optimizerCMAES, "cmaes", "CMAES_restart_with_default_run", 
                             maxit = NULL, stopFitness = 1e-08, maxFE = 100000, 
                             function_ids = 1:24, instances = 1:15, dimensions = c(2,5,10,20),
                             max_restarts = 100000, restart_multiplier = 1, 
                             restart_triggers = c("tolX", "noEffectAxis", "noEffectCoord",
                                                  "conditionCov", "indefCovMat"), debug.logging = FALSE))

suppressWarnings(bbob_custom(optimizerCMAES, "cmaes", "CMAES_restart_with_default_run", 
                                      maxit = NULL, stopFitness = 1e-08, maxFE = 100000, 
                                      function_ids = 1:24, instances = 1:15, dimensions = c(2,5,10,20),
                                      max_restarts = 100000, restart_multiplier = 1, 
                                      restart_triggers = c("tolX", "noEffectAxis", "noEffectCoord",
                                                           "conditionCov", "indefCovMat"), debug.logging = FALSE))

suppressWarnings(bbob_custom(optimizerCMAES, "cmaes", "CMAES_mergetest", 
                             maxit = NULL, stopFitness = NULL, maxFE = NULL, 
                             function_ids = 5, instances = 1, dimensions = 20, debug.logging = TRUE))

# testing Andi
suppressWarnings(bbob_custom(optimizerCMAES, "cmaes", "CMAES_OCD", 
                             maxit = , stopFitness = NULL, maxFE = NULL, OCD = TRUE,
                             function_ids = 1, instances = 1, dimensions = c(2,5)))

