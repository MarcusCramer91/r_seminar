#default run of CMAES with only default stopping criteria
suppressWarnings(bbob_custom(optimizerCMAES, "cmaes", "CMAES_default_run", 
                             maxit = NULL, stopFitness = NULL, maxFE = NULL, 
                             function_ids = 1:24, instances = 1:15, dimensions = c(2,5,10,20)))

#run of random search with 234204 function evaluations
#this is the longest runs of above CMAES run
#this result is used for comparison with simple random search
#since results are purely random anyway, only one instance is run
bbob_custom(optimizerRS, "random search", "Random_Search_234204", function_ids = 1:24, instances = 1,
            dimensions = c(2,5,10,20), maxFE = 234204)


suppressWarnings(bbob_custom(optimizerCMAES, "cmaes", "logtest", 
                             maxit = NULL, stopFitness = NULL, maxFE = NULL, 
                             function_ids = 1, instances = 1, dimensions = 2, debug.logging = TRUE))