source("./cmaes/cmaes.R")
source("customized_bbob.R")


#default run of CMAES with only default stopping criteria
suppressWarnings(bbob_custom_parallel(optimizerCMAES, "cmaes", "CMAES_default_run", 
                             maxit = NULL, stopFitness = NULL, maxFE = NULL, 
                             function_ids = 1:24, instances = 1:15, dimensions = c(2, 5, 10, 20)))

#simulate 5 restarts by running 5 times (used in the presentation)
#also simulate a default run with 250000 max evaluations
suppressWarnings(bbob_custom(optimizerCMAESWithoutDef, "cmaes1", "CMAES_restart_test", 
                             maxit = NULL, stopFitness = NULL,  
                             function_ids = 12, instances = 1, dimensions = 20, maxFE = 50000))
suppressWarnings(bbob_custom(optimizerCMAESWithoutDef, "cmaes2", "CMAES_restart_test", 
                             maxit = NULL, stopFitness = NULL, 
                             function_ids = 12, instances = 1, dimensions = 20, maxFE = 50000))
suppressWarnings(bbob_custom(optimizerCMAESWithoutDef, "cmaes3", "CMAES_restart_test", 
                             maxit = NULL, stopFitness = NULL, 
                             function_ids = 12, instances = 1, dimensions = 20, maxFE = 50000))
suppressWarnings(bbob_custom(optimizerCMAESWithoutDef, "cmaes4", "CMAES_restart_test", 
                             maxit = NULL, stopFitness = NULL, 
                             function_ids = 12, instances = 1, dimensions = 20, maxFE = 50000))
suppressWarnings(bbob_custom(optimizerCMAESWithoutDef, "cmaes5", "CMAES_restart_test", 
                             maxit = NULL, stopFitness = NULL, 
                             function_ids = 12, instances = 1, dimensions = 20, maxFE = 50000))
suppressWarnings(bbob_custom(optimizerCMAESWithoutDef, "cmaes6", "CMAES_restart_test", 
                             maxit = NULL, stopFitness = NULL, 
                             function_ids = 12, instances = 1, dimensions = 20, maxFE = 250000))


#run of random search with 100000 function evaluations
#this result is used for comparison with simple random search
suppressWarnings(bbob_custom(optimizerRS, "random search", "Random_Search_100000", 
                                      function_ids = 1:24, instances = 1:15,
                                      dimensions = c(2,5,10,20), maxFE = 100000))

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
suppressWarnings(bbob_custom_parallel(optimizerCMAES, "cmaes", "CMAES_default_with_restart", 
                             maxit = NULL, stopFitness = 1e-08, maxFE = 100000, 
                             function_ids = 1:24, instances = 1:15, dimensions = c(2,5,10,20),
                             max_restarts = 100000, restart_multiplier = 1, 
                             restart_triggers = c("tolX", "noEffectAxis", "noEffectCoord",
                                                  "conditionCov", "indefCovMat"), debug.logging = FALSE))

suppressWarnings(bbob_custom(optimizerCMAES, "cmaes", "CMAES_default_with_restart", 
                                      maxit = NULL, stopFitness = 1e-08, maxFE = 100000, 
                                      function_ids = 1:24, instances = 1:15, dimensions = c(2,5,10,20),
                                      max_restarts = 100000, restart_multiplier = 1, 
                                      restart_triggers = c("tolX", "noEffectAxis", "noEffectCoord",
                                                           "conditionCov", "indefCovMat"), debug.logging = FALSE))

suppressWarnings(bbob_custom_parallel(optimizerGA, "GA", "GA_default", 
                                      maxit = NULL, stopFitness = 1e-08, maxFE = 100000,
                                      function_ids = 1:24, instances = 1:15, dimensions = c(2,5,10,20)))
suppressWarnings(bbob_custom(optimizerGA, "GA", "GA_default", 
                                      maxit = NULL, stopFitness = 1e-08, maxFE = 100000,
                                      function_ids = 1:24, instances = 1:15, dimensions = c(2,5,10,20)))


#apply OCD
#find good values for varLimit and nPreGen
suppressWarnings(bbob_custom_parallel(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", 
                                      data_directory = "OCD_RUN_0.01_10", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.01, nPreGen = 10, fitnessValue = TRUE, 
                                      dispersion = FALSE, evolutionPath = FALSE, restart_multiplier = 2, 
                                      restart_triggers = "OCD"))

suppressWarnings(bbob_custom_parallel(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", 
                                      data_directory = "OCD_OCD_RUN_0.01_100", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.01, nPreGen = 100, fitnessValue = TRUE, 
                                      dispersion = FALSE, evolutionPath = FALSE, restart_multiplier = 2, 
                                      restart_triggers = "OCD"))

suppressWarnings(bbob_custom_parallel(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", 
                                      data_directory = "OCD_RUN_0.01_1000", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.01, nPreGen = 1000, fitnessValue = TRUE, 
                                      dispersion = FALSE, evolutionPath = FALSE, restart_multiplier = 2, 
                                      restart_triggers = "OCD"))

suppressWarnings(bbob_custom_parallel(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", 
                                      data_directory = "OCD_RUN_0.001_10", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.001, nPreGen = 10, fitnessValue = TRUE, 
                                      dispersion = FALSE, evolutionPath = FALSE, restart_multiplier = 2, 
                                      restart_triggers = "OCD"))

suppressWarnings(bbob_custom_parallel(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", 
                                      data_directory = "OCD_OCD_RUN_0.001_100", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.001, nPreGen = 100, fitnessValue = TRUE, 
                                      dispersion = FALSE, evolutionPath = FALSE, restart_multiplier = 2, 
                                      restart_triggers = "OCD"))

suppressWarnings(bbob_custom_parallel(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", 
                                      data_directory = "OCD_RUN_0.001_1000", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.001, nPreGen = 1000, fitnessValue = TRUE, 
                                      dispersion = FALSE, evolutionPath = FALSE, restart_multiplier = 2, 
                                      restart_triggers = "OCD"))

suppressWarnings(bbob_custom_parallel(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", 
                                      data_directory = "OCD_RUN_0.0001_10", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.0001, nPreGen = 10, fitnessValue = FALSE, 
                                      dispersion = TRUE, evolutionPath = FALSE, restart_multiplier = 2, 
                                      restart_triggers = "OCD"))

suppressWarnings(bbob_custom_parallel(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", 
                                      data_directory = "OCD_RUN_0.0001_100", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.0001, nPreGen = 100, fitnessValue = TRUE, 
                                      dispersion = FALSE, evolutionPath = FALSE, restart_multiplier = 2, 
                                      restart_triggers = "OCD"))

suppressWarnings(bbob_custom_parallel(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", 
                                      data_directory = "OCD_RUN_0.0001_1000", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.0001, nPreGen = 1000, fitnessValue = TRUE, 
                                      dispersion = FALSE, evolutionPath = FALSE,restart_multiplier = 2, 
                                      restart_triggers = "OCD"))

#run for different performance indicators
#test for dispersion only
suppressWarnings(bbob_custom_parallel(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD_", 
                                      data_directory = "OCD_RUN_0.01_10", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.01, nPreGen = 10, fitnessValue = FALSE, 
                                      dispersion = TRUE, evolutionPath = FALSE, restart_multiplier = 2, 
                                      restart_triggers = "OCD"))


#test for evolution path only
suppressWarnings(bbob_custom_parallel(optimizer = optimizerGA, algorithm_id = "GA", 
                                      data_directory = "GA_OCD_RUN_0.0001_1000", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.0001, nPreGen = 100, fitnessValue = FALSE, 
                                      dispersion = FALSE,  evolutionPath = TRUE, restart_multiplier = 1, 
                                      restart_triggers = "OCD"))

#test for evolution path and disperion
suppressWarnings(bbob_custom_parallel(optimizer = optimizerGA, algorithm_id = "GA", 
                                      data_directory = "GA_OCD_RUN_0.0001_1000", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.0001, nPreGen = 100, fitnessValue = FALSE, 
                                      dispersion = TRUE,  evolutionPath = TRUE, restart_multiplier = 1, 
                                      restart_triggers = "OCD"))

#test for evolution path and fitness
suppressWarnings(bbob_custom_parallel(optimizer = optimizerGA, algorithm_id = "GA", 
                                      data_directory = "GA_OCD_RUN_0.0001_1000", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.0001, nPreGen = 100, fitnessValue = TRUE, 
                                      dispersion = FALSE,  evolutionPath = TRUE, restart_multiplier = 1, 
                                      restart_triggers = "OCD"))

#test for dispersion and fitness
suppressWarnings(bbob_custom_parallel(optimizer = optimizerGA, algorithm_id = "GA", 
                                      data_directory = "GA_OCD_RUN_0.0001_1000", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.0001, nPreGen = 100, fitnessValue = TRUE, 
                                      dispersion = TRUE,  evolutionPath = FALSE, restart_multiplier = 1, 
                                      restart_triggers = "OCD"))

#test for all three
suppressWarnings(bbob_custom_parallel(optimizer = optimizerGA, algorithm_id = "GA", 
                                      data_directory = "GA_OCD_RUN_0.0001_1000", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.0001, nPreGen = 100, fitnessValue = TRUE, 
                                      dispersion = TRUE,  evolutionPath = TRUE, restart_multiplier = 1, 
                                      restart_triggers = "OCD"))

###### OCD with GA
#fitnessValue as indicator is taken from above runs
#test for only dispersion
suppressWarnings(bbob_custom_parallel(optimizer = optimizerGA, algorithm_id = "GA", 
                                      data_directory = "GA_OCD_RUN_0.0001_1000", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.0001, nPreGen = 100, fitnessValue = FALSE, 
                                      dispersion = TRUE,  evolutionPath = FALSE, restart_multiplier = 1, 
                                      restart_triggers = "OCD"))













#debugging

suppressWarnings(bbob_custom(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", data_directory = "debugging", 
                             dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                             stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                             OCD = TRUE, varLimit = 0.001, nPreGen = 100, restart_multiplier = 2, 
                             restart_triggers = "OCD"))

suppressWarnings(bbob_custom(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", data_directory = "asdf", 
                             dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1, maxit = NULL, 
                             stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                             OCD = TRUE, varLimit = 0.001, nPreGen = 100, restart_multiplier = 2, 
                             restart_triggers = "OCD", debug.logging = TRUE, fitnessValue = TRUE))


#debugging end


suppressWarnings(bbob_custom(optimizerCMAES, "cmaes", "CMAES_mergetest", 
                             maxit = NULL, stopFitness = NULL, maxFE = NULL, 
                             function_ids = 5, instances = 1, dimensions = 20, debug.logging = TRUE))

# testing Andi
suppressWarnings(bbob_custom(optimizerCMAES, "cmaes", "CMAES_OCD", 
                             maxit = , stopFitness = NULL, maxFE = NULL, OCD = TRUE,
                             function_ids = 1, instances = 1, dimensions = c(2,5)))

#some more testing

suppressWarnings(bbob_custom(optimizerCMAES, "cmaes", "test_def", 
                             maxit = NULL, stopFitness = NULL, maxFE = NULL, 
                             function_ids = 6, instances = 1, dimensions = 20))


suppressWarnings(bbob_custom(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", data_directory = "debug_ocd", 
                             dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                             stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                             OCD = TRUE, varLimit = 0.01, nPreGen = 10, restart_multiplier = 2, 
                             restart_triggers = "OCD"))

