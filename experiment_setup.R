source("./cmaes/cmaes.R")
source("./cmaes/helpers.R")
source("./cmaes/makeMonitor.R")
source("./cmaes/makeStoppingCondition.R")
source("./cmaes/OnlineConvergenceDetection.R")
source("./cmaes/stoppingConditions.R")
source("customized_bbob.R")
source("output_analysis_function.R")
source("output_interpreter.R")
require(BBmisc)
require(parallel)
require(snow)


##########################################################################################################
#in this section, all the experiments are run
#if you already have the results (the ones from the USB stick) proceed to line 240
#these runs generate the results required for the analysis part
#these runs will take multiple days, so if you have the results aready you probably want to skip this part
#if you do not skip this part: For safety reasons, the created directories have the current date in front
#you need to remove this date manually in order for the output generator to work properly


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


#run CMAES with maximum of 100k function evaluations and default conditions as restart conditions
#set max restarts = maxFE so it does not limit the algorithm
#throws an error sometimes, but i dont know why and when
#also limit the stopfitness to 1e-08 so that no computation time is wasted
suppressWarnings(bbob_custom_parallel(optimizerCMAES, "CMAES", "CMAES_default_with_restart", 
                             maxit = NULL, stopFitness = 1e-08, maxFE = 100000, 
                             function_ids = 1:24, instances = 1:15, dimensions = c(2,5,10,20),
                             max_restarts = 100000, restart_multiplier = 2, 
                             restart_triggers = c("tolX", "noEffectAxis", "noEffectCoord",
                                                  "conditionCov", "indefCovMat"), debug.logging = FALSE))

suppressWarnings(bbob_custom_parallel(optimizerGA, "GA", "GA_default", 
                                      maxit = NULL, stopFitness = 1e-08, maxFE = 100000,
                                      function_ids = 1:24, instances = 1:15, dimensions = c(2,5,10,20)))


#apply OCD
#find good values for varLimit and nPreGen
#for the analysis, the output of these functions must be copied in a folder called "OCD_parametrization"
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

suppressWarnings(bbob_custom_parallel(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", 
                                      data_directory = "OCD_RUN_0.00001_10", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.00001, nPreGen = 10, fitnessValue = TRUE, 
                                      dispersion = FALSE, evolutionPath = FALSE,restart_multiplier = 2, 
                                      restart_triggers = "OCD"))

suppressWarnings(bbob_custom_parallel(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", 
                                      data_directory = "OCD_RUN_0.00001_100", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.00001, nPreGen = 100, fitnessValue = TRUE, 
                                      dispersion = FALSE, evolutionPath = FALSE,restart_multiplier = 2, 
                                      restart_triggers = "OCD"))

suppressWarnings(bbob_custom_parallel(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", 
                                      data_directory = "OCD_RUN_0.00001_1000", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.00001, nPreGen = 1000, fitnessValue = TRUE, 
                                      dispersion = FALSE, evolutionPath = FALSE,restart_multiplier = 2, 
                                      restart_triggers = "OCD"))
#################

#run for different performance indicators
#fitnessValue as indicator is taken from above runs
#test for only dispersion
#test for dispersion only
suppressWarnings(bbob_custom_parallel(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", 
                                      data_directory = "OCD_disp", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.01, nPreGen = 10, fitnessValue = FALSE, 
                                      dispersion = TRUE, evolutionPath = FALSE, restart_multiplier = 2, 
                                      restart_triggers = "OCD"))


#test for evolution path only
suppressWarnings(bbob_custom_parallel(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", 
                                      data_directory = "OCD_evo", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.0001, nPreGen = 100, fitnessValue = FALSE, 
                                      dispersion = FALSE,  evolutionPath = TRUE, restart_multiplier = 2, 
                                      restart_triggers = "OCD"))

#test for evolution path and dispersion
suppressWarnings(bbob_custom_parallel(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", 
                                      data_directory = "OCD_evo_disp", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.0001, nPreGen = 100, fitnessValue = FALSE, 
                                      dispersion = TRUE,  evolutionPath = TRUE, restart_multiplier = 2, 
                                      restart_triggers = "OCD"))

#test for evolution path and fitness
suppressWarnings(bbob_custom_parallel(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", 
                                      data_directory = "OCD_evo_fit", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.0001, nPreGen = 100, fitnessValue = TRUE, 
                                      dispersion = FALSE,  evolutionPath = TRUE, restart_multiplier = 2, 
                                      restart_triggers = "OCD"))

#test for dispersion and fitness
suppressWarnings(bbob_custom_parallel(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", 
                                      data_directory = "OCD_disp_fit", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.0001, nPreGen = 100, fitnessValue = TRUE, 
                                      dispersion = TRUE,  evolutionPath = FALSE, restart_multiplier = 2, 
                                      restart_triggers = "OCD"))

#test for all three
suppressWarnings(bbob_custom_parallel(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", 
                                      data_directory = "OCD_evo_disp_fit", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.0001, nPreGen = 100, fitnessValue = TRUE, 
                                      dispersion = TRUE,  evolutionPath = TRUE, restart_multiplier = 2, 
                                      restart_triggers = "OCD"))

###### OCD with GA
suppressWarnings(bbob_custom_parallel(optimizer = optimizerGA, algorithm_id = "GA", 
                                      data_directory = "GA_OCD_RUN_0.0001_100", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.0001, nPreGen = 100, fitnessValue = TRUE, 
                                      dispersion = FALSE,  evolutionPath = FALSE, restart_multiplier = 2, 
                                      restart_triggers = "OCD"))

#run OCD without restarts
suppressWarnings(bbob_custom_parallel(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", 
                                      data_directory = "CMAES_OCD_no_restarts", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 0, 
                                      OCD = TRUE, varLimit = 0.0001, nPreGen = 100, fitnessValue = TRUE, 
                                      dispersion = FALSE,  evolutionPath = FALSE, restart_multiplier = 2, 
                                      restart_triggers = "OCD"))


#repeat final runs to test whether the result remains stable
suppressWarnings(bbob_custom_parallel(optimizer = optimizerCMAES, algorithm_id = "CMAES_OCD", 
                                      data_directory = "OCD_evo_disp2", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.0001, nPreGen = 100, fitnessValue = FALSE, 
                                      dispersion = TRUE,  evolutionPath = TRUE, restart_multiplier = 2, 
                                      restart_triggers = "OCD"))

suppressWarnings(bbob_custom_parallel(optimizerCMAES, "cmaes", "CMAES_default_with_restart2", 
                                      maxit = NULL, stopFitness = 1e-08, maxFE = 100000, 
                                      function_ids = 1:24, instances = 1:15, dimensions = c(2,5,10,20),
                                      max_restarts = 100000, restart_multiplier = 2, 
                                      restart_triggers = c("tolX", "noEffectAxis", "noEffectCoord",
                                                           "conditionCov", "indefCovMat"), debug.logging = FALSE))

suppressWarnings(bbob_custom_parallel(optimizerGA, "GA", "GA_default2", 
                                      maxit = NULL, stopFitness = 1e-08, maxFE = 100000,
                                      function_ids = 1:24, instances = 1:15, dimensions = c(2,5,10,20)))

suppressWarnings(bbob_custom_parallel(optimizer = optimizerGA, algorithm_id = "GA", 
                                      data_directory = "GA_OCD2", 
                                      dimensions = c(2, 5, 10, 20), instances = 1:15, function_ids = 1:24, maxit = NULL, 
                                      stopFitness = 1e-08, maxFE = 100000, max_restarts = 100000, 
                                      OCD = TRUE, varLimit = 0.0001, nPreGen = 100, fitnessValue = TRUE, 
                                      dispersion = FALSE,  evolutionPath = FALSE, restart_multiplier = 2, 
                                      restart_triggers = "OCD"))

###############################################################################################################
#analysis part
#check if all required results exist in the working directory
#this step might take several hours
#lean back and enjoy the ride (or skip and have a look at the pngs provided by us)
#for presentation purposes the images do not have a caption
#so for interpretation, have a look at the self-explanatory names or read our documentation
isComplete = checkLogCompleteness(usedFunctions = 1:24, usedDimensions = c(2, 5, 10, 20), nInstances = 15)
#the progress bars appearing relate to loading the logs
#this will happen multiple times, so ignore them for this function call
if (isComplete) createOutputImages("./resultImages")
