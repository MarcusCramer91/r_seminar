# create package structure
library(devtools)
library(roxygen2)
package.skeleton(name = "cmaesbenchmarking", code_files = c("customized_bbob.R", "output_interpreter.R", "./cmaes/cmaes.R",
                                                            "./cmaes/helpers.R", "./cmaes/makeMonitor.R","./cmaes/makeStoppingCondition.R",
                                                            "./cmaes/stoppingConditions.R"))
unlink(c("./cmaesbenchmarking/man/bbob_custom.Rd", "./cmaesbenchmarking/man/bbob_custom_parallel.Rd",
         "./cmaesbenchmarking/man/optimizerCMAESWithoutDef.Rd", "./cmaesbenchmarking/man/optimizerRS.Rd",
         "./cmaesbenchmarking/man/optimizerCMAES.Rd", "./cmaesbenchmarking/man/optimizerGA.Rd", 
         "./cmaesbenchmarking/man/getAggregatedConvergenceFunctions.Rd", "./cmaesbenchmarking/man/aggregateResults.Rd", 
         "./cmaesbenchmarking/man/getAvgBestPerDimension.Rd", "./cmaesbenchmarking/man/getAvgBestPerFunction.Rd", 
         "./cmaesbenchmarking/man/getAvgBestPerFunctionAndDimension.Rd", "./cmaesbenchmarking/man/loadAllResultsParallel.Rd",
         "./cmaesbenchmarking/man/loadAllResults.Rd",  "./cmaesbenchmarking/man/helpers.Rd", "./cmaesbenchmarking/man/shouldStop.Rd",
         "./cmaesbenchmarking/man/shouldStop.cma_stopping_condition.Rd", "./cmaesbenchmarking/man/print.cma_result.Rd",
         "./cmaesbenchmarking/man/pReg.Rd", "./cmaesbenchmarking/man/pChi2.Rd", "./cmaesbenchmarking/man/norm2.Rd",
         "./cmaesbenchmarking/man/getRBGAParameter.Rd", "./cmaesbenchmarking/man/getCMAESParameter.Rd",
         "./cmaesbenchmarking/man/generateGraphs.Rd", "./cmaesbenchmarking/man/checkStoppingConditions.Rd",
         "./cmaesbenchmarking/man/checkLogCompleteness.Rd", "./cmaesbenchmarking/man/checkLogCompleteness.Rd",
         "./cmaesbenchmarking/man/cmaesbenchmarking-package.Rd"))
devtools::document("./cmaesbenchmarking")
# paste DECSRIPTION into package folder cmaesbenchmarking
install_local("./cmaesbenchmarking")

