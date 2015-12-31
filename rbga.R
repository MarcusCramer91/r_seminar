source("./cmaes/helpers.R")
source("./cmaes/makeMonitor.R")
source("./cmaes/makeStoppingCondition.R")
source("./cmaes/stoppingConditions.R")

rbga = function (stringMin = c(), stringMax = c(), suggestions = NULL, 
          popSize = 200, iters = 100, mutationChance = NA, elitism = NA, 
          monitorFunc = NULL, evalFunc = NULL, showSettings = FALSE, 
          verbose = FALSE, instance = 1, control = list()) 
{
  
  #=================================================================================
  stop.ons = getRBGAParameter(control, "stop.ons", NULL)
  restart.triggers = getRBGAParameter(control, "restart.triggers", character(0L))
  stop.ons.names = sapply(stop.ons, function(stop.on) stop.on$code)
  # check restart multiplier!!!!!! especially "default = 2"
  restart.multiplier = getRBGAParameter(control, "restart.multiplier", 2)
  max.restarts = getRBGAParameter(control, "max.restarts", 0L)
  #=================================================================================
  
  if (is.null(evalFunc)) {
    stop("A evaluation function must be provided. See the evalFunc parameter.")
  }
  vars = length(stringMin)
  if (is.na(mutationChance)) {
    mutationChance = 1/(vars + 1)
  }
  if (is.na(elitism)) {
    elitism = floor(popSize/5)
  }
  if (verbose) 
    cat("Testing the sanity of parameters...\n")
  if (length(stringMin) != length(stringMax)) {
    stop("The vectors stringMin and stringMax must be of equal length.")
  }
  if (popSize < 5) {
    stop("The population size must be at least 5.")
  }
  if (iters < 1) {
    stop("The number of iterations must be at least 1.")
  }
  if (!(elitism < popSize)) {
    stop("The population size must be greater than the elitism.")
  }
  if (showSettings) {
    if (verbose) 
      cat("The start conditions:\n")
    result = list(stringMin = stringMin, stringMax = stringMax, 
                  suggestions = suggestions, popSize = popSize, iters = iters, 
                  elitism = elitism, mutationChance = mutationChance)
    class(result) = "rbga"
    cat(summary(result))
  }
  else {
    if (verbose) 
      cat("Not showing GA settings...\n")
  }
  
  result = paste("Starting optimization. Instance:", instance, sep = "")
  
  if (vars > 0) {
    
    bestEvals = rep(NA, iters)
    meanEvals = rep(NA, iters)
    evalVals = rep(NA, popSize)
    bestValue = Inf
    
    #=================================================================================
    generation.bestfitness = list()
    worst.fitness = Inf
    n.evals = 0L
    do.terminate = FALSE
    restarts = -1
    # initialize stopped.on.t and stopped.on.chi that indicate the type of test which caused the termination of cma-es.
    # the stopping condition "stopOnOCD" sets the corresponding variable to "1" if that specific test has been significant.
    stopped.on.t = 0
    stopped.on.chi = 0
    
    for (run in 0:max.restarts){
      restarts = restarts + 1
      if(run == 0) {
        popSize = popSize
      }else{
        popSize = popSize*restart.multiplier
        
      }
      # no restart trigger fired until now
      restarting = FALSE
      restartIter = 0
      #=================================================================================
      
    if (!is.null(suggestions)) {
      if (verbose) 
        cat("Adding suggestions to first population...\n")
      population = matrix(nrow = popSize, ncol = vars)
      suggestionCount = dim(suggestions)[1]
      for (i in 1:suggestionCount) {
        population[i, ] = suggestions[i, ]
      }
      if (verbose) 
        cat("Filling others with random values in the given domains...\n")
      for (var in 1:vars) {
        population[(suggestionCount + 1):popSize, var] = stringMin[var] + 
          runif(popSize - suggestionCount) * (stringMax[var] - 
                                                stringMin[var])
      }
    }
    else {
      if (verbose) 
        cat("Starting with random values in the given domains...\n")
      population = matrix(nrow = popSize, ncol = vars)
      for (var in 1:vars) {
        population[, var] = stringMin[var] + runif(popSize) * 
          (stringMax[var] - stringMin[var])
      }
    }

    for (iter in 1:iters) {
      
      #=================================================================================
      restartIter = restartIter + 1
      if(restarting) break
      #=================================================================================
      
      if (verbose) 
        cat(paste("Starting iteration", iter, "\n"))
      if (verbose) 
        cat("Calucating evaluation values... ")
      for (object in 1:popSize) {
        if (is.na(evalVals[object])) {
          evalVals[object] = evalFunc(population[object, 
                                                 ])
          if (verbose) 
            cat(".")
        }
      }
      bestEvals[iter] = min(evalVals)
      meanEvals[iter] = mean(evalVals)
      if (verbose) 
        cat(" done.\n")
      if (!is.null(monitorFunc)) {
        if (verbose) 
          cat("Sending current state to rgba.monitor()...\n")
        result = list(type = "floats chromosome", stringMin = stringMin, 
                      stringMax = stringMax, popSize = popSize, iter = iter, 
                      iters = iters, population = population, elitism = elitism, 
                      mutationChance = mutationChance, evaluations = evalVals, 
                      best = bestEvals, mean = meanEvals)
        class(result) = "rbga"
        monitorFunc(result)
      }
      if (iter < iters) {
        if (verbose) 
          cat("Creating next generation...\n")
        newPopulation = matrix(nrow = popSize, ncol = vars)
        newEvalVals = rep(NA, popSize)
        if (verbose) 
          cat("  sorting results...\n")
        sortedEvaluations = sort(evalVals, index = TRUE)
        if (sortedEvaluations$x[1] < bestValue) bestValue = sortedEvaluations$x[1]
        #=================================================================================
        if (sortedEvaluations$x[length(sortedEvaluations$x)] > worst.fitness | is.infinite(worst.fitness)) worst.fitness = sortedEvaluations$x[length(sortedEvaluations$x)]
        #=================================================================================
        sortedPopulation = matrix(population[sortedEvaluations$ix, 
                                             ], ncol = vars)
        if (elitism > 0) {
          if (verbose) 
            cat("  applying elitism...\n")
          newPopulation[1:elitism, ] = sortedPopulation[1:elitism, 
                                                        ]
          newEvalVals[1:elitism] = sortedEvaluations$x[1:elitism]
        }
        if (vars > 1) {
          if (verbose) 
            cat("  applying crossover...\n")
          for (child in (elitism + 1):popSize) {
            parentProb = dnorm(1:popSize, mean = 0, sd = (popSize/3))
            parentIDs = sample(1:popSize, 2, prob = parentProb)
            parents = sortedPopulation[parentIDs, ]
            crossOverPoint = sample(0:vars, 1)
            if (crossOverPoint == 0) {
              newPopulation[child, ] = parents[2, ]
              newEvalVals[child] = sortedEvaluations$x[parentIDs[2]]
            }
            else if (crossOverPoint == vars) {
              newPopulation[child, ] = parents[1, ]
              newEvalVals[child] = sortedEvaluations$x[parentIDs[1]]
            }
            else {
              newPopulation[child, ] = c(parents[1, ][1:crossOverPoint], 
                                         parents[2, ][(crossOverPoint + 1):vars])
            }
          }
        }
        else {
          if (verbose) 
            cat("  cannot crossover (#vars=1), using new randoms...\n")
          newPopulation[(elitism + 1):popSize, ] = sortedPopulation[sample(1:popSize, 
                                                                           popSize - elitism), ]
        }
        population = newPopulation
        evalVals = newEvalVals
        if (mutationChance > 0) {
          if (verbose) 
            cat("  applying mutations... ")
          mutationCount = 0
          for (object in (elitism + 1):popSize) {
            for (var in 1:vars) {
              if (runif(1) < mutationChance) {
                dempeningFactor = (iters - iter)/iters
                direction = sample(c(-1, 1), 1)
                mutationVal = stringMax[var] - stringMin[var] * 
                  0.67
                mutation = population[object, var] + 
                  direction * mutationVal * dempeningFactor
                if (mutation < stringMin[var]) 
                  mutation = stringMin[var] + runif(1) * 
                  (stringMax[var] - stringMin[var])
                if (mutation > stringMax[var]) 
                  mutation = stringMin[var] + runif(1) * 
                  (stringMax[var] - stringMin[var])
                population[object, var] = mutation
                evalVals[object] = NA
                mutationCount = mutationCount + 1
              }
            }
          }
          if (verbose) 
            cat(paste(mutationCount, "mutations applied\n"))
        }
      }
      n.evals = n.evals + popSize
      gap = bestValue - getGlobalOptimum(evalFunc)$value
      result = c(result, paste(iter, n.evals, gap, bestValue))
      
      #=================================================================================
      best.fitness = bestValue
      ######### normalization and logging functionality for OCD ########
      if ("OCD" %in% stop.ons.names) {
        # get the call parameters from OCD needed for normalization
        param.set = stop.ons[[grep("OCD",stop.ons)]]$param.set
        # log best fitness value per generation
        
        
        generation.bestfitness[[iter]] = bestValue
        
        # define upper and lower bound for normalization after nPreGen generations.
        # bounds are fixed once nPreGen generations are reached.
        if(iter == param.set[[2]]){
          upper.bound = worst.fitness
          lower.bound = best.fitness
        }
        
        # initialize list "dispersion" to be used as a performance indicator
        
        # CHECK if dispersion could be integrated
        #dispersion[[iter]] = sum(abs(m-arx.repaired))
        
        # populate list with performance indicators for OCD.
        # if necessary, normalize the performance indicator of interest. 
        # For example, fitnessValue is normalized in the range upper.bound - lower.bound, 
        # i.e. the range of the objective values after nPreGen generations as defined above. This value is fixed for all upcomming generations
        
        
        performance.measures = list("fitnessValue.iter" = if(iter < param.set[[2]]) best.fitness else (best.fitness)/(upper.bound-lower.bound),
                                    "fitnessValue.all" = if(iter < param.set[[2]]) generation.bestfitness[-length(generation.bestfitness)]
                                    else unlist(generation.bestfitness[-length(generation.bestfitness)])/(upper.bound-lower.bound))
        
      }
      
      
      stop.obj = checkStoppingConditions(stop.ons)
      n.stop.codes = length(stop.obj$codes)
      if (max.restarts > 0L && any(stop.obj$codes %in% restart.triggers)) {
        n.stop.codes = sum(!(stop.obj$codes %in% restart.triggers))
        restarting = TRUE
      }
      if(!restarting && (n.stop.codes > 0L)) {
        do.terminate = TRUE
        break
      }
      #inner loop
    }
      if(do.terminate){
        break
      }
      # restarting loop
    }
    #=================================================================================
  }
  result = c(result, "Optimization terminated")
  result = c(result, paste("-1", restarts))
  # log the type of test that caused the termination of cma-es in the output data.
  # "-2" indicated the termination based on the chi-squared test, "-3" indicates the termination based on the t-test
  if ("OCD" %in% stop.ons.names) {
    result = c(result, paste("-2", stopped.on.t))
    result = c(result, paste("-3", stopped.on.chi))
  }
  #result = list(type = "floats chromosome", stringMin = stringMin, 
  #              stringMax = stringMax, popSize = popSize, iters = iters, 
  #              suggestions = suggestions, population = population, elitism = elitism, 
  #              mutationChance = mutationChance, evaluations = evalVals, 
  #             best = bestEvals, mean = meanEvals)
  #class(result) = "rbga"
  return(result)
}
