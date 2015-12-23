#does what the name suggests
#samples random values and stores the best
random_search = function(fun, maxFE = NULL) {
  #get box constraints
  
  #hardcode constraints since the smoof functions are buggy
  ub = 5
  lb = -5
  dimensions = length(getLowerBoxConstraints(fun))
  
  #if no stopping criterion specified, stop after 10000 function evaluations
  if (is.null(maxFE)) maxFE = 10000
  bestFitness = Inf
  result = character(0)
  for (i in 1:maxFE) {
    solution = runif(dimensions, lb, ub)
    fitness = fun(solution)
    if (fitness < bestFitness) bestFitness = fitness
    result = c(result, paste(i, i, bestFitness))
  }
  return(result)
}