# install and load cma_es from git repository
#devtools::install_github("jakobbossek/cmaesr")
#library(cmaesr)

stopOnOCD = function(varLimit, nPreGen, maxGen)
  {
  # Initialize OCD
  # The variance limit VarLimit corresponds to the desired approximation accuracy in
  # single-objective optimization, but does not require knowledge about the actual
  # minima of the objectives. The algorithm stops when the standard deviation of
  # the indicator values over the given time window of nPreGen generations is significantly below sqrt(VarLimit).
  assertNumber(varLimit, na.ok = FALSE)
  # nPreGen determines the number of preceding generations for comparisons
  assertInt(nPreGen, na.ok = FALSE)
  # significance level alpha for the tests (established levels are alpha= 0.05 and alpha=0.01). As proposed by Wagner and Trautmann (2010),
  # the default value will be alpha = 0.05. This value should not be tuned or adjusted by the user.
  alpha = 0.01
  p = numeric()
  # MaxGen specifies the maximum number of function evaluations. However, this value is optional in case the upper limit 
  # is specified by an other stopping condition (e.g. stopOnMaxEvals)
  # implement optionality?!
  assertInt(maxGen, na.ok = FALSE)
  return(makeStoppingCondition(
    name = "Online Convergence Detection",
    message = sprintf("OCD successful", varLimit),
    stop.fun = function(envir = parent.frame()) {
      # f a specific indicator P Ij does not use a reference set and evaluates each set separately (e.g., the hypervolume indicator), 
      # the difference between the indicator value of the preceding and the current set is calculated and stored in PIj,i.
      if(envir$iter >= maxGen){
        return(envir$iter >= maxGen)
      }
      if(!is.null(envir$iter)){
        if(envir$iter > nPreGen){
          PI_all = rbind(envir$generation.fitness[[1]], cbind(unlist(envir$generation.fitness))) - rbind(cbind(unlist(envir$generation.fitness)), envir$best.fitness)
          PI_currentgen = PI_all[(envir$iter-nPreGen):envir$iter]
          if((envir$iter - nPreGen) > 1){
            PI_precedinggen =  PI_all[(envir$iter - (nPreGen+1)):(envir$iter - 1)]
          }else{
            PI_precedinggen = PI_currentgen
          }
          pvalue_currentgen = pChi2(varLimit, PI_currentgen)
          pvalue_precedinggen = pChi2(varLimit, PI_precedinggen)
          print(pvalue_precedinggen)
          print(pvalue_currentgen)
          print(pvalue_currentgen <= alpha && pvalue_precedinggen <= alpha)
          return (pvalue_currentgen <= alpha && pvalue_precedinggen <= alpha)
        }
        else{
          return(FALSE)
          }
      }
    }
  ))
}

pChi2 <- function (varLimit, PI) {
  # Determine degrees of freedom
  N = length(PI)-1
  Chi = (var(PI)*N)/varLimit
  p = pchisq(Chi, N, lower.tail = TRUE)
  return (p)
}
