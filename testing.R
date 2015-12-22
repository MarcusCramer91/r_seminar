if (!"devtools" %in% rownames(installed.packages())) install.packages("devtools")
if (!"smoof" %in% rownames(installed.packages())) install.packages("smoof")

require(smoof)
require(devtools)
require(cmaesr)
install_github(repo = "MarcusCramer91/cmaesr")


fn = makeRosenbrockFunction(dimensions = 2L)
res = cmaes(
  fn,
  monitor = makeSimpleMonitor(),
  control = list(
    sigma = 1.5, lambda = 10,
    stop.ons = list(stopOnOCD(0.0001, 18, 1000))
  )
)
getGlobalOptimum(fn)

fn = makeBBOBFunction(4, 2, 4)
res = cmaes(
  fn,
  monitor = makeSimpleMonitor(),
  control = list(
    sigma = 1.5, lambda = 30,
    stop.ons = list(stopOnOCD(0.00000000001, 100, 1000))
  )
)
getGlobalOptimum(fn)


fn = makeRosenbrockFunction(dimensions = 2L)
res = cmaes(
  fn,
  monitor = makeSimpleMonitor(),
  control = list(
    sigma = 1.5, lambda = 10,
    stop.ons = getDefaultStoppingConditions()
  )
)
cmaes