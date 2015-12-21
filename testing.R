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


fn = makeBBOBFunction(4, 24, 4)
res = cmaes(
  fn,
  monitor = makeSimpleMonitor(),
  control = list(
    sigma = 1.5, lambda = 10,
    stop.ons = list(stopOnOCD(0.00000000001, 55, 1000))
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
