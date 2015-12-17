file = "cmaes_test/data_f24/bbobexp_f24_DIM2.tdat"
#read file
data = read.table(file, skip = 1, fill = TRUE)

#name columns
#when x1 and x2 are not given
colnames(data) = c("FE", "fitness_minus_Fopt", "best_fitness_minus_Fopt", 
              "measured_fitness", "best_measured_fitness")
#when x1 and x2 are given

colnames(data) = c("FE", "fitness_minus_Fopt", "best_fitness_minus_Fopt", 
                   "measured_fitness", "best_measured_fitness", "x1", "x2")

#convert everything to integer
#values that are no integers are indicative of a new function run
#dataframe should be separated at these points
data = suppressWarnings(apply(data, 2, as.double))

#get separate runs
#get split points (NAs) and increment run counter accordingly
data = as.data.frame(cbind(data, run_id = integer(nrow(data))))
run = 1
inBreakPoint = FALSE
for (i in 1:nrow(data)) {
  data$run_id[i] = run
  if (is.na(data[i,1]) && inBreakPoint == FALSE) {
    run = run + 1
    inBreakPoint = TRUE
  }
  if (!is.na(data[i,1]) && inBreakPoint == TRUE) inBreakPoint = FALSE
}

#remove NA rows
data = data[!is.na(data[,1]),]

#get overall best fitness
overallBest = min(data$best_fitness_minus_Fopt)

#get average best
allBest = double()
for (i in min(data$run_id):max(data$run_id)) {
  allBest = c(allBest, min(data$best_fitness_minus_Fopt[which(data$run_id %in% i)]))
}
avgBest = mean(allBest)

#get sd
sdBest = sd(allBest)

#get worst best fitness
overallWorst = max(allBest)
