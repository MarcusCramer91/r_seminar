fitness_i = 5
fitness_1 = 30
fitness_2 = 25
lb = 1
ub = 400000
fitness1_normalized = abs(fitness_i - fitness_1)/(ub-lb)
fitness2_normalized = abs(fitness_i - fitness_2)/(ub-lb)
fitness1_not = abs (fitness_i - fitness_1)
fitness2_not= abs(fitness_i - fitness_2)

fitness1_normalized
fitness2_normalized
fitness1_not
fitness2_not

fitness1_not-fitness2_not
fitness1_normalized*39 - fitness2_normalized*39

first = var(c(fitness1_normalized,fitness2_normalized))
var(c(fitness1_not,fitness2_not))
    
test <- list ("hi"= c("ha","jo"), "ja"=c(4,6,7))
test <- list(test)
test
grep("ha",test)
