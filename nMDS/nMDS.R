nmds = function(data, dimensions, distance, ..., interations = 10) {
  
  genRandomPopulation = function(npoints, dimensions) {
    matrix(runif(npoints * dimensions), nrow = npoints, ncol = dimensions)
  }
  
  fitness = function(target, population) {
    n = dim(population)[3]
    scores = numeric(length = n)
    for(i in 1:n ) {
      scores[i] = sqrt(sum(( dist(population[,,i]) - target ) ^ 2) / sum(dist(population[,,i])^2))
    }
    scores
  }
  
  reproduce = function(parents, npop) {
    dims = dim(parents)
    pop = array(NA, c(dims[1:2], npop) )
    for(i in 1:npop) {
      cut = sample(dims[1] - 1, 1)
      parent_cut1 = parents[1:cut,,1]
      parent_cut2 = parents[(cut+1):dims[1],,2]
      pop[,,i] = rbind(parent_cut1, parent_cut2)
    }
    pop
  }
  
  crossover = function(target, population) {
    scores = fitness(target, population)
    parents = population[,,order(scores)[1:2]] # select the two best parents
    reproduce(parents, 10) # 10 new 
  }
  
  mutate = function(population, probability) {
    dims = dim(population)
    for(i in 1:dims[3] ) {
      if ( runif(1) < probability ) {
        line = sample(1:dims[1], 1)
        population[line, , i] = replicate(dims[2], runif(1))
      }
    }
    population
  }
  
  gaNMDS = function(target, dimensions, npoints, maxpop) {
    
    pb = txtProgressBar(1, maxpop, style = 3)
    bestfit = numeric(length = maxpop)
    
    for(i in 1:maxpop) {
      
      if ( i == 1 ) {
        pop = replicate(10, genRandomPopulation(npoints, dimensions))
      }
      pop = crossover(target, pop)
      pop = mutate(pop, 0.10)
      bestfit[i] = max(fitness(target, pop))
      setTxtProgressBar(pb, i)
      
    }
    close(pb)
    list(solution = pop[,,which.max(fitness(target, pop))], FOT = bestfit)
    #pop
    
  }
  
  dataDist = distance(data, ...)
  gaNMDS(dataDist, dimensions, nrow(data), interations)
  
}

require(vegan)
best = nmds(iris[, 1:4], 2, vegdist, method = "bray", interations = 100000)

par(bty='l')
plot(best$solution, type='p', xlab = 'nMDS 1', ylab = 'nMDS 2', col = iris$Species)
plot(best$FOT, type = 'l', xlab = 'ln Iteração', ylab ='Stress', log = 'x')
best
