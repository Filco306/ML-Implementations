
library(mvtnorm)
library(Filips.ML.package)

EuclidDistanceMatrix = function(X, Y) {
  Xhat = X/sqrt(rowSums(X^2))
  Yhat = Y/sqrt(rowSums(Y^2))
  C_matrix = Xhat%*%t(Yhat)
  return(1 - C_matrix)
}



GenerateClusters = function(avgSampPerCluster, clusterVar, nrClusters, featureMins, featureMaxes, seed = 123) {
  
  set.seed(seed)
  
  df = data.frame(matrix(rep(0,length(featureMins)), ncol = length(featureMins), nrow = 1))
  names = colnames(df)
  
  for (i in 1:nrClusters) {
    
    means = as.numeric()
    vars = as.numeric()
    for (j in 1:length(featureMins)) {
      means[j] = runif(1, min = featureMins[j], max = featureMaxes[j])
      vars[j] = abs(featureMins[j] - featureMaxes[j])
    }
    
    
    nrInCluster = floor(rnorm(1, avgSampPerCluster, clusterVar))
    
    
    print(nrInCluster)
    cluster = rmvnorm(nrInCluster, mean = means, sigma = diag(vars))
    colnames(cluster) = names
    df = rbind(df, cluster)
    
  }
  
  df = df[-c(1),]
  return(df)
}

standardizeDataframe = function(df) {
  names = colnames(df)
  dataframe = data.frame(apply(df, 2, function(col) {
    return(standardizeFeature(col))
  }))
  
  colnames(dataframe) = names
  return(dataframe)
}

euclidDistAlgo = function(initCenters, features) {
  # use euclidian distance!!
}




#Just start simple with this one
# features is a data frame with the features 
LLoydsAlgorithm = function(k, features, seed = 123) {
  X = as.matrix(features)
  # N = number of data points in data set.
  N = nrow(features)
  # Initialize assignments uniformly at randomly
  set.seed(seed)
  assignments = sample(x = seq(1,k,1), replace = TRUE, size = N)
  # initialize mu based on random assignments
  mus = apply(as.matrix(seq(1,k,1)), 1, function(k_i, X) {
    return((1/length(X[assignments == k_i,]))*colSums(X[assignments == k_i,]))
  }, X)
  print(mus)
  #delete this line later
  points(mus[1,], mus[2,], col = "blue", pch = "X")
  #While not converged
    #Assign each point xi to closest center
    #Update center as mean of assigned data points
  converged = FALSE
  count = 0
  while (converged == FALSE) {
    converged = TRUE
    
    DistanceMatrix = EuclidDistanceMatrix(X, t(mus))
    for (i in 1:N) {
      closestMu = which.min(DistanceMatrix[i,])
      
      if (closestMu != assignments[i]) {
        print(i)
        converged = FALSE
        assignments[i] = closestMu
        
      }
    }
    count = count + 1
    print(count)
  }
  
  # Recalculate mus
  mus = apply(as.matrix(seq(1,k,1)), 1, function(index) {
    return((1/nrow(X[assignments == index,]))*colSums(X[assignments == index,]))
  })
  points(mus[1,], mus[2,], col = "blue", pch = "X")
  print("Finished!")
  return(mus)
}

KMeansOnline = function(initCenters, features) {
  
}

#Fix coresets functions

samples = GenerateClusters(300, 40, 6, c(-100,-100), c(100,100))

samples = standardizeDataframe(samples)

plot(samples$X1, samples$X2, col = "red")

kMeans = LLoydsAlgorithm(k = 5, samples)
