
library(mvtnorm)
library(Filips.ML.package)


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

#Just start simple with this one
LLoydsAlgorithm = function(initCenters, feats) {
  
  #vector keeping hold to which mean each vector is assigned
  assignment = rep(-1, nrow(feats))
  
  
  
  #While not converged
  
  
  #Assign each point xi to closest center
  
  #Update center as mean of assigned data points
}

KMeansOnline = function(initCenters) {
  
}

#Fix coresets functions

samples = GenerateClusters(300, 40, 6, c(-100,-100), c(100,100))

samples = standardizeDataframe(samples)

plot(samples$X1, samples$X2)

kMeans = LLoydsAlgorithm(rmvnorm(n = 6), samples)
