
library(mvtnorm)
library(Filips.ML.package)





# Just start simple with this one
# features is a data frame with the features 
KMeansOnline = function(k, features, seed = 123, learnRateThreshold = 10^(-6), learnRateFunc = cDivT, ...) {
  X = as.matrix(features)
  # N = number of data points in data set.
  N = nrow(features)
  # Initialize assignments uniformly at randomly
  set.seed(seed)
  assignments = sample(x = seq(1,k,1), replace = TRUE, size = N)
  # initialize mu based on random assignments
  mus = t(apply(as.matrix(seq(1,k,1)), 1, function(k_i, X) {
    return((1/length(X[assignments == k_i,]))*colSums(X[assignments == k_i,]))
  }, X))
  t = 1
  dataRow = 1
  while (learnRateFunc(t,...) > learnRateThreshold) {
    
    mu_chosen = which.min(apply(mus, 1, function(mu, dataRow) {
      return(sum((mu - dataRow)^2))
    }, X[dataRow,]))
    oldMu = mus[mu_chosen, ]
    mus[mu_chosen, ] = oldMu + learnRateFunc(t, ...)*(X[dataRow,] - oldMu)
    
    
    
    if (dataRow >= nrow(X)) {
      dataRow = 1
    } else {
      dataRow = dataRow + 1
    }
    t = t + 1
    print(learnRateFunc(t,...))
  }
  return(mus)
}

#Fix coresets functions

samples = GenerateClusters(1000, 40, 10, c(-100,-100, -100), c(100,100, 100), seed = 12)

samples = standardizeData(as.matrix(samples), typeIn = "Matrix")



kMeans = LLoydsAlgorithm(k = 9, samples, convergenceCheckType = "Threshold")
kMeanOnline = KMeansOnline(k = 9, samples)
plot(samples$X1, samples$X2, col = "red")
points(kMeans[,1], kMeans[,2], col = "blue", pch = "X")
points(kMeanOnline[,1], kMeanOnline[,2], col = "green", pch = "X")
plot(samples$X1, samples$X3, col = "red")
points(kMeans[,1], kMeans[,3], col = "green", pch = "X")
