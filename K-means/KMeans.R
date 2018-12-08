
library(mvtnorm)
library(Filips.ML.package)

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
