library(caret)
library(mvtnorm)
library(Filips.ML.package)

adam = function(wInitVals, data, labels, alpha = 0.001, beta_1 = 0.9, beta_2 = 0.999, epsilon = 10^(-8)) {
  
  X = as.matrix(data)
  nFeatures = ncol(X)
  N = nrow(X)
  
  m = rep(0, N)
  v = rep(0, N)
  w = wInitVals
  for (t in 1:N) {
    if (sum(y[t]*(w%*%X[t,])) < 1) {
      g = y[t]*X[t,]
      old_m = m
      m = beta_1*old_m + (1 - beta_1)*g
      old_v = v
      v = beta_2*old_v + (1 - beta_2)*(g^2)
      m_hat = m/(1 - beta_1^t)
      v_hat = v/(1 - beta_2^t)
      old_w = w
      w = old_w - alpha*m_hat/(sqrt(v_hat) + epsilon)
    }
  }
  
  return(w)
}

nSamples = 300

data1 = rmvnorm(nSamples, mean = c(2,-1), sigma = diag(2))
data2 = rmvnorm(nSamples, mean = c(-1,2), sigma = diag(2))

plot(data1, col = "red", xlim = c(-3,3), ylim = c(-3,3))
points(data2, col = "green")

x = c(data1[,1],data2[,1])
y = c(data1[,2],data2[,2])
label = c(rep(-1, nSamples), rep(1, nSamples))
data = data.frame(x,y,label)


testAndTrain = trainTestSplit(data, data$label, 0.8)
data = testAndTrain[[1]]
test = testAndTrain[[2]]
testLabels = testAndTrain[[3]]
trainLabels = testAndTrain[[4]]
plot(data[1:nrow(data)/2,], col = "red", xlim = c(-3,3), ylim = c(-3,3))
points(data[(nrow(data)/2):nrow(data),], col = "green")

w = SGD(rep(0,2), data, trainLabels)
norm = c(w[2,], - w[1,])
abline(c(norm[2], - norm[1]))
polyDegree = 100
alphasPolyKernelTrain = KernelizedPerceptron(t(data),trainLabels, t(test), testLabels, PolynomialKernel, polyDegree)
h = 0.1
alphasGaussianKernelTrain = KernelizedPerceptron(t(data),trainLabels, t(test), testLabels, GaussianKernel, h)


w = ADAM(rep(0,2), data, trainLabels)
w = ADAM(w, data, trainLabels)
norm = c(w[2,], - w[1,])
abline(c(norm[2], - norm[1]), col = "yellow", lwd = 3)
y_new = sign(t(w)%*%t(data))

accuracyNormalTrain = testSVMAccuracy(w, data, trainLabels)
accuracyNormalTest = testSVMAccuracy(w, test, testLabels)
