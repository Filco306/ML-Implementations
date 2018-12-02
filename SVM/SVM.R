library(caret)
library(mvtnorm)
library(Filips.ML.package)



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
polyDegree = 100
alphasPolyKernelTrain = KernelizedPerceptron(t(data),trainLabels, t(test), testLabels, PolynomialKernel, polyDegree)
h = 0.2
alphasGaussianKernelTrain = KernelizedPerceptron(t(data),trainLabels, t(test), testLabels, GaussianKernel, h)


w = matrix(w, nrow = length(w), ncol = 1)
y_new = sign(t(w)%*%t(data))

accuracyNormalTrain = testSVMAccuracy(w, data, trainLabels)
accuracyNormalTest = testSVMAccuracy(w, test, testLabels)
