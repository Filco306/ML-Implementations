
#' Stochastic Gradient Descent
#'
#' This function is a very simple implementation of stochastic gradient descent.
#' Step size is set to 1/sqrt(t), which is a common stepsize, but one should be able to alter it.
#' @param wInitVals are the initial values of your vector
#' @param data is the features, i.e. the X vector.
#' @param trainLabels is the y vector
#' @keywords Stochastic Gradient Descent
#' @export
#' @examples
#'
#' trainLabels = y
#' data = X
#' w = SGD(rnorm(nrow(data), mean = 0, sd = 1), data, trainLabels = y)
#'

SGD <- function (wInitVals, data, trainLabels) {
  w = as.matrix(wInitVals)

  for (t in 1:nrow(data)) {
    stepSize = 1/sqrt(t)
    w[,1] = w[,1] - stepSize*trainLabels[t]*data[t,]
    w = w*min(1, 1/(t(w)%*%w))

  }
  return(w)
}

#' cLogT
#'
#' Step size function, deciding the learning rate.
#' Step size is set to 1/log(t), which is a common stepsize, possible to alter through c and power
#' Return c/(power*log(t)), with default c = 1 and power = 1
#' @param t is the timestep
#' @param c is a coefficient to tune with.
#' @param power is to what power one wants to elevate t to.
#' @keywords Stepsize, learning rate
#' @export
#' @examples
#'
#'
#'
cDivT = function(t, c = 1, power = 1) {
  return(c/(power*log(t)))
}

#' cDivT
#'
#' Step size function, deciding the learning rate.
#' Step size is set to 1/t, which is a common stepsize, possible to alter through c and power
#' Return c/(t^power), with default c = 1 and power = 1
#' @param t is the timestep
#' @param c is a coefficient to tune with.
#' @param power is to what power one wants to elevate t to.
#' @keywords Stepsize, learning rate
#' @export
#' @examples
#'
#'
#'
cDivT = function(t, c = 1, power = 1) {
  return(c/(t^power))
}


#' Perceptron Stochastic Gradient Descent
#'
#' This function is a very simple implementation of gradient descent in a perceptron.
#' Step size is set to 1/t, which is a common stepsize, but one should be able to alter it.
#' @param wInitVals are the initial values of your vector
#' @param data is the features, i.e. the X vector.
#' @param trainLabels is the y vector
#' @param stepSizeFunc is the step size function used.
#' @param ... is used to be able to specify additional parameters sent in to stepSizeFunc.
#' @keywords Stochastic Gradient Descent
#' @export
#' @examples
#'
#' trainLabels = y
#' data = X
#' w = PerceptronSGD(rnorm(nrow(data), mean = 0, sd = 1), data, trainLabels = y)
#'
PerceptronSGD <- function (wInitVals, data, trainLabels, stepSizeFunc = cDivT, ...) {
  w = matrix(wInitVals, nrow = length(wInitVals), ncol = 1)

  for (t in 1:nrow(data)) {

    if (trainLabels[t] != sign(w[,1]*data[t,])) {
      stepSize = 1/t
      w[,1] = w[,1] - stepSizeFunc(t, ...)*trainLabels[t]*data[t,]
      w = w*min(1, 1/(t(w)%*%w))
    }
  }
  return(w)
}

#' Adagrad
#'Implementation of the Adagrad algorithm. Left TBD at the moment.
#'
Adagrad = function(wInitVals, data, labels) {
  return(1)
}

#' ADAM
#' Implementation of the ADAM algorithm. For more info, check out https://arxiv.org/abs/1412.6980
#'
#'
#' @param wInitVals are the initial values of your vector
#' @param data is the features, i.e. the X vector. Send in as Nxd matrix or dataframe.
#' @param labels is the y vector
#' @param nLoops is the number of times to use each data point. Default set to 2.
#' @param alpha is the alpha parameter in the algorithm. In other words, the learning rate.
#' @param beta_1 is the exponential decay rate for first moment estimates. Set to good default setting.
#' @param beta_2 is the exponential decay rate for second moment estimates. Set to good default setting.
#' @param epsilon is a small number to prevent division by zero. Default set to 10^(-8)
#' @keywords Stochastic Gradient Descent
#' @export
#' @examples
#'
#'
#'
#'
ADAM = function(wInitVals, data, labels, nLoops = 2, alpha = 0.001, beta_1 = 0.9, beta_2 = 0.999, epsilon = 10^(-8)) {

  X = as.matrix(data)
  nFeatures = ncol(X)
  N = nrow(X)

  m = rep(0, nFeatures)
  v = rep(0, nFeatures)
  w = as.matrix(wInitVals)
  for (i in 1:nLoops) {
    for (t in 1:N) {

      if (sum(y[t]*(t(w)%*%as.matrix(X[t,]))) < 1) {
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
  }
  return(w/sqrt(sum(w^2)))
}


#' Kernelized Perceptron
#'
#' This function is a very simple implementation of a kernelized perceptron
#' Step size is set to 1/t, which is a common stepsize, but one should be able to alter it. This is TBD
#' Currently WAY too slow. No point in even using unless improvements are made.
#' @param X is the training set features
#' @param y_vec is the labels for training set
#' @param X_test is the test set
#' @param Y_test is the is the labels for the training set
#' @param kernel is the kernel function used. The "..." are the extra parameters necessary for tuning the kernel (e.g. the variance for a gaussian kernel).
#' @keywords Kernel
#' @export
#' @examples
#'
#'
#send in X matrix being d x n matrix, i.e. dimensions as rows, samples as columns.
KernelizedPerceptron <- function(X, y_vec, X_test, Y_test, kernel, ...) {
  alphas = matrix(rep(0, ncol(X)), nrow = ncol(X), ncol = 1)

  for (t in 1:ncol(X)) {
    stepSize = 1/t
    y_hat = matrix(rep(0, nrow(X)), nrow = nrow(X), ncol = 1)
    for (j in 1:ncol(X)) {
      y_hat = y_hat + alphas[j]*y_vec[j]*kernel(X[,t],X[,j], ...)
    }
    y_hat = sign(y_hat)

    if (all(y_hat != y_vec[t])) {
      alphas[t] = alphas[t] + stepSize
    }
  }
  y = rep(0, ncol(X))

  #For the test set

  for (i in 1:length(y_vec)) {
    for (j in 1:length(y_vec)) {
      y[i] = y[i] + alphas[j]*y_vec[j]*kernel(X[,i],X[,j], ...)
    }
    y[i] = sign(y[i])
  }

  print("Accuracy for train is ")
  print(length(y[y == y_vec])/length(y_vec))

  y_test = rep(0, length(Y_test))
  for (i in 1:length(Y_test)) {
    for (j in 1:length(y_vec)) {
      y_test[i] = y_test[i] + alphas[j]*y_vec[j]*kernel(X[,j], X_test[,i], ...)
    }
    y_test[i] = sign(y_test[i])
  }
  print("Accuracy for test is ")
  print(length(y_test[y_test == Y_test])/length(Y_test))
  return(alphas)
}


#' testSVMaccuracy
#'
#' Given you gradient vector and a dataset, test how well it classifies the dataset.
#' Step size is set to 1/t, which is a common stepsize, but one should be able to alter it. This is TBD
#' Currently WAY too slow. No point in even using unless improvements are made.
#' @param w is the gradient vector
#' @param test is a matrix with the features.
#' @param Y_Test are the labels corresponding.
#' @keywords SVM
#' @export
#' @examples
#'

testSVMAccuracy <- function(w, test, Y_Test) {
  w = matrix(w, nrow = length(w), ncol = 1)
  #test the accuracy
  y_new = sign(t(w)%*%t(test))
  return(length(y_new[y_new == Y_Test])/length(Y_Test))
  amountEq = 0

  for (t in 1:ncol(y_new)) {
    if (Y_Test[t] == y_new[,t]) {
      amountEq = amountEq + 1
    }
  }
  return(amountEq/ncol(y_new))
}

