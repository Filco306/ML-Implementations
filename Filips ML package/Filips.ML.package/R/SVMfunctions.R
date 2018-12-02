#' Stochastic Gradient Descent
#'
#' This function is a very simple implementation of stochastic gradient descent. 
#' Step size is set to 1/sqrt(t), which is a common stepsize, but one should be able to alter it. This is TBD
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
  w = as.matrix(wInit)
  
  for (t in 1:nrow(data)) {
    stepSize = 1/sqrt(t)
    w[,1] = w[,1] - stepSize*trainLabels[t]*data[t,]
    w = w*min(1, 1/(t(w)%*%w))
    norm = c(w[2,], - w[1,])
    
    abline(c(norm[2], - norm[1]))
  }
  return(w) 
}



#' Perceptron Stochastic Gradient Descent
#'
#' This function is a very simple implementation of stochastic gradient descent. 
#' Step size is set to 1/t, which is a common stepsize, but one should be able to alter it. This is TBD
#' @param wInitVals are the initial values of your vector
#' @param data is the features, i.e. the X vector. 
#' @param trainLabels is the y vector
#' @keywords Stochastic Gradient Descent
#' @export
#' @examples
#' 
#' trainLabels = y
#' data = X
#' w = PerceptronSGD(rnorm(nrow(data), mean = 0, sd = 1), data, trainLabels = y)
#' 
PerceptronSGD <- function (wInitVals, data, trainLabels) {
  w = matrix(wInitVals, nrow = length(wInitVals), ncol = 1)
  
  for (t in 1:nrow(data)) {
    
    if (trainLabels[t] != sign(w[,1]*data[t,])) {
      stepSize = 1/t
      w[,1] = w[,1] - stepSize*trainLabels[t]*data[t,]
      w = w*min(1, 1/(t(w)%*%w))
    }
  }
  return(w) 
}

#' Adagrad
#'
#' TBD
Adagrad = function(wInitVals, data, labels) {
  return(1)
}

#' ADAM
#'
#' TBD
ADAM = function(wInitVals, data, labels) {
  return(1)
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
      print("Came in here")
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


#' Kernelized Perceptron
#'
#' This function is a very simple implementation of a kernelized perceptron
#' Step size is set to 1/t, which is a common stepsize, but one should be able to alter it. This is TBD
#' Currently WAY too slow. No point in even using unless improvements are made. 
#' @param w is the gradient vector
#' @param test is a frame or matrix with the features.
#' @param Y_Test are the labels corresponding. 
#' @keywords SVM
#' @export
#' @examples
#' 
#' 
#' 
#' 
#' 

testSVMAccuracy <- function(w, test, Y_Test) {
  w = matrix(w, nrow = length(w), ncol = 1)
  #test the accuracy
  y_new = sign(t(w)%*%t(test))
  
  amountEq = 0
  
  for (t in 1:ncol(y_new)) {
    if (Y_Test[t] == y_new[,t]) {
      amountEq = amountEq + 1
    } 
  }
  return(amountEq/ncol(y_new))
}

