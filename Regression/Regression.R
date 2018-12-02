# Create an R package? https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/

#Send in a data frame and which degree to convert it to. 
#Returned is a data frame with all features in specified polynomial degree
#does not return features multiplied with each other
transformFromLin <- function(data, degree) {
  
  if(is.null(dim(data))) {
    data = data.frame(data)
  }
  
  orgNrRows = nrow(data)
  
  frameToReturn = data.frame()
  
  newFrame = apply(data, 2, function(column, polyDeg) {
    
    a = matrix(rep(0, length(column)*polyDeg), nrow=length(column), ncol = polyDeg)
    for (i in 1:(polyDeg)) {
      a[,i] = column^i
    }
    
    return(a)
  }, degree)
  
  
  finalNewFrame = data.frame(matrix(rep(1, (ncol(newFrame)*degree +1)*nrow(newFrame)), nrow=nrow(newFrame), ncol=(ncol(newFrame)*degree+1)))
  
  colNames = colnames(data)
  names(finalNewFrame)[1] = paste("Const")
  for (i in 1:ncol(newFrame)) {
    
    for (j in 1:degree) {
      finalNewFrame[(i-1)*degree + j+1] = newFrame[(((j-1)*nrow(newFrame)/degree)+1):(j*nrow(newFrame)/degree),i]
      names(finalNewFrame)[(i-1)*degree + j+1] = paste(colNames[i],"^",j)
      
    }
    
  }
  
  finalNewFrame = finalNewFrame[1:orgNrRows,]
  
  return(finalNewFrame)
  
}

closedFormSolution <- function(X, y) {
  if (is.null(dim(X))) { #if X is just a vector
    X = as.matrix(X)
  }
  if (is.null(y)) {
    y = as.matrix(y)
  }
  w = solve(t(X) %*% as.matrix(X)) %*% t(X) %*%y
  return(w)
}

CalculateError = function(w, X, y) {
  n = length(y)
  if (is.null(dim(w))) {
    w = matrix(w, nrow = length(w), ncol = 1)
  } 
  
  if (is.null(dim(y))) {
    y = matrix(y, nrow = length(y), ncol = 1)
  }
  
  
  error = sum((y - t(t(w) %*% X))^2)/n
  return(error)
}


#Constants is a vector with (degree + 1) constans, i.e. a + bx + cx^2 for degree = 2
#xSequence is the x interval on which the dots are generated; all dots included
# var is the variance of the noise
#Just samples from a normal distribution, but will build in to make it possible to sample with noise from any distribution
Generate2DimSampleData <- function(n, constants, xMin, xMax, var, integers = FALSE, seed = 123) {
  library(mvtnorm)
  y = as.numeric()
  
  set.seed(seed)
  if (integers == TRUE) {
    xSequence = seq(xMin, xMax, 1)
    X = sample(x = xSequence, replace = TRUE, size = n)
  } else {
    X = runif(n, xMin, xMax)
  }
  xSeq = transformFromLin(data.frame(X), length(constants) - 1)
  
  ySeq = constants*t(xSeq)
  ySeq = apply(ySeq, 2, function(column) {
    return(sum(column))
  })
  
  y = rmvnorm(1, mean = ySeq, sigma = diag(var, length(ySeq), length(ySeq)))
  y = t(y)
  frame = data.frame(X, y)
  print("Best fit (for polynomial used) is ")
  error = CalculateError(constants, t(xSeq), y)
  print(error)
  
  return(frame)
}

RegressionGradient <- function (w, X, y) {
  y = as.matrix(y)
  X = as.matrix(X)
  
  gradient = t(-2*(y - t(w %*% t(X)))) %*% X
  
  gradient = apply(gradient, 2, function(col) {
    return(sum(col))
  })

  return(gradient)
}

# TBD. Not functioning well at the moment. 
SGD <- function(w, X, y, gradient, ...) {
  
  w = t(as.matrix(w))
  
  for (t in 1:length(y)) {
    stepSize = 1/t
    print(w)
    w[,1] = w[,1] - stepSize*gradient(...)
    
    
  }
  
  return(w)
}

#Here ends the functions and starts the testing

library(mvtnorm)

constants = c(6, -2, -12, 2, 1)

xMin = -10
xMax = 10
data = Generate2DimSampleData(n = 400, constants, xMin = xMin, xMax = xMax, var = 1000000)
plot(data$X, data$y)
wInit = t(rnorm(2))
dataPoints = transformFromLin(data = data$X, 1)
w = SGD(wInit, dataPoints, data$y, gradient = RegressionGradient, wInit, dataPoints, data$y)

for (i in 1:8) {
 dataPoints = transformFromLin(data = data$X, i)
 w = as.matrix(closedFormSolution(dataPoints, data$y)) #returns w vector from closed form
 wInit = rmvnorm(1, mean = rep(0, i+1), sigma = diag(1, i+1))
 
 lineToDraw = t(w) %*% t(transformFromLin(data.frame(seq(xMin, xMax, 0.1)), i))
 colors = c("red", "green", "blue", "yellow", "purple", "green", "gray", "black")
 Sys.sleep(1.5)
 lines(x = seq(xMin, xMax, 0.1), y = lineToDraw, col = colors[i])
 print(paste("Error is ",CalculateError(w = w, X = t(dataPoints), y = data$y)))
 
}





#FeatureOne = rnorm(10, 5,1)
#FeatureTwo = rnorm(10, 5,1)
#FeatureThree = rnorm(10, 5,1)

#frame = data.frame(FeatureOne, FeatureTwo, FeatureThree)

#polyTransformed = transformFromLin(frame, 3)
