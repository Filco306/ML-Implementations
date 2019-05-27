#Constants is a vector with (degree + 1) constans, i.e. a + bx + cx^2 for degree = 2


#Just samples from a normal distribution, but will build in to make it possible to sample with noise from any distribution


#' Generate 2-dim Sample-data from polynomial
#' 
#' Generates 2-dimensional according to a polynomial
#' @param Constants is a vector with (degree + 1) constans, i.e. a + bx + cx^2 for degree = 2
#' @param xSequence is the x interval on which the dots are generated; all dots included. 
#' @param var is the variance of the noise.
#' @param xMin is the minimum x-value. 
#' @param xMax is the maximum x-value. 
#' @param integers specifies whether only to generate integers or not. Default \code{FALSE}.
#' @param seed for reproducability. Default 123. 
#' @export
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