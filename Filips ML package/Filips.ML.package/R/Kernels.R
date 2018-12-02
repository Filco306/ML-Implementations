#' Polynomial Kernel
#'
#' Returns (1 + x1*x2)^degree for features of any dimension
#' @param x1 is the first vector sent in. 
#' @param x2 is the second vector sent in. 
#' @param degree is the degree to which the
#' @keywords Kernel, Polynomial
#' @export
#' @examples
#' 
#'  

PolynomialKernel <- function(x1, x2, degree) {
  return((1 + x1*x2)^degree)
}

#' Polynomial Kernel
#'
#' Returns a gaussian kernel operation
#' @param x1 is the first vector sent in. 
#' @param x2 is the second vector sent in. 
#' @param h is the variance used in the operation.
#' @keywords Kernel, Gaussian
#' @export
#' @examples
#' 
#'  

GaussianKernel <- function (x1, x2, h) {
  return(exp(-(sum(norm(x1- x2, type="2")/(h^2)))))
}
