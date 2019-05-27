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

#' Gaussian Kernel
#'
#' Returns a gaussian kernel operation
#' @param x1 is the first datapoint.
#' @param x2 is the second datapoint.
#' @param h is the variance used in the operation.
#' @param sigmaF is the \eqn{\sigma} before.
#' @keywords Kernel, Gaussian
#' @export
#' @examples
#'
#'

GaussianKernel <- function (x1, x2, h, sigmaF = 1) {
  return((sigmaF^2)*exp(-(sum(norm(x1- x2, type="2")/(h^2)))))
}

#' Periodic + Gaussian Kernel
#'
#' Returns a mix of a periodic and a gaussian kernel. This kernel will be \deqn{(sigmaF^2)*exp(-2*((sin(pi*abs(x1-x2)/d)^2)/(l1^2)))*exp(-0.5*((x1 - x2)^2)/(l2^2))}
#' @param sigmaF is the constant before.
#' @param x1 is the first datapoint.
#' @param x2 is the second datapoint.
#' @param l1 is the lengthscale for the periodic kernel.
#' @param l2 is the lengthscale for the gaussian kernel.
#' @param d is for the periodic kernel.
#' @keywords Kernel, Gaussian
#' @export
#' @examples
P_G_Kernel = function(sigmaF = 1,x1,x2, l1, l2, d) {
  return((sigmaF^2)*exp(-2*((sin(pi*abs(x1-x2)/d)^2)/(l1^2)))*exp(-0.5*((x1 - x2)^2)/(l2^2)))
}



#' Periodic  Kernel
#'
#' Returns a periodic kernel. This kernel will be \deqn{(sigmaF^2)*exp(-2*((sin(pi*abs(x1-x2)/d)^2)/(l1^2)))}
#' @param x1 is the first datapoint.
#' @param x2 is the second datapoint.
#' @param l is the lengthscale for the periodic kernel.
#' @param d is for the periodic kernel.
#' @param sigmaF is the constant before.
#' @keywords Kernel, Gaussian
#' @export
#' @examples
Periodic_Kernel = function(x1,x2, l, d, sigmaF = 1) {
  return((sigmaF^2)*exp(-2*((sin(pi*abs(x1-x2)/d)^2)/(l^2))))
}
