% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SVMfunctions.R
\name{SGD_SVM}
\alias{SGD_SVM}
\title{Stochastic Gradient Descent}
\usage{
SGD_SVM(wInitVals, data, y)
}
\arguments{
\item{wInitVals}{are the initial values of your vector}

\item{data}{is the features, i.e. the X vector.}

\item{y}{is the y vector, with labels}
}
\description{
This function is a very simple implementation of stochastic gradient descent, used for SVM:s.
Step size is set to 1/sqrt(t), which is a common stepsize, but one should be able to alter it.
}
\examples{

trainLabels = y
data = X
w = SGD(rnorm(nrow(data), mean = 0, sd = 1), data, trainLabels = y)

}
\keyword{Descent}
\keyword{Gradient}
\keyword{Stochastic}
