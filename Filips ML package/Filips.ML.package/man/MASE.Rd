% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/error_measures.R
\name{MASE}
\alias{MASE}
\title{MASE}
\usage{
MASE(y, y_hat)
}
\arguments{
\item{y}{is a vector of the actual values.}

\item{y_hat}{is a vector of the predictions.}
}
\description{
Calculates the MASE value for predictions. The MASE value can be calculated as \deqn{(\Sigma |y[i] - y_hat[i]|)/(\Sigma |y_[t] - y[t-1]|)}
}
\details{
Due to the equation of the MASE, the prediction for the first point is not included.
NOT tested yet!
}
