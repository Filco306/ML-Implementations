% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Kernels.R
\name{Periodic_Kernel}
\alias{Periodic_Kernel}
\title{Periodic  Kernel}
\usage{
Periodic_Kernel(x1, x2, l, d, sigmaF = 1)
}
\arguments{
\item{x1}{is the first datapoint.}

\item{x2}{is the second datapoint.}

\item{l}{is the lengthscale for the periodic kernel.}

\item{d}{is for the periodic kernel.}

\item{sigmaF}{is the constant before.}
}
\description{
Returns a periodic kernel. This kernel will be \deqn{(sigmaF^2)*exp(-2*((sin(pi*abs(x1-x2)/d)^2)/(l1^2)))}
}
\keyword{Gaussian}
\keyword{Kernel,}
