% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayesian_inference.R
\name{posterior_mode}
\alias{posterior_mode}
\title{Posterior mode}
\usage{
posterior_mode(samples, exclude_zeros = TRUE)
}
\arguments{
\item{samples}{is the vector of samples used to estimate.}

\item{exclude_zeros}{is a boolean specifying whether the zeros should be excluded for the inference. This should be true (which is default) when doing posterior mode for coefficients .}
}
\description{
Returns the posterior mode point estimate, given a vector of samples.
}
