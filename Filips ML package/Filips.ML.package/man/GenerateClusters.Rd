% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{GenerateClusters}
\alias{GenerateClusters}
\title{GenerateClusters}
\usage{
GenerateClusters(avgSampPerCluster, clusterVar, nrClusters, featureMins,
  featureMaxes, seed = NULL)
}
\arguments{
\item{avgSampPerCluster}{is the expected number of datapoints in one cluster}

\item{clusterVar}{is the variance used to sample the number of particles in each cluster.}

\item{nrClusters}{is the number of clusters desired.}

\item{featureMins}{is an N-dimensional vector, containing the smallest possible value for each feature}

\item{featuresMaxes}{is an N-dimensional vector, containing the biggest possible value for each feature}
}
\description{
Generates clusters. In other words, generates clusters out of sampling from a normal distribution, in N dimensions. The number in each cluster is also sampled from a normal distribution.
}
\examples{



}
\keyword{cluster}
