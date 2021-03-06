% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/KMeans.R
\name{EuclidDistanceMatrix}
\alias{EuclidDistanceMatrix}
\title{EuclidDistanceMatrix}
\usage{
EuclidDistanceMatrix(X, Y)
}
\arguments{
\item{X}{is the matrix that will be on the rows in the distance matrix (dim = N x d)}

\item{Y}{is the matrix that will be on the columns in the distance matrix (dim = M x d)}
}
\description{
Returns the distance of two matrices with dimensions N x d and M x d in the shape N x M
}
\examples{
DistanceMatrix = EuclidDistanceMatrix(X,Y)


}
\keyword{Distance}
\keyword{Euclidian,}
