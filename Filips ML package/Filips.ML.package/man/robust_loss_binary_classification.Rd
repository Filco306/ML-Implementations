% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{robust_loss_binary_classification}
\alias{robust_loss_binary_classification}
\title{Robustness loss binary classification}
\usage{
robust_loss_binary_classification(probs_positive, y)
}
\arguments{
\item{probs_positive}{are the probabilities for a positive classification result, yielded by the model.}

\item{y}{are the actual predictions. Can be sent in as factors, but preferred that they are converted to 1 for positive, 0 for negative.}
}
\description{
Function used for binary classification to measure quality of the predictions. Gives an indication of the "robustness" of the model, i.e. if the model is in general confident on its predictions or the probabilities are rather centered around 0.5, making it a worse predictive model.
}
\keyword{binary,}
\keyword{classification}
\keyword{loss,}
\keyword{robustness,}
