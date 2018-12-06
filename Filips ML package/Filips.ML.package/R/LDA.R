
#' DiscFunc
#'
#' Discriminant function used for LDA.
#' @param x is the data point to be scored.
#' @param cov_k is the cov matrix for class k. Is same for all classes in LDA
#' @param mu_k is the mean for class k.
#' @param prior is the prior for class k
#' @keywords LDA, binary, Discriminant function
#' @export
#' @examples
#'
#'
DiscFunc = function(x, cov_k, mu_k, prior) {
  x = as.matrix(x)
  mu_k = as.matrix(mu_k)
  express = t(x)%*%solve(cov_k)%*%mu_k -
    (1/2)%*%t(mu_k)%*%solve(cov_k)%*%mu_k + log(prior)
  return(express)
}



#' Binary LDA
#'
#' Does an LDA on a dataset. Returns the classifications of the test set
#' @param x1 contains all rows of the first class sent in of the training set
#' @param x2 contains all rows of the first class sent in of the training set
#' @param priors are the priors for the 2 classes
#' @param test is the test set to evaluate on.
#' @param classifications are the factor names, sent in as strings
#' @keywords LDA, binary
#' @export
#' @examples
#'
binaryLDA = function(x1, x2, priors, test, classifications = c("2","1")) {
  mu_1 = apply(x1, 2, mean)
  mu_2 = apply(x2, 2, mean)
  cov_1 = cov(x1)
  cov_2 = cov(x2)
  n1 = nrow(x1)
  n2 = nrow(x2)
  N = n1+n2
  covM = 1/N*(n1*cov_1 + n2*cov_2)
  classificationsTest = apply(test, 1, function(row) {
    class1 = DiscFunc(row, covM, mu_1, priors[1])
    class2 = DiscFunc(row, covM, mu_2, priors[2])
    return(which.max(c(class1,class2)))
  })
  classificationsTest = factor(ifelse(classificationsTest == 2, classifications[2], classifications[1]))

  # Is proportional to, so we do not get the constant needed.
  decisionBoundary = solve(covM)%*%(mu_2 - mu_1)

  return(classificationsTest)
}
