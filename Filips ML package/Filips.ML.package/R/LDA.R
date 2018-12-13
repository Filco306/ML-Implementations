
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
#' Does an LDA on a dataset. Returns the classifications of the test set. Should perhaps be modified to return more information such as decision boundary, covM matrix etc, but left as TBD.
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

#' Multiclass LDA
#'
#' Does an LDA on a dataset with several different classes. Returns the classifications of the test set. Manages to do the exact same thing as lda() from MASS, except it already predicts. Should perhaps be modified to return more information such as decision boundaries, covM matrix etc, but left as TBD.
#' @param train is a dataframe containing the features we are interested to try out for. Do not send in the labels in this dataframe.
#' @param labelsTrain is a vector containing the labels for the training set
#' @param priors are the priors for the 2 classes
#' @param validation is the test set to evaluate on. Do not send in the labels in this dataframe.
#' @param classifications are the factor names, sent in as strings
#' @keywords LDA, multiclass
#' @export
#' @examples
#'
multiclassLDA = function(train, labelsTrain, priors, validation) {
  # Create means for each class
  levelsClasses = levels(labelsTrain)
  # Will return means for each feature for each class.

  mus = apply(as.matrix(levelsClasses), 1, function(class) {

    return(apply(train, 2, function(feature) {
      return(mean(feature[factor(class, levels = levelsClasses) == labelsTrain]))
    }))
  })

  # Extract covariance matrix for each class. Returns the covariance matrices in columns for each class.
  covs = apply(as.matrix(levelsClasses), 1, function(class) {
    return(cov(train[class == labelsTrain,]))
  })

  nC = apply(as.matrix(levelsClasses), 1, function(class) {
    return(sum(class == labelsTrain))
  })
  N = sum(nC)
  covMM = 1/N*t(as.matrix(nC))%*%t(covs)

  # Now convert the covM to the dxd matrix it should be
  d = ncol(train)
  covM = matrix(covMM, nrow = d, ncol = d)
  # Now we have a nice matrix.

  # Now the fun begins

  classificationsTest = apply(validation, 1, function(x) {
    # For each class, get the disc func value
    classes = as.numeric()
    for (i in 1:ncol(mus)) {

      classes[i] = DiscFunc(x = x, cov_k = covM, mu_k = mus[,i], prior = priors[i])
    }

    return(which.max(classes))
  })
  classificationsTest = factor(apply(as.matrix(classificationsTest), 1, function(lab) {

    return(levelsClasses[lab])
  }), levels = levelsClasses)

  # Is proportional to, so we do not get the constant needed.
  # TODO: Fix decision boundaries
  #decisionBoundary = solve(covM)%*%(mu_2 - mu_1)

  return(classificationsTest)
}
