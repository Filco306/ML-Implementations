
#' Lloyd's algorithm
#'
#' Implementation of Lloyd's algorithm. Solves the k-means problem.
#'
#' @param k is the number of clusters
#' @param features is the data sent in. Send in as a data frame or matrix, but ensure to send in data points as rows and features as columns.
#' @param seed used to set the seed for initializing. Default set to 123.
#' @param convergenceCheckType can be "No Change" or "Threshold". If none of these, it will never converge. If "Threshold", algorithm will stop when maximum change of any mu in any direction is lower than the threshold. If "No Change", the algorithm will stop when no change of assignment of points to mus has been done during an iteration.
#' @param threshold used if convergenceCheckType = "Threshold". Algorithm will stop when maximum change of any mu in any direction is lower than the threshold.
#' @keywords cluster, k-means
#' @export
#' @examples
#'
#'
#'
LLoydsAlgorithm = function(k, features, seed = 123, convergenceCheckType = "No Change", threshold = 10^(-10)) {
  X = as.matrix(features)
  # N = number of data points in data set.
  N = nrow(features)
  # Initialize assignments uniformly at randomly
  set.seed(seed)
  assignments = sample(x = seq(1,k,1), replace = TRUE, size = N)
  # initialize mu based on random assignments
  mus = apply(as.matrix(seq(1,k,1)), 1, function(k_i, X) {
    return((1/length(X[assignments == k_i,]))*colSums(X[assignments == k_i,]))
  }, X)

  #While not converged
  #Assign each point xi to closest center
  #Update center as mean of assigned data points
  converged = FALSE
  while (converged == FALSE) {
    converged = TRUE
    DistanceMatrix = EuclidDistanceMatrix(X, t(mus))
    for (i in 1:N) {
      closestMu = which.min(DistanceMatrix[i,])
      if (closestMu != assignments[i]) {
        assignments[i] = closestMu
        if (convergenceCheckType == "No Change") {
          print("Here")
          converged = FALSE
        }
      }
    }
    # Recalculate mus
    prevMus = mus
    mus = apply(as.matrix(seq(1,k,1)), 1, function(index) {
      return((1/nrow(X[assignments == index,]))*colSums(X[assignments == index,]))
    })

    diff = max(abs((prevMus - mus)^2))

    if (convergenceCheckType == "Threshold" && threshold < diff) {
      converged = FALSE
    }

  }
  return(t(mus))
}

#' EuclidDistanceMatrix
#'
#' Returns the distance of two matrices with dimensions N x d and M x d in the shape N x M
#' @param X is the matrix that will be on the rows in the distance matrix (dim = N x d)
#' @param Y is the matrix that will be on the columns in the distance matrix (dim = M x d)
#' @keywords Euclidian, Distance
#' @export
#' @examples
#' DistanceMatrix = EuclidDistanceMatrix(X,Y)
#'
#'
EuclidDistanceMatrix = function(X, Y) {
  Xhat = X/sqrt(rowSums(X^2))
  Yhat = Y/sqrt(rowSums(Y^2))
  C_matrix = Xhat%*%t(Yhat)
  return(1 - C_matrix)
}


#' Online k-means
#'
#' Implementation of K-means online algorithm. Although it does not receive point by point, it follows the same principle as k-means online. However, not really the best algorithm perhaps. Everything depends on the learning rate.
#'
#' @param k is the number of clusters
#' @param features is the data sent in. Send in as a data frame or matrix, but ensure to send in data points as rows and features as columns.
#' @param seed used to set the seed for initializing. Default set to 123.
#' @param learnRateThreshold is the threshold of learning rate when to stop.
#' @param learnRateFunc is the function calculating the learning rate. Set to function cDivT (check package)
#' @keywords cluster, k-means
#' @export
#' @examples
#'
#'
#'
KMeansOnline = function(k, features, seed = 123, learnRateThreshold = 10^(-6), learnRateFunc = cDivT, ...) {
  X = as.matrix(features)
  # N = number of data points in data set.
  N = nrow(features)
  # Initialize assignments uniformly at randomly
  set.seed(seed)
  assignments = sample(x = seq(1,k,1), replace = TRUE, size = N)
  # initialize mu based on random assignments
  mus = t(apply(as.matrix(seq(1,k,1)), 1, function(k_i, X) {
    return((1/length(X[assignments == k_i,]))*colSums(X[assignments == k_i,]))
  }, X))
  t = 1
  dataRow = 1
  while (learnRateFunc(t,...) > learnRateThreshold) {

    mu_chosen = which.min(apply(mus, 1, function(mu, dataRow) {
      return(sum((mu - dataRow)^2))
    }, X[dataRow,]))
    oldMu = mus[mu_chosen, ]
    mus[mu_chosen, ] = oldMu + learnRateFunc(t, ...)*(X[dataRow,] - oldMu)



    if (dataRow >= nrow(X)) {
      dataRow = 1
    } else {
      dataRow = dataRow + 1
    }
    t = t + 1
    print(learnRateFunc(t,...))
  }
  return(mus)
}
