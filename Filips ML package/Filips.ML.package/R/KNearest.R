#' K-nearest neighbours classification
#'
#' Returns the classifications of the test set.
#' @param train is the training set
#' @param K is the k in k-nearest neighbours. That is, the algorithm will consider the k nearest neighbours to the point in each iteration.
#' @param test is the test set
#' @param labelsTrain are the labels of the training set. Not really used at the moment in the function. TBD.
#' @param labelsTest are the labels of the test set.
#' @keywords Volume, sphere
#' @export
#' @examples
#'
#'
knearest = function(train, K, test, labelsTrain, labelsTest, pLimit = 0.5) {
  #data is the training data,
  #test returns the predicted class probabilities
  # for test by using K-nearest neighbour approach.
  responsesX = labelsTrain
  responsesY = labelsTest

  X = as.matrix(train)
  Y = as.matrix(test)


  Xhat = X/sqrt(rowSums(X^2))
  Yhat = Y/sqrt(rowSums(Y^2))


  C_matrix = Xhat%*%t(Yhat)

  D_matrix = 1 - C_matrix
  # For the data test set,
  # take the distance to the training data points,
  # take the k nearest neighbours and classify them
  # as the average of the training set's labels of the k points

  # return the indices of the k nearest neighbours
  # for all different new data points, generating a matrix with the k nearest neighbou
  indicesKnearest = apply(D_matrix, 2, function(col, k){
    names(col) = seq(1, length(col), 1)

    col = sort(col)

    return(strtoi(names(col[1:k])))
  }, K)

  classifications = factor(apply(indicesKnearest, 2, function(row) {
    # Classify according to the mean of the training data
    avg = mean(responsesX[row,])
    return(ifelse(avg >= pLimit, "1", "0"))
  }))


  return(classifications)
}

#' Volume of an n-dimensional sphere
#'
#' Returns the volume of an n-dimensional sphere
#' @param nDim is the number of dimensions
#' @param radius is the radius of the sphere, given in an absolute value.
#' @keywords Volume, sphere
#' @export
#' @examples
#'
#'
volume_N_sphere = function(nDim, radius) {
  n = nDim
  R = radius
  return(((pi^(n/2))/gamma(n/2 + 1))*R^n)
}


#' k-nearest density estimation
#'
#' Returns a density estimation over the grid of new data.
#' @param data is the data given.
#' @param k
#' @param newData is the data to perform the density estimation over. Can in two dimensions be just a sequence of values (for example)
#' @keywords k-nearest, density estimation
#' @export
#' @examples
#'
#'
kNearestNeighDensEstimation = function(data, k, newData) {
  # data is the training data
  # newData is the data

  N = nrow(data)

  nDim = ncol(data)
  X = as.matrix(data) # We do not assume responses to exist
  Y = as.matrix(newData) # We do not assume responses to exist




  D_matrix = abs(apply(Y, 1, function(row) {
    return(abs(row - X))
  }))
  # return the indices of the k+1 nearest neighbours
  # for all different new data points, generating a matrix with the k nearest neighbours
  # Since we include the point itself, we return k+1 nearest radiuses.


  radiuses = apply(D_matrix, 2, function(col, k) {


    sortedCol = sort(col)


    return(sortedCol[k])
  }, k+1)

  # Making sure no radius is 0
  for (i in 1:length(radiuses)) {
    if (radiuses[i] == 0) {
      #If it is 0, do linear extrapolation
      radiuses[i] = (radiuses[i-1] + radiuses[i+1])/2
    }
  }

  #Density estimation
  densities = apply(as.matrix(radiuses), 1, function(R, nDim) {
    return(k/(N*volume_N_sphere(nDim, R)))
  }, nDim)


  return(densities)
}
