#' crossValidationSplits
#'
#' Splits the data into k partitions, which can later be used together with the function "CVtrainAndTestSet"
#'
#' @param dataSet is the set of data to split up into. Make sure samples are the rows and the columns are the features.
#' @param k is the number to splits to partition the data into.
#' @param seed is the seed used for the randomization
#' @keywords Cross validation
#' @export
#' @examples
#'
#'
#'

crossValidationSplits <- function(dataSet, k, seed = 123) {
  smp_size = floor(nrow(dataSet)/k)

  ## set the seed to make your partition reproducible
  set.seed(seed)

  folds <- list()

  for (i in 1:k) {
    newFold <- sample(seq_len(nrow(dataSet)), size = smp_size)
    folds[[i]] = data.frame(dataSet[newFold,])
    dataSet <- dataSet[-newFold,]
  }
  return(folds)

}


#' trainTestSplit
#'
#' Splits the data into a training and test set
#'
#' @param dataSet is the set of data to split up. Assumes data set does not contain labels but is split up separately.
#' @param labels is a vector containing the labels for the dataSet. Needs to have the same order and length as dataSet (of course).
#' @param testSetFraction
#' @keywords Cross validation
#' @export
#' @examples
#'
#'
#'

trainTestSplit <- function(dataSet, labels, testSetFraction, seed = 123) {

  smp_size <- floor((1 - testSetFraction) * nrow(dataSet))

  ## set the seed to make your partition reproducible
  set.seed(seed)
  train_ind <- sample(seq_len(nrow(dataSet)), size = smp_size)

  train <- dataSet[train_ind, ]
  test <- dataSet[-train_ind, ]

  trainLabels = labels[train_ind]
  testLabels = labels[-train_ind]
  normParam <- preProcess(train)

  train = predict(normParam,train)
  test = predict(normParam, test)

  train$label = NULL
  test$label = NULL

  train = matrix(c(train$x,train$y), nrow = nrow(train), ncol = 2)
  test = matrix(c(test$x, test$y), nrow = nrow(test), ncol = 2)

  return(list(test, train, trainLabels, testLabels))
}


#' CVtrainAndTestSet
#'
#' Returns a training set (k-1 folds) and a test set (1 fold) in a list.
#'
#' @param splits List of partitions of the dataset. Obtained in correct format through function crossValidationSplits.
#' @param splitIndex Instructs which partition to take. If it is -1, it will just take a random split. Otherwise, it will take the corresponding index in the list.
#' @param stdDev to standardize the vector with. If nothing is sent in, the sd of the feature vector will be used.
#' @keywords Cross Validation
#' @export
#' @examples
#'
#'
#'
CVtrainAndTestSet <- function(splits, splitIndex = -1) {


  if (splitIndex == -1 || splitIndex > length(splits)) {
    testIndex = floor(runif(1, min = 1, max = length(splits)))
    test = splits[[testIndex]]
    train = splits
    train[[testIndex]] = NULL
  } else {
    test = splits[[splitIndex]]
    train = splits
    train[[splitIndex]] = NULL
  }
  trainSet = train[[1]]
  for (i in 2:length(train)) {
    trainSet = rbind(trainSet, train[[i]])
  }

  return(list(trainSet,test))
}

#' standardizeFeatures
#'
#' Standardizes a vector of features with mu and sd. Not sure yet if correct, not validated yet.
#'
#' @param feature Vector of features to standardize according to its length
#' @param mu to standardize the vector with. If nothing is sent in, the mean of the feature vector will be used.
#' @param stdDev to standardize the vector with. If nothing is sent in, the sd of the feature vector will be used.
#' @keywords standardization
#' @export
#' @examples
#'
#'
#'
standardizeFeature <- function(feature, mu = NULL, stdDev = NULL) {

  if (is.null(mu) & is.null(stdDev)) {
    feat = (feature - mean(feature))/sd(feature)
  } else {
    feat = (feature - mu)/stdDev
  }


  return(feat)
}

#' rescaleFeature
#'
#' Standardizes a vector of features with mu and sd. Not sure yet if correct, not validated yet.
#'
#' @param feature Vector of scaled features to rescale
#' @param mu to rescale the vector with.
#' @param stdDev to rescale the vector with.
#' @keywords standardization
#' @export
#' @examples
#'
#'
#'
rescaleFeature = function(feature, mu, stdDev) {
  return(stdDev*scaledData + avg)
}

#' GenerateClusters
#'
#' Generates clusters. In other words, generates clusters out of sampling from a normal distribution, in N dimensions. The number in each cluster is also sampled from a normal distribution.
#'
#' @param avgSampPerCluster is the expected number of datapoints in one cluster
#' @param clusterVar is the variance used to sample the number of particles in each cluster.
#' @param nrClusters is the number of clusters desired.
#' @param featureMins is an N-dimensional vector, containing the smallest possible value for each feature
#' @param featuresMaxes is an N-dimensional vector, containing the biggest possible value for each feature
#' @keywords cluster
#' @export
#' @examples
#'
#'
#'
GenerateClusters = function(avgSampPerCluster, clusterVar, nrClusters, featureMins, featureMaxes, seed = 123) {
  set.seed(seed)
  df = data.frame(matrix(rep(0,length(featureMins)), ncol = length(featureMins), nrow = 1))
  names = colnames(df)
  for (i in 1:nrClusters) {
    means = as.numeric()
    vars = as.numeric()
    for (j in 1:length(featureMins)) {
      means[j] = runif(1, min = featureMins[j], max = featureMaxes[j])
      vars[j] = abs(featureMins[j] - featureMaxes[j])
    }
    nrInCluster = floor(rnorm(1, avgSampPerCluster, clusterVar))
    cluster = rmvnorm(nrInCluster, mean = means, sigma = diag(vars))
    colnames(cluster) = names
    df = rbind(df, cluster)
  }
  df = df[-c(1),]
  return(df)
}



#' Standardize data
#'
#' Standardize all data in a data frame or a matrix that are of type double. Assumes normal distribution on data.
#'
#' @param dataIn is a matrix or a dataframe sent in to be standardized.
#' @param typeOut specifies whether we want a dataframe or matrix out
#' @keywords cluster
#' @export
#' @examples
#'
#'
#'
standardizeData = function(dataIn, typeOut = "dataframe") {

  names = colnames(dataIn)

  dataframe = apply(dataIn, 2, function(col) {
    if (typeof(col) == "double") {
      return(standardizeFeature(col))
    } else {
      return(col)
    }

  })

  if (typeOut == "dataframe") {
    dataframe = data.frame(dataframe)
  }
  colnames(dataframe) = names
  return(dataframe)
}

#' True positive rate
#'
#' Calculates the true positive rate, given predictions and labels. Send in labels and predictions with 1 as true, 0 as false.
#'
#' @param predictions is the vector of predictions, with true labels sent in as 1, false as 0
#' @param labels is the vector of actual labels, with true labels sent in as 1, false as 0
#' @keywords TPR
#' @export
#' @examples
#'
#'
#'
TPR = function(predictions, labels) {
  N_plus = sum(labels == 1)
  labels = as.numeric(labels)
  return(sum(predictions == 1 & labels == predictions)/N_plus)
}



#' True positive rate
#'
#' Calculates the false positive rate, given predictions and labels. Send in labels and predictions with 1 as true, 0 as false.
#'
#' @param predictions is the vector of predictions, with true labels sent in as 1, false as 0
#' @param labels is the vector of actual labels, with true labels sent in as 1, false as 0
#' @keywords FPR
#' @export
#' @examples
#'
#'
#'
FPR = function(predictions, labels) {
  N_minus = sum(labels == 1)
  labels = as.numeric(labels)
  return(sum(predictions == 1 & labels != predictions)/N_minus)
}

#' Area Under Curve
#'
#' Calculates the area under the curve.
#'
#' @param TPR is a vector of true positive rates.
#' @param FPR is a vector of the false positive rates.
#' @keywords AUC
#' @export
#' @examples
#'
#'
#'
AUC = function(TPR, FPR) {
  # TPR is y, FPR is x
  # Order after FPR
  xInd = order(FPR)
  x = FPR[xInd]
  y = TPR[xInd]
  area = 0
  for (i in 2:length(TPR)) {
    area = (x[i]-x[i-1])*(y[i] + y[i-1])/2 + area
  }
  return(area)
}



#' Classification rate
#'
#' Calculates, out of a confusion matrix, the amount of correctly classified data points.
#'
#' @param confMatrix is the confusion matrix to calculate the classification rate for.
#' @keywords confusion matrix, accuracy
#' @export
#' @examples
#'
#'
#'
classificationRate = function(confMatrix) {
  return(sum(diag(confMatrix))/sum(confMatrix))
}
