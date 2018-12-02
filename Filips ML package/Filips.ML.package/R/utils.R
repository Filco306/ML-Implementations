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

trainTestSplit <- function(dataSet, labels, testSetFraction) {
  
  smp_size <- floor((1 - testSetFraction) * nrow(dataSet))
  
  ## set the seed to make your partition reproducible
  set.seed(123)
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
#' @param splits List of partitions of the dataset.
#' @param splitIndex Instructs which partition to take. If it is -1, it will just take a random split. Otherwise, it will take the corresponding index in the list.
#' @param stdDev to standardize the vector with. If nothing is sent in, the sd of the feature vector will be used.
#' @keywords Cross Validation
#' @export
#' @examples
#' 
#' 
#'
CVtrainAndTestSet <- function(splits, splitIndex) {
  
  
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
