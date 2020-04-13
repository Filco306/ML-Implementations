#' Multinomial Naive Bayes
#'
#' This function creates an implementation of NB. The equation reads \deqn{p(y[i] == 1|x[i]) = (\Pi[j] p(y|x[i,j]))*p(y[i] == 1)}
#'
#' @param X should be a data frame. Should only contain categorical features (factors preferrably) the will be used for inference.
#' @param y are the labels.
#' @param priors are the priors to use for the classes. Default is "weighted", meaning the proportions of each class in the training set.
#' @keywords Naive, Bayes, NB
#' @export
#' @examples
#' library("bnlearn")
#' data(asia)
#' asia
#' MNB = MultinomialNB(X = asia[,1:7], y = asia[,8])
#' predict(MNB, asia[,1:7])
#' t(predict(MNB, asia[,1:7], ret_probabilities = T))
MultinomialNB = function(X, y, priors = "weighted") {
  thisEnv <- environment()

  y = as.factor(y)
  n = nrow(X)

  NB_model = list(class = "MultinomialNB",
                              target_variable = y,
                              predictors = X,
                              getEnv = function()
                              {
                                return(get("thisEnv",thisEnv))
                              })


  if (length(y) != nrow(X)) {
    stop("Stop. Labels are not as many as the number of features. ")
  }


  prob_tables = list()
  levels_cols = list()
  for (i in 1:ncol(X)) {
    prob_tables[[i]] = table(X[,i],y)/n
    names(dimnames(prob_tables[[i]])) = c(colnames(X)[i],"Response variable")
    levels_cols[[i]] = levels(X[,i])
  }
  names(prob_tables) = colnames(X)
  NB_model$probability_tables = prob_tables
  NB_model$levels_cols = levels_cols

  if (is.null(priors)) {
    NB_model$priors = NULL
  } else if (!is.null(priors)) {
    if (priors == "weighted") {
      NB_model$priors = as.numeric(table(y))/n
    } else {
      NB_model$priors = priors
    }
  }

  NB_model$levels_response = levels(y)


  assign('this',NB_model, thisEnv)

  class(NB_model) = append(class(NB_model), "MultinomialNB")
  NB_model$train_preds = predict(NB_model, X)
  NB_model$train_probs = predict(NB_model, X, ret_probabilities = T)
  return(NB_model)
}


#' Predict MultinomialNB
#'
#' Predictor for multinomial naive bayes.
#' @param modelObject is the multinomial naive bayes with which to predict.
#' @param X is the new data for which to predict. Should have same format as original data frame
#' @param ret_probabilities is a boolean deciding whether to return probabilities or class predictions. Default \code{FALSE}.
#' @export
predict.MultinomialNB = function(modelObject, X, ret_probabilities = F) {
  return(apply(X, 1, function(data_point) {
    probs = rep(0,times=length(modelObject$levels_response))
    for (i in 1:length(data_point)){
      if (!is.null(modelObject$probability_tables[names(data_point)[i]][[1]][data_point[i],])) {
        probs = probs + log1p(modelObject$probability_tables[names(data_point)[i]][[1]][data_point[i],])
      }
    }
    if (!is.null(modelObject$priors)) {
      probs = probs + log1p(modelObject$priors)
    }
    if (ret_probabilities == F) {
      return(modelObject$levels_response[which.max(probs)])
    } else {return(expm1(probs)/sum(expm1(probs)))}


  }))
}


#' Gaussian Naive Bayes
#'
#' Implement a Gaussian Naive Bayes.
#' @param X is the vector of features.
#' @param y is the response variable.
#' @param priors are priors used.
#' @export
#' @examples
#' library(gclus)
#' data("wine")
#' wine
#' GNB = GaussianNaiveBayes(wine[,-c(1)], wine$Class)
#' table(wine$Class, fitted(GNB))
#' t(fitted(GNB, ret_probabilities = T))
GaussianNaiveBayes = function(X, y, priors = "weighted") {
  y = as.factor(y)
  n = nrow(X)
  thisEnv <- environment()
  NB_model = list(class = "GaussianNB",
                  target_variable = y,
                  predictors = X,
                  getEnv = function()
                  {
                    return(get("thisEnv",thisEnv))
                  })


  if (length(y) != nrow(X)) {
    stop("Stop. Labels are not as many as the number of features. ")
  }

  classes = levels(y)

  means_and_sds = list()
  for (i in 1:ncol(X)) {
    #matr = matrix(NA, nrow = length(classes), ncol = 2)
    matr = t(apply(as.matrix(classes), 1, function(clas) {
      return(c(mean(X[y == clas,i]), sd(X[y == clas,i])))
    }))
    rownames(matr) = classes
    colnames(matr) = c("mean", "stdev")
    means_and_sds[[i]] = matr
  }

  names(means_and_sds) = colnames(X)


  NB_model$means_sds = means_and_sds


  if (is.null(priors)) {
    NB_model$priors = NULL
  } else if (!is.null(priors)) {
    if (priors == "weighted") {
      NB_model$priors = as.numeric(table(y))/n
    } else {
      NB_model$priors = priors
    }
  }

  NB_model$levels_response = levels(y)


  assign('this',NB_model, thisEnv)

  class(NB_model) = append(class(NB_model), "GaussianNB")
  NB_model$train_preds = predict(NB_model, X)
  NB_model$train_probs = predict(NB_model, X, ret_probabilities = T)
  return(NB_model)
}

#' Predict GaussianNB
#'
#' Predicts for a Gaussian Naive Bayes model.
#' @param modelObj is the model itself, the \code{GaussianNaiveBayes} model.
#' @param X is the matrix/dataframe of features.
#' @param ret_probabilities is a boolean deciding whether to return probabilities or class predictions. Default \code{FALSE}.
#' @export
predict.GaussianNB = function(modelObj, X, ret_probabilities = F) {
  probs = matrix(rep(1, length(modelObj$levels_response)*nrow(X)),ncol=length(modelObj$levels_response),nrow=nrow(X))
  colnames(probs) = modelObj$levels_response
  for (clas in modelObj$levels_response) {
    for (i in 1:ncol(X)) {
      probs[,clas] = probs[,clas]*dnorm(X[,i], mean=modelObj$means_sds[colnames(X)[i]][[1]][clas,1], sd=modelObj$means_sds[colnames(X)[i]][[1]][clas,2])
    }
    probs[,which(clas == modelObj$levels_response)] = probs[,which(clas == modelObj$levels_response)]*modelObj$priors[which(clas == modelObj$levels_response)]
  }

  if (!ret_probabilities) {
    return(factor(apply(probs, 1, function(x) {
      return(modelObj$levels_response[which.max(x)])
    }), levels = modelObj$levels_response))
  } else {
    return(apply(probs,1,function(x){return(x/sum(x))}))
  }

}

#' Fitted values GaussianNB
#'
#' Returns the fitted values for a Gaussian Naive Bayes model.
#' @param modelObj is the model itself, the \code{GaussianNaiveBayes} model.
#' @export
fitted.GaussianNB = function(modelObj, ret_probabilities = F) {
  if (!ret_probabilities) {
    return(modelObj$train_preds)
  } else {
    return(modelObj$train_probs)
  }

}


#' Gaussian inference Naive Bayes
#'
#' Internal function to calculate names.
#' @param X is the data matrix to predict for.
#' @param i is the column index.
#' @param classes are the response variable classes.
#' @keywords internal
#' @export
gauss_inf_NB = function(X, i, classes) {
  matr = t(apply(as.matrix(classes), 1, function(clas) {
    return(c(mean(X[y == clas,i]), sd(X[y == clas,i])))
  }))
  rownames(matr) = classes
  colnames(matr) = c("mean", "stdev")
  return(matr)
}

#' Inference Multinomial Naive Bayes
#'
#' Internal function to calculate names.
#' @param X is the data matrix to predict for.
#' @param i is the column index.
#' @param classes are the response variable classes.
#' @keywords internal
#' @export
multinomial_table = function(X, i, classes) {
  table_  = table(X[,i],y)/nrow(X)
  names(dimnames(table_)) = c(colnames(X)[i],"Response variable")
  return(table_)
}


#' Naive Bayes
#'
#' A mixture between Gaussian Naive Bayes and Multinomial Naive Bayes, depending on which class the feature's column is.
#' @param X is the feature matrix.
#' @param y is the response variable
#' @param priors are the priors to use for the classes. Default is "weighted", meaning the proportions of each class in the training set. Can also be \code{NULL}.
#' @param factor_max_length is used if the column is of class integer. If the number of different integers in a column is smaller than \code{factor_max_length}, it will be converted to a factor and a table will be constructed. Otherwise it will be a \code{GaussianNB}.
#' @export
#' @examples
#' library("bnlearn")
#' X = clgaussian.test[,-c(1)]
#' y = clgaussian.test[,1]
#' X$C = as.integer(X$C)
#' X$G = as.integer(X$G)
#' NB_ = NB(X, y)
#' table(y,fitted(NB_))
#' t(fitted(NB_, ret_probabilities = T))
#' # See how priors changes accuracy and incorrectly classified
#' priors = t(apply(as.matrix(seq(0,1,length=100)), 1, function(x) c(x, 1 - x)))
#'
#' res = t(apply(priors, 1, function(priors_) {
#'  NB_ = NB(X, y, priors = priors_)
#'  print(priors_)
#'  accuracy = sum(diag(table(y, fitted(NB_))))/sum(table(y, fitted(NB_)))
#'  a_b = table(Actual = y, Predicted = fitted(NB_))[1,2]
#'  b_a = table(Actual = y, Predicted = fitted(NB_))[2,1]
#'  return(c(a_b, b_a, accuracy))
#' }))
#' par(mfrow=c(2,1))
#' plot(x=priors[,1],y=res[,1], type = "l", lwd = 3, col = "blue", ylim = c(0, max(res[,1:2])))
#' lines(x=priors[,1],res[,2], lwd = 3, col = "red")
#' abline(v=sum(y == "a")/length(y), lwd = 3)
#' plot(x = priors[,1], y=res[,3], type = "l", lwd = 3, col = "blue",)
#' # Check the weighted prior's accuracy
#' abline(v=sum(y == "a")/length(y), lwd = 3)
#' par(mfrow=c(1,1))
NB = function(X, y, priors = "weighted", factor_max_length = 10) {
  y = as.factor(y)

  n = nrow(X)
  classes_response = levels(y)

  thisEnv <- environment()
  NB_model = list(class = "NB",
                  target_variable = y,
                  predictors = X,
                  getEnv = function()
                  {
                    return(get("thisEnv",thisEnv))
                  })


  if (length(y) != nrow(X)) {
    stop("Stop. Labels are not as many as the number of features. ")
  }

  classes_cols = sapply(X, class)
  NB_model$inferences = list()
  for (i in 1:ncol(X)) {
    if (classes_cols[i] == "numeric") {
      # Gaussian
      NB_model$inferences[[i]] = gauss_inf_NB(X, i, classes_response)
    } else if (classes_cols[i] %in% c("factor","logical","character")) {
      # multinomial
      NB_model$inferences[[i]] = multinomial_table(X, i, classes_response)
    } else if (classes_cols[i] == "integer") {
      if (length(levels(as.factor(X[,i]))) > factor_max_length) {
        # Gaussian
        NB_model$inferences[[i]] = gauss_inf_NB(X, i, classes_response)
      } else {
        # multinomial
        NB_model$inferences[[i]] = multinomial_table(X, i, classes_response)
      }
    }
  }

  if (is.null(priors)) {
    NB_model$priors = NULL
  } else if (!is.null(priors)) {
    if (priors == "weighted") {
      NB_model$priors = as.numeric(table(y))/n
    } else {
      NB_model$priors = priors
    }
  }

  names(NB_model$inferences) = colnames(X)
  NB_model$levels_response = levels(y)


  assign('this',NB_model, thisEnv)

  class(NB_model) = append(class(NB_model), "NB")
  NB_model$train_preds = predict(NB_model, X)
  NB_model$train_probs = predict(NB_model, X, ret_probabilities = T)
  return(NB_model)
}

#' Predict Naive Bayes
#'
#' Prediction function for the \code{NB} class.
#' @param model is the model in question.
#' @param X is the matrix of features to predict for.
#' @param ret_probabilities is a boolean deciding whether to return probabilities or class predictions. Default \code{FALSE}.
#' @export
predict.NB = function(model, X, ret_probabilities = F) {
  #is.null(names(dimnames(model$inferences[[3]])))

  probs = matrix(rep(0, length(model$levels_response)*nrow(X)),ncol=length(model$levels_response),nrow=nrow(X))

  colnames(probs) = model$levels_response

  for (clas in model$levels_response) {
    for (i in 1:ncol(X)) {
      if (!is.null(names(dimnames(model$inferences[[i]])))) {
        # multinomial
        probs[,clas] = probs[,clas] + log1p(apply(as.matrix(X[,i]), 1, function(data_point) {
          return(model$inferences[colnames(X)[i]][[1]][data_point,clas])
        }))
      } else {
        # gaussian
        probs[,clas] = probs[,clas] + log1p(
          dnorm(X[,i], mean=model$inferences[colnames(X)[i]][[1]][clas,1], sd=model$inferences[colnames(X)[i]][[1]][clas,2])
          )

      }

    }
    if (!is.null(model$priors)) {
      probs[,which(clas == model$levels_response)] = probs[,which(clas == model$levels_response)] + log1p(model$priors[which(clas == model$levels_response)])
    }

  }
  if (!ret_probabilities) {
    return(factor(apply(probs, 1, function(x) {
      return(model$levels_response[which.max(x)])
    }), levels = model$levels_response))
  } else {
    return(apply(expm1(probs),1,function(x) {x/sum(x)}))
  }

}

#' Fitted Naive Bayes
#'
#' Returns the fitted values of a Naive Bayes model.
#' @param model is the model in question to return the predictions for.
#' @param ret_probabilities is a boolean deciding whether to return probabilities or class predictions. Default \code{FALSE}.
#' @export
fitted.NB = function(model, ret_probabilities = F) {
  if (!ret_probabilities) {
    return(model$train_preds)
  } else {
    return(model$train_probs)
  }
}



