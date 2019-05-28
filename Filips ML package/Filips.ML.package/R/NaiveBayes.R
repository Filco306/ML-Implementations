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
#' data(asia)
#' asia
#' MNB = MultinomialNB(X = asia[,1:7], y = asia[,8])
#' predict(MNB, asia[,1:7])
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
  return(NB_model)
}


#' Predict MultinomialNB
#'
#' Predictor for multinomial naive bayes.
#' @param modelObject is the multinomial naive bayes with which to predict.
#' @param X is the new data for which to predict. Should have same format as original data frame
#' @export
predict.MultinomialNB = function(modelObject, X) {
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

    return(modelObject$levels_response[which.max(probs)])


  }))
}



GaussianNaiveBayes = function(X, y, priors = "weighted") {

}


