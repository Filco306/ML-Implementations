#' Linear regression 
#' 
#' Performs linear regression in closed form. 
#' @param X is the data frame with data. 
#' @param y are the response variables. 
#' @param transform_func is an optional parameter to transform the parameters into another dimensional space. Can be \code{log}, creating \deqn{y[i] = \beta[0]\prod \beta[j]x[i,j]}
#' for example. Default is \code{NULL}.
#' @param ... are additional parameters to \code{transform_func}.
#' 
#' Note that the standard errors are not really applicable if \code{transform_func} is used. 
#' @export
linear_reg = function(X, y, transform_func = NULL,...) {
  thisEnv <- environment()
  model = list(class = "LinearRegression",
                  target_variable = y,
                  predictors = X,
                  transform_func = transform_func,
                  getEnv = function()
                  {
                    return(get("thisEnv",thisEnv))
                  })
  assign('this',model, thisEnv)
  class(model) = append(class(model), "LinearRegression")
  
  if (length(y) != nrow(X)) {
    stop("Stop. Labels are not as many as the number of features. ")
  }
  if (!is.null(transform_func)) {
    X = transform_func(X)
  }
  X = cbind(data.frame(Intercept = rep(1, times = nrow(X))), X)
  
  model$betas = solve(t(X) %*% as.matrix(X)) %*% t(X) %*%as.matrix(y)
  
  model$n = nrow(X)
  model$k = dim(model$betas)[1] - 1
  model$df = model$n - model$k - 1
  model$residuals = y - predict(model, X[,-c(1)])
  model$sigma_est = 1/(model$df)*sum(model$residuals^2)
  model$SEs_betas = sqrt(diag(solve(t(X)%*%as.matrix(X)))*model$sigma_est)
  model$t_vals = as.numeric(model$betas)/model$SEs_betas
  # https://stats.stackexchange.com/questions/5135/interpretation-of-rs-lm-output
  model$p_vals = 2 * pt(abs(model$t_vals), df = model$df, lower.tail = FALSE)
  model$coef_summary = matrix(c(modelObject$betas, modelObject$SEs_betas, 
                           modelObject$t_vals,
                           modelObject$p_vals), nrow = nrow(modelObject$betas),ncol=4, byrow=F)
  colnames(model$coef_summary) = c("Estimate","Std. Err", "t-value", "p-value")
  rownames(model$coef_summary) = rownames(modelObject$betas)
  
  
  return(model)
}

#' Predict linear regression
#' 
#' Predicts using the model created in function \code{linear_reg}.
#' @param modelObject is the LinearRegression model object with which to predict. 
#' @param X is the data with which to predict. Ensure it is in the same order as the original matrix. 
predict.LinearRegression = function(modelObject,X) {
  
  
  if (!is.null(modelObject$transform_func)) {
    X = transform_func(X)
  }
  
  
  X = cbind(data.frame(Intercept = rep(1, times = nrow(X))), X)
  
  
  return(as.matrix(X)%*%as.matrix(modelObject$betas))
}

#' Residuals linear regression
#' 
#' Returns the residuals of the fitted training data. 
#' @param modelObject is the LinearRegression model in question
#' @export
resid.LinearRegression = function(modelObject) {
  return(modelObject$residuals)
}

#' Summary linear regression
#' 
#' @param modelObject is the modelObject for which to return the summary. 
#' @export
summary.LinearRegression = function(modelObject) {
  summary = list()
  summary$coef_summary = model$coef_summary
  colnames(summary$coef_summary) = c("Estimate","Std. Err", "t-value", "p-value")
  rownames(summary$coef_summary) = rownames(modelObject$betas)
  summary$degrees_of_freedom = modelObject$df
  
  return(summary)
}

coef.LinearRegression = function(modelObject) {
  return(modelObject$betas)
}


#' Get polynomial data
#' 
#' Get a dataframe with all the original data frame's variables also as polynomial. Thus allows for polynomial regression.
#' 
#' Can be used in conjunction with \code{linear_reg} by using this function as \code{transform_func}.
#' @param X is the original dataframe.
#' @param degree is how many degrees to convert the frame to. 
#' @export
to_polynomial = function(X, degree = 2) {
  all_data = X
  for (i in 2:degree) {
    X_sq = X^i
    colnames(X_sq) = paste(colnames(X),"^",i,sep="")
    all_data = cbind(all_data, X_sq)
  }
  return(all_data)
}
