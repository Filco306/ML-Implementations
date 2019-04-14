#' MAE
#' 
#' Returns the MAE, given true and predicted values. 
#' 
#' NOT tested yet!
#' @param y_true 
#' @param y_pred
#' @export
MAE = function(y_true, y_pred) {
  return(mean(abs(y_true - y_pred)))
}

#' MSE
#' 
#' Returns the MSE, given true and predicted values. 
#' 
#' NOT tested yet!
#' @param y_true 
#' @param y_pred
#' @export
MSE = function(y_true, y_pred) {
  return(mean((y_true - y_pred)^2))
}

#' RMSE
#' 
#' Returns the RMSE, given true and predicted values. 
#' 
#' NOT tested yet!
#' @param y_true 
#' @param y_pred
#' @export
RMSE = function(y_true, y_pred) {
  return(sqrt(MSE(y_true, y_pred)))
}


#' MAPE
#'
#' Calculates the MAPE value for predictions.
#' 
#' NOT tested yet!
#' @param y is a vector of the actual values.
#' @param y_hat is a vector of the predictions.
#' @export
MAPE = function(y, y_hat) {
  return(mean(abs((y - y_hat)/y)))
}

#' wMAPE
#'
#' Calculates the wMAPE value for predictions. The wMAPE is calculated as \deqn{(\Sigma |y[i] - y_hat[i]|)/(\Sigma y[i])}
#' 
#' NOT tested yet!
#' @param y is a vector of the actual values.
#' @param y_hat is a vector of the predictions.
#' @export
wMAPE = function(y, y_hat) {
  return(sum(abs(y - y_hat))/sum(y))
}

#' MASE
#'
#' Calculates the MASE value for predictions. The MASE value can be calculated as \deqn{(\Sigma |y[i] - y_hat[i]|)/(\Sigma |y_[t] - y[t-1]|)}
#' 
#' Due to the equation of the MASE, the prediction for the first point is not included. 
#' NOT tested yet!
#' @param y is a vector of the actual values.
#' @param y_hat is a vector of the predictions.
#' @export
MASE = function(y, y_hat) {
  MASEs = as.numeric()
  
  upper = sum(y[2:length(y)] - y_hat[2:length(y)])
  for (i in 2:length(y)) {
    MASEs[i-1] = abs(y[i] - y[i-1])
  }
  
  return(upper/sum(MASEs))
}