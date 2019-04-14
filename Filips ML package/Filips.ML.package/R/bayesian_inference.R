#' Posterior mode
#' 
#' Returns the posterior mode point estimate, given a vector of samples. 
#' @param samples is the vector of samples used to estimate. 
#' @param exclude_zeros is a boolean specifying whether the zeros should be excluded for the inference. This should be true (which is default) when doing posterior mode for coefficients . 
#' @export
posterior_mode = function(samples, exclude_zeros = TRUE) {
  if (exclude_zeros == TRUE) {
    density_samples = density(samples[samples != 0])
  } else {
    density_samples = density(samples)
  }
  
  return(density_samples$x[which.max(density_samples$y)])
}


#' Posterior mean
#' 
#' Returns the posterior mean estimate, given a vector of samples from the posterior distribution. 
#' @param samples
#' @param excludes_zeros is a boolean specifying whether the zeros should be excluded for the inference. This should be true (which is default) when using bsts. 
#' @export 
posterior_mean = function(samples, exclude_zeros = TRUE) {
  if (exclude_zeros == TRUE) {
    return(mean(samples[samples != 0]))
  } else {
    return(mean(samples))
  }
}

