#' @import numDeriv
#' @import dplyr
#' @import testthat
require(numDeriv)
require(dplyr)
library(devtools)
require(testthat)

#' A function that generates samples from an unnormalized density based on reject sampling method
#'
#'
#' @param n a numeric value as sample size.
#' @param g a log-concave function; h=log(g).
#' @param k a numeric value as the max size of initial abscissae.
#' @param left a numeric value as the left bound of function h; default is -Inf.
#' @param right a numeric value as the right bound of function h; default is Inf.
#' @param stepsize an integer that indicates the number of samples generated in the first iteration of sampling; 
#'        it increases with each iteration at rate 1.1.
#' @return a numeric vector of length n sampled from the normalized density function d.
#' @export 
#' @examples
#' 
#' ars(n = 1000, g = dnorm)
#' 

ars <- function(n, g, k = 30, left = -Inf, right = Inf, stepsize = 10){
      
      # Compute h, the log of function g:
      h <- function(t){
            return(log(g(t)))
      }
      
      
      # Tk is the initial abscissae:
      # Need to have derivative of h(x_1) > 0 and derivative of h(x_k) < 0:
      #-------------------------------Need modification-----------------------------#

      abscissae_summary <- findInitAbsc(g = g , k = k , left = left, right = right)
      #-----------------------------------------------------------------------------#
      
      # Initialize final samples:
      final_samples <- NULL
      
      # Test for uniform case:
      # If all derivatives are 0, return uniform samples:
      if(all(abscissae_summary[, 3] == 0)){
            warning("WARNING: All derivatives are 0. Function is from a uniform distribution.")
            res <- runif(n, min = left, max = right)
            return(res)
      }
      
      rej_num <- 0
      # Repeat rejection sampling until we reach the predetermined sample size.
      while(length(final_samples) < n){
            
            
            # Generate a vector of samples from the upper hull:
            upper_samples <- sampleUpper(stepsize, 
                                         CDFInverse = 
                                               function(x) 
                                                     upperCDFInverse(x, abscissae_summary,
                                                                     xlow = left,
                                                                     xhigh = right))
            # Compute the value of upper hull for each sample on the original scale:
            gu <- exp(upperHull(upper_samples, abscissae_summary))
            
            # Compute the value of lower hull for each sample on the original scale:
            gl <- exp(lowerHull(upper_samples, abscissae_summary))
            
            # If lower bound is greater than upper bound, the function is not log-concave
            # Need to be careful about the numeric rounding:
            if(max(gl > gu + 1e-10)){
                  stop("Adaptive-rejection sampling 
           method only works for log-concave functions")   
            }
            
            # Generate a uniform random variable for the rejection step:
            unif_samples <- runif(stepsize, min = 0, max = 1)
            
            # Find the index of the first x that has been rejected
            if(all(unif_samples <= gl/ gu)){
                  false_index <- Inf
            }else{
                  false_index <- min(which((unif_samples <= gl/ gu) == FALSE))
            }
            # Second test:
            if(false_index != Inf){
                  # Record the number of times f has been evaluated
                  rej_num <- rej_num + 1
                  x_star <- upper_samples[false_index]
                  x_star_h <- h(x_star)
                  x_star_deriv <- grad(func = h, x = x_star)
                  # If accepted in this step, then add this point back into the sample:
                  if(unif_samples[false_index] < exp(x_star_h)/gu[false_index]){
                        false_index <- false_index + 1
                  }
                  
                  
                  # The adaptive part, update abscissae_summary:
                  # If derivative is NA (e.g, sample 0 for unif(0,1)), exclude this sample.
                  if(!is.na(x_star_deriv)){
                        abscissae_summary <- updateAbscissae(x_star, 
                                                             x_star_h,
                                                             x_star_deriv,
                                                             abscissae_summary)
                  }
            }
            # Get accepted new samples and append them to the final results.
            # We need to avoid the [1:0] case:
            if(false_index != 1 & false_index <= stepsize){
                  new_samples <- upper_samples[1: (false_index - 1)]
                  final_samples <- append(final_samples, new_samples)
            }else{
                  # If none of the samples is rejected
                  final_samples <- append(final_samples, upper_samples)
            }
            
            # increase the stepsize:
            stepsize <- floor(stepsize * 1.1)
      }
      print(paste("We evaluated g", rej_num, "times during the rejection sampling"))
      names(final_samples) <-NULL
      # Return the n final samples (since we vectorized the process, it may took more than n computations)
      return(final_samples[1:n])
}

