##' Given a function and a sequence of points, function computes the value of the 
##' given function and the derivative at each point.
##' 
##' @title abscissaeSummary
##' @param x a numeric vector composed of the abscissae
##' @param h the original function on log scale
##' @return a numeric matrix, with the three columns being the x's, h(x)'s, and derivatives of the h(x)'s respectively. 
##'         If h(x) or h'(x) is infinite, the corresponding x will be dropped.
##' @export
##' @examples
##' x <- seq(-2, 2, length.out = 10)
##' abscissaeSummary(x, function(x) dnorm(x))

abscissaeSummary <- function(x, h){

    # Compute the length of abscissae:
    n <- length(x)
    # Compute values of the function:
    x_value <- h(x)
    finite_indexes <- (x_value != -Inf & x_value != Inf )
    # Compute the derivatives:
    x_deriv <- rep(0, length(finite_indexes))
    x_deriv <- grad(func = h, x = x[finite_indexes])
    res <- cbind(x[finite_indexes], x_value[finite_indexes], x_deriv)
    
    # Exclude x's with an infinite gradient:
    deriv_indexes <- is.finite(res[, 3])
    return(res[deriv_indexes, ])
}

    #-------------------Find the intersections ----------------#

##' A function that computes the intersections of the tangent lines, provided the 
##' abscissae matrix
##'
##' @title computeZ
##' @param abscissae_summary the abscissae matrix with the three columns being the x's,
##' h(x)'s, and derivatives of the h(x)'s respectively. There should be no infinite values
##' in the third column
##' @return a numeric vector consists of the x-coordinates of the intersections
##' (i.e. z values in paper by Gilks et al.)
computeZ <- function(abscissae_summary){
    
    h_diff <- diff(abscissae_summary[,2])

    x_hDerive <- abscissae_summary[, 1] * abscissae_summary[, 3]

    x_hDerive_diff <- diff(x_hDerive)
    # Compute the denominator
    hDeriv_diff <- diff(abscissae_summary[,3])
    # Compute z, the intersections
    z <- (h_diff - x_hDerive_diff)/ (-hDeriv_diff)
    z <- z[!is.infinite(z)]
    if(mean(abs(hDeriv_diff)) < 1e-6){
          warning("WARNING: the input function may not be strictly log-concave")
    }
    else if(is.unsorted(z)){
          stop("the input function is not log concave.")
    }
    return(z)
}

#----------------- Updating functions----------------------#

updateAbscissae <- function(new_x, new_x_h, new_x_h_deriv, abscissae_summary){
    # Given a new set of abscissae with their function values and gradients,
    # append it to the abscissae_summary matrix in ascending order of x.
    new_dat <- cbind(new_x, new_x_h, new_x_h_deriv)
    old_x <- abscissae_summary[, 1]
    # Find the place to insert the new abscissae

    if(new_x < min(old_x)){
        new_summary <- rbind(new_dat, abscissae_summary)
    }else if(new_x > max(old_x)){
        new_summary <- rbind(abscissae_summary, new_dat)
    }else{
        insert_index <- min(which((new_x < old_x) == TRUE))

        k <- nrow(abscissae_summary)

        new_summary <- rbind(abscissae_summary[1:insert_index- 1, ],
                             new_dat,
                             abscissae_summary[insert_index:k, ])
    }
    return(new_summary)
}


#----------- Upper and lower hull functions----------------#

#' A function that, given a vector x, returns the corresponding function values of the upper hull
#'
#' @param x a numeric vector as the abscissae
#' @param abscissae_summary a matrix with the three columns being x, h(x), and h'(x) at the support points respectively
#' 
#' @return a numeric vector that contains the corresponding upper hull values, with same length as x
#'
#' @export
#' @examples
#' # Test1: log of normal distribution
#' h <- function(x){
#'   return(log(dnorm(x)))
#' }
#'
#' xSupport <- seq(-3, 3, length.out = 10)
#'
#' abscissae_summary = abscissaeSummary(xSupport,h)
#'
#' x <- seq(-3, 3, length.out = 10)
#' upperHull(x,abscissae_summary)
#' lowerHull(x,abscissae_summary)
#'
#' x <- seq(-3, 3, length.out = 30)
#' upperHull(x,abscissae_summary)
#' lowerHull(x,abscissae_summary)
#'
#'h <- function(x){
#'  return(log(dchisq(x,df = 2)))
#'}
#'
#'xSupport <- seq(-3, 3, length.out = 10)
#'abscissae_summary = abscissaeSummary(xSupport,h)
#'x <- seq(-3, 3, length.out = 10)
#'upperHull(x,abscissae_summary)
#'lowerHull(x,abscissae_summary)
#'
#'# Test2: log of uniform distribution
#'h <- function(x){
#'  return(log(dunif(x,0,1)))
#'}
#'
#'xSupport <- seq(-3, 3, length.out = 100)
#'abscissae_summary = abscissaeSummary(xSupport,h)
#'x <- seq(-3, 3, length.out = 10)
#'x <- seq(0.2, 0.9, length.out = 10)
#'upperHull(x,abscissae_summary)
#'lowerHull(x,abscissae_summary)
#'
#'# Test3: log of piecewise constant distribution
#'h <- function(x){
#'  a= sapply(x,function(x){if(x<1) return (x)
#'  else if(x>=1 & x <=2) return (1)
#'  else if(x>2) return (-x+3)})
#'  return (a)
#'}
#'
#'xSupport <- seq(-3, 3, length.out = 15)
#'abscissae_summary = abscissaeSummary(xSupport,h)
#'x <- seq(-3, 3, length.out = 10)
#'x <- seq(0.2, 0.9, length.out = 10)
#'upperHull(x,abscissae_summary)
#'lowerHull(x,abscissae_summary)


upperHull <- function(x, abscissae_summary){
  
  numberSupport = length(abscissae_summary[,1])
  Z = vector("numeric", length = numberSupport - 1)
  U = vector("numeric", length = length(x))
  
  # use these sub-vectors to vectorize calculation of the formula
  tempX1 = abscissae_summary[-1,1]
  tempX2 = abscissae_summary[- numberSupport,1]
  tempH1 = abscissae_summary[-1,2]
  tempH2 = abscissae_summary[- numberSupport,2]
  tempDer1 = abscissae_summary[-1,3]
  tempDer2 = abscissae_summary[- numberSupport,3]
  
  # Note the extreme case where function is piecewise constant i.e. two adjacent support points may have the same derivative
  mark = (tempDer2 == tempDer1)
  
  Z = (tempH1 - tempH2 - tempX1 * tempDer1 + tempX2 * tempDer2) / (tempDer2 - tempDer1)
  
  Z[mark] = ((tempX1 + tempX2)/2)[mark]
  
  # Use sapply here!!
  index = sapply(x, function(y){return(sum(Z<=y))})
  index = index + 1
  
  U = abscissae_summary[index,2] + (x - abscissae_summary[index,1]) * abscissae_summary[index,3]
  return(U)
}

#' A function that, given a vector x, returns the corresponding values of the upper hull function
#' @param x a numeric vector as the abscissae
#' @param abscissae_summary a matrix with the three columns being x, h(x), and h'(x) at the support points respectively
#' 
#' @return a numeric vector that contains the corresponding upper hull values, with same length as x
#'
#' @export
#' @examples
#' h <- function(x){
#'   return(log(dnorm(x)))
#' }
#'
#' xSupport <- seq(-3, 3, length.out = 10)
#'
#' abscissae_summary = abscissaeSummary(xSupport,h)
#'
#' x <- seq(-3, 3, length.out = 10)
#' upperHull(x,abscissae_summary)
#' lowerHull(x,abscissae_summary)
#'
#' x <- seq(-3, 3, length.out = 30)
#' upperHull(x,abscissae_summary)
#' lowerHull(x,abscissae_summary)


lowerHull <- function(x, abscissae_summary){
    
    numberSupport = length(abscissae_summary[,1])
    L = vector("numeric", length = length(x))
    
    # Use sapply here!!
    # mark the leftmost and rightmost points, and set the corresponding lower hull values to be -Inf
    # however, after the above step, set the lowerHull value at x, the biggest support point, to be h(x)
    index = sapply(x, function(y){return(sum(abscissae_summary[,1]<=y))})
    mark = (index == 0 | index == numberSupport)
    index[mark] = 1
    
    L = (abscissae_summary[index,2] * (abscissae_summary[index+1,1] - x)
    + abscissae_summary[index+1,2] * (x - abscissae_summary[index,1]) ) / (abscissae_summary[index+1,1] - abscissae_summary[index,1])
    L[mark] = -Inf
    L[which (x == abscissae_summary[numberSupport,1])] = abscissae_summary[numberSupport,2]
    
    return(L)
}
