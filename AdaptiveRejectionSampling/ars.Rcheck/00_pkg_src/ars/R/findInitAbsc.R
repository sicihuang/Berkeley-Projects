#' A function that returns a matrix with the three columns being the abscissae x1,...,xk
#' and their corresponding h(x)'s and h'(x)'s respectively. 
#'
#' @title findInitAbsc
#'  
#' @param g a log-concave function.
#' @param k a numeric value that indicates the max size of the abscissae.
#' @param left a numeric value that indicates the left bound for domain of function g; 
#'        default value is -Inf
#' @param right a numeric value that indicates the right bound for domain of function g; 
#'        default value is Inf
#' @param c a numeric value by which we shift the finite bound to find initial value for the 
#'        optim function; default value is 3 
#' @return an abscissae matrix 
#' @export 
#' 
#' @examples 
#' 
#'  findInitAbsc(function(x){
#'    return(dnorm(x, mean=3, sd=2))
#'  },6)
#'  
#'  # Chi-square distribution
#'  g <- function(x){
#'    return(dchisq(x, 10, ncp=3))
#'  }
#'  findInitAbsc(g,4,3)
#'  
#'  # Uniform distribution
#'  g <- function(x){
#'    return(dunif(x, min=2, max=5))
#'  }
#'  findInitAbsc(g,6,2,5)
#'  
#'  # Piecewise-constant distribution
#'  h <- function(x){
#'    a <- sapply(x,function(x){if(x<1) return (x)
#'      else if(x>=1 & x <=2) return (1)
#'      else if(x>2) return (-x+3)})
#'    return (a)
#'  }
#'  g <- function(x){
#'    return(exp(h(x)))
#'  }
#'  findInitAbsc(g,6)


findInitAbsc <- function(g,k,left=-Inf,right=Inf, c=3){
      
      if(!(k%%1==0)){
            stop("k must be an integer")
      }
      
      h <- function(t){
            return(log(g(t)))
      }
      
      if(left>=right){
            stop("left bound needs to be smaller than right bound")
      }
      
      mode <- findMode(g, left, right, c = c)
      if(right == Inf & left == -Inf){
            
            #mode is guaranteed to be in interval; sample from both sides of mode 
            x <- seq(mode-2*k, mode+2*k, length.out=k) 
            
      }else if(right == Inf){
            
            if(mode>left){
                  #if mode is in interval, sample within interval from both sides of mode 
                  x <- seq(left + 1e-2, 2*mode-left, length.out=k)
            }else{
                  
                  warning("Mode is outside of or on boundaries of the interval specified; 
                          abscissae may not satisfy h'(x1)>0 and h'(xk)<0")
                  
                  #if mode is not in interval, sample from left bound and above
                  x <- seq(left + 1e-2, left+k, length.out=k)
            }
            
      }else if(left == -Inf){
            
            if(mode<right){
                  #if mode is in interval, sample within interval from both sides of mode 
                  x <- seq(2*mode-right, right-1e-2, length.out=k)
            }else{
                  warning("Mode is outside of or on boundaries of the interval specified; 
                          abscissae may not satisfy h'(x1)>0 and h'(xk)<0")
                  #if mode is not in interval, sample from right bound and below
                  x <- seq(right-k, right-1e-2, length.out=k)
            }
            
      }else{
            #whether mode is in interval or not, sample within interval
            x <- seq(left+1e-2, right-1e-2, length.out=k)
      }
      
      #compute h(x)
      h_x <- h(x)
      #only keep abscissae with finite h(x)
      finite_ind <- is.finite(h_x)
      x <- x[finite_ind]
      h_x <- h_x[finite_ind]
      
      #compute h'(x)
      h_x_deriv <- grad(h, x)
      #only keep abscissae with finite h'(x)
      deriv_ind <- is.finite(h_x_deriv)
      
      mat <- cbind(x[deriv_ind], h_x[deriv_ind], h_x_deriv[deriv_ind])
      #rearrange rows in increasing order of x
      mat <- mat[order(mat[,1]),]
      colnames(mat) <- c("x", "h(x)", "h'(x)")
      
      if(nrow(mat)<k){
            
            paste("Size of initial abscissae is", nrow(mat), "after excluding x such that h(x) or h'(x) is infinite")
            
      }
      
      if(nrow(mat)<2){
            
            warning("Size of abscissae is less than 2 after excluding x such that h(x) or h'(x) is infinite, 
          choose a larger k")
            
      }
      
      return(mat)
}

#' A function that returns the mode of a function
#'
#' @title findMode
#' 
#' @param g a log-concave function.
#' @param left a numeric value that indicates the left bound for domain of function g; 
#'        default value is -Inf
#' @param right a numeric value that indicates the right bound for domain of function g; 
#'        default value is Inf
#' @param c a numeric value by which we shift the finite bound to find initial value for the 
#'        optim function; default value is 3 
#' @return a numeric value that is the mode of the function h=log(g)
#' @export 
#' 
#' @examples 
#' # Normal distribution
#'  
#'  findMode(g <- function(x){
#'    return(dnorm(x, mean=3, sd=2))
#'  })
#'  
#'  # Chi-square distribution
#'  g <- function(x){
#'    return(dchisq(x, 10, ncp=3))
#'  }
#'  findMode(g,3)
#'  
#'  # Uniform distribution
#'  g <- function(x){
#'    return(dunif(x, min=2, max=5))
#'  }
#'  findMode(g,2,5)
#'  
#'  # Piecewise constant distribution
#'  h <- function(x){
#'    a <- sapply(x,function(x){if(x<1) return (x)
#'      else if(x>=1 & x <=2) return (1)
#'      else if(x>2) return (-x+3)})
#'    return (a)
#'  }
#'  g <- function(x){
#'    return(exp(h(x)))
#'  }
#'  findMode(g)

findMode <- function(g, left = -Inf, right = Inf, c = 3){
      
      #optim finds minimum, need negative of function h(x) to find maximum
      negh <- function(t){
            return(-log(g(t)))
      }
      
      if(left>=right){
            stop("left bound needs to be smaller than right bound")
      }
      
      if(right == Inf & left == -Inf){
            #start near center of domain; shift a bit to the right to avoid error when g is chi-square
            start <- 0.5
            mode <- optim(start, negh, gr=NULL, method="BFGS")$par 
            return(mode)
      }else if(right == Inf){
            #start a little bit to the right of finite left bound
            start <- left + c 
      }else if(left == -Inf){
            #start a little bit to the left of finite right bound
            start <- right - c 
      }else{
            #start at middle point of domain
            start <- floor((left+right)/2) 
      }
      
      
      # print(start)
      mode <- optim(start, negh, gr=NULL, method="L-BFGS-B", 
                    lower=left + 1e-10, upper=right - 1e-15)$par
      
      if(mode<=left | mode>=right){
            warning("Global supremum is outside of or on boundaries of the interval specified")
      }
      
      return(mode)
}