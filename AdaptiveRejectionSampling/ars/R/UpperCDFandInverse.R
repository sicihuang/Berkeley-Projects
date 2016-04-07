##' Given a numeric vector and the abscissae matrix, function returns the CDF function of the upper hull.
##' @title upperCDF
##' @param x a numeric vector that takes values in [0,1] as the input for the CDF
##' @param abscissae_summary  a numeric matrix comprised of abscissae, h(x)'s and h'(x)'s
##' @param xlow a numeric value as the left bound of the support of g
##' @param xhigh a numeric value as the right bound of the support of g
##' @return A numeric vector consists of values of the CDF for the upper hull at each abscissa.
##' @export

upperCDF <- function(x, abscissae_summary, xlow = -Inf, xhigh = Inf){

    if (xlow > xhigh){
        return("Invalid input. Left bound must be smaller than right bound.")
    }

    # k is the number of elements in the abscissae
    k <- dim(abscissae_summary)[1]
    # z is the vector consists of kink points in the envelope function
    z <- computeZ(abscissae_summary)

    # binIntegral is a vector of lenth k s.t.
    #       binIntegral[j] = integral of exp(UpperHall) over [z[j-1], z[j]], z[0] = -Inf, z[k] = +Inf
    binIntegral <- rep(0, k)
    # First, we calculate the special cases where j = 0 and k
    # We treat the cases in which xlow = -Inf and in which xlow > -Inf differently
    if (xlow == -Inf){
        binIntegral[1] <- exp(abscissae_summary[1,2] + (z[1] - abscissae_summary[1,1]) * abscissae_summary[1, 3]) / abscissae_summary[1, 3]
    } else {
        binIntegral[1] <- (exp(abscissae_summary[1,2] + (z[1] - abscissae_summary[1,1]) * abscissae_summary[1, 3])
                           - exp(abscissae_summary[1,2] + (xlow - abscissae_summary[1,1]) * abscissae_summary[1, 3])) / abscissae_summary[1, 3]
    }

    if (xhigh == Inf) {
        binIntegral[k] <- -1 * exp(abscissae_summary[k,2] + (z[k-1] - abscissae_summary[k,1]) * abscissae_summary[k, 3]) / abscissae_summary[k, 3]
    } else {
        binIntegral[k] <- (exp(abscissae_summary[k,2] + (xhigh - abscissae_summary[k,1]) * abscissae_summary[k, 3])
                           - exp(abscissae_summary[k,2] + (z[k-1] - abscissae_summary[k,1]) * abscissae_summary[k, 3])) / abscissae_summary[k, 3]
    }
    # Then, we calculate the general cases where j = 2, ..., k-1
    if (k > 2){
        for(j in 2:(k-1)){
            if (abscissae_summary[j, 3] == 0){
                binIntegral[j] <- (z[j] - z[j-1]) * exp(abscissae_summary[j, 2])
            } else{
                binIntegral[j] <- (exp(abscissae_summary[j,2] + (z[j] - abscissae_summary[j,1]) * abscissae_summary[j, 3])
                                   - exp(abscissae_summary[j,2] + (z[j-1] - abscissae_summary[j,1]) * abscissae_summary[j, 3])) / abscissae_summary[j, 3]
            }
        }
    }

    # stepIntegral is a vector of length k s.t.
    #       stepIntegral[j] = integral of exp(UpperHall) over [-Inf, z[j]]
    stepIntegral <- cumsum(binIntegral)

    upperCDF <- function(t){
    # A function that returns the value of CDF for the normalized envelope function at a single point
    # j is the index such that z[j] < t < z[j+1], z[0] = -Inf, z[k] = +Inf
        j <- sum(z < t)

    # We treat the j == 0 case seperately.
        if (j == 0){
            if (xlow == -Inf){
                integral <- exp(abscissae_summary[1,2] + (t - abscissae_summary[1,1]) * abscissae_summary[1, 3]) / abscissae_summary[1, 3]
            } else {
                integral <- (exp(abscissae_summary[1,2] + (t - abscissae_summary[1,1]) * abscissae_summary[1, 3]) - exp(abscissae_summary[1,2] + (xlow - abscissae_summary[1,1]) * abscissae_summary[1, 3])) / abscissae_summary[1, 3]
            }
        } else{
            if (abscissae_summary[j + 1, 3] == 0){
                increment <- (t - z[j]) * exp(abscissae_summary[j+1, 2])
            } else{
                increment <- (exp(abscissae_summary[j+1,2] + (t - abscissae_summary[j+1,1]) * abscissae_summary[j+1, 3]) - exp(abscissae_summary[j+1,2] + (z[j] - abscissae_summary[j+1,1]) * abscissae_summary[j+1, 3])) / abscissae_summary[j+1, 3]
            }
            integral <- stepIntegral[j] + increment
        }
        cdf <- integral / stepIntegral[k]
        return(cdf)
    }

    cdf <- sapply(x, upperCDF)
    return(cdf)
}



##' Given a numeric vector and the abscissae matrix, function returns the inverse CDF function of the upper hull.
##' @title upperCDFInverse
##' @param x a numeric vector that takes values in [0,1] as the input for the CDF
##' @param abscissae_summary  a numeric matrix comprised of abscissae, h(x) and h'(x)
##' @param xlow a numeric value as the left bound of the support of g
##' @param xhigh a numeric value as the right bound of the support of g
##' @return A numeric vector consists of values of the inverse CDF for the upper hull at each abscissa.
##' @export

upperCDFInverse <- function(x, abscissae_summary, xlow = -Inf, xhigh = Inf){

    if (xlow > xhigh){
        return("Invalid input. Left bound must be smaller than right bound.")
    }

    # k is the number of elements in the abscissae
    k <- dim(abscissae_summary)[1]
    # z is the vector consists of kink points in the envelope function
    z <- computeZ(abscissae_summary)

    # binIntegral is a vector of lenth k s.t.
    #       binIntegral[j] = integral of exp(UpperHall) over [z[j-1], z[j]], z[0] = -Inf, z[k] = +Inf
    binIntegral <- rep(0, k)
    # First, we calculate the special cases where j = 0 and k
    # We treat the cases in which xlow = -Inf and in which xlow > -Inf differently
    if (xlow == -Inf){
        binIntegral[1] <- exp(abscissae_summary[1,2] + (z[1] - abscissae_summary[1,1]) * abscissae_summary[1, 3]) / abscissae_summary[1, 3]
    } else{
        binIntegral[1] <- (exp(abscissae_summary[1,2] + (z[1] - abscissae_summary[1,1]) * abscissae_summary[1, 3])
                           - exp(abscissae_summary[1,2] + (xlow - abscissae_summary[1,1]) * abscissae_summary[1, 3])) / abscissae_summary[1, 3]
    }

    if (xhigh == Inf){
        binIntegral[k] <- -1 * exp(abscissae_summary[k,2] + (z[k-1] - abscissae_summary[k,1]) * abscissae_summary[k, 3]) / abscissae_summary[k, 3]
    } else{
        binIntegral[k] <- (exp(abscissae_summary[k,2] + (xhigh - abscissae_summary[k,1]) * abscissae_summary[k,3])
                           - exp(abscissae_summary[k,2] + (z[k-1] - abscissae_summary[k,1]) * abscissae_summary[k,3])) / abscissae_summary[k, 3]
    }

    # Then, we calculate the general cases where j = 2, ..., k-1
    if (k > 2){
        for(j in 2:(k-1)){
            if (abscissae_summary[j, 3] == 0){
                binIntegral[j] <- (z[j] - z[j-1]) * exp(abscissae_summary[j, 2])
            } else{
                binIntegral[j] <- (exp(abscissae_summary[j,2] + (z[j] - abscissae_summary[j,1]) * abscissae_summary[j, 3])
                                   - exp(abscissae_summary[j,2] + (z[j-1] - abscissae_summary[j,1]) * abscissae_summary[j, 3])) / abscissae_summary[j, 3]
            }
        }
    }
    # print(binIntegral)
    # print(abscissae_summary)
    # stepIntegral is a vector of length k s.t.
    #       stepIntegral[j] = integral of exp(UpperHall) over [-Inf, z[j]]
    stepIntegral <- cumsum(binIntegral)

    norm <- stepIntegral[k]
    stepCDF <- stepIntegral / norm
    upperCDFInverse <- function(s){
    # A function that returns value of the inverse CDF for the normalized envelope function at a single point t
    # j is the index such that z[j] < upperCDFInverse(s) < z[j+1], z[0] = -Inf, z[k] = +Inf
        j <- sum(stepCDF < s)
    # print(j)
    # print(s)
    # print(stepCDF)
    # We treat the j == 0 case seperately. Note that we transform s onto the original scale
    # (i.e. the scale before normalization) by calculating s * norm. The increment is
    # also on the original scale.
        if (j == 0) {
            if (xlow == -Inf) {
                xvalue <- (log(s * norm * abscissae_summary[1, 3]) - abscissae_summary[1,2]) / abscissae_summary[1, 3] +  abscissae_summary[1,1]
            } else {
                xvalue <- (log(s * norm * abscissae_summary[1, 3] + exp(abscissae_summary[1, 2] +
                                                                            (xlow - abscissae_summary[1,1]) * abscissae_summary[1, 3])) -
                                                                                abscissae_summary[1, 2]) / abscissae_summary[1, 3] + abscissae_summary[1, 1]
            }
        } else{
            increment <- s * norm - stepIntegral[j]
            if(abscissae_summary[j+1, 3] == 0){
                xvalue <- increment / exp(abscissae_summary[j+1, 2]) + z[j]
            } else{
                xvalue <- (log(increment * abscissae_summary[j+1, 3] + exp(abscissae_summary[j+1, 2] +
                                                                               (z[j] - abscissae_summary[j+1,1]) * abscissae_summary[j+1, 3])) -
                                                                                   abscissae_summary[j+1, 2]) / abscissae_summary[j+1, 3] + abscissae_summary[j+1, 1]
            }
        }
        return(xvalue)
    }
    inverseCDF <- sapply(x, upperCDFInverse)
    return(inverseCDF)
}




##' Sample from upper hull based on the inverse CDF
##' @title sampleUpper
##' @param n a positive integer as the number of samples
##' @param CDFInverse an inverse CDF function of the upper hull
##' @return a numeric vector of n samples from the upper hull
##' @export

sampleUpper <- function(n, CDFInverse){
    # Given a CDF, generate n samples from it using the inverse method.
    tmp_unif <- runif(n = n)
    samples <- CDFInverse(tmp_unif)
    return(samples)
}




