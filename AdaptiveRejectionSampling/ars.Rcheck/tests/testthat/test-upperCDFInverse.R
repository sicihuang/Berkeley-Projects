testCDFandInverse <- function(f, inverseCdf = 0, xlow = -Inf, xhigh = Inf){
  # A function that tests the upperCDFInverse and upperCDF functions with a general function f.
  #         (f is a L1 function whose integral over the real line is finite
  #          i.e. can generate a density from f.)
  # Args:
  #       f: a density function (or c * density function)
  #       inverseCdf: an inverse cdf for the density f
  # Return:
  #       a L2 norm of the difference
  # Note: The test function does not perform very well due to unusual behaviors of the integration function
  upperCDFHere <- function(x, abscissae_summary) {
    upperCDF(x, abscissae_summary, xlow, xhigh)
  }
  upperCDFInverseHere <- function(x, abscissae_summary) {
    upperCDFInverse(x, abscissae_summary, xlow, xhigh)
  }
  h <- function(x){
    return(log(f(x)))
  }
  # n is an approximation parametor that characterizes the range and # of points of
  # the interval on which the upper hull is constructed
  n <- 8
  x <- seq(-1 * n, n, length.out = floor(2 + n^1.5))
  abscissae_summary <- abscissaeSummary(x, h)
  y <- seq(0, 1, length.out = 1000)
  # se checks whether upperCDF and upperCDFInverse are precisely the inverse of each other
  # se == 0 means that they are
  se <- sum((upperCDFHere(upperCDFInverseHere(y[-1000], abscissae_summary), abscissae_summary) - y[-1000])^2)
  return(se < 1e-5)
}

# unit test of upperCDF and upperCDFInverse
test_that("results from upperCDF and upperCDFInverse are consistent", {
  print("Test the consistency of results from upperCDF and upperCDFInverse.")
  # normal case
  print(expect_equal(testCDFandInverse(f = dnorm, inverseCdf = qnorm), TRUE))
  # chi-square df = 5 case
  f1 <- function(x){
    dchisq(x, df = 5)
  }
  inverseCdf1 <- function(x){
    qchisq(x, df = 5)
  }
  print(expect_equal(testCDFandInverse(f = f1, inverseCdf = inverseCdf1), TRUE))
})

# unit test of upperCDFInverse
test_that("upperCDFInverse returns numeric values", {
  print("Test that upperCDFInverse returns numeric values")
  y <- seq(0, 1, length.out = 1000)
  # normal case
  h <- function(x){
    log(dnorm(x))
  }
  x <- seq(-8, 8, length.out = 100)
  abscissae_summary <- abscissaeSummary(x, h)
  print(expect_is(upperCDFInverse(y, abscissae_summary), "numeric"))
  # chi-square df = 5 case
  h <- function(x){
    log(dchisq(x, df = 5))
  }
  abscissae_summary <- abscissaeSummary(x, h)
  print(expect_is(upperCDFInverse(y, abscissae_summary), "numeric"))
  # exp case
  h <- function(x){
    return(dexp(x))
  }
  x <- seq(0 , 8, length.out = 100)
  abscissae_summary <- abscissaeSummary(x, h)
  print(expect_is(upperCDFInverse(y, abscissae_summary, xlow = 0), "numeric"))
  
})

# main test
test_that("upperCDFInverse returns numeric values", {
  print("Test that upperCDFInverse returns numeric values")
  n <- 10000
  # normal case
  print(expect_more_than(ks.test(ars(n, dnorm), pnorm)$p.value, 0.05))
  # chisq df = 5
  f1 <- function(x){
    dchisq(x, df = 5)
  }
  Cdf1 <- function(x){
    pchisq(x, df = 5)
  }
  print(expect_more_than(ks.test(ars(n = n, g =f1, left = 0), Cdf1)$p.value, 0.05))
  # some other cases
})