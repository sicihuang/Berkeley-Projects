set.seed(2015243)
#-----------------------Test Normal distribution-----------#


test_that("Test sample from normal distribution", {
      print("Test sample from normal distribution")
      g <- function(x){
            return(dnorm(x))
      }
      
      normal_sample <- ars(n = 1000, g)
      # hist(normal_sample)
      # Should not be rejected
      shapiro <- shapiro.test(normal_sample)
      print(expect_true(shapiro$p.value > 0.05))
})


#---------------Test truncated Normal distribution-----------#

test_that("Check the bounds for truncated normal", {
      
      print("Test sample from truncated normal distribution")
      # Truncated normal
      g <- function(x){
            return(dnorm(x))
      }
      
      truncate_normal_sample <- ars(n = 1000, g = g, left = -2, right = 2)
      # hist(normal_sample)
      print(expect_true(max(truncate_normal_sample) < 2))
      print(expect_true(min(truncate_normal_sample) > -2))
})


#-----------------------Test chi-square distribution---------#


# chisq must give left bound otherwise optim can not find mode.

test_that("Check the bounds for Chi-square with degree of freedom 1", {
      # Degree = 1
      # Should get error
      print("Check the bounds for Chi-square with degree of freedom 1 (non-log-concave)")
      g <- function(x){
            return(dchisq(x, df = 1))
      }
      # It will return both error and warning
      print(expect_error(chisq_sample <- ars(n = 10000, g = g, left = 0)))
      print(expect_error(chisq_sample <- ars(n = 10000, g = g)))
})



test_that("Check the bounds for Chi-square with degree of freedom 2", {
      
      print("Check the bounds for Chi-square with degree of freedom 2 (log-concave)")
      g <- function(x){
            return(dchisq(x, df = 2))
      }
      # for chi-square need to specify left end bound
      
      # Warning because it is not strictly log-concave
      print(expect_warning(
            print(expect_true(length(ars(n = 10000, g = g, left = 0)) == 10000))))
})


test_that("Check t distribution with degree of freedom 10", {
      print("Check the bounds for t distribution with degree of freedom 10 (log-concave)")
      g <- function(x){
            return(dt(x, df = 10))
      }
      # Should be getting an error!
      print(expect_error(t_sample <- ars(n = 10000, g = g)))
})


#-----------------------Test unif distribution------------------#
test_that("Check the uniform distribution", {
      # Expect warnings for all 0 derivatives
      print("Check the uniform distribution")
      print(expect_warning(unif_sample <- ars(n = 10000, g = dunif, left = 0, right = 1)))
})



#-----------------------Test exp distribution------------------#
test_that("Check the uniform distribution", {
      # Expect warnings for not strictly concave functions
      print("Check the uniform distribution")
      print(expect_warning(exp_sample <- ars(n = 10000, g = dexp, left = 0)))
})


