# context("Test Up Low Log Density")

set.seed(2015243)



#-------Test: h function, upperhull, and lowerhull have the same value at support points-----#


test_that("Test: h function, upperhull, and lowerhull have the same value at support points", {
  print("Test: h function, upperhull, and lowerhull have the same value at support points")
  val_equal <- function(x,h){
    abs_sum <- abscissaeSummary(x, h)
    mark = vector("numeric")
    if( sum( (round(abs_sum[,2],5) != round(upperHull(abs_sum[,1], abs_sum),5))
             + (round(upperHull(abs_sum[,1], abs_sum),5) != round(lowerHull(abs_sum[,1], abs_sum),5)) ) == 0 ){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  print("normal distribution")
  expect_true(val_equal(seq(-3, 3, length.out = 10),function(x){return(log(dnorm(x)))}))
  print("chi square distribution")
  print(expect_true(val_equal(seq(-5, 5, length.out = 10),function(x){return(log(dchisq(x,df = 2)))})))
  print(expect_true(val_equal(seq(1, 30, length.out = 15),function(x){return(log(dchisq(x,df = 2)))})))
  
  print("exponential distribution")
  print(expect_true(val_equal(seq(-5, 5, length.out = 10),function(x){return(log(dexp(x)))})))
  print(expect_true(val_equal(seq(1, 30, length.out = 15),function(x){return(log(dexp(x)))})))
  print("beta distribution")
  print(expect_true(val_equal(seq(-5, 5, length.out = 20),function(x){return(log(dbeta(x,0.5,0.5)))})))
  print(expect_true(val_equal(seq(0, 7, length.out = 20),function(x){return(log(dbeta(x,0.5,0.5)))})))
  print(expect_true(val_equal(seq(0, 1, length.out = 10),function(x){return(log(dbeta(x,0.5,0.5)))})))
  print("uniform distribution")
  print(expect_true(val_equal(seq(0.5, 0.9, length.out = 10),function(x){return(log(dunif(x,0,1)))})))
  print("piecewise-constant distribution") 
  print(expect_true(val_equal(seq(-3, 3, length.out = 15),function(x){
    a= sapply(x,function(x){if(x<1) return (x)
                            else if(x>=1 & x <=2) return (1)
                            else if(x>2) return (-x+3)})
    return (a)
  })))
})

#-----------------------Test upper hull always lie above h function-----------#

test_that("Test: upper hull always lie above h function", {
  print("Test: upper hull always lie above h function")
  val_equal <- function(x,h){
    abs_sum <- abscissaeSummary(x, h)
    mark = vector("numeric")
    # select 100 random points to test relationship between their values
    xRange = range(abs_sum[,1], finite = TRUE, na.rm = TRUE)
    testPoint = runif(1000,xRange[1],xRange[2])
    
    testtrueValue = h(testPoint)
    testupperValue = upperHull(testPoint, abs_sum)
    testlowerValue = lowerHull(testPoint, abs_sum)
    if( sum(round(testtrueValue,5) > round(testupperValue,5)) == 0){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  print("normal distribution")
  expect_true(val_equal(seq(-3, 3, length.out = 10),function(x){return(log(dnorm(x)))}))
  
  print("chi square distribution")
  print(expect_true(val_equal(seq(-5, 5, length.out = 10),function(x){return(log(dchisq(x,df = 2)))})))
  print(expect_true(val_equal(seq(1, 30, length.out = 15),function(x){return(log(dchisq(x,df = 2)))})))
  
  print("exponential distribution")
  print(expect_true(val_equal(seq(-5, 5, length.out = 10),function(x){return(log(dexp(x)))})))
  print(expect_true(val_equal(seq(1, 30, length.out = 15),function(x){return(log(dexp(x)))})))
  
  print("beta distribution")
  print(expect_false(val_equal(seq(-5, 5, length.out = 20),function(x){return(log(dbeta(x,0.5,0.5)))})))
  print(expect_false(val_equal(seq(0, 7, length.out = 20),function(x){return(log(dbeta(x,0.5,0.5)))})))
  print(expect_false(val_equal(seq(0, 1, length.out = 10),function(x){return(log(dbeta(x,0.5,0.5)))})))
  print("uniform distribution")
  print(expect_true(val_equal(seq(0.5, 0.9, length.out = 10),function(x){return(log(dunif(x,0,1)))})))
  print("piecewise-constant distribution") 
  
  print(expect_true(val_equal(seq(-3, 3, length.out = 15),function(x){
    a= sapply(x,function(x){if(x<1) return (x)
                            else if(x>=1 & x <=2) return (1)
                            else if(x>2) return (-x+3)})
    return (a)
  })))
})


#-----------------------Test lower hull always lie below h function-----------#

test_that("Test: lower hull always lie below h function", {
  print("Test: lower hull always lie below h function")
  val_equal <- function(x,h){
    abs_sum <- abscissaeSummary(x, h)
    mark = vector("numeric")
    # select 100 random points to test relationship between their values
    xRange = range(abs_sum[,1], finite = TRUE, na.rm = TRUE)
    testPoint = runif(1000,xRange[1],xRange[2])
    
    testtrueValue = h(testPoint)
    testupperValue = upperHull(testPoint, abs_sum)
    testlowerValue = lowerHull(testPoint, abs_sum)
    if( sum(round(testtrueValue,5) < round(testlowerValue,5)) == 0){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  print("normal distribution")
  expect_true(val_equal(seq(-3, 3, length.out = 10),function(x){return(log(dnorm(x)))}))
  print("chi square distribution")
  print(expect_true(val_equal(seq(-5, 5, length.out = 10),function(x){return(log(dchisq(x,df = 2)))})))
  print(expect_true(val_equal(seq(1, 30, length.out = 15),function(x){return(log(dchisq(x,df = 2)))})))
  print("exponential distribution")
  print(expect_true(val_equal(seq(-5, 5, length.out = 10),function(x){return(log(dexp(x)))})))
  print(expect_true(val_equal(seq(1, 30, length.out = 15),function(x){return(log(dexp(x)))})))
  print("beta distribution")
  print(expect_false(val_equal(seq(-5, 5, length.out = 20),function(x){return(log(dbeta(x,0.5,0.5)))})))
  print(expect_false(val_equal(seq(0, 7, length.out = 20),function(x){return(log(dbeta(x,0.5,0.5)))})))
  print(expect_false(val_equal(seq(0, 1, length.out = 10),function(x){return(log(dbeta(x,0.5,0.5)))})))
  print("uniform distribution")
  print(expect_true(val_equal(seq(0.5, 0.9, length.out = 10),function(x){return(log(dunif(x,0,1)))})))
  print("piecewise-constant distribution") 
  print(expect_true(val_equal(seq(-3, 3, length.out = 15),function(x){
    a= sapply(x,function(x){if(x<1) return (x)
                            else if(x>=1 & x <=2) return (1)
                            else if(x>2) return (-x+3)})
    return (a)
  })))
})
