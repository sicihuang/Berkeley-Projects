pkgname <- "ars"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ars')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("abscissaeSummary")
### * abscissaeSummary

flush(stderr()); flush(stdout())

### Name: abscissaeSummary
### Title: abscissaeSummary
### Aliases: abscissaeSummary

### ** Examples

x <- seq(-2, 2, length.out = 10)
abscissaeSummary(x, function(x) dnorm(x))



cleanEx()
nameEx("ars")
### * ars

flush(stderr()); flush(stdout())

### Name: ars
### Title: A function that returns samples from unnormalized density based
###   on reject sampling
### Aliases: ars

### ** Examples

ars(n = 1000, g = dnorm)



cleanEx()
nameEx("findInitAbsc")
### * findInitAbsc

flush(stderr()); flush(stdout())

### Name: findInitAbsc
### Title: A function that returns a matrix with the three columns being
###   the abscissae x1,...,xk and their corresponding h(x)'s and h'(x)'s
###   respectively.
### Aliases: findInitAbsc

### ** Examples

findInitAbsc(function(x){
   return(dnorm(x, mean=3, sd=2))
 },6)

 # Chi-square distribution
 g <- function(x){
   return(dchisq(x, 10, ncp=3))
 }
 findInitAbsc(g,4,3)

 # Uniform distribution
 g <- function(x){
   return(dunif(x, min=2, max=5))
 }
 findInitAbsc(g,6,2,5)

 # Piecewise constant distribution
 h <- function(x){
   a <- sapply(x,function(x){if(x<1) return (x)
     else if(x>=1 & x <=2) return (1)
     else if(x>2) return (-x+3)})
   return (a)
 }
 g <- function(x){
   return(exp(h(x)))
 }
 findInitAbsc(g,6)



cleanEx()
nameEx("findMode")
### * findMode

flush(stderr()); flush(stdout())

### Name: findMode
### Title: A function that returns the mode of a function
### Aliases: findMode

### ** Examples

# Normal distribution
 g <- function(x){
   return(dnorm(x, mean=3, sd=2))
 }
 findMode(g)

 # Chi-square distribution
 g <- function(x){
   return(dchisq(x, 10, ncp=3))
 }
 findMode(g,3)

 # Uniform distribution
 g <- function(x){
   return(dunif(x, min=2, max=5))
 }
 findMode(g,2,5)

 # Piecewise constant distribution
 h <- function(x){
   a <- sapply(x,function(x){if(x<1) return (x)
     else if(x>=1 & x <=2) return (1)
     else if(x>2) return (-x+3)})
   return (a)
 }
 g <- function(x){
   return(exp(h(x)))
 }
 findMode(g)



cleanEx()
nameEx("lowerHull")
### * lowerHull

flush(stderr()); flush(stdout())

### Name: lowerHull
### Title: A function that, given a vector x, returns the corresponding
###   values of the upper hull function
### Aliases: lowerHull

### ** Examples

h <- function(x){
  return(log(dnorm(x)))
}

xSupport <- seq(-3, 3, length.out = 10)

abscissae_summary = abscissaeSummary(xSupport,h)

x <- seq(-3, 3, length.out = 10)
upperHull(x,abscissae_summary)
lowerHull(x,abscissae_summary)

x <- seq(-3, 3, length.out = 30)
upperHull(x,abscissae_summary)
lowerHull(x,abscissae_summary)



cleanEx()
nameEx("upperHull")
### * upperHull

flush(stderr()); flush(stdout())

### Name: upperHull
### Title: A function that, given a vector x, returns the corresponding
###   function values from the upper hull
### Aliases: upperHull

### ** Examples

# Test1: log of normal distribution
h <- function(x){
  return(log(dnorm(x)))
}

xSupport <- seq(-3, 3, length.out = 10)

abscissae_summary = abscissaeSummary(xSupport,h)

x <- seq(-3, 3, length.out = 10)
upperHull(x,abscissae_summary)
lowerHull(x,abscissae_summary)

x <- seq(-3, 3, length.out = 30)
upperHull(x,abscissae_summary)
lowerHull(x,abscissae_summary)

h <- function(x){
 return(log(dchisq(x,df = 2)))
}

xSupport <- seq(-3, 3, length.out = 10)
abscissae_summary = abscissaeSummary(xSupport,h)
x <- seq(-3, 3, length.out = 10)
upperHull(x,abscissae_summary)
lowerHull(x,abscissae_summary)

# Test2: log of uniform distribution
h <- function(x){
 return(log(dunif(x,0,1)))
}

xSupport <- seq(-3, 3, length.out = 100)
abscissae_summary = abscissaeSummary(xSupport,h)
x <- seq(-3, 3, length.out = 10)
x <- seq(0.2, 0.9, length.out = 10)
upperHull(x,abscissae_summary)
lowerHull(x,abscissae_summary)

# Test3: log of piecewise constant distribution
h <- function(x){
 a= sapply(x,function(x){if(x<1) return (x)
 else if(x>=1 & x <=2) return (1)
 else if(x>2) return (-x+3)})
 return (a)
}

xSupport <- seq(-3, 3, length.out = 15)
abscissae_summary = abscissaeSummary(xSupport,h)
x <- seq(-3, 3, length.out = 10)
x <- seq(0.2, 0.9, length.out = 10)
upperHull(x,abscissae_summary)
lowerHull(x,abscissae_summary)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
