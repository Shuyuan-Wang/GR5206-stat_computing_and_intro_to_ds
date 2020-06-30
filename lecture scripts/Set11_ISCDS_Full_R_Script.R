
setwd("~/Desktop/Data")

## Slide 7

sample


## Slide 8

log


## Slide 9

rowSums

## Slide 10

class(sin)
class(sample)
resample = function(x) { 
  return(sample(x, size=length(x), replace=TRUE)) 
}
class(resample)

## Slide 11

body(resample)
args(resample)

## Slide 12

environment(resample)

## Slide 15

install.packages("numDeriv")
library(numDeriv)
args(grad)

## Slide 16

simpleFun = function(x) { 
  # x is a vector of length 2
  return(x[1]^2 + 1/3*x[2]^2)
}

xpt <- runif(n = 2, min = -2, max = 2)
grad(simpleFun, xpt)
c(2*xpt[1], 2/3*xpt[2])

## Slide 34

grad.descent <- function(f, x0, max.iter = 200, step.size = 0.05, stopping.deriv = 0.01, ...) {
  
  n    <- length(x0)
  xmat <- matrix(0, nrow = n, ncol = max.iter)
  xmat[,1] <- x0
  
  for (k in 2:max.iter) {
    # Calculate the gradient
    grad.cur <- grad(f, xmat[ ,k-1], ...) 
    
    # Should we stop?
    if (all(abs(grad.cur) < stopping.deriv)) {
      k <- k-1; break
    }
    
    # Move in the opposite direction of the grad
    xmat[ ,k] <- xmat[ ,k-1] - step.size * grad.cur
  }
  
  xmat <- xmat[ ,1:k] # Trim
  return(list(x = xmat[,k], xmat = xmat, k = k))
}

## Slide 35

x0 <- c(-1.9, -1.9)
gd <- grad.descent(simpleFun, x0)
gd$x
gd$k

## Slide 36 

n <- 100
p <- 2
pred <- matrix(rnorm(n*p), n, p)
beta <- c(1, 4)
resp <- pred %*% beta + rnorm(n)
lm.coefs <- coef(lm(resp ~ pred + 0))
lm.coefs

## Slide 37

MSE <- function(beta) { 
  return(sum((resp - pred %*% beta)^2))
}
grad.descent(MSE, x0 = c(0,0), step.size = 0.05,max.iter = 200)

## Slide 38

out = grad.descent(MSE, x0 = c(0,0), step.size = 1e-3,max.iter = 200) 
out$k
out$x
lm.coefs 



## Slide 56

gmp     <- read.table("gmp.txt", header = TRUE)
gmp$pop <- gmp$gmp/gmp$pcgmp


#library(numDeriv)

mse <- function(theta) {
  mean((gmp$pcgmp - theta[1]*gmp$pop^theta[2])^2)
}

grad.mse <- function(theta) {grad(func = mse, x = theta)}
theta0   <- c(5000, 0.15)

fit1     <- optim(theta0, mse, grad.mse, method = "BFGS",hessian = TRUE)

## Slide 57

fit1[1:3]


## Slide 58

fit1[4:6]


## Slide 61

fit2 <- nls(pcgmp ~ theta0*pop^theta1, data = gmp,start = list(theta0 = 5000, theta1 = 0.10))

## Slide 62

summary(fit2)

## Slide 63

plot(pcgmp ~ pop, data = gmp, log = 'x')
pop.order <- order(gmp$pop)
lines(gmp$pop[pop.order], fitted(fit2)[pop.order])
curve(fit1$par[1]*x^fit1$par[2], add = TRUE,lty = "dashed", col = "red")



## Slide 79

solve(matrix(c(40,1,60,3),nrow=2),c(1600,70))
# Intersection at (10,20)


## Slide 85

factory.n <- list(c("labor","steel"), c("car","truck"))
factory   <- matrix(c(40, 1, 60, 3), nrow = 2, 
                    dimnames = factory.n)
available        <- c(1600, 70)
names(available) <- rownames(factory)
prices           <- c(car = 13, truck = 27)

revenue <- function(output) {return(-output %*% prices)}
plan <- constrOptim(theta = c(5, 5), f = revenue, 
                    grad = NULL, ui = -factory, 
                    ci = -available, meth = "Nelder-Mead")
plan$par


