


## Slide 7

runif(10)


## Slide 7

set.seed(10)
runif(10)



## Slide 11

seed <- 10
new.random <- function(a = 5, c = 12, m = 16) {
  out <- (a*seed + c) %% m
  seed <<- out 
  return(out)
}



## Slides 13-15


out.length <- 20
variants   <- rep(NA, out.length)

for (i in 1:out.length) {variants[i] <- new.random()}
variants


## Slide 16

out.length <- 20
variants   <- rep(NA, out.length)
for (i in 1:out.length) {
  variants[i] <- new.random(a=1664545, c=1013904223, 
                            m=2^32)
}
variants/2^(32)


## Slide 23

dnorm(0, mean = 0, sd = 1) 
1/sqrt(2*pi) 

## Slide 24

x <- seq(-5, 5, by = .001)
plot(x, dnorm(x), main="Standard Normal Density", pch=20)

## Slide 25

# P(Z < 0)
pnorm(0)

# P(-1.96 < Z < 1.96)
pnorm(1.96) - pnorm(-1.96)

## Slide 26

# P(Z < ?) = 0.5
qnorm(.5) 

# P(Z < ?) = 0.975
qnorm(.975)

## Slide 27

rnorm(1)
rnorm(5)
rnorm(10, mean = 100, sd = 1)

## Slide 31

t  <- seq(-10, 10, by = .01)
df <- c(1, 2, 5, 30, 100)

plot(t, dnorm(t), lty = 1, col = "red", ylab = "f(t)", 
     main = "Student's t")

for (i in 1:5) {
  lines(t, dt(t, df = df[i]), lty = i) 
}


legend <- c(paste("df=", df, sep = ""), "N(0,1)")

legend("topright", legend = legend, lty = c(1:5, 1), 
       col = c(rep(1, 5), 2))

## Slide 34

pgamma(2, shape = 2, rate = 1) # P(0 < X < 2)
1 - pgamma(2, shape = 2, rate = 1) # P(X > 2)

## Slide 35

alpha <- 2:6
beta  <- 1
x     <- seq(0, 10, by = .01)
plot(x, dgamma(x, shape = alpha[1], rate = beta), 
     col = 1, type = "l", ylab = "f(x)", 
     main = "Gamma(alpha, 1)")

for (i in 2:5) {
  lines(x, dgamma(x, shape = alpha[i], rate = beta), 
        col = i) 
}

legend <- paste("alpha=", alpha, sep = "")

legend("topright", legend = legend, fill = 1:5)

## Slide 38

val <- 190
n   <- 1000
p   <- 0.20
correction <- (val + 0.5 - n*p)/(sqrt(n*p*(1-p)))
pnorm(correction) # P(Z < correction)

## Slide 39

# P(X <= 190)
pbinom(val, size = n, prob = p)

# P(x = 0) + P(X = 1) + ... + P(X = 190)
sum(dbinom(0:val, size = n, prob = p))

x <- rbinom(500, size = n, prob = p)
hist(x, main = "Normal Approximation to the Binomial")

## Slide 43

n <- 1000; p <- c(0.1, 0.2, 0.7)
x <- sample(1:3, size = n, prob = p, replace = TRUE)
head(x, 10)
rbind(p, p.hat = table(x)/n)


## Slide 45

n     <- 100
rolls <- sample(1:6, n, replace = TRUE)
table(rolls)

rolls <- floor(runif(n, min = 0, max = 6))
table(rolls)

## Slide 51


lambda <- 2
n      <- 1000
u      <- runif(n) # Simulating uniform rvs

Finverse <- function(u, lambda) {
  # Function for the inverse transform
  return(ifelse((u<0|u>1), 0, -(1/lambda)*log(1-u)))
}

## Slide 52


# x should be exponentially distributed
x <- Finverse(u, lambda)
hist(x, prob = TRUE, breaks = 15)
y <- seq(0, 10, .01)
lines(y, lambda*exp(-lambda*y), col = "blue")

## Slide 54

n         <- 1000
u         <- runif(n)
F.inverse <- function(u) {return(u^{1/3})}
x         <- F.inverse(u)
hist(x, prob = TRUE) # histogram
y         <- seq(0, 1, .01)
lines(y, 3*y^2) # density curve f(x)

## Slide 57

plot(c(0,1), c(0,3), ty="n", main="A Sample Distribution", ylab="Density f(x)", xlab="x")
curve (dbeta(x, 3, 6), add=TRUE)
lines(c(0,0,1,1), c(0,3,3,0))

## Slide 58

x1 <- runif(300, 0, 1); y1 <- runif(300, 0, 2.6)
selected <- y1 < dbeta(x1, 3, 6)

plot(c(0,1), c(0,3), ty="n", main="A Sample Distribution", 
     ylab="Density f(x)", xlab="x")
curve (dbeta(x, 3, 6), add=TRUE)
lines(c(0,0,1,1), c(0,3,3,0))
points (x1, y1, col=1+1*selected, cex=0.1)


## Slide 59

mean(selected)
accepted.points <- x1[selected]

# Proportion of sample points less than 0.5.
mean(accepted.points < 0.5) 

# The true distribution.
pbeta(0.5, 3, 6)


## Slide 60

x2       <- runif(100000, 0, 1)
y2       <- runif(100000, 0, 10)
selected <- y2 < dbeta(x2, 3, 6)
mean(selected)


## Slide 61

plot(c(0,1), c(0,6), ty="n", main="A Sample Distribution", 
     ylab="Density f(x)", xlab="x")
curve (dbeta(x, 3, 6), add=TRUE)
lines(c(0,0,1,1), c(0,3,3,0))
points (x2, y2, col=1+1*selected, cex=0.1)


## Slide 67

f <- function(x) {
  return(ifelse((x < 0 | x > 1), 0, 60*x^3*(1-x)^2))
}

x <- seq(0, 1, length = 100)

plot(x, f(x), type="l", ylab="f(x)")

xmax  <- 0.6
f.max <- 60*xmax^3*(1-xmax)^2


## Slide 68


e <- function(x) {
  return(ifelse((x < 0 | x > 1), Inf, f.max))
}
lines(c(0, 0), c(0, e(0)), lty = 1)
lines(c(1, 1), c(0, e(1)), lty = 1)
lines(x, e(x), lty = 1)

## Slide 70


n.samps <- 1000   # number of samples desired
n       <- 0		     # counter for number samples accepted
samps   <- numeric(n.samps) # initialize the vector of output
while (n < n.samps) {
  y <- runif(1)    #random draw from g
  u <- runif(1)
  if (u < f(y)/e(y)) {
    n        <- n + 1
    samps[n] <- y
  }
}
x <- seq(0, 1, length = 100)
hist(samps, prob = T, ylab = "f(x)", xlab = "x",
     main = "Histogram of draws from Beta(4,3)")
lines(x, dbeta(x, 4, 3), lty = 2)

## simulate from f=2/(1+x^2)pi
#target density
f <- function(x) {
  return(ifelse((x < -1 | x > 1), 0, 2/((1+x^2)*pi)))
}

f.max <- 2/pi
x <- seq(-1.5, 1.5, length = 100)
plot(x, f(x), type="l", ylab="f(x)")

#envelope
e <- function(x) {
  return(ifelse((x < -1 | x > 1), Inf, f.max))
} # same supprot
lines(x, e(x), lty = 1,col='green')

#accept-reject
n.samps <- 1000   # number of samples desired
n       <- 0		     # counter for number samples accepted
samps   <- numeric(n.samps) # initialize the vector of output
while (n < n.samps) {
  y <- runif(1,min=-1,max=1)    #random draw from g
  u <- runif(1)
  if (u < f(y)/e(y)) {
    n        <- n + 1
    samps[n] <- y
  }
}
x <- seq(-1, 1, length = 100)
hist(samps, prob = T, ylab = "f(x)", xlab = "x",xlim=c(-1.5,1.5))
lines(x, f(x), lty = 2,col='red')

## Slide 78

g.over.p <- function(x) {
  return(sqrt(2*pi) * x^2 * exp(-(1/2)*x^2))
}
mean(g.over.p(rnorm(10000))) # Try n = 10000
sqrt(pi)/2

## Slide 84

n <- 10000
mean(rexp(n, rate = 1/3) < 3)

pexp(3, rate = 1/3)

## Slide 91

R <- 1000
n <- 10
binom.list <-NULL
for (i in 1:R) {
  U <- runif(n)
  binom.list[i] <-sum(U<.3)
}

## Slide 94

Sigma <- matrix(c(4,-3,-3,9),nrow=2)
Sigma

svd(Sigma)


Sq.Sigma <- (svd(Sigma)$u)%*%sqrt(diag(svd(Sigma)$d))%*%t(svd(Sigma)$v)
Sq.Sigma 
Sq.Sigma%*%Sq.Sigma 


