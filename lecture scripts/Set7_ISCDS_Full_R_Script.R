
#### Week 5

#setwd("~/Desktop/Data")

## Slide 7

significant <- function(x) {
  if (x <= 0.05) { return(TRUE) } 
  else { return(FALSE) }
}

# Test

significant(.12)
significant(.03)


## Slide 8


# Inputs:  A vector of numbers (x)
# Outputs: A loss vector with x^2 for small elements, 
#          and 2|x|-1 for large ones

res_loss <- function(x) {
  loss_vec <- ifelse(x^2 > 1, 2*abs(x) - 1, x^2)
  return(loss_vec)
}



vec <- c(-0.5, 0.9, -3, 4)
res_loss(vec)


## Slide 12


FiveTimesSum <- function(vec){
  return(5*sum(vec))
}

FiveTimesSum(1:3)


## Slide 14 

# Inputs: A vector of numbers (x), 
#         crossover location (c > 0)
# Outputs: A loss vector with x^2 for small elements, 
#          and 2*sqrt(c)*|x|-c for large ones

res_loss2 <- function(x, c = 1) {
  loss_vec <- ifelse(x^2 > c, 2*sqrt(c)*abs(x) - c, x^2)
  return(loss_vec)
}


## Slide 15

identical(res_loss(vec), res_loss2(vec, c=1))

identical(res_loss(vec), res_loss2(vec, c=2))

identical(res_loss2(x=vec, c=2), res_loss2(c=2, x=vec))


## Slide 16

vec <- c(-0.5, 0.9, -3, 4)
res_loss2(vec, c = c(1,1,1,5))

res_loss2(vec, c = -1)

## Slide 17


res_loss2 <- function(x, c = 1) {
  # Scale should be a single positive number
  stopifnot(length(c) == 1, c > 0)
  loss_vec <- ifelse(x^2 > c, 2*sqrt(c)*abs(x) - c, x^2)
  return(loss_vec)
}



## Slide 19

KTimesSum <- function(vec, K = 5){
  return(K*sum(vec))
}
KTimesSum(1:3); KTimesSum(1:3, K = 10)


## Slide 20


curve(res_loss2, from = -2, to = 2)

## Slide 21

rm(list=ls())
?rm

## Slide 24

x <- 7
y <- c("dog", "cat")
addition <- function(y) {x <- x + y; return(x)}
addition(1)

x
y



## Slide 25

circle.area <- function(r) {return(pi*r^2)}
circle.area(1:3)

true.pi <- pi # Save the real value
pi      <- 3 # Assign a new value
circle.area(1:3)

pi <- true.pi # Restore the real value
circle.area(1:3)


## Slide 29

gmp <- read.table("gmp.txt", as.is = TRUE, header = TRUE)
head(gmp)[1:3, ] 

## Slide 

gmp$pop <- gmp$gmp/gmp$pcgmp
head(gmp)[1:3, ]

## Slide 31


plot(gmp$pop, gmp$pcgmp, log = "x", xlab = "Population",ylab = "Per-capita Economic Output")


## Slide 32


# beta_0 = 6611; beta_1 = 1/8
curve(6611*x^{1/8}, add = TRUE, col = "blue")


## Slide 38-39 


# Parameters
max.iter         <- 100     # How long we run the alg.
stop.deriv       <- 1/100   # If derivative is small, stop
deriv.step  <- 1/1000  # This is h
step.scale       <- 1e-15   # This is c

# Initializations
iter             <- 0       # Compare to max.iteration     
deriv            <- Inf     # Compare to stop.deriv
beta             <- 0.15

while((iter < max.iter) & (deriv > stop.deriv)) {
  iter  <- iter + 1
  sse.1 <- sum((gmp$pcgmp - 6611*gmp$pop^beta)^2)
  sse.2 <- sum((gmp$pcgmp 
                - 6611*gmp$pop^(beta + deriv.step))^2)
  deriv <- (sse.2 - sse.1)/deriv.step
  beta  <- beta - step.scale*deriv
}
list(beta = beta, iteration = iter, 
     converged = (iter < max.iter))


# Plot loss function as a function of beta_1
b <- 1:100/100 
loss <- function(x){ sum((gmp$pcgmp - 6611*gmp$pop^x)^2)}
loss(.15)
plot(0:20/100,sapply(0:20/100,loss),type="l")




## Slide 40 

list(beta = beta, iteration = iter,converged = (iter < max.iter))

## Slide 42


est.scaling.exponent <- function(beta) {
  max.iter     <- 100    # How long we run the alg.
  stop.deriv   <- 1/100  # If derivative is small, stop
  deriv.step   <- 1/1000 # This is h
  step.scale   <- 1e-15  # This is c
  iter         <- 0             
  deriv        <- Inf
  while((iter < max.iter) & (abs(deriv) > stop.deriv)) {
    iter  <- iter + 1
    sse.1 <- sum((gmp$pcgmp - 6611*gmp$pop^beta)^2)
    sse.2 <- sum((gmp$pcgmp 
                  - 6611*gmp$pop^(beta + deriv.step))^2)
    deriv <- (sse.2 - sse.1)/deriv.step
    beta  <- beta - step.scale*deriv 
  }
  fit <- list(beta = beta, iteration = iter, 
              converged = (iter < max.iter))
  return(fit)
} 


## Slide 42

est.scaling.exponent <- function(beta) {
  max.iter     <- 100    # How long we run the alg.
  stop.deriv   <- 1/100  # If derivative is small, stop
  deriv.step   <- 1/1000 # This is h
  step.scale   <- 1e-15  # This is c
  iter         <- 0             
  deriv        <- Inf
  while((iter < max.iter) & (abs(deriv) > stop.deriv)) {
    iter  <- iter + 1
    sse.1 <- sum((gmp$pcgmp - 6611*gmp$pop^beta)^2)
    sse.2 <- sum((gmp$pcgmp 
                  - 6611*gmp$pop^(beta + deriv.step))^2)
    deriv <- (sse.2 - sse.1)/deriv.step
    beta  <- beta - step.scale*deriv 
  }
  fit <- list(beta = beta, iteration = iter, 
              converged = (iter < max.iter))
  return(fit)
}

# Test
est.scaling.exponent(1)
est.scaling.exponent(.12)
est.scaling.exponent(.05)
est.scaling.exponent(0)


## Slide 44

est.scaling.exponent <- function(beta, beta_0 = 6611, 
                                 max.iter = 100, stop.deriv = .01, deriv.step = .001, 
                                 step.scale = 1e-15) {
  
  iter  <- 0             
  deriv <- Inf
  
  while((iter < max.iter) & (abs(deriv) > stop.deriv)) {
    iter  <- iter + 1
    sse.1 <- sum((gmp$pcgmp - beta_0*gmp$pop^beta)^2)
    sse.2 <- sum((gmp$pcgmp 
                  - beta_0*gmp$pop^(beta + deriv.step))^2)
    deriv <- (sse.2 - sse.1)/deriv.step
    beta  <- beta - step.scale*deriv 
  }
  fit <- list(beta = beta, iteration = iter, 
              converged = (iter < max.iter))
  return(fit)
}  

## Test
est.scaling.exponent(beta=0)

## Slide 46

est.scaling.exponent <- function(beta, beta_0 = 6611, 
                                 max.iter = 100, stop.deriv = .01, deriv.step = .001, 
                                 step.scale =1e-15) {
  
  iter  <- 0             
  deriv <- Inf
  
  sse <- function(b) {sum((gmp$pcgmp - beta_0*gmp$pop^b)^2)}
  
  while((iter < max.iter) & (abs(deriv) > stop.deriv)) {
    iter  <- iter + 1
    deriv <- (sse(beta + deriv.step) - sse(beta))/deriv.step
    beta  <- beta - step.scale*deriv 
  }
  fit <- list(beta = beta, iteration = iter, 
              converged = (iter < max.iter))
  return(fit)
}  


## Test
est.scaling.exponent(beta=0)


## Slide 48

est.scaling.exponent <- function(beta, beta_0 = 6611, 
                                 response = gmp$pcgmp, predictor = gmp$pop, 
                                 max.iter = 100, stop.deriv = .01, deriv.step = .001, 
                                 step.scale =1e-15) {
  
  iter  <- 0             
  deriv <- Inf
  
  sse <- function(b) {sum((response - beta_0*predictor^b)^2)}
  
  while((iter < max.iter) & (abs(deriv) > stop.deriv)){
    iter  <- iter + 1
    deriv <- (sse(beta + deriv.step) - sse(beta))/deriv.step
    beta  <- beta - step.scale*deriv 
  }
  fit <- list(beta = beta, iteration = iter, 
              converged = (iter < max.iter))
  return(fit)
}  


## Slide 50



est.scaling.exponent <- function(beta, beta_0 = 6611, 
                                 response = gmp$pcgmp, predictor = gmp$pop, 
                                 max.iter = 100, stop.deriv = .01, deriv.step = .001, 
                                 step.scale =1e-15) {
  iter <- 0
  deriv <- Inf
  
  sse <- function(b) {sum((response - beta_0*predictor^b)^2)}
  
  for (i in 1:max.iter) {
    iter  <- iter + 1
    deriv <- (sse(beta + deriv.step) - sse(beta))/deriv.step
    beta  <- beta - step.scale*deriv 
    if (abs(deriv) < stop.deriv) {break()}
  }
  fit <- list(beta = beta, iteration = iter, 
              converged = (iter < max.iter))
  return(fit)
} 

## Test
est.scaling.exponent(beta=0,max.iter = 1000)


## Slide 
Grocery <- read.table("Kutner_6_9.txt", header=T)
lm0     <- lm(Y ~ X1 + X2 + X3, data = Grocery)
lm0

## Slide 71

library(ISLR)
head(Smarket, 3)


## Slide 72

mean(Smarket$Lag1[Smarket$Direction == "Up"])
mean(Smarket$Lag1[Smarket$Direction == "Down"])



## Slide  73

plot(Smarket$Lag1, Smarket$Lag2, col = Smarket$Direction, 
     xlab="Lag1", ylab="Lag2", main="Today's Direction")
legend("bottomright", legend = levels(Smarket$Direction), 
       col=1:length(levels(Smarket$Direction)), pch=1)


## Slide  76


K           <- 4
Lag1.new    <- -2
Lag2.new    <- 4.25

#K           <- 5
#Lag1.new    <- 2
#Lag2.new    <- 4.25


# K = 5 and new point (2, 4.25).

dists       <- sqrt((Smarket$Lag1 - Lag1.new)^2 
                    + (Smarket$Lag2 - Lag2.new)^2)

neighbors  <- order(dists)[1:K]
neighbors  <- sort(dists,index.return=T)[[2]][1:K]

neighb.dir <- Smarket$Direction[neighbors]
choice     <- names(which.max(table(neighb.dir)))
choice

# The order function 
x <- c(5,6,1,10,7)
order(x)
order(x)[1:2]


## Slide 79

KNN.decision <- function(Lag1.new, Lag2.new, K = 5, 
                         Lag1 = Smarket$Lag1, 
                         Lag2 = Smarket$Lag2,
                         Dir = Smarket$Direction) {
  
  n <- length(Lag1)
  stopifnot(length(Lag2) == n, length(Lag1.new) == 1, 
            length(Lag2.new) == 1, K <= n)
  
  dists <- sqrt((Lag1-Lag1.new)^2 + (Lag2-Lag2.new)^2)
  
  neighbors  <- order(dists)[1:K]
  neighb.dir <- Dir[neighbors]
  choice     <- names(which.max(table(neighb.dir)))
  return(choice)
}

## Test 
KNN.decision(0,0)
KNN.decision(-2,4.25,K=3)


## Slide 80

test  <- Smarket[Smarket$Year == 2005, ] 
train <- Smarket[Smarket$Year != 2005, ]

n.test <- nrow(test)
predictions <- rep(NA, n.test)

for (i in 1:n.test){
  predictions[i] <- KNN.decision(test$Lag1[i], 
                                 test$Lag2[i], Lag1 = train$Lag1, 
                                 Lag2 = train$Lag2,
                                 Dir = train$Direction) 
}

test.error <- sum(predictions != test$Direction)/n.test
test.error


## Slide 81

test  <- Smarket[Smarket$Year == 2005, ] 
train <- Smarket[Smarket$Year != 2005, ]

n.test <- nrow(test)
predictions <- rep(NA, n.test)

for (i in 1:n.test){
  predictions[i] <- KNN.decision(test$Lag1[i], 
                                 test$Lag2[i], K = 7, 
                                 Lag1 = train$Lag1, 
                                 Lag2 = train$Lag2,
                                 Dir = train$Direction) 
}

test.error <- sum(predictions != test$Direction)/n.test
test.error


## Slide 82
## Look at all K values 

# K.list=1:998
# n.test <- nrow(test)
# test.error.list <- NULL
# 
# for (k in 1:length(K.list)) {
# 
# K <- K.list[k]
# predictions <- rep(NA, n.test)
# 
# 
# for (i in 1:n.test){
#   predictions[i] <- KNN.decision(test$Lag1[i], 
#                                  test$Lag2[i], K = K, 
#                                  Lag1 = train$Lag1, 
#                                  Lag2 = train$Lag2,
#                                  Dir = train$Direction) 
# }
# 
# print(round(100*k/length(K.list),2))
# test.error <- sum(predictions != test$Direction)/n.test
# test.error.list[k] <- test.error
# }
# 
# plot(K.list,test.error.list,xlab="K",ylab="Error rate")
# lines(K.list,test.error.list)
# abline(h=mean(test.error.list),lty=2)


