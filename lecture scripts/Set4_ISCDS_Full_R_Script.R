

####################################
######  Begin Document 
####################################

#P5
#1
hist(iris$Sepal.Width,xlab='sepal width',breaks = 20)

#2
iris$versi <- ifelse(iris$Species=='versicolor','versi','not versi')
plot(y=iris$Sepal.Width,x=iris$Sepal.Length,col=factor(iris$versi))
legend('topright',legend = levels(factor(iris$versi)),fill=1:2)

#plot(y=iris$Sepal.Width,x=iris$Sepal.Length,col=factor(iris$Species=='versicolor'))

#3
boxplot(iris$Petal.Length~iris$Species)

## Slide 20 -------------------------------

setwd("~/Desktop/Data")
Grocery <- read.table("Kutner_6_9.txt", header=T)
head(Grocery)

# Construct design matrix
X <- cbind(rep(1,52), Grocery$X1, Grocery$X2, Grocery$X3)

## Slide 21 -------------------------------

beta_hat <- solve((t(X) %*% X)) %*% t(X) %*% Grocery$Y
round(t(beta_hat), 2)


## Slide 23 -------------------------------

lm0 <- lm(Y ~ X1 + X2 + X3, data = Grocery)
lm0


## Slide 25 -------------------------------

lm0 <- lm(Y ~ X1 + X2 + X3, data = Grocery)
residuals(lm0)[1:5] 
fitted(lm0)[1:5]

## Slide 26 -------------------------------

summary(lm0)


## Slide 28 -------------------------------

A <- cbind(c(3,-2,7),c(-2,12,9),c(8,-16,5))
qr(A)$rank

## Slides 30, 31 -------------------------------

# Define X_4
X4 <- ifelse(Grocery$X3==1,0,1)

# Define redundant design matrix 
X <- cbind(rep(1,52), Grocery$X1, Grocery$X2, Grocery$X3,X4)
head(X)

# Rank, Determinant, Inverse 
qr(t(X) %*% X)$rank
det(t(X) %*% X)
solve((t(X) %*% X))

# Linear model
lm(Y~X1+X2+X3+X4,data=Grocery)

# Define design matrix 
X <- cbind(rep(1,52), Grocery$X1, Grocery$X2, Grocery$X3)
head(X)

# Rank, Determinant, Inverse 
qr(t(X) %*% X)$rank
det(t(X) %*% X)
solve((t(X) %*% X))

# Linear model
lm(Y~X1+X2+X3,data=Grocery)

######################################################
# Bootstrap
######################################################


## Slide 37 -------------------------------

set.seed(1)
mu = 0

n <- 100

vec <- rnorm(n, mean = mu)

head(vec)

mean(vec)


## Slide 38 -------------------------------


B <- 1000
estimates <- vector(length = B)
for (b in 1:B) {
  new_sample <- sample(vec, size = n, replace = TRUE)
  estimates[b] <- mean(new_sample)
}
head(estimates)


## Slide 41 -------------------------------


var(estimates)


## Slide 42 -------------------------------
# Extended examples

L <- 2*mean(vec)-quantile(estimates,.975);L
U <- 2*mean(vec)-quantile(estimates,.025);U


## Slide 45 -------------------------------
# Extended examples

L <- quantile(estimates,.025);L 
U <- quantile(estimates,.975);U

