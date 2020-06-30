



# Slide 23
setwd("~/Desktop/Data")
cancer <- read.table("logistic.txt")




# Slide 27

logistic.NLL <- function(beta,data=cancer) {
  
  beta_0 <- beta[1]
  beta_1 <- beta[2]
  y <- data$y
  x <- data$x
  linear.component <- beta_0 + beta_1*x
  p.i <- exp(linear.component)/(1+exp(linear.component))
  return(-sum(dbinom(y,size=1,prob=p.i,log=TRUE)))
}

logistic.NLL(beta=c(-1,.5),data=cancer)

# Slide 28
nlm(logistic.NLL,p=c(-1,.5),data=cancer)

# Slide 30
cancer <- read.table("logistic.txt")
model <- glm(y~x,data=cancer,family=binomial(link="logit"))
model

# Slide 31
summary(model)

# Slide 32
x.test <- data.frame(x=7)
linear.pred <- predict(model,newdata = x.test)
linear.pred
exp(linear.pred)/(1+exp(linear.pred))

# Slide 37

n <- nrow(cancer)
X <- cbind(rep(1,n),cancer$x)
y <- cancer$y

# Iterations
R <- 10

# Starting values
theta <- matrix(0,nrow=2,ncol=R+1)
beta0 <- log(mean(y)/(1-mean(y)))
theta[,1] <- c(beta0,0)

for (i in 1:R) {
  theta.i <- theta[,i]
  linear.term.i <- theta.i[1]*X[,1]+theta.i[2]*X[,2]
  p.i <- exp(linear.term.i)/(1+exp(linear.term.i))
  W.i <- diag(p.i*(1-p.i))
  theta[,i+1] <- theta[,i] + solve(t(X)%*%W.i%*%X)%*%(t(X)%*%(y-p.i))
}

# Slide 38

# Iteratively Reweighted Least Squares
theta
# Base R code 
model$coefficients

