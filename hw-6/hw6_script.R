## practice hw6
# 1 
n <- 100
p <- 10
s <- 3
set.seed(0)
x <- matrix(rnorm(n * p), n, p)
b <- c(-0.7, 0.7, 1, rep(0, p - s)) 
y <- x %*% b + rt(n, df = 2)

cors <- apply(x,2,cor,y) #x的每一列和y的correlation
cors
order(abs(cors),decreasing=T) #return index in decreasing order of abs(correlation)

# 2
xval <- seq(-5,5,0.01)
plot(xval,dnorm(xval),type='l',ylab='Density')
lines(xval,dt(xval,3),col='red')

# 3
psi <- function(r,c=1){
  return(ifelse(r^2 > c^2, 2*c*abs(r) - c^2, r^2))
}

huber.loss <- function(beta){
  r <- y - x %*% beta
  return(sum(psi(r)))
}

# 4
library(numDeriv)
grad.descent <- function(f, x0, max.iter = 200, step.size = 0.05, stopping.deriv = 0.01, ...) {
  n    <- length(x0) #维度
  xmat <- matrix(0, nrow = n, ncol = max.iter) #将迭代的x存入此matrix中
  xmat[,1] <- x0 #x0是initial vector
  
  for (k in 2:max.iter) {
    # Calculate the gradient
    grad.cur <- grad(f, xmat[ ,k-1], ...) #计算f在x=xmat[,k-1]处的gradient
    
    # Should we stop?
    if (all(abs(grad.cur) < stopping.deriv)) {
      k <- k-1; break
    }
    
    # Move in the opposite direction of the grad
    xmat[ ,k] <- xmat[ ,k-1] - step.size * grad.cur #迭代新的x
  }
  
  xmat <- xmat[ ,1:k] # Trim,保留iteration的x
  return(list(x = xmat[,k], xmat = xmat, k = k))
}

beta <- rep(0,p)
gd <- grad.descent(huber.loss,beta,max.iter = 200,step.size = 0.001,stopping.deriv = 0.1)
gd$k 
gd$x

# 5
obj <- apply(gd$xmat,2,huber.loss)
plot(y=obj,x=1:gd$k,type='l',xlab='Iteration',ylab='Objective function value',
     main='Objective function value during gradient descent')
  
# 6
# step.size=0.1, not converging!
gd2 <- grad.descent(huber.loss,beta,max.iter = 200,step.size = 0.1,stopping.deriv = 0.1)
obj2 <- apply(gd2$xmat[,(gd2$k-50):gd2$k],2,huber.loss)
plot(y=obj2,x=(gd2$k-50):gd2$k,type='l',xlab='Iteration',ylab='Objective function value',
     main='Objective function value during gradient descent (last 50)')


# 7
gd$x
b

sparse.grad.descent <- function(f, x0, max.iter = 200, step.size = 0.05, stopping.deriv = 0.01, ...) {
  n    <- length(x0) #维度
  xmat <- matrix(0, nrow = n, ncol = max.iter) #将迭代的x存入此matrix中
  xmat[,1] <- x0 #x0是initial vector
  
  for (k in 2:max.iter) {
    # Calculate the gradient
    grad.cur <- grad(f, xmat[ ,k-1], ...) #计算f在x=xmat[,k-1]处的gradient
    
    # Should we stop?
    if (all(abs(grad.cur) < stopping.deriv)) {
      k <- k-1; break
    }
    
    # Move in the opposite direction of the grad
    # sparse!!
    xmat[,k] <- ifelse(abs(xmat[,k-1]-step.size*grad.cur)<0.05,0,xmat[,k-1]-step.size*grad.cur) #迭代新的x
  }
  
  xmat <- xmat[ ,1:k] # Trim,保留iteration的x
  return(list(x = xmat[,k], xmat = xmat, k = k))
}


beta <- rep(0,p)
gd.sparse <- sparse.grad.descent(huber.loss,beta,max.iter = 200,step.size = 0.001,stopping.deriv = 0.1)
gd.sparse$k 
gd.sparse$x


# 8
model <- lm(y~x-1)
coef(model)
gd$x
gd.sparse$x

mse.calc <- function(beta){
  return(mean((b-beta)^2))
}

mse.calc(coef(model))
mse.calc(gd$x)
mse.calc(gd.sparse$x)
# sparse estimate has the smallest MSE

# 9
set.seed(10)
y = x %*% b + rt(n, df=2)
beta <- rep(0,p)
gd.new <- grad.descent(huber.loss,beta,max.iter = 200,step.size = 0.001,stopping.deriv = 0.1)
gd.sparse.new <- sparse.grad.descent(huber.loss,beta,max.iter = 200,step.size = 0.001,stopping.deriv = 0.1)

mse.calc(gd.new$x)
mse.calc(gd.sparse.new$x)
# regular gradient descent estimate has the smallest MSE
# this suggests high variability in the sparse estimate

# 10
set.seed(10)
gd.mse <- rep(NA,10)
gd.sparse.mse <- rep(NA,10)
for (i in 1:10){
  y = x %*% b + rt(n, df=2)
  beta <- rep(0,p)
  gd <- grad.descent(huber.loss,beta,max.iter = 200,step.size = 0.001,stopping.deriv = 0.1)
  gd.sparse <- sparse.grad.descent(huber.loss,beta,max.iter = 200,step.size = 0.001,stopping.deriv = 0.1)
  gd.mse[i] <- mse.calc(gd$x)
  gd.sparse.mse[i] <- mse.calc(gd.sparse$x)
  
}
mean(gd.mse)
mean(gd.sparse.mse)

min(gd.mse)
min(gd.sparse.mse)
# the minimum of the MSE with sparse estiamtes is much smaller
# but the mean of the MSE for sparse estimates is larger
# this support the interpretation-the sparse estimate MSE has large variance




  

