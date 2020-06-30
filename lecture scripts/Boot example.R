
# data
x <- c(49.3,59.3,68.3,48.1,57.61,78.1,76.1)
y <-  c(1894,2050,2353,1838,1948,2528,2568)
my_data <- data.frame(x,y)

# lm summary
summary(lm(y~x))

# 95% conf int
confint(lm(y~x))

beta_hat <- lm(y~x)$coefficients["x"]

# Boot loop
B <- 1000
boot_slope <- rep(NA,B)
for (b in 1:B) {
  
  boot_ind <- sample(1:7,size=7,replace = T)
  boot_slope[b] <- lm(y~x,data=my_data[boot_ind,])$coefficients["x"]
  
}

hist(boot_slope,breaks=40)

# Compare
confint(lm(y~x))

# Boot interval
2*beta_hat-quantile(boot_slope,probs = .975)
2*beta_hat-quantile(boot_slope,probs = .025)

