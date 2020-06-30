
####################################
######  Begin Document 
####################################



## Slide 19 -------------------------------

x <- c(5, 29, 13, 87)
x

## Slide 22 -------------------------------

x <- 1:50
x


## Slide 30 -------------------------------

x <- rnorm(100,mean=10,sd=3)
length(x)
head(x,20)

hist(x)


## Slide 32 -------------------------------

z <- 1:10
z

rm(z)
z



## Slide 36 -------------------------------

x <- 2
mode(x)
typeof(x)
y <- as.integer(3)
typeof(y)



## Slide 37 -------------------------------



z <- 1 - 2i
z
typeof(z)

## Slide 38 -------------------------------

name <- "Columbia University"
name
typeof(name)

## Slide 39 -------------------------------

a <- TRUE
b <- F
a
b
typeof(a)


### Check Yourself -------------------------------
## Slide 41 -------------------------------

3*TRUE # Logicals in arithmetic
mode(3*TRUE)
mode("147")


## Slide 43 -------------------------------


x <- c(2, pi, 1/2, 3^2)
x

y <- c("NYC", "Boston", "Philadelphia")
y

## Slide 44 -------------------------------

z <- 5:10
z
  
u <- rep(1, 18)
u


## Slide 45 -------------------------------

v <- c()

v[1] <- TRUE
v[2] <- TRUE
v[3] <- FALSE

v

## Slide 46 -------------------------------

vec1 <- rep(-27, 3)
vec1

vec2 <- c(vec1, c(-26, -25, -24))
vec2



## Slide 47  -------------------------------


mat <- matrix(1:9, nrow = 3, ncol = 3)
mat

## Slide 48  -------------------------------


mat <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
mat


## Slide 49  -------------------------------

this_mat <- matrix(nrow = 2, ncol = 2)

this_mat[1,1] <- sqrt(27)
this_mat[1,2] <- round(sqrt(27), 3)
this_mat[2,1] <- exp(1)
this_mat[2,2] <- log(1)

this_mat

## Slide 50 -------------------------------


vec1 <- rep(0, 4)
vec2 <- c("We're", "making", "matrices", "!")

final_mat <- rbind(vec1, vec2)
final_mat


## Slide 51 -------------------------------

this_mat # Defined previously
colnames(this_mat) # Nothing there yet

## Slide 52 -------------------------------

colnames(this_mat) <- c("Column1", "Column2")
this_mat

## Slide 53 -------------------------------

vec <- c(1.75, TRUE, "abc")
vec
str(vec)

## Slide 54 -------------------------------

# What does the str() function do?

# Function help:
?str

# Fuzzy matching:
??"structure"

## Slide 56 -------------------------------

y <- c(27, -34, 19, 7, 61)

y[2]

y[3:5]

y[c(1, 4)]


## Slide 57 -------------------------------

y <- c(27, -34, 19, 7, 61)
y
y[c(1, 4)] <- 0
y

EE## Slide 58 -------------------------------

y <- c(27, -34, 19, 7, 61)
y
y[-c(1, 4)]
y <- y[-1]
y

## Slide 59 -------------------------------

mat <- matrix(1:8, ncol = 4)
mat
mat[, 2:3]

## Slide 60 -------------------------------

this_mat
this_mat[, "Column2"]
this_mat[, -1]

## Slide 62 -------------------------------

# Installing the "pixmap" package.
install.packages("pixmap")
library("pixmap")

## Slide 64 -------------------------------

# Set working directory
getwd()
setwd("/Users/wangshuyuan/Desktop/GR5206-Stat Comp & DS/Course material/")
casablanca_pic <- read.pnm("casablanca.pgm")
casablanca_pic

plot(casablanca_pic)

## Slide 66 -------------------------------

dim(casablanca_pic@grey)
casablanca_pic@grey[360, 100]
casablanca_pic@grey[180, 10]

# Note: locator

locator(1)

## Slide 67 -------------------------------

casablanca_pic@grey[15:70, 220:265] <- 1
plot(casablanca_pic)


## Slide 70 -------------------------------

z <- matrix(rep(1:9), nrow = 3)
colnames(z) <- c("First", "Second", "Third")
z

## Slide 71 -------------------------------

z
z[2:3, "Third"]
c(z[,-(2:3)], "abc")

## Slide 72 -------------------------------

z
rbind(z[1,], 1:3)


## Slide 79 -------------------------------

# Define covariate and response variable 
x <- c(49.3,59.3,68.3,48.1,57.61,78.1,76.1)
y <- c(1894,2050,2353,1838,1948,2528,2568) 

## Slide 80 -------------------------------

n <- length(x) # Sample size
n

max(x)
sd(x)


## Slide 81 -------------------------------

summary(x) # Summary statistics
summary(y)


## Slide 83 -------------------------------

u <- c(1,3,5)
v <- c(1,3,5)
v + 4 # Recycling
v + c(1,3) # Recycling
v + u

## Slide 84 -------------------------------

u <- c(1,3,5)
v <- c(1,3,5)

'+'(v,u)
'*'(v,u)


## Slide 86 -------------------------------

plot(x,y, xlab = "Body Mass", ylab = "Energy Expenditure")



## Slide 87 -------------------------------

# First, compute x and y deviations 
dev_x <- x - mean(x)
dev_y <- y - mean(y)

# Next, compute sum of squares of xy and xx
Sxy <- sum(dev_x * dev_y)
Sxx <- sum(dev_x * dev_x)



## Slide 88 -------------------------------


# Compute the estimated slope 
Sxy/Sxx

# Compute the estimated intercept 
mean(y) - (Sxy/Sxx) * mean(x)

## Slide 91 -------------------------------

# Define matrix A
A <- matrix(c(3,1,1,-2,1/2,1,1,-12,3), nrow = 3) 

# Define vector b
b <- c(-1, 2, 3) 

# Use the solve function 
solve(A, b) 

## Slide 92 -------------------------------


x <- c(1, 2, 0) # Define solution vector x
A %*% x         # Then check with matrix multiplication

## Slide 96 -------------------------------

1 > 3
1 == 3
1 != 3


## Slide 97 -------------------------------

(1 > 3) & (4*5 == 20)
(1 > 3) | (4*5 == 20)


## Slide 98 -------------------------------

c(0,1,4) < 3
which(c(0,1,4) < 3)
which(c(TRUE, TRUE, FALSE))

## Slide 99 -------------------------------

c(0,1,4) >= c(1,1,3)
c("Cat","Dog") == "Dog"

## Slide 100 -------------------------------

w <- c(-3, 20, 9, 2)

w[w > 3] ### Extract elements of w greater than 3

### What's going on here?
w > 3
w[c(FALSE, TRUE, TRUE, FALSE)]


## Slide 101 -------------------------------

w <- c(-3, 20, 9, 2)

### Extract elements of w with squares between 3 and 10
w[w*w >= 3 & w*w <= 10]

w*w >= 3 ### What's going on here?
w*w <= 10
w*w >= 3 & w*w <= 10


## Slide 102 -------------------------------

w <- c(-1, 20, 9, 2)
v <- c(0, 17, 10, 1)

### Extract elements of w greater than elements from v
w[w > v]

### What's going on here?
w > v
w[c(FALSE, TRUE, FALSE, TRUE)]


## Slide 103 -------------------------------

M <- matrix(c(rep(4,5), 5:8), ncol=3, nrow=3)
M

### We can do element-wise comparisons with matrices too.
M > 5



## Slide 104 -------------------------------

M
M[,3] < 8
M[M[,3] < 8, ]

## Slide 105 -------------------------------

M

### Assign elements greater than 5 with zero 
M[M > 5] <- 0
M


### Check yourself -------------------------------
## Slide 106 -------------------------------


z <- matrix(c(1:3, TRUE, FALSE, TRUE, 9, 16, 25), nrow = 3)
colnames(z) <- c("First", "Second", "Third")
z



## Slide 107 -------------------------------
z
z[z[, "Second"], ]


## Slide 108 -------------------------------

z
z[, 1] != 1
z[(z[, 1] != 1), 3]


## Slide 109 -------------------------------

z[(z[, 1] != 1), 3]
z[(z[, 1] != 1), 3, drop = FALSE]

## Slide 111 -------------------------------

length(c(-1, 0, NA, 5))
length(c(-1, 0, NULL, 5))

## Slide 112 -------------------------------

t <- c(-1,0,NA,5)
mean(t)
mean(t, na.rm = TRUE)

### NA values are missing, but NULL values don't exist.
s <- c(-1, 0, NULL, 5)
mean(s)


## Slide 113 -------------------------------


# Define an empty vector
x <- NULL
# Fill in the vector
x[1] <- "Blue"
x[2] <- "Green"
x[3] <- "Red"
x

## Slide 117 -------------------------------

# Define covariate and response variable 
x <- c(49.3,59.3,68.3,48.1,57.61,78.1,76.1)
y <- c(1894,2050,2353,1838,1948,2528,2568) 

# Combine data into single matrix
data <- cbind(x, y)

# Summary values for x and y
sum_x <- summary(x)
sum_y <- summary(y)

# We computed Sxy and Sxx previously 
est_vals <- c(Sxy/Sxx, mean(y) - Sxy/Sxx*mean(x))


## Slide 118 -------------------------------

body_fat <- list(variable_data = data, 
                 summary_x = sum_x, summary_y = sum_y, 
                 LOBF_est = est_vals)


## Slide 120 -------------------------------

# Extract the first list element 
body_fat[[1]]

## Slide 121 -------------------------------

# Extract the Line of Best Fit estimates 
body_fat$LOBF_est

# Extract the summary of x 
body_fat[["summary_x"]]

## Slide 123 -------------------------------

# Single bracket 
body_fat[1]

## Slide 124 -------------------------------

# Double bracket 
body_fat[[1]]


## Slide 125 -------------------------------


# Single bracket
# Try and run the below code (uncomment it!)
body_fat[1][1:3,]

# Double bracket 
body_fat[[1]][1:3,]

## Slide 127 -------------------------------

# Single bracket
body_fat["LOBF_est"]

# Double bracket 
body_fat[["LOBF_est"]]

# Inside the pepper packet we can extract the slope 
body_fat[["LOBF_est"]][1]


