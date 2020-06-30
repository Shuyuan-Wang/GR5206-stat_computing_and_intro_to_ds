

setwd("/Users/wangshuyuan/Desktop/GR5206-DS/Course material")

## Slide 9

strikes <- read.csv("strikes.csv", as.is = TRUE)
dim(strikes) 
head(strikes, 3)


## Slide 11

italy.strikes <- subset(strikes, country == "Italy")
# Equivalently,
italy.strikes <- strikes[strikes$country == "Italy", ]
dim(italy.strikes)

## Slide 12

head(italy.strikes, 5)


## Slide 13


italy.fit <- lm(strike.volume ~ left.parliament, 
                data = italy.strikes)
#plot(y~x,data=)
plot(strike.volume~left.parliament, data = italy.strikes, 
     main="Italy Strike Volume Versus Leftwing Alignment", 
     ylab = "Strike volume", xlab = "Leftwing Alignment")
abline(italy.fit, col = 2)

## Slide 14


my.strike.lm <- function(country.df) {
  return(lm(strike.volume ~ left.parliament, 
            data = country.df)$coeff)
}
my.strike.lm(subset(strikes, country == "Italy"))


## Slide 15


strike.coef  <- NULL
my.countries <- c("France", "Italy", "USA")
for (this.country in my.countries) {
  country.dat <- subset(strikes, country == this.country)
  new.coefs   <- my.strike.lm(country.dat)
  strike.coef <- cbind(strike.coef, new.coefs)
}
colnames(strike.coef) <- my.countries
strike.coef


## Slide 17


strikes.split <- split(strikes, strikes$country)
names(strikes.split)

## Slide 18


strike.coef <- sapply(strikes.split[1:12], my.strike.lm)
strike.coef



## Slide 19



plot(1:ncol(strike.coef), strike.coef[2, ], xaxt = "n",
     xlab = "", ylab = "Regression coefficient", 
     main="Countrywise labor activity by leftwing score")
axis(side = 1, at = 1:ncol(strike.coef),
     labels = colnames(strike.coef), las = 2, 
     cex.axis = 0.5)
abline(h = 0, col = "grey")



## Check Yourself
## Slide 22


three.mean <- function(df) {
  return(apply(df[, c("unemployment", "inflation", 
                      "strike.volume")], 2, mean))
}
years.split <- split(strikes, strikes$year)
years.mat   <- sapply(years.split, three.mean)
dim(years.mat)



## Slide 23


years.mat[, 1:8]


## Slide 24


max.rate <- max(years.mat[1:2, ])
min.rate <- min(years.mat[1:2, ])
plot(colnames(years.mat), years.mat[1, ], xlab = "Year", 
     ylab="Rate", type="l", ylim = c(min.rate, max.rate))
points(colnames(years.mat), years.mat[2, ], type = "l", 
       col = "red")
legend("topleft", c("Unemployment", "Inflation"), 
       fill = c("black", "red"), cex = .5)


## Slide 30

my.array           <- array(1:27, c(3,3,3))
rownames(my.array) <- c("R1", "R2", "R3")
colnames(my.array) <- c("C1", "C2", "C3")
dimnames(my.array)[[3]] <- c("Bart", "Lisa", "Maggie")


## Slide 31


my.array


## Slide 32


my.array[, , 3] #提取第三个dimension的数值


## Slide 33


library(plyr)
aaply(my.array, 1, sum) # Get back an array
adply(my.array, 1, sum) # Get back a data frame

## Slide 34

alply(my.array, 1, sum) # Get back a list



## Slide 35


aaply(my.array, 2:3, sum) # Get back a 3 x 3 array



## Slide 36


adply(my.array, 2:3, sum) # Get back a data frame


## Slide 37


alply(my.array, 2:3, sum) # Get back a list



## Slide 39


my.list <- list(nums = rnorm(1000), lets = letters, 
                pops = state.x77[ ,"Population"])
head(my.list[[1]], 5)
head(my.list[[2]], 5)
head(my.list[[3]], 5)

## Slide 40


laply(my.list, range) # Get back an array, (same mode)
ldply(my.list, range) # Get back a data frame

## Slide 41

llply(my.list, range) # Get back a list


## Slide 42

# Doesn't work! Outputs have different types/lengths
# laply(my.list, summary) 
# ldply(my.list, summary)
llply(my.list, summary) # Works just fine

## Slide 43

par(mfrow = c(3, 3), mar = c(4, 4, 1, 1))
a_ply(my.array, 2:3, plot, ylim = range(my.array), 
      pch = 19, col = 6)

## Slide 46


# Function to compute coefficients from regressing number 
# of strikes (per 1000 workers) on leftwing share of the 
# government

my.strike.lm <- function(country.df) {
  return(coef(lm(strike.volume ~ left.parliament, 
                 data=country.df)))
}

## Slide 47

# Getting regression coefficients separately 
# for each country, old way:

strikes.list  <- split(strikes, f = strikes$country)
strikes.coefs <- sapply(strikes.list, my.strike.lm)
strikes.coefs[, 1:12]


## Slide 48


# Getting regression coefficient separately for each 
# country, new way, in three formats:

strike.coef.a <- daply(strikes, .(country), my.strike.lm)

# Get back an array, note the difference to sapply()

head(strike.coef.a)

## Slide 49


strike.coef.d <- ddply(strikes, .(country), my.strike.lm)
head(strike.coef.d) # Get back a data frame

## Slide 50


strike.coef.l <- dlply(strikes, .(country), my.strike.lm)
head(strike.coef.l, 3) # Get back a list



## Slide 51

# First create a variable that indicates whether the year 
# is pre 1975, and add it to the data frame
strikes$yearPre1975 <- strikes$year <= 1975

# Then use (say) ddply() to compute regression 
# coefficients for each country pre & post 1975

strike.coef.75 <- ddply(strikes, .(country, yearPre1975), 
                        my.strike.lm)
dim(strike.coef.75) # Note there are 18 x 2 = 36 rows


## Slide 52


head(strike.coef.75)


## Slide 53

# Can also create factor variables on-the-fly with I()

strike.coef.75 <- ddply(strikes, 
                        .(country, I(year<=1975)),
                        my.strike.lm)
dim(strike.coef.75) # Again, 18 x 2 = 36 rows

## Slide 54


head(strike.coef.75)




## Check yourself
## Slide 58

inflation.mean <- function(country.df) {
  return(mean(country.df$inflation))
}
inflation75 <- ddply(strikes, .(country, I(year<=1975)),
                     inflation.mean)
dim(inflation75)



## Slide 59

head(inflation75)


## Slide 60

split.list <- list(strikes$country, I(strikes$year<=1975))
data.split <- split(strikes, f = split.list)
inflation75 <- sapply(data.split, inflation.mean)
dim(inflation75)
head(inflation75)


