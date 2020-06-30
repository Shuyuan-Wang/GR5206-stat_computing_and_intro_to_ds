

####################################
######  Begin Document 
####################################


setwd("~/Desktop/Data")


## Slide 4 -------------------------------

#1
sum((iris$Species=='versicolor') & (iris$Petal.Width<=1.2))

#2
tapply(iris$Petal.Length,iris$Species,mean)['setosa']

mean(iris$Petal.Length[iris$Species=='setosa'])

#3
#iris[iris$Sepal.Width>=3.0,]['Species']

table(iris$Species[iris$Sepal.Width>=3.0])

#4
ifelse(iris$Species=='versicolor',1,0)

iris$Versicolor <- ifelse(iris$Species=='versicolor',1,0)

table(iris$Versicolor)

## Slide 6 -------------------------------

getwd()
setwd("~/Desktop/GR5206-Stat Comp & DS/Course material")
diamonds         <- read.csv("diamonds.csv", as.is = T)
diamonds$cut     <- factor(diamonds$cut)
diamonds$color   <- factor(diamonds$color)
diamonds$clarity <- factor(diamonds$clarity)

## Slide 14 -------------------------------

table(diamonds$cut) 
names(table(diamonds$cut))

## Slide 15 -------------------------------

barplot(height = table(diamonds$cut), 
        names.arg = names(table(diamonds$cut)))


## Slide 16 -------------------------------

levels(diamonds$cut)
diamonds$cut <- factor(diamonds$cut, level = c("Fair", 
                                               "Good", "Very Good", "Premium", 
                                               "Ideal"))

#改一下顺序，比如原来5代表very good，改成5代表ideal

levels(diamonds$cut)


## Slide 17 -------------------------------

barplot(height = table(diamonds$cut), names.arg = names(table(diamonds$cut)))

## Slide 19 -------------------------------


hist(diamonds$carat, main = "Histogram of Carats", 
     xlab = "Carats")


## Slide 20 -------------------------------


hist(diamonds$carat[diamonds$carat < 3], breaks = 100, 
     main = "Histogram of Carats", xlab = "Carats")


## Slide 21 -------------------------------


hist(diamonds$carat[diamonds$carat < 3], breaks = seq(0,3,by=.1), 
     main = "Histogram of Carats", xlab = "Carats")


## Slide 22 -------------------------------


hist(diamonds$carat[diamonds$carat < 3], breaks = c(0,.5,.75,1.25,2,3), 
     main = "Histogram of Carats", xlab = "Carats")


## Slide 29 -------------------------------

boxplot(price ~ cut, data = diamonds, ylab = "Price", 
        xlab = "Cut")

## Slide 29 -------------------------------


plot(diamonds$carat, diamonds$price, xlab = "Carats", 
     ylab = "Price ($)")


## Slide 27
plot(diamonds$carat, diamonds$price, xlab = "Carats", 
     ylab = "Price ($)")


## Slide 30
boxplot(carat ~ cut, data = diamonds, ylab = "Carats", 
        xlab = "Cut")

## Slide 34 -------------------------------

set.seed(1)

rows       <- dim(diamonds)[1]
small_diam <- diamonds[sample(1:rows, 1000), ]

## Slide 35 -------------------------------

plot(log(small_diam$carat), log(small_diam$price),
     col = small_diam$cut)
legend("bottomright", legend = levels(small_diam$cut), 
       fill = 1:length(levels(small_diam$cut)), cex = .5)


## Slide 37 -------------------------------


abline(8, 0, col = "orange", lty = 2)
lm1 <- lm(log(small_diam$price) ~ log(small_diam$carat))
abline(lm1)


## Slide 39 -------------------------------

cuts        <- levels(small_diam$cut)
col_counter <- 1

for (i in cuts) {
  this_cut    <- small_diam$cut == i
  this_data   <- small_diam[this_cut, ]
  this_lm     <- lm(log(this_data$price) 
                    ~ log(this_data$carat))
  abline(this_lm, col = col_counter)
  col_counter <- col_counter + 1
}


## Slide 32 -------------------------------

points(-0.4, 6.8,  pch = "*", col = "purple")



## Slide 44 -------------------------------

text(-0.4, 6.8 - .2, "New Diamond", cex = .5)



