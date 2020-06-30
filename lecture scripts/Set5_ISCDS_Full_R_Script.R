



####################################
######  Begin Document 
####################################


setwd("~/Desktop/Data")


## Slide 7 -------------------------------

diamonds         <- read.csv("diamonds.csv", as.is = T)
diamonds$cut     <- factor(diamonds$cut)
diamonds$color   <- factor(diamonds$color)
diamonds$clarity <- factor(diamonds$clarity)

set.seed(1)
rows <- dim(diamonds)[1]
diam <- diamonds[sample(1:rows, 1000), ]

## Slide 8 -------------------------------

plot(log(diam$carat), log(diam$price), col = diam$cut)
legend("bottomright", legend = levels(diam$cut), 
       fill = 1:length(levels(diam$cut)), cex = .5)

## Slide 9 -------------------------------

cuts        <- levels(diam$cut)
col_counter <- 1

for (i in cuts) {
  this_cut    <- diam$cut == i
  this_data   <- diam[this_cut, ]
  this_lm     <- lm(log(this_data$price) 
                    ~ log(this_data$carat))
  abline(this_lm, col = col_counter)
  col_counter <- col_counter + 1
}

## 11
iris$setosa <- ifelse(iris$Species=='setosa',1,0)
plot(x=iris$Sepal.Width,y=iris$Sepal.Length,col=factor(iris$setosa)) #factor一定是从1开始的（col=0表示白色）
lm1 <- lm(Sepal.Length~Sepal.Width,data=iris[iris$setosa==1,])
lm2 <- lm(Sepal.Length~Sepal.Width,data=iris[iris$setosa==0,])
abline(lm1,col='red')
abline(lm2) 

## Slide 12 -------------------------------

points(-0.4, 6.8,  pch = "*", col = "purple")



## Slide 13 -------------------------------

text(-0.4, 6.8 - .2, "New Diamond", cex = .5)


## Slide 28 -------------------------------

levels(diamonds$cut)
diamonds$cut <- factor(diamonds$cut, level = c("Fair", 
                                               "Good", "Very Good", "Premium", 
                                               "Ideal"))

par(mfrow=c(1,2))
barplot(height = table(diamonds$cut), 
        names.arg = names(table(diamonds$cut)),
        main="Barchart")

pie(table(diamonds$cut), labels = names(table(diamonds$cut)),
    main="Pie Chart",cex=.75)

## Slide 30 -------------------------------

par(mfrow=c(1,2),mai=c(.5,.4,.5,.4))
barplot(height = table(diamonds$cut), 
        names.arg = names(table(diamonds$cut)),
        main="Barchart")

pie(table(diamonds$cut), labels = names(table(diamonds$cut)),
    main="Pie Chart",cex=.75)

par(mfrow=c(1,1))

## Slide 36 -------------------------------

library(ggplot2)

dim(mpg)
head(mpg, 3)

## Slide 37 -------------------------------

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))


## 40 -----------------------------------
# for categorical variables
#single bargraph:
ggplot(data=mpg)+
  geom_bar(mapping=aes(x=class))

#multiple bargraph:
ggplot(data=mpg)+
  geom_bar(mapping=aes(x=class,fill=drv))

## Slide 43 -------------------------------

ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y=hwy, color=class))

## 44 --------------------------------------
#在mapping里包括的颜色、透明度、形状等会自动生成legend
ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y=hwy, alpha=class)) 

ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y=hwy, size=class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y=hwy, shape=class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y=hwy), color="blue") 
#change the color of the entire plot

ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y=hwy, alpha=cty))



## Slide 46 -------------------------------

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 3)

## 47
ggplot(data=mpg)+
  geom_point(mapping=aes(x=displ,y=hwy))+
  facet_grid(drv~class)

## Slide 50 -------------------------------

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

# Or 
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy),method = "lm")



## Slide 53 -------------------------------

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))


## Slide 54 -------------------------------


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(x=3, y=30),color = "red") +
  geom_text(mapping = aes(x=3, y=31, label = "New Point"), size=4) +
  labs(title = "New Plot", x = "Engine Weight", y = "Highway mpg")

## Slide 55 -------------------------------

ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price),alpha = 1/10)


## Some other examples -------------------------------

## Slide 56 -------------------------------
#long way to do
iris_not_setosa <- iris[iris$Species!='setosa',]
lm_not_setosa <- lm(Sepal.Length~Sepal.Width,data=iris_not_setosa)
not_setosa_intercept <- lm_not_setosa$coefficients[1]
not_setosa_slope <- lm_not_setosa$coefficients[2]

iris_setosa <- iris[iris$Species=='setosa',]
lm_setosa <- lm(Sepal.Length~Sepal.Width,data=iris_setosa)
setosa_intercept <- lm_setosa$coefficients[1]
setosa_slope <- lm_setosa$coefficients[2]

ggplot(data=iris)+
  geom_point(mapping=aes(x=Sepal.Width,y=Sepal.Length,color=factor(setosa)))+
  geom_abline(intercept = not_setosa_intercept,slope=not_setosa_slope)+
  geom_abline(intercept = setosa_intercept,slope=setosa_slope)
#如何统一回归线与点的颜色，老师没讲

# easy way
ggplot(data=iris)+
  geom_point(mapping=aes(x=Sepal.Width,y=Sepal.Length,color=factor(setosa)))+
  geom_smooth(mapping=aes(x=Sepal.Width,y=Sepal.Length,color=factor(setosa)),
              method=lm,se=F,fullrange=T)


## Slide 58 -------------------------------

ggplot(diamonds)+
  geom_bar(aes(x=cut))

upper <- diamonds$price > quantile(diamonds$price,probs = .75) 
diamonds$Expensive <- ifelse(upper,"high","not-high")

theme_update(plot.title = element_text(hjust = 0.5)) #center title
ggplot(data=diamonds) +
  geom_bar(aes(x=cut,fill=factor(Expensive)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Title is Centered",fill="Price",x="")


## Slide 60 -------------------------------


x <- seq(-5,5,by=.01)
hist_data <- data.frame(x.var=rnorm(1000))
plot_data <- data.frame(x=x,f=dnorm(x))

ggplot(hist_data)+
  geom_histogram(mapping=aes(x=x.var,y=..density..), #y: to use density or frequency
                 col="blue",fill="white",binwidth=.2)+
  geom_line(plot_data,mapping = aes(x = x, y = f), #referencing a different df plot_data
            col="red")+
  labs(title = "Nomal Example",x="x",y="Density")


## Slide 60 -------------------------------



## Slide 62 -------------------------------

ggplot(data=iris)+
  geom_point(mapping = aes(x=Sepal.Length,y=Petal.Length,
                           color = Species))+
  geom_smooth(mapping=aes(x=Sepal.Length,y=Petal.Length,
                          color =Species))

## Slide 64 -------------------------------

ggplot(data=iris)+
  geom_point(mapping = aes(x=Sepal.Length,y=Petal.Length,
                           color = Species))+
  geom_smooth(mapping=aes(x=Sepal.Length,y=Petal.Length,
                          color =Species),
              method=lm,se=FALSE,fullrange=TRUE)

## Slide 66 -------------------------------

# Define function that extracts intercept and slope 
slopes <- function(df) {
  return(coef(lm(Petal.Length~Sepal.Length,data=df)))
}
# Define dataframe of the slopes
line_data <- data.frame(t(sapply(split(iris,iris$Species),
                                 slopes)),
                        Species=levels(iris$Species))
# ggplot
ggplot(data=iris)+
  geom_point(mapping = aes(x=Sepal.Length,y=Petal.Length,
                           colour = Species))+
  geom_abline(data=line_data,aes(slope=Sepal.Length,
                                 intercept=X.Intercept.,
                                 colour= Species))

## Slide 69 -------------------------------

wtid <- read.csv("wtid-report.csv", as.is = TRUE)
wtid <- wtid[, c("Year", "P99.income.threshold","P99.5.income.threshold", "P99.9.income.threshold")]
names(wtid) <- c("Year", "P99", "P99.5", "P99.9") 
head(wtid)

## Slide 70 -------------------------------

n <- length(wtid$Year)
wtid.new <- data.frame(Year=rep(wtid$Year,3),
                       IncomeLevels=c(wtid$P99,
                                      wtid$P99.5,
                                      wtid$P99.9),
                       Percentile=c(rep("P99",n),
                                    rep("P99.5",n),
                                    rep("P99.9",n))
)
ggplot(data = wtid.new) +
  geom_line(mapping = aes(x = Year, y = IncomeLevels,
                          color=Percentile))+
  labs(title = "Thresholds for the Richest People", 
       x = "Year", y = "Threshold Amount",
       color="Percentile")

