



levels(iris$Species)


iris$Species=="versicolor"
iris$Petal.Width <= 1.2


# A
sum(iris$Species=="versicolor" & iris$Petal.Width <= 1.2)

# B
tapply(iris$Petal.Length,iris$Species,mean)["setosa"]

mean(iris$Petal.Length[iris$Species=="setosa"])

# C
table(iris$Species[iris$Sepal.Width>=3])

# D
ifelse(iris$Species=="versicolor",1,0)

iris$Versi <- ifelse(iris$Species=="versicolor",1,0)

table(iris$Versi)

