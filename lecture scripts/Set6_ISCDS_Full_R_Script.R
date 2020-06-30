


## Slide 5

mode("d")
mode("cat, squirrel")


## Slide 6

mode(" ")
nchar(" "); nchar("  "); nchar("")


## Slide 8

length("cat, squirrel, hedgehog")
length(c("cat", "squirrel", "hedgehog")) 
nchar("cat, squirrel, hedgehog") # Not 25
nchar(c("cat", "squirrel", "hedgehog"))

## Slide 9

print("cat, squirrel")
cat("cat, squirrel")
x <- 6
y <- 7
cat("I have", x, "cats and", y, "hedgehogs as pets.")




## Slide 10

print("cat, \n squirrel")
cat("cat, \nsquirrel")
print("In R, an \"array\" is a multi-dimension matrix.")
cat("A group of hedgehogs is called an \"array\".")

## 11
print("Columbia\tUniversity")
cat("Columbia\tUniversity")
nchar("Columbia\tUniversity")

## Slide 13

phrase <- "Christmas Bonus"
substr(phrase, start = 8, stop = 12)
substr(phrase, start = 13, stop = 13) <- "g"
phrase



## Slide 14

fav_animals <- c("cat", "squirrel", "hedgehog")
substr(fav_animals, start = 1, stop = 2)
substr(fav_animals, nchar(fav_animals)-1, 
       nchar(fav_animals))
substr(fav_animals, start = 4, stop = 4)


## Slide 15


todo <- "Lecture, Lab, Homework"
strsplit(todo, split = ",")
strsplit(todo, split = ", ")



## Slide 16

todo <- "Lecture, Lab, Homework"
strsplit(c(todo, "Midterm, Final"), split = ",")
strsplit(c(todo,'Midterm, Final','Hello World'),split=',')

## Slide 17
# Check yourself 

fun <- c("Columbia","slumber party","sugarplum")
substr(fun,start = c(3,2,7), stop = c(5,4,10))

strsplit(fun,split="lum")

## Slide 18

paste("cat", "squirrel", "hedgehog")
paste("cat", "squirrel", "hedgehog", sep = ", ")
paste(c("cat", "squirrel", "hedgehog"), 1:3)
paste(c("cat", "squirrel", "hedgehog"), 1:2)

## Slide 19


paste(c("cat", "squirrel", "hedgehog"), "(", 1:3, ")")
paste(c("cat", "squirrel", "hedgehog"), "(", 1:3, ")", 
      sep = "")
paste(c("cat", "squirrel", "hedgehog"), " (", 1:3, ")", 
      sep = "")
paste(c("cat", "squirrel", "hedgehog"), " (", 1:3, ")", 
      sep = c("-", ";", ";")) #即使sep是一个vector，结果上只会显示第一个分隔符

## Slide 20


paste(c("cat", "squirrel", "hedgehog"), " (", 1:3, ")", 
      sep = "", collapse = "; ")






## Slide 21
# Check yourself

fun <- c("Columbia","slumber party","sugarplum")
paste(fun,' [',c(3,2,7),'-',c(5,4,9),']',sep='',collapse = '; ')
paste(fun," ","[",c(3,2,7),"-",c(5,4,9),"]",sep="",collapse="; ")



## Slide 24

# Note two weeks ago we used scan
setwd("~/Desktop/GR5206-DS/Course material")
HC2 <- scan("HonorCode2.txt", what ="")  
# scan() output is a vector with elements anything from the ﬁle separated by whitespace.
# By default, R expects the input of scan to be numeric data. 
# The argument what="" tells R that our data is a vector of character values.

head(HC2, 20)
str(HC2)

# Now we use readLines 

HC <- readLines("HonorCode2.txt") # HC is a vector with one element per line of the text
length(HC)
head(HC, 5)



## Slide 25

grep("students", HC)
grep("Students", HC)
head(grepl("students", HC), 15)

## Slide 26

grep("students", HC)
HC[grep("students", HC)]

## Slide 27

HC <- paste(HC, collapse = " ") # One long string
HC.words <- strsplit(HC, split = " ")[[1]] # List output
head(HC.words, 10)

## Slide 28

word_count <- table(HC.words)
word_count <- sort(word_count, decreasing = TRUE)
head(word_count, 10)


## Slide 30

head(word_count, 10)
tail(word_count, 10)


## Slide 31




# Check yourself

grep(";", HC.words)
HC.words[grep(";", HC.words)]

# Or 
grep(";", names(word_count))
names(word_count)[grep(";", names(word_count))]



## Slide 34-35

fav_animals <- "cat,squirrel, hedgehog,   octopus"
strsplit(fav_animals, split = ",")
strsplit(fav_animals, split = " ")
strsplit(fav_animals, split = ", ")


## Slide 39

grep("cat|dog", c("categorize", "work doggedly"))
grep("A|b", c("Alabama", "blueberry", "work doggedly"))

grep('[^ae]',c('ae','aerfs','ab','e'))


## Slide 46

head(word_count, 10)
tail(word_count, 10)

## Slide 47

HC <- readLines("HonorCode2.txt")
length(HC)
head(HC, 5)



## Slide 48

HC <- paste(HC, collapse = " ") # One long string
HC.words <- strsplit(HC, split=" ")[[1]] # Last Time
HC.words <- strsplit(HC, split="(\\s|[[:punct:]])+")[[1]]
head(HC.words, 10)

## Slide 52


quakes <- readLines("/Users/wangshuyuan/Desktop/GR5206-DS/Course material/NCEDC_Search_Results.html",warn = FALSE) 
head(quakes)

## Slide 53


tail(quakes)



## Slide 54

quakes[8:15]


## Slide 55

date_express <- "^[0-9]{4}/[0-9]{2}/[0-9]{2}"
head(grep(quakes, pattern = date_express))
head(grep(quakes, pattern = date_express, value = TRUE))

## Slide 56
grep(quakes, pattern = date_express,invert = TRUE, value = TRUE)


## Slide 57


# Check yourself

ID_express <- "[0-9]{12}$"
head(grep(quakes, pattern = ID_express))



## Slide 58


# Is there a match?
grep("a[a-z]", "Alabama")

# Information about the first match.
regexpr("a[a-z]", "Alabama")


## Slide 60

# Information on all matches.
gregexpr("a[a-z]", "Alabama")

# What are the matches?
regmatches("Alabama", gregexpr("a[a-z]", "Alabama"))

## Slide 61

quakes[11:15]
coord_exp <- "-?[0-9]+\\.[0-9]{4}"
full_exp <- paste(coord_exp, "\\s+", coord_exp, sep = "")


## Slide 62

head(grepl(quakes, pattern = full_exp), 15)
coord_log <- grepl(quakes, pattern = full_exp)
matches   <- gregexpr(pattern = full_exp,text = quakes[coord_log])
head(matches, 1)



## Slide 63

coords <- regmatches(quakes[coord_log], matches)
head(coords, 4)

## Slide 64

coords_split <- sapply(coords, strsplit, split="\\s+")
head(coords_split, 3)


## Slide 65

coords_mat <- matrix(unlist(coords_split), ncol = 2,byrow = TRUE)
colnames(coords_mat) <- c("Latitude", "Longitude")
head(coords_mat)


## Slide 66
library(maps)
map("world")
points(coords_mat[,"Longitude"],coords_mat[,"Latitude"],pch = 19,col = "red",cex = .5)






