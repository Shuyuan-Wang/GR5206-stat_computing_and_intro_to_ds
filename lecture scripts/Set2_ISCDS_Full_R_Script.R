

####################################
######  Begin Document 
####################################



## Slide 5 -------------------------------

data <- rep(c("Control","Treatment"),c(3,4))
data # A character vector
group <- factor(data)
group

## Slide 6 -------------------------------

str(group)
mode(group) # Numeric?
summary(group)


## Slide 7 -------------------------------

group
ages <- c(20, 30, 40, 35, 35, 35, 35)
sex <- c("M", "M", "F", "M", "F", "F", "F")


## Slide 8 -------------------------------

split(ages, list(group, sex))


## Slide 9 -------------------------------

group
table(group)

## Slide 10 -------------------------------

table(sex, group)


## Slide 11 -------------------------------

new_table <- table(sex, group)
new_table[, "Control"]
round(new_table/length(group), 3) # Gives proportions

## Slide 14 -------------------------------


Name <- c("John", "Jill", "Jacob", "Jenny")
Year <- c(1,1,2,4)
Grade <- c("B", "A+", "B-", "A")
student_data <- data.frame(Name, Year, Grade, 
                           stringsAsFactors = FALSE)
str(student_data)

## Slide 15 -------------------------------

student_data
dim(student_data)


## Slide 16 -------------------------------

str(student_data)
summary(student_data)

## Slide 17 -------------------------------

library(datasets)
states <- data.frame(state.x77, Region = state.region, 
                     Abbr = state.abb)
head(states, 2)

## Slide 18 -------------------------------

student_data
student_data[3:4,]

## Slide 19 -------------------------------

student_data
student_data$Grade


## Slide 20 -------------------------------

states["New York", ] # Can also use rownames


## Slide 21 -------------------------------


student_data[student_data$Grade == "A+", ]
student_data[student_data$Year <= 2, ]
states[states$Region == "Northeast", "Population"]


## Slide 22 -------------------------------

new_stu <- data.frame(Name="Bobby", Year=3, Grade="A")
student_data <- rbind(student_data, new_stu)
student_data


## Slide 23 -------------------------------

student_data$School <- "Columbia"
student_data



## Slide 32 -------------------------------

HC <- scan("HonorCode.txt", what = "")
head(HC, 20)
str(HC)


## Slide 37 -------------------------------


HC <- scan("HonorCode.txt", what = "")
head(HC, 15)

## Slide 39 -------------------------------

square_it <- function(x){
  out <- x*x
  return(out)
}

square_it(2); square_it(-4); square_it(146)


## Slide 40 -------------------------------

HC <- factor(HC, levels = unique(HC))



## Slide 41 -------------------------------

findwords <- function(text_vec){
  words <- split(1:length(text_vec), text_vec)
  return(words)
}

# Recall the split funciton 
split(1:20,rep(c("dog","cat"),10))



## Slide 42 -------------------------------

findwords(HC)[1:3]



## Slide 43 -------------------------------


HC[c(1, 48, 142, 204, 232, 310, 331)] # students

HC[c(2, 206)] # should

## Slide 44 -------------------------------


alphabetized_list <- function(wordlist) {
  nms <- names(wordlist) # The names are the words
  sorted <- sort(nms) # The words, but now in ABC order
  return(wordlist[sorted]) # Returns the sorted version
}

# Recall the names function
split(1:20,rep(c("dog","cat"),10))
names(split(1:20,rep(c("dog","cat"),10)))


## Slide 45 -------------------------------

wl <- findwords(HC)
alphabetized_list(wl)[1:3]


## Slide 49 -------------------------------

x <- c(5, 12, -3)
for (i in x) {
  print(i^2)
}


## Slide 52 -------------------------------

i <- 1
while (i <= 10) i <- i + 4

## Slide 55 -------------------------------

for (i in seq(4)) {
  if (i %% 2 == 0) {print(log(i))}
  else {print("Odd")}
}

## Slide 56 -------------------------------

library(matlab)    
total <- 0
for (i in 1:10) {
  if(isprime(i)) {
    total <- total + i
  }
}

## Slide 59 -------------------------------

u <- c(1,2,3)
v <- c(10,-20,30)
c <- vector(mode = "numeric", length = length(u))

for (i in 1:length(u)) {
  c[i] <- u[i] + v[i]
}
c


## Slide 60 -------------------------------

c <- u + v
c

## Slide 62 -------------------------------

for (i in seq(4)) {
  if (i %% 2 == 0) {print(log(i))}
  else {print("Odd")}
}


ifelse(seq(4) %% 2 == 0, log(seq(4)), "Odd")



## Slide 63 -------------------------------

mat <- matrix(1:12, ncol = 6)
mat
colSums(mat) # Recall colSums() from lab.


## Slide 64 -------------------------------

colSums(mat) 
apply(mat, 2, sum)

apply(mat, 1, sum) # Calculates the row sums



## Slide 66 -------------------------------


vec1 <- c(1.1,3.4,2.4,3.5)
vec2 <- c(1.1,3.4,2.4,10.8)
not_robust <- list(vec1, vec2)  
lapply(not_robust, mean)


## Slide 67 -------------------------------


lapply(not_robust, median) #return a list
sapply(not_robust, median) #return a vector
unlist(lapply(not_robust,median)) #unlist a list to get a vector

## Slide 69 -------------------------------


wl[1:3] # wl for word list


## Slide 70 -------------------------------

freq_list <- function(wordlist) {
  freqs <- sapply(wordlist, length) # The frequencies
  return(wordlist[order(freqs)])
}


## Slide 71 -------------------------------

head(freq_list(wl), 3)



## Slide 72 -------------------------------

tail(freq_list(wl), 3)



## Slide 74 -------------------------------

group
ages <- c(20, 30, 40, 35, 35, 35, 35)
tapply(ages, group, mean)


## Slide 76 -------------------------------


# Run proc.time()
proc.time()

# Store third element as start.time 
start.time  <- proc.time()[3]

## Slide 77 -------------------------------

dice_mat <- matrix(sample(1:6,500000,replace=T),ncol=10)
dim(dice_mat)
head(dice_mat,4) 

## Slide 78 -------------------------------


start.time.1 <- proc.time()[3]
my_means <- rep(NA,nrow(dice_mat))
for (i in 1:nrow(dice_mat)) {
  my_means[i] <- mean(dice_mat[i,]) 
}
head(my_means)
# Print run-time
end.time.1 <- proc.time()[3]-start.time.1
end.time.1

## Slide 79 -------------------------------

start.time.2 <- proc.time()[3]
my_means <- NULL
for (i in 1:nrow(dice_mat)) {
  my_means[i] <- mean(dice_mat[i,]) 
}
head(my_means)
# Print run-time
end.time.2 <- proc.time()[3]-start.time.2
end.time.2


## Slide 80 -------------------------------

start.time.3 <- proc.time()[3]
my_means <- apply(dice_mat,1,mean)
head(my_means)
# Print run-time
end.time.3 <- proc.time()[3]-start.time.3
end.time.3

## Slide 81 -------------------------------

c(end.time.1,end.time.2,end.time.3)


