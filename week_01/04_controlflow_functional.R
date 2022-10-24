
### ----------------------------------------------------------------------------
### --- DataKolektiv DATA SCIENCE SUMMER SCHOOL 2022
### --- MACHINE LEARNING IN R
### --- Week 1. Introduction to R for ML
### --- author: Goran S. Milovanović, Phd
### --- DataKolektiv, Chief Scientist/Owner
### --- DataKolektiv, 2022.
### --- script: 03_controlflow_functional.R
### --- description: control flow and functional programming in R
### --- license: GPL-3.0 license
### --- https://www.gnu.org/licenses/gpl-3.0.html

# - data_dir
data_dir <- paste0(getwd(), "/_data/")

# - elementary, dear Watson:
for (a in 1:10) print(a)

# - code blocks
for (i in 1:100) {
  print(sqrt(i))
}

# - function call in iterations
cities <- c('NYC', 'Belgrade', 'Rome', 'Berlin')
for (x in 1:length(cities)) {
  print(
    paste0("This is a large city: ", 
           cities[x])
  )
}

# - when to use for loops, and when not
numbers <- numeric()
for (i in 1:100) {
  numbers[i] <- i
}

# - it's a vectorized language, right?
numbers <- 1:100

# - look:
lF <- list.files(data_dir)
lF <- lF[grepl("data_chunk", lF)]
data_set <- lapply(paste0(data_dir, lF),
                   read.csv, header = T, stringsAsFactors = F)
data_set[[1]]
data_set[[2]]

# - slow with for loop:
# - list files
lF <- list.files(data_dir)
lF <- lF[grepl("data_chunk", lF)]
data_set <- list()
# - iterate:
for (i in 1:length(lF)) {
  data_set[[i]] <- read.csv(paste0(data_dir, lF[i]), 
                           header = T, 
                           stringsAsFactors = F)
}
data_set[[2]]

# - faster, if you plan the size of your
# - data structures:
# - list files
lF <- list.files(data_dir)
lF <- lF[grepl("data_chunk", lF)]
# - how many files?
num_files <- length(lF)
# - prepare a list to store the dataframes
data_set <- vector(mode = "list", length = num_files)
# - iterate:
for (i in 1:num_files) {
  data_set[[i]] <- read.csv(paste0(data_dir, lF[i]), 
                           header = T, 
                           stringsAsFactors = F)
}
data_set[[1]]

# - put them together w. functional Reduce()
data_set <- Reduce(rbind, data_set)

# - plan the size of your data structures
emptyList <- vector(mode = "list", length = 4)
emptyList

# - more loops: while
counter <- 0
while (counter <= 100) {
  counter <- counter + 1
  if (counter %% 10 == 0) {
    print(counter)
  }
}

a <- 1
repeat {
  a <- a + 1
  print(a)
  
  if (a > 90) break
}

# - decisions
num_rows <- dim(data_set)[1]
if (num_rows >= 10000) {
  print("data_set have more than 10,000 rows!")
} else {
  print("data_set is a very small dataset.")
}

# - nesting decisions
num_rows <- dim(data_set)[1]
num_cols <- dim(data_set)[2]
if (num_rows >= 100) {
  print("data_set have more than 100 rows!")
  if (num_cols > 10) {
    print("And it has more than ten columns!")
  } else {
    print("But it has less than ten columns!")
  }
} else {
  print("data_set is a very small dataset.")
  if (num_cols > 10) {
    print("And it has more than ten columns!")
  } else {
    print("But it has less than ten columns!")
  }
}

# - chain if... else
this_number <- 11
if (this_number > 10) {
  print("This number is less than ten...")
} else if (this_number < 5) {
  print("This number is less than five.")
}

# - switch
this_animal <- "dog"
switch(this_animal,
       "dog" = "It's a dog!",
       "elephant" = "It's an elephant!", 
       "cat" = "Meow!", 
       "tiger" = "A tiger? In Africa?")
this_animal <- 'cat'
switch(this_animal,
       dog = "It's a dog!",
       elephant = "It's an elephant!", 
       cat = "Meow!", 
       tiger = "A tiger? In Africa?")

# - code blocks in switch
some_expression = 'hey'
switch(some_expression, 
       hey = { 
         print(2 + 2)
         print('Hey!') 
       },
       hi = { 
         print(5 + 5)
         print('Hi!')
       },
       {
         print(6 * 3)
         print('Default case!')
       }
)

# - vectorized ifelse
ifelse(10 < 5, 
       "I do not understand basic arithmetics.", 
       "Ok I got at least that one right."
)
trues <- sample(c(TRUE, FALSE), 100, replace = TRUE)
print(trues)
ifelse(trues,
       print("Yes"),
       print("No")
)

rand_a <- runif(100, 0, 1)
ifelse(rand_a > .5, TRUE, FALSE)

### --- functional programming
# - functionals in R

# - lapply()
cities <- c("Paris", "Rome", "NYC", "Moscow", "Tokyo")
lapply(cities, function(x) {
  return(
    paste0("A big city: ", x)
  )
})
big_cities <- lapply(cities, function(x) {
  return(
    paste0("A big city: ", x)
  )
})
class(big_cities)
big_cities <- unlist(big_cities)
big_cities

# - or use sapply() instead:
sapply(cities, function(x) {
  return(
    paste0("A big city: ", x)
  )
})
big_cities <- sapply(cities, function(x) {
  return(
    paste0("A big city: ", x)
  )
})
class(big_cities)
big_cities
names(big_cities)
big_cities <- unname(big_cities)
big_cities

# - mapply
a <- 1:10
b <- 11:20
mapply("+", a, b)

v1 <- c(1, 2, 3, 4, 5)
v2 <- c(2, 4, 1, 2, 10)
mapply(max, v1, v2)

# - mapply() is to Map() what sapply() is to lapply()
Map(max, v1, v2)

# - apply, for matrices
mat <- matrix(1:9, nrow = 3)
print(mat)
apply(mat, 1, mean)
apply(mat, 2, var)

mat <- matrix(c(3, 1, 9, 3, 4, 1, 0, 0, 9), 
              ncol = 3)

# - Reduce
Reduce("+", 1:6)
Reduce("+", 1:6, accumulate = TRUE)

# - lapply() + Reduce()
data_dir <- paste0(getwd(), "/_data/")
lF <- list.files(data_dir)
lF <- lF[grepl("data_chunk", lF)]
lF
# - read all with lapply():
data <- lapply(lF, function(x) {
  read.csv(paste0(data_dir, x), 
           header = TRUE,
           check.names = FALSE,
           row.names = 1,
           stringsAsFactors = FALSE)
})
data[[1]]
data[[2]]
dataset <- Reduce(rbind, data)
