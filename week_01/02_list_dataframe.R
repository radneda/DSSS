
### ----------------------------------------------------------------------------
### --- DataKolektiv DATA SCIENCE SUMMER SCHOOL 2022
### --- MACHINE LEARNING IN R
### --- Week 1. Introduction to R for ML
### --- author: Goran S. Milovanović, Phd
### --- DataKolektiv, Chief Scientist/Owner
### --- DataKolektiv, 2022.
### --- script: 02_list_dataframe.R
### --- description: list and data.frame class
### --- license: GPL-3.0 license
### --- https://www.gnu.org/licenses/gpl-3.0.html

# - where am i?
getwd()
# - note: always use RStudio projects!
list.files(getwd())
list.files()

# - lists
a <- list("Belgrade", 2022, TRUE)
class(a)
length(a)
a[[1]]
a[[2]]
a[[3]]
class(a[[1]])
class(a[[2]])
class(a[[3]])

# - apply a function over a list
lapply(a, class)

# - it would work for a vector too
v1 <- seq(1, 10, by = 1)
v1 <- 1:10
v1
plus_one <- function(x) {
  return(x+1)
}
lapply(v1, plus_one)
# - but the result is a list
class(lapply(v1, plus_one))

# - to unlist
unlist(lapply(v1, plus_one))
class(unlist(lapply(v1, plus_one)))

# - or use sapply
sapply(v1, plus_one)

# - combine functions: that is R programming
sum(sapply(v1, plus_one))

# - lists can be nested
a <- list()
a[[1]] <- list(5, 7)
a[[2]] <- list(1, 3)
print(a)
a[[1]]
a[[1]][[1]]
a[[2]][[2]]

# - lists can be named
a <- list(first = list(first = 1, second = 2),
          second = list(first = "a", second = "b"))
print(a)
a$first$first
a$first$second
a$second$first
a$second$second
names(a)
names(a$first)
names(a$first) <- c("prva", "druga")
names(a$first)
a


# - data.frame
num <- c(1, 2, 3, 4)
city <- c("Paris", "Belgrade", "NYC", "Tokyo")
timezone <- c("CET", "CET", "EDT", "JST")
population <- c(2.23, 1.4, 8.83, 14)
cities <- data.frame(no = num,
                     city = city,
                     tz = timezone,
                     pop = population)
cities
str(cities)
# - works for lists
str(a)
str(cities)
# - access columns
cities$city
cities$tz
cities$pop
class(cities$city)
class(cities$pop)
# - acess rows
cities[1, ]
cities[1:2, ]
cities[1:2, 1]
cities[1:2, 1:2]

# - subsetting data.frame

which(cities$pop > 3)
cities[which(cities$pop > 3), ]

cities$score <- c(1, 3, 4, 7)
dim(cities)[2]
cities$score <- NULL

head(cities, 2)
tail(cities, 2)
colnames(cities)
colnames(cities)[1]
colnames(cities)[1] <- "redni_broj"
colnames(cities)

cities[1:2, c('city', 'tz')]
cities[1:2, c('city', 'pop')]

cities[ , 'pop']

mean(cities[, 'pop'])
cities$pop

mean(cities$pop)

paste0("tz_", cities$tz)
cities$tz <- paste0("tz_", cities$tz)
cities
cities[1:3, c(2, 4)]

# - principle
cities[c(1, 2, 3), c(2, 4)]

cities[cities$pop > 1.5, c(2, 4)]

# - find a column by a name
colnames(cities)
cities[ , grepl("^pop", colnames(cities))]

# built-in data.frame to practice: mtcars
data(mtcars)
print(mtcars)
dim(mtcars)

head(mtcars, 2)
tail(mtcars, 5)

colnames(mtcars)

colnames(mtcars)[1]
rownames(mtcars)

mtcars[1:2, c('mpg', 'wt')]

mtcars[1:10, c('hp', 'gear')]

class(mtcars[, 'hp'])
class(mtcars[, c('hp', 'gear')])

mean(mtcars[, 'gear'])

mtcars$mpg
mean(mtcars$mpg)

paste0("tz_", mtcars$carb) # implicit type conversion

mtcars$carb <- paste0("carb_", mtcars$carb)
mtcars
mtcars[1:3, c(2, 4)]
# - principle
mtcars[c(1, 2, 3), c(2, 4)]
mtcars[mtcars$hp > 100, c(2, 4)]
# - find a column by a name
colnames(mtcars)
mtcars[, grepl("gear", colnames(mtcars))]
sd(mtcars[, grepl("gear", colnames(mtcars))])


nasa_lista <- list(grad = list(ime = "Beograd",
                               populacija = "1.4M"),
                   drzava = list(ime = "Srbija",
                                 populacija = "6.9M"))
nasa_lista$drzava$populacija
nasa_lista$drzava$ime

nasa_lista[[1]]$populacija

m = matrix(1:20, 
           nrow=4)

m = apply(m, 1, median)

m










