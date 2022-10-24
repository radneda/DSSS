
### ----------------------------------------------------------------------------
### --- DataKolektiv DATA SCIENCE SUMMER SCHOOL 2022
### --- MACHINE LEARNING IN R
### --- Week 1. Introduction to R for ML
### --- author: Goran S. Milovanović, Phd
### --- DataKolektiv, Chief Scientist/Owner
### --- DataKolektiv, 2022.
### --- script: 04_IO_operations.R
### --- description: elementary I/O in R
### --- license: GPL-3.0 license
### --- https://www.gnu.org/licenses/gpl-3.0.html

# - the data are found in the /_data directory
# - relative to the project path:
getwd()
list.dirs()
data_dir <- paste0(getwd(), "/_data/")
# - now we have the _data directory path

# - read.csv() to read in a .csv file format
# - into an R data.frame
air_quality_data <- read.csv(paste0(data_dir, "AirQualityUCI.csv"), 
                             header = TRUE,
                             sep = ";",
                             check.names = FALSE, 
                             stringsAsFactors = FALSE)
# - sep = ";" - to be discussed in the classroom
head(air_quality_data, 10)

# let's load another one (a pure `.csv`)
# - there is an in-built mtcars dataset, look:
head(mtcars)
head(iris)
# - write.csv mtcars to our data_dir
write.csv(mtcars, 
          paste0(data_dir, "mtcars.csv"))
# - load mtcars.csv into a data.frame
mtcats_data <- read.csv(paste0(data_dir, "AirQualityUCI.csv"),
                        header = TRUE,
                        sep = ";",
                        check.names = FALSE,
                        stringsAsFactors = FALSE)
# - sep = ";" - to be discussed in the classroom
head(air_quality_data, 10)

# - load several .csv files and put them together 
# - in one data.frame
# - first, let's produce the data
data_sets <- list() 
for (i in 1:4) {
  dat <- data.frame(measure_A = runif(100, 0, 100),
                    measure_B = runif(100, 0, 500), 
                    code = sample(letters, 100, replace = TRUE), 
                    month = sample(month.name, 100, replace = TRUE),
                    stringsAsFactors = FALSE)
  data_sets[[i]] <- dat
}
# - inspect
data_sets[[1]]

# - write all elements of data_sets as separate .csv files
lapply(data_sets, function(x) {
  filename = paste0("data_chunk_", round(runif(1, 1, 100), 0), ".csv")
  write.csv(x, paste0(data_dir, "/", filename))
})

# read; first we need to recognize the files that we need
lF <- list.files(data_dir)
lF <- lF[grepl("data_chunk", lF)]
print(lF)
collect_set <- lapply(lF, function(x) {
  read.csv(paste0(data_dir, x), 
           header = TRUE, 
           check.names = FALSE,
           row.names = 1,
           stringsAsFactors = FALSE)
})
# - check
length(collect_set)
class(collect_set)
collect_set[[1]]
# - put them together
final_data_set <- Reduce(rbind, collect_set)
head(final_data_set, 50)
dim(final_data_set)

library(data.table)

fdf <- rbindlist(collect_set)

collect_set[[1]]



