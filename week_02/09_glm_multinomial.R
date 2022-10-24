
### ----------------------------------------------------------------------------
### --- DataKolektiv DATA SCIENCE SUMMER SCHOOL 2022
### --- MACHINE LEARNING IN R
### --- Week 1. Introduction to R for ML
### --- author: Goran S. Milovanović, Phd
### --- DataKolektiv, Chief Scientist/Owner
### --- DataKolektiv, 2022.
### --- script: 09_glm_multinomial.R
### --- description: Linear and Multiple Linear Regression
### --- with lm() in R.ss
### --- license: GPL-3.0 license
### --- https://www.gnu.org/licenses/gpl-3.0.html

### ----------------------------------------------------------------------------

### --- Setup
library(tidyverse)
library(nnet)
data_dir <- paste0(getwd(), "/_data/")

### --- Iris dataset
data(iris)
head(iris)
glimpse(iris)

### --- Multinomial Regression model for Iris
### --- predict Species

# - set 'versicolor' as baseline
iris$Species <- relevel(iris$Species,
                        ref = 'versicolor')

# - model w. nnet package
mnr_model <- nnet::multinom(Species ~ .,
                            data = iris)

# - model summary
summary(mnr_model)

# - model coefficients
exp(coefficients(mnr_model))

### --- Multicolinearity in Multinomial Regression

# - step 1: recode your categorical outcome variable
# - just in order to be able to run a multiple linear regression
# - on your data:
iris$Species <- dplyr::recode(iris$Species,
                              versicolor = 0,
                              virginica = 1,
                              setosa = 2)
# - step 2: produce a Multiple Linear Regression model
# - for your data with a recoded outcome
mlr_model <- lm(Species ~ ., 
                data = iris)
# - step 3: use vif() from the car package
# - to compute VIF for your predictors:
library(car)
vif(mlr_model)
