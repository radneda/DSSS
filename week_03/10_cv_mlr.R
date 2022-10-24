### ----------------------------------------------------------------------------
### --- DataKolektiv DATA SCIENCE SUMMER SCHOOL 2022
### --- MACHINE LEARNING IN R
### --- Week 3. Introduction to R for ML
### --- author: Goran S. Milovanović, Phd
### --- DataKolektiv, Chief Scientist/Owner
### --- DataKolektiv, 2022.
### --- script: 10_cv_mlr.R
### --- description: CV and reguralization in Multiple Linear Regression
### --- with glmnet() and cv.glmnet()
### --- license: GPL-3.0 license
### --- https://www.gnu.org/licenses/gpl-3.0.html

### ----------------------------------------------------------------------------

### --- setup
library(tidyverse)
library(glmnet)

### --- data
data_dir <- paste0(getwd(), "/_data/")
data_set <- read.csv(paste0(data_dir, "kc_house_data.csv"), 
                     header = TRUE,
                     check.names = FALSE,
                     stringsAsFactors = FALSE)
dim(data_set)

### --- prepare for modeling
# - remove some redundant or problematic predictors
model_set <- data_set %>% 
  select(-all_of(c("id", "lat", "long", "date",
                   "yr_renovated")))
glimpse(model_set)

### --- categorical predictors
# - categorical predictors to factors:
categorical_set <- model_set %>% 
  select(zipcode, view)
model_set <- model_set %>% 
  select(-all_of(c("zipcode", "view")))
categorical_set$zipcode <- factor(categorical_set$zipcode)
categorical_set$view <- factor(categorical_set$view)
# - dummy coding for glmnet:
unique_view <- length(unique(categorical_set$view))
unique_zipcode <- length(unique(categorical_set$zipcode))
categorical_set <- model.matrix( ~ ., categorical_set)
# - remove intercept
categorical_set <- categorical_set[, -1]
dim(categorical_set)
dim(categorical_set)[2] == unique_view + unique_zipcode - 2
# - cbind categorical_set back to model_set
model_set <- cbind(model_set, 
                   categorical_set)

# - remove the outcome variable from the dataset:
y <- model_set$price
model_set$price <- NULL

### --- train and test data
X <- model_set
ix <- runif(length(y), 0, 1) > .5
X_train <- X[ix, ]
X_test <- X[!ix, ]
y_train <- y[ix]
y_test <- y[!ix]

### --- Elastic Net 2-fold cross-validation
# - CV parameter grid for alpha: Elastic Net mixing parameter
alpha <- seq(0, 1, by = .01)
# - run CV
cv_models <- lapply(alpha, function(x) {
  
  # - cross-validate: Lambda across X_train
  cv_model <- cv.glmnet(x = as.matrix(X_train), 
                        y = y_train,
                        alpha = x,
                        family = "gaussian")
  
  # - optimal lambda:
  w_min <- which(cv_model$lambda == cv_model$lambda.min)
  error <- cv_model$cvm[w_min]
  optimal_lambda = cv_model$lambda.min
  
  # - estimate from optimal lambda, alpha in CV
  cv_model_optimal <- glmnet(x = as.matrix(X_train), 
                             y = y_train,
                             alpha = x,
                             lambda = optimal_lambda,
                             family = "gaussian")
  
  predictions <- predict(cv_model_optimal, 
                         newx = as.matrix(X_test), 
                         s = optimal_lambda,
                         alpha = x)
  
  cv_error_1 <- sum((y_test - predictions)^2)
  
  # - switch train and test now
  
  # - cross-validate: Lambda across X_test
  cv_model <- cv.glmnet(x = as.matrix(X_test), 
                        y = y_test,
                        alpha = x,
                        family = "gaussian")
  
  # - optimal lambda:
  w_min <- which(cv_model$lambda == cv_model$lambda.min)
  error <- cv_model$cvm[w_min]
  optimal_lambda = cv_model$lambda.min
  
  # - estimate from optimal lambda, alpha in CV
  cv_model_optimal <- glmnet(x = as.matrix(X_test), 
                             y = y_test,
                             alpha = x,
                             lambda = optimal_lambda,
                             family = "gaussian")
  
  predictions <- predict(cv_model_optimal, 
                         newx = as.matrix(X_train), 
                         s = optimal_lambda,
                         alpha = x)
  
  cv_error_2 <- sum((y_train - predictions)^2)
  
  # - mean error from both folds
  cv_error <- mean(c(cv_error_1, cv_error_2))
    
  # - output
  return(
    list(alpha = x, 
         lambda = optimal_lambda, 
         error = cv_error)
  )
  
})

# - alpha vs model error
cv_frame <- data.frame(alpha = sapply(cv_models, function(x) x$alpha), 
                       cv_error = sapply(cv_models, function(x) x$error))

# - plot alpha vs model error
ggplot(cv_frame, 
       aes(x = alpha, 
           y = cv_error)) + 
  geom_path(group = 1) + geom_point(size = .1) +
  ggtitle("Elastic Net CV for Multiple Regression") +
  theme_bw() + 
  theme(panel.border = element_blank()) + 
  theme(plot.title = element_text(hjust = .5))
  
# - find optimal alpha
w_min_error <- which.min(cv_frame$cv_error)
print(alpha[w_min_error])
# - find optimal lambda for optimal alpha
w_min_alpha <- which(alpha == alpha[w_min_error])
print(alpha[w_min_alpha])
print(cv_models[[w_min_alpha]])
optimal_alpha <- alpha[w_min_alpha]
optimal_lambda <- cv_models[[w_min_alpha]]$lambda
print(optimal_lambda)

### - train optimal model
optimal_mlr <- glmnet(x = as.matrix(X), 
                      y = y,
                      alpha = optimal_alpha,
                      lambda = optimal_lambda,
                      family = "gaussian")

optimal_predictions <- predict(optimal_mlr, 
                               newx = as.matrix(X), 
                               s = optimal_lambda,
                               alpha = optimal_alpha)

plot_frame <- data.frame(observations = y,
                         predictions = optimal_predictions[, 1])
ggplot(plot_frame, 
       aes(x = predictions, 
           y = observations)) +
  geom_point(size = .5, color = "blue") +
  ggtitle("Elastic Net CV Model Predictions") +
  theme_bw() + 
  theme(panel.border = element_blank()) + 
  theme(plot.title = element_text(hjust = .5))
