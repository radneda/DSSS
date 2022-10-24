### ----------------------------------------------------------------------------
### --- DataKolektiv DATA SCIENCE SUMMER SCHOOL 2022
### --- MACHINE LEARNING IN R
### --- Week 4. Introduction to R for ML
### --- author: Goran S. Milovanović, Phd
### --- DataKolektiv, Chief Scientist/Owner
### --- DataKolektiv, 2022.
### --- script: 12_bagging_random_forest.R
### --- description: Random Forest in R
### --- license: GPL-3.0 license
### --- https://www.gnu.org/licenses/gpl-3.0.html
### ----------------------------------------------------------------------------

### --- Data
# - We will continue to use the Kaggle Bank Customer Churn Prediction competition 
# - dataset. You will need to download the Churn_Modelling.csv data file 
# - into your _data directory. 
# - The task is to predict the Exited variable - a churn prediction problem.
# - Kaggle: https://www.kaggle.com/code/kmalit/bank-customer-churn-prediction/data
data_dir <- paste0(getwd(), "/_data/")
# - dataset: Churn_Modelling.csv
data_set <- read.csv(paste0(data_dir, "Churn_Modelling.csv"), 
                     header = TRUE,
                     check.names = FALSE,
                     stringsAsFactors = FALSE)
data_set$Exited <- factor(data_set$Exited)
data_set$CustomerId <- NULL
data_set$Surname <- NULL
data_set$RowNumber <- NULL

### --- Setup
library(tidyverse)
library(randomForest)
library(snowfall)
library(pROC)

### --- Class Weights
class_weights <- as.numeric(table(data_set$Exited))
class_weights <- class_weights/sum(class_weights)

### --- Parameters
# - ntree
ntree <- seq(100, 500, by = 100)
print(ntree)
# - mtry
mtry <- 3:10
print(mtry)
# - folds
folds <- 1:3
print(folds)
# - CV Design
cv_design <- expand.grid(ntree, mtry, folds)
colnames(cv_design) <- c('ntree', 'mtry', 'fold')
cv_design <- apply(cv_design, 1, function(x) {
  list(ntree = x[1],
       mtry = x[2],
       fold = x[3])
})

### --- Prepare folds in data_set
data_set$ix <- sample(1:length(folds), dim(data_set)[1], replace = T)
table(data_set$ix)

### --- Prepare parallelization
num_cores <- parallel::detectCores()
# - Init Cluster
sfInit(parallel = TRUE, cpus = num_cores-1)
# - Export Libraries to Cluster
sfLibrary(randomForest)
sfLibrary(pROC)
# - Export Data to Cluster
sfExport("data_set")
sfExport("class_weights")
sfExport("folds")

### --- Run Cross-Validation

# - measuring point: start
tstart <- Sys.time()

# - FIRE!
rfModels <- sfClusterApplyLB(cv_design, function(x) {
  
  # - pick up parameters from x
  ntree <- x$ntree
  mtry <- x$mtry
  fold <- x$fold
  
  # - split training and test sets
  test_ix <- fold
  train_ix <- setdiff(folds, test_ix)
  train_set <- data_set[data_set$ix %in% train_ix, -12]
  test_set <- data_set[!(data_set$ix %in% train_ix), -12]
  
  # - Random Forest model
  model <- randomForest::randomForest(formula = Exited ~ .,
                                      data = train_set,
                                      ntree = ntree,
                                      mtry = mtry,
                                      classwt = class_weights)
  
  # - predict on test_set
  predictions <- predict(model,
                         newdata = test_set, 
                         type = "response")
  
  # - ROC analysis
  hit <- sum(predictions == 1 & test_set$Exited == 1)
  hit <- hit/sum(test_set$Exited == 1)
  fa <- sum(predictions == 1 & test_set$Exited == 0)
  fa <- fa/sum(test_set$Exited == 0)
  miss <- sum(predictions == 0 & test_set$Exited == 1)
  miss <- miss/sum(test_set$Exited == 1)
  cr <- sum(predictions == 0 & test_set$Exited == 0)
  cr <- cr/sum(test_set$Exited == 0)
  acc <- sum(predictions == test_set$Exited)
  acc <- acc/length(test_set$Exited)
  # - AUC
  roc_set <- data.frame(response = as.integer(test_set$Exited),
                        predictor = as.integer(predictions))
  roc_obj <- roc(roc_set,
                 response = response,
                 predictor = predictor)
  auc <- as.numeric(roc_obj$auc) 
  
  # - Output:
  return(
    data.frame(ntree = ntree, 
               mtry = mtry,
               fold = fold,
               hit = hit,
               fa = fa,
               miss = miss, 
               cr = cr,
               acc = acc,
               AUC = auc))
})

# - Stop Cluster
sfStop()

# - Report timing:
print(paste0("The estimation took: ", 
             difftime(Sys.time(), tstart, units = "mins"), 
             " minutes."))

# - Collect results
rfModels <- Reduce(rbind, rfModels)

# - Select model
rfModels_AUC <- rfModels %>% 
  dplyr::select(ntree, mtry, AUC) %>% 
  dplyr::group_by(ntree, mtry) %>% 
  dplyr::summarise(mean_AUC = mean(AUC)) %>% 
  dplyr::arrange(desc(mean_AUC))

rfModels %>% 
  dplyr::filter(ntree == rfModels_AUC$ntree[1] & mtry == rfModels_AUC$mtry[1])

