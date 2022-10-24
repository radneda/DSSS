### ----------------------------------------------------------------------------
### --- DataKolektiv DATA SCIENCE SUMMER SCHOOL 2022
### --- MACHINE LEARNING IN R
### --- Week 4. Introduction to R for ML
### --- author: Goran S. Milovanović, Phd
### --- DataKolektiv, Chief Scientist/Owner
### --- DataKolektiv, 2022.
### --- script: 13_boosting_XGBoost.R
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
data_set$CustomerId <- NULL
data_set$Surname <- NULL
data_set$RowNumber <- NULL

### --- Setup
library(tidyverse)
library(xgboost)
library(pROC)

### --- Split: train vs test (80/20)
data_set$split <- runif(dim(data_set)[1], 0, 1) < .25
train_set <- data_set[data_set$split == 0, ]
test_set <- data_set[data_set$split == 1, ]

# - test matrix 
test_set$split <- NULL
Exited_test <- test_set$Exited
test_set <- model.matrix(Exited ~ ., data = test_set)[ , -1]


### --- test_set to {xgboost} DMatrix
test_set <- xgb.DMatrix(test_set, label = Exited_test)
dim(test_set)

# - Downsampling and Upscaling
t <- table(train_set$Exited)
print(t)
# - use only 25% of negative (0) samples
# - downsampling factor
p_choice <- .25
downsample_factor <- p_choice * sum(Exited_train == 0)
print(paste0("p_choice is: ", p_choice))
print(paste0("downsample_factor is: ", downsample_factor))
# - downsample
train_set$select <- 0
train_set$select[train_set$Exited == 1] <- 1
train_set$select[train_set$Exited == 0] <- 
  rbinom(sum(train_set$Exited == 0), 1, p = p_choice)
table(train_set$select[train_set$Exited == 0])
table(train_set$select[train_set$Exited == 1])
train_set <- train_set[train_set$select == 1, ]
train_set$select <- NULL

# - train matrix 
train_set$split <- NULL
Exited_train <- train_set$Exited
train_set <- model.matrix(Exited ~ ., data = train_set)[ , -1]

### --- train_set to {xgboost} DMatrix
train_set <- xgb.DMatrix(train_set, label = Exited_train)
dim(train_set)

### --- CV Parameters

# - nthread
cv_nthread <- parallel::detectCores() - 1
print(paste0("Using: ", cv_nthread,  ' cores.'))

# - colsample_bytree
cv_colsample_bytree <- .75
print(paste0("Using: ", cv_colsample_bytree,  ' colsample_bytree.'))

# - eta
cv_eta <- c(.01, .1, .2)
print(paste0(paste0("Set eta as: "), 
             paste(cv_eta, collapse = ", ", sep = "")))

# - nrounds
cv_nrounds <- c(10000, 10000, 10000, 10000)
print(paste0(paste0("Set n_rounds as: "), 
             paste(cv_nrounds, collapse = ", ", sep = ""), 
             " - NOTE: respective of eta."))  # - max_depth

# - max_depth
cv_max_depth <- c(5, 10)
print(paste0(paste0("Set max_depth as: "), 
             paste(cv_max_depth, collapse = ", ", sep = "")))

# - sub_sample
cv_sub_sample <- seq(.25, .5, .75)
print(paste0(paste0("Set sub_sample as: "), 
             paste(cv_sub_sample, collapse = ", ", sep = "")))


print("-----------------------------------------------------------------------")
print("RUN XGBOOST w. LOGISTIC LOSS CROSS-VALIDATION NOW:")
print("-----------------------------------------------------------------------")
print("Prepare list to store data now.")
cv_list_length <- length(cv_eta)*length(cv_max_depth)*length(cv_sub_sample)
print(paste0("Will estimate ", cv_list_length, " models."))
cv_list <- vector(mode = "list", length = cv_list_length)
print("-----------------------------------------------------------------------")
print("FIRE:.")
print("-----------------------------------------------------------------------")
# - count models
num_models <- 0
for(cv_eta_i in 1:length(cv_eta)) {
  
  for (cv_max_depth_i in 1:length(cv_max_depth)) {
    
    for (cv_sub_sample_i in 1:length(cv_sub_sample)) {
      
      num_models <- num_models + 1
      print("-----------------------------------------------------------------------")
      print(paste0("Running model: " , num_models, "/", cv_list_length, "."))
      print("XGBooost at: ")
      print(paste(
        paste0("eta = ", cv_eta[cv_eta_i]),
        paste0("max_depth = ", cv_max_depth[cv_max_depth_i]),
        paste0("sub_sample = ", cv_sub_sample[cv_sub_sample_i]),
        collapse = "", sep = "; "))
      print(paste0("Training begins: ", Sys.time()))
      print(paste0("n_rounds will be: ", cv_nrounds[cv_eta_i]))
      t1 <- Sys.time()
      res_boost <- xgb.train(
        data = train_set,
        watchlist = list(validation = test_set), 
        params = list(booster = "gbtree",
                      nthread = cv_nthread,
                      eta = cv_eta[cv_eta_i], 
                      max_depth = cv_max_depth[cv_max_depth_i], 
                      subsample = cv_sub_sample[cv_sub_sample_i], 
                      colsample_bytree = cv_colsample_bytree,
                      scale_pos_weight = downsample_factor,
                      objective = "binary:logistic"),
        nrounds = cv_nrounds[cv_eta_i],
        verbose = 0,
        print_every_n = 0,
        eval_metric = "logloss",
        early_stopping_rounds = NULL,
        maximize = NULL,
        save_period = NULL,
        save_name = NULL,
        xgb_model = NULL
      )
      print(paste0("Training ends: ", Sys.time()))
      training_time <- difftime(Sys.time(), t1, units = "mins")
      print(paste0("XGBoost CV took: ", training_time))
      print("-----------------------------------------------------------------------")
      # - feature importance
      print("Compute feature importance now.")
      importance <- xgb.importance(feature_names = colnames(train_set), 
                                   model = res_boost)
      # - ROC analysis
      print("ROC analysis now.")
      predTest <- predict(res_boost, test_set)
      accTest_05 <- mean(as.numeric(predTest > .5) == Exited_test)
      print(paste0("Accuracy at decision boundary of .5: ", round(accTest_05, 8)))
      predFrameTest <- data.frame(prediction = predTest,
                                  observed = Exited_test)
      rocFrameTest <- data.frame(prediction = predTest > .5,
                                 observed = Exited_test)
      TPR_05 <- sum(rocFrameTest$prediction == 1 & Exited_test == 1)/sum(Exited_test == 1)
      print(paste0("TPR (Hit) at decision boundary of .5: ", round(TPR_05, 8)))
      FPR_05 <- sum(rocFrameTest$prediction == 1 & Exited_test == 0)/sum(Exited_test == 0)
      print(paste0("FPR (FA) at decision boundary of .5: ", round(FPR_05, 8)))
      TNR_05 <- sum(rocFrameTest$prediction == 0 & Exited_test == 0)/sum(Exited_test == 0)
      print(paste0("TNR (CR) at decision boundary of .5: ", round(TNR_05, 8)))
      FNR_05 <- sum(rocFrameTest$prediction == 0 & Exited_test == 1)/sum(Exited_test == 1)
      print(paste0("FNR (Miss) at decision boundary of .5: ", round(FNR_05, 8)))
      
      # - compute AUC
      roc_obj <- roc(predFrameTest, 
                     response = observed, 
                     predictor = prediction,
                     ci = T)
      model_auc <- as.numeric(roc_obj$ci) 
      names(model_auc) <- c('AUC_CI_lower', 'AUC', 'AUC_CI_upper')
      # - compute accuracy across decision boundary == seq(.1, .9, .1) 
      model_acc <- sapply(seq(.1, .9, .1), function(x) {
        mean(as.numeric(predTest > x) == Exited_test)
      })
      names(model_acc) <- paste("ACC_db_", seq(.1, .9, .1))
      # - Model outputs
      print("ROC analysis now.")
      model_params <- unlist(res_boost$params)
      model_n_features <- res_boost$nfeatures
      names(model_n_features) <- 'num_feats'
      model_n_iters <- res_boost$niter
      names(model_n_iters) <- 'num_iters'
      # - Model log
      cv_list[[num_models]]$logFrame <- cbind(
        as.data.frame(t(model_auc)),
        as.data.frame(t(model_acc)),
        as.data.frame(t(model_params)),
        as.data.frame(t(model_n_features)),
        as.data.frame(t(model_n_iters))
        )
      # - Model feature importance
      cv_list[[num_models]]$featureImportance <- importance
      # - Model predictive frame
      cv_list[[num_models]]$predFrame <- predFrameTest
      # - done
      print("DONE.")
      print("-----------------------------------------------------------------------")
      
      
    } # - END cv_sub_sample_i loop
    
  } # - END cv_max_depth loop
  
} # - END cv_eta loop

### --- Inspect results
log_frame <- Reduce(rbind, lapply(cv_list, function(x) x$logFrame))

### --- Pick best model and ROC at decision thrashold = .5
# - pick up predictions
pred_frame <- cv_list[[2]]$predFrame
# - probability to response
pred_frame$prediction <- as.numeric(pred_frame$prediction > .5)
# - Hit rate at .5
TPR_05 <- sum(pred_frame$prediction == 1 & pred_frame$observed == 1)/sum(pred_frame$observed == 1)
print(paste0("TPR (Hit) at decision boundary of .5: ", round(TPR_05, 2)))
FPR_05 <- sum(pred_frame$prediction == 1 & pred_frame$observed == 0)/sum(pred_frame$observed == 0)
print(paste0("FPR (FA) at decision boundary of .5: ", round(FPR_05, 2)))
TNR_05 <- sum(pred_frame$prediction == 0 & pred_frame$observed == 0)/sum(pred_frame$observed == 0)
print(paste0("TNR (CR) at decision boundary of .5: ", round(TNR_05, 2)))
FNR_05 <- sum(pred_frame$prediction == 0 & pred_frame$observed == 1)/sum(pred_frame$observed == 1)
print(paste0("FNR (Miss) at decision boundary of .5: ", round(FNR_05, 2)))

print(paste0("Recall: ", round(TPR_05, 2)))
precision <- round(TPR_05/(TPR_05+FPR_05), 2)
print(paste0("Precision: ", precision))


