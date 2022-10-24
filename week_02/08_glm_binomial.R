
### ----------------------------------------------------------------------------
### --- DataKolektiv DATA SCIENCE SUMMER SCHOOL 2022
### --- MACHINE LEARNING IN R
### --- Week 1. Introduction to R for ML
### --- author: Goran S. Milovanović, Phd
### --- DataKolektiv, Chief Scientist/Owner
### --- DataKolektiv, 2022.
### --- script: 08_glm_binomial.R
### --- description: GLMs: Binomial Logistic Regressions
### --- with lm() in R.ss
### --- license: GPL-3.0 license
### --- https://www.gnu.org/licenses/gpl-3.0.html

### ----------------------------------------------------------------------------

### --- Setup
library(tidyverse)
library(ggrepel)
data_dir <- paste0(getwd(), "/_data/")

### --- Dataset
churn_data <- read.csv(paste0(data_dir, "churn_data.csv"), 
                       header = TRUE,
                       check.names = FALSE,
                       stringsAsFactors = FALSE)
churn_data <- churn_data[complete.cases(churn_data), ]
glimpse(churn_data)
model_frame <- churn_data %>% 
  select(Churn, tenure, MonthlyCharges, TotalCharges)
model_frame$Churn <- ifelse(model_frame$Churn == "Yes", 1, 0)
table(model_frame$Churn)

### --- Binomial Logistic Model
binomial_linear_model <- glm(Churn ~ tenure + MonthlyCharges + TotalCharges,
                             data = model_frame,
                             family = "binomial")
class(binomial_linear_model)
summary(binomial_linear_model)

# - coefficients
binomial_linear_model$coefficients
coefficients(binomial_linear_model)
# - important
coefs <- exp(binomial_linear_model$coefficients)
print(coefs)
# - exp(coefficients) --> delta odds

# - prediction
predictions <- predict(binomial_linear_model, 
                       newdata = model_frame, 
                       type = "response")
predictions <- as.numeric(predictions > .5)
table(predictions)
table(model_frame$Churn)
predictions <- data.frame(observation = model_frame$Churn,
                          prediction = predictions)
# - model accuracy
accuracy <- as.numeric(predictions$observation == predictions$prediction)
accuracy <- sum(accuracy)/length(accuracy)
print(accuracy)
# - model true positive rate (hit rate)
hit = as.numeric(predictions$observation == 1 & predictions$prediction == 1)
hit <- sum(hit)/sum(predictions$observation == 1)
print(hit)
# - model false positive rate (false alarm rate)
fa = as.numeric(predictions$observation == 0 & predictions$prediction == 1)
fa <- sum(fa)/sum(predictions$observation == 0)
print(fa)

# - decision criterion
dec_criterion <- seq(.01, .99, by = .01)
predictions <- predict(binomial_linear_model, 
                       newdata = model_frame, 
                       type = "response")
predictions_frame <- lapply(dec_criterion, function(x) {
  # - predictions
  preds <- as.numeric(predictions > x)
  preds <- data.frame(observation = model_frame$Churn,
                      prediction = preds)
  # - model accuracy
  accuracy <- as.numeric(preds$observation == preds$prediction)
  accuracy <- sum(accuracy)/length(accuracy)
  # - model true positive rate (hit rate)
  hit = as.numeric(preds$observation == 1 & preds$prediction == 1)
  hit <- sum(hit)/sum(preds$observation == 1)
  # - model false positive rate (false alarm rate)
  fa = as.numeric(preds$observation == 0 & preds$prediction == 1)
  fa <- sum(fa)/sum(preds$observation == 0)
  return(
    data.frame(hit, fa, accuracy, dec = x)
  )
})
roc_frame <- Reduce(rbind, predictions_frame)

# - ROC analysis
roc_frame$diff <- roc_frame$hit - roc_frame$fa
roc_frame$label <- ""
roc_frame$label[which.max(roc_frame$diff)] <- "Here!"
ggplot(data = roc_frame, 
       aes(x = fa, 
           y = hit, 
           label = label)) +
  geom_path(color = "blue") + 
  geom_abline(intercept = 0, slope = 1) +
  geom_text_repel(arrow = arrow(length = unit(0.06, "inches"),
                                ends = "last", 
                                type = "closed"), 
                  min.segment.length = unit(0, 'lines'),
                  nudge_y = .1) + 
  ggtitle("ROC analysis for the Binomial Regression Model") +
  xlab("True Negative (False Alarm) Rate") + ylab("True Positive (Hit) Rate") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = .5)) + 
  theme(panel.border = element_blank())

### --- model diagnostics

# - model log-likelihood
logLik(binomial_linear_model)

# - model deviance
binomial_linear_model$deviance

# - model deviance residuals
deviance_residuals <- residuals(binomial_linear_model,
                                type = "deviance")
sum(deviance_residuals^2) == binomial_linear_model$deviance

# - model deviance and model log-likelihood
sum(deviance_residuals^2) == -2 * as.numeric(logLik(binomial_linear_model))

# - Akaike Information Criterion
AIC(binomial_linear_model)
binomial_linear_model$aic
k = 3 # - how many predictors?
aic = -2*as.numeric(logLik(binomial_linear_model)) + 2*k
print(aic)

# - Comparison to a Null model
# - The follows the Chi-Square Distribution
binomial_linear_model$null.deviance
binomial_linear_model$deviance
dev_diff <- binomial_linear_model$null.deviance - binomial_linear_model$deviance
print(dev_diff)
dfs <- binomial_linear_model$df.null - binomial_linear_model$df.residual
print(dfs)
pchisq(dev_diff, dfs, lower.tail = FALSE)


### --- include categorical predictors
exp(binomial_linear_model$coefficients)
binomial_linear_model$aic
model_frame <- churn_data
model_frame$Churn <- ifelse(model_frame$Churn == "Yes", 1, 0)
model_frame$customerID <- NULL
binomial_linear_model <- glm(Churn ~ .,
                             data = model_frame,
                             family = "binomial")
exp(binomial_linear_model$coefficients)
binomial_linear_model$aic

### --- ROC for the full model

# - decision criterion
dec_criterion <- seq(.01, .99, by = .01)
predictions <- predict(binomial_linear_model, 
                       newdata = model_frame, 
                       type = "response")
predictions_frame <- lapply(dec_criterion, function(x) {
  # - predictions
  preds <- as.numeric(predictions > x)
  preds <- data.frame(observation = model_frame$Churn,
                      prediction = preds)
  # - model accuracy
  accuracy <- as.numeric(preds$observation == preds$prediction)
  accuracy <- sum(accuracy)/length(accuracy)
  # - model true positive rate (hit rate)
  hit = as.numeric(preds$observation == 1 & preds$prediction == 1)
  hit <- sum(hit)/sum(preds$observation == 1)
  # - model false positive rate (false alarm rate)
  fa = as.numeric(preds$observation == 0 & preds$prediction == 1)
  fa <- sum(fa)/sum(preds$observation == 0)
  return(
    data.frame(hit, fa, accuracy, dec = x)
  )
})
roc_frame <- Reduce(rbind, predictions_frame)
roc_frame$diff <- roc_frame$hit - roc_frame$fa
roc_frame$label <- ""
roc_frame$label[which.max(roc_frame$diff)] <- "Here!"
ggplot(data = roc_frame, 
       aes(x = fa, 
           y = hit, 
           label = label)) +
  geom_path(color = "red") + 
  geom_abline(intercept = 0, slope = 1) +
  geom_text_repel(arrow = arrow(length = unit(0.06, "inches"),
                                ends = "last", 
                                type = "closed"), 
                  min.segment.length = unit(0, 'lines'),
                  nudge_y = .1) + 
  ggtitle("ROC analysis for the Binomial Regression Model") +
  xlab("True Negative (False Alarm) Rate") + ylab("True Positive (Hit) Rate") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = .5)) + 
  theme(panel.border = element_blank())

